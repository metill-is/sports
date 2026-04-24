#' Closing-Line Value (CLV) from lengjan-odds history + bets_log.
#'
#' For each settled (or pending) bet, the last scrape for that match × line
#' gives the closing odds o_T. CLV = o_T / o_t - 1, where o_t is the odds
#' column in bets_log.csv (placement odds). Averaged over many bets, CLV > 0
#' means we consistently beat the market — the sharpest skill signal there is
#' because it strips realised variance. See Knowledge/Betting Optimisation/
#' roadmap-2026-04-19.md Phase 4 for design rationale.
#'
#' Programmatic exports:
#'   normalise_odds_long(raw, market)  — wide → long per-outcome format
#'   compute_closing_odds(long)        — max(scraped_at) per key
#'   compute_clv(bets, closing_odds)   — left-join + clv column per bet
#'   summarise_clv(df, by)             — grouped mean_clv / n / sd_clv
#'   clv_report(sports_dir, ...)       — orchestrator
#'
#' CLI entry point:
#'   Rscript R/bets/clv_tracker.R

box::use(
  dplyr[
    tibble, bind_rows, group_by, summarise, arrange, desc, filter,
    mutate, ungroup, select, left_join, slice_max, all_of, pull, n
  ],
  tidyr[pivot_longer],
  readr[read_csv, cols, col_character, col_guess],
  rlang[syms],
  stats[sd],
  . / kelly_joint[parse_handicap]
)

# ─── shape normalisation ──────────────────────────────────────────────────

#' Empty long-odds shell with the canonical column set.
empty_long <- function() {
  tibble(
    date = as.Date(character(0)),
    home = character(0), away = character(0),
    market = character(0), outcome = character(0),
    line = numeric(0), odds = numeric(0),
    scraped_at = as.POSIXct(character(0), tz = "UTC")
  )
}

#' Normalise a wide lengjan-odds CSV into long format.
#'
#' Canonical long columns: date, home, away, market, outcome, line, odds,
#' scraped_at. The `outcome` draw/tie mapping converts Lengjan's "draw"
#' column name (`o_draw`) to `tie` for parity with Sports bets_log.csv.
#'
#' @param raw tibble loaded from e.g. odds_1x2.csv / odds_totals.csv / odds_handicap.csv
#' @param market one of "outcome" (1x2), "totals", "handicap"
#' @export
normalise_odds_long <- function(raw, market) {
  if (nrow(raw) == 0) {
    return(empty_long())
  }
  if (market == "outcome") {
    long <- raw |>
      pivot_longer(
        cols = all_of(c("o_home", "o_draw", "o_away")),
        names_to = "outcome", values_to = "odds",
        names_prefix = "o_"
      ) |>
      mutate(
        outcome = ifelse(.data$outcome == "draw", "tie", .data$outcome),
        line = NA_real_,
        market = "outcome"
      )
  } else if (market == "totals") {
    long <- raw |>
      pivot_longer(
        cols = all_of(c("o_over", "o_under")),
        names_to = "outcome", values_to = "odds",
        names_prefix = "o_"
      ) |>
      mutate(
        line = as.numeric(.data$limit),
        market = "totals"
      )
  } else if (market == "handicap") {
    parsed <- raw |>
      mutate(
        line = if (is.character(.data$change)) {
          parse_handicap(.data$change)
        } else {
          as.numeric(.data$change)
        }
      )
    long <- parsed |>
      pivot_longer(
        cols = all_of(c("o_home", "o_draw", "o_away")),
        names_to = "outcome", values_to = "odds",
        names_prefix = "o_"
      ) |>
      mutate(
        outcome = ifelse(.data$outcome == "draw", "tie", .data$outcome),
        market = "handicap"
      )
  } else {
    stop("unknown market: ", market)
  }
  long |>
    select(
      "date", "home", "away", "market", "outcome", "line", "odds",
      "scraped_at"
    )
}

# ─── closing-odds extraction ──────────────────────────────────────────────

#' Collapse a long odds-history tibble to one row per key with the latest
#' scrape. Rows where `scraped_at` is NA are kept as-is (no dedup possible).
#' @export
compute_closing_odds <- function(long) {
  if (nrow(long) == 0) {
    return(long)
  }
  long |>
    group_by(
      .data$date, .data$home, .data$away, .data$market, .data$outcome,
      .data$line
    ) |>
    slice_max(.data$scraped_at, n = 1, with_ties = FALSE) |>
    ungroup()
}

# ─── CLV ──────────────────────────────────────────────────────────────────

#' Per-bet CLV: left-join bets to closing odds, compute `clv = close/placed - 1`.
#'
#' Bets that don't find a matching closing row retain `clv = NA`.
#'
#' @param bets tibble with date_match, home, away, market, outcome, line, odds
#' @param closing_odds tibble with date, home, away, market, outcome, line, odds
#' @return `bets` unchanged plus `closing_odds` + `clv` columns
#' @export
compute_clv <- function(bets, closing_odds) {
  close <- closing_odds |>
    select(
      date_match = "date", "home", "away", "market", "outcome", "line",
      closing_odds = "odds"
    )
  bets |>
    left_join(
      close,
      by = c("date_match", "home", "away", "market", "outcome", "line")
    ) |>
    mutate(clv = .data$closing_odds / .data$odds - 1)
}

# ─── aggregation ──────────────────────────────────────────────────────────

#' Grouped CLV summary. NA CLV rows are dropped before averaging.
#' @param df tibble with a `clv` column plus grouping columns
#' @param by character vector of grouping columns (may be length 0)
#' @export
summarise_clv <- function(df, by = character(0)) {
  valid <- df[!is.na(df$clv), , drop = FALSE]
  inner <- function(d) {
    tibble(
      n = nrow(d),
      mean_clv = if (nrow(d) > 0) mean(d$clv) else NA_real_,
      sd_clv = if (nrow(d) > 1) sd(d$clv) else NA_real_
    )
  }
  if (length(by) == 0) {
    return(inner(valid))
  }
  valid |>
    group_by(!!!syms(by)) |>
    dplyr::group_modify(~ inner(.x)) |>
    ungroup() |>
    arrange(desc(.data$mean_clv))
}

# ─── orchestrator ─────────────────────────────────────────────────────────

#' Load all lengjan-odds market CSVs under `lengjan_odds_dir` and normalise
#' to the canonical long format, tagging each row with sport/country from
#' the subdirectory name (`{sport}_{country}/{market_file}.csv`).
#'
#' @export
load_odds_history_all <- function(lengjan_odds_dir) {
  sub <- list.dirs(lengjan_odds_dir, recursive = FALSE)
  out <- list()
  for (d in sub) {
    key <- basename(d)
    parts <- strsplit(key, "_", fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    sport <- parts[1]
    country <- paste(parts[-1], collapse = "_")
    market_files <- c(
      outcome = "odds_1x2.csv",
      totals = "odds_totals.csv",
      handicap = "odds_handicap.csv"
    )
    for (mkt in names(market_files)) {
      f <- file.path(d, market_files[[mkt]])
      if (!file.exists(f)) next
      raw <- tryCatch(
        read_csv(f, show_col_types = FALSE, progress = FALSE),
        error = function(e) NULL
      )
      if (is.null(raw) || nrow(raw) == 0) next
      long <- normalise_odds_long(raw, market = mkt)
      long$sport <- sport
      long$country <- country
      out[[length(out) + 1]] <- long
    }
  }
  if (length(out) == 0) {
    return(empty_long())
  }
  bind_rows(out)
}

#' Attach `line` column to a bets tibble according to market conventions.
#' - outcome: line = NA
#' - totals:   line = info (parsed numeric)
#' - handicap: line = info (parsed numeric, Lengjan strings already parsed by
#'                         lengjan-bets so values should be numeric already)
#' @export
prepare_bets_for_clv <- function(bets) {
  bets$info_num <- suppressWarnings(as.numeric(bets$info))
  bets$line <- ifelse(
    bets$market == "outcome", NA_real_,
    bets$info_num
  )
  bets$info_num <- NULL
  bets
}

#' End-to-end CLV report.
#'
#' Loads all bets_log.csv + lengjan-odds history under the repo, normalises
#' both, joins them, and returns per-bet + grouped summaries.
#'
#' @param sports_dir repo root containing `*/history/bets_log.csv`
#' @param lengjan_odds_dir path to the lengjan-odds/data directory
#' @return named list: per_bet, by_country, by_market, by_sport_country_market
#' @export
clv_report <- function(sports_dir = getwd(),
                       lengjan_odds_dir = file.path(
                         sports_dir, "..", "lengjan-odds", "data"
                       )) {
  if (!dir.exists(lengjan_odds_dir)) {
    message(
      "lengjan-odds dir not found at ", lengjan_odds_dir,
      "; returning empty"
    )
    return(list())
  }
  roi_mod <- local({
    env <- new.env()
    source(file.path(sports_dir, "R/bets/roi_report.R"), local = env)
    env
  })
  all_bets <- roi_mod$load_bets_logs(sports_dir)
  if (nrow(all_bets) == 0) {
    return(list())
  }
  bets <- prepare_bets_for_clv(all_bets)
  odds_long <- load_odds_history_all(lengjan_odds_dir)
  closing <- compute_closing_odds(odds_long)
  per_bet <- compute_clv(bets, closing)
  settled <- per_bet[!is.na(per_bet$win), , drop = FALSE]
  list(
    per_bet = per_bet,
    coverage = tibble(
      n_bets = nrow(per_bet),
      n_with_clv = sum(!is.na(per_bet$clv)),
      coverage_pct = 100 * sum(!is.na(per_bet$clv)) / nrow(per_bet)
    ),
    by_country = summarise_clv(settled, by = "country"),
    by_market = summarise_clv(settled, by = "market"),
    by_sport_country = summarise_clv(settled, by = c("sport", "country")),
    by_sport_country_market = summarise_clv(
      settled,
      by = c("sport", "country", "market")
    )
  )
}

# ─── CLI ──────────────────────────────────────────────────────────────────

if (sys.nframe() == 0L && identical(Sys.getenv("R_MAIN_CLV"), "")) {
  Sys.setenv(R_MAIN_CLV = "1")
  res <- clv_report()
  if (length(res) == 0) {
    cat("CLV report empty — no odds history or no bets.\n")
    quit(status = 0)
  }
  cat("=== Coverage ===\n")
  print(as.data.frame(res$coverage))
  cat("\n=== CLV by country ===\n")
  print(as.data.frame(res$by_country))
  cat("\n=== CLV by market ===\n")
  print(as.data.frame(res$by_market))
  cat("\n=== CLV by sport x country ===\n")
  print(as.data.frame(res$by_sport_country))
  cat("\n=== CLV by sport x country x market ===\n")
  print(as.data.frame(res$by_sport_country_market))
}

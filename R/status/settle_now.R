#!/usr/bin/env Rscript
#' Standalone Settlement — settle all leagues and output JSON results
#'
#' Lightweight alternative to `Rscript run.R --all --step settle` that
#' outputs structured JSON for Raycast integration.
#'
#' Usage:
#'   cd Sports/
#'   Rscript R/status/settle_now.R                    # Settle all leagues
#'   Rscript R/status/settle_now.R --dry-run           # Preview only (no writes)
#'   Rscript R/status/settle_now.R --league football_england  # One league
#'
#' Output: JSON to stdout with settlement results.
#'
#' JSON schema:
#'   {
#'     timestamp: ISO 8601,
#'     dry_run: bool,
#'     settled: [{ date_match, sport, country, sex, market, home, away,
#'                 outcome, odds, bet_amount, info, home_score, away_score,
#'                 win, pnl }],
#'     summary: { count, wins, losses, total_pnl },
#'     still_pending: int
#'   }

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(jsonlite)
})

sports_dir <- here::here()
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

# Auto-sync livesport-data (settlement needs fresh results).
# Path-scoped: only update data/ from origin. Hard-resetting the whole tree
# clobbers local edits to CLAUDE.md / config / etc silently.
ls_path <- file.path(dirname(sports_dir), "livesport-data")
if (dir.exists(file.path(ls_path, ".git"))) {
  branch <- trimws(system2("git", c("-C", ls_path, "rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE))
  system2("git", c("-C", ls_path, "fetch", "origin", "-q"), stdout = TRUE, stderr = TRUE)
  res <- system2("git", c("-C", ls_path, "checkout", paste0("origin/", branch), "--", "data/"),
    stdout = TRUE, stderr = TRUE
  )
  status <- attr(res, "status")
  if (is.null(status) || status == 0L) {
    message("Synced livesport-data data/")
  } else {
    message("livesport-data sync failed: ", paste(res, collapse = " "))
  }
}

# Parse --league flag
league_idx <- which(args == "--league")
league_filter <- if (length(league_idx) > 0 && league_idx < length(args)) {
  args[league_idx + 1]
} else {
  NULL
}

msg <- function(...) message(sprintf(...))

# Column name normalisation (same as settle.R)
col_map <- c(
  dagsetning = "date", dags = "date",
  heima = "home", gestir = "away",
  home_goals = "home_score", away_goals = "away_score",
  stig_heima = "home_score", stig_gestir = "away_score"
)

normalise_columns <- function(d) {
  for (old in names(col_map)) {
    new <- col_map[[old]]
    if (old %in% names(d) && !new %in% names(d)) names(d)[names(d) == old] <- new
  }
  d
}

# Settlement logic (same as settle.R::compute_settlement)
compute_settlement <- function(bets) {
  bets |>
    mutate(
      info_num = suppressWarnings(as.numeric(info)),
      win = case_when(
        market == "outcome" & outcome == "home" ~ home_score > away_score,
        market == "outcome" & outcome == "away" ~ away_score > home_score,
        market == "outcome" & outcome == "tie" ~ home_score == away_score,
        market == "handicap" & !is.na(info_num) & outcome == "home" ~
          (home_score - away_score) + info_num > 0,
        market == "handicap" & !is.na(info_num) & outcome == "away" ~
          (home_score - away_score) + info_num < 0,
        market == "handicap" & !is.na(info_num) & outcome == "tie" ~
          (home_score - away_score) + info_num == 0,
        market == "totals" & !is.na(info_num) & outcome == "over" ~
          (home_score + away_score) > info_num,
        market == "totals" & !is.na(info_num) & outcome == "under" ~
          (home_score + away_score) <= info_num
      ),
      pnl = ifelse(win, odds * bet_amount - bet_amount, -bet_amount)
    ) |>
    select(-info_num)
}


# ═══════════════════════════════════════════════════════════════════════════════
# Load livesport-data results (fresh from git, bypasses step_data processing)
# ═══════════════════════════════════════════════════════════════════════════════

ls_data_dir <- file.path(dirname(sports_dir), "livesport-data", "data")

#' Parse livesport "DD.MM. HH:MM" dates into Date
#' Season spans Aug-Dec (year N) and Jan-Jul (year N+1).
#' If current month is Jan-Jul, Aug-Dec dates belong to previous year.
parse_ls_date <- function(x) {
  m <- regmatches(x, regexpr("^\\d{2}\\.\\d{2}\\.", x))
  day <- as.integer(substr(m, 1, 2))
  mon <- as.integer(substr(m, 4, 5))
  cur_yr <- as.integer(format(Sys.Date(), "%Y"))
  cur_mon <- as.integer(format(Sys.Date(), "%m"))
  # If we're in Jan-Jul, Aug-Dec dates are from last year
  yr <- ifelse(mon >= 8 & cur_mon <= 7, cur_yr - 1L, cur_yr)
  as.Date(sprintf("%04d-%02d-%02d", yr, mon, day))
}

#' Load results directly from livesport-data for a given sport/country/sex
load_livesport_results <- function(sport, country, sex) {
  # Map sport names: pipeline uses "football", livesport uses "soccer"
  ls_sport <- if (sport == "football") "soccer" else sport
  base <- file.path(ls_data_dir, ls_sport, country, sex)
  if (!dir.exists(base)) {
    return(NULL)
  }

  divs <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  results <- lapply(divs, function(d) {
    f <- file.path(d, "results.csv")
    if (!file.exists(f)) {
      return(NULL)
    }
    tryCatch(
      {
        r <- read_csv(f,
          show_col_types = FALSE,
          col_types = cols(home_score = "i", away_score = "i")
        )
        if (!all(c("date", "home", "away", "home_score", "away_score") %in% names(r))) {
          return(NULL)
        }
        r |>
          filter(!is.na(home_score), !is.na(away_score)) |>
          mutate(date = parse_ls_date(date))
      },
      error = function(e) NULL
    )
  })
  bind_rows(Filter(Negate(is.null), results))
}

# ═══════════════════════════════════════════════════════════════════════════════
# Load leagues and settle
# ═══════════════════════════════════════════════════════════════════════════════

leagues_yml <- yaml::yaml.load(read_file(file.path(sports_dir, "config", "leagues.yml")))
leagues_yml$defaults <- NULL

# Filter to specific league if requested
if (!is.null(league_filter)) {
  leagues_yml <- leagues_yml[names(leagues_yml) == league_filter]
  if (length(leagues_yml) == 0) {
    cat(toJSON(list(error = paste("League not found:", league_filter)), auto_unbox = TRUE))
    quit(save = "no", status = 1)
  }
}

all_settled <- list()
total_pending <- 0L

for (league_key in names(leagues_yml)) {
  lcfg <- leagues_yml[[league_key]]
  if (!isTRUE(lcfg$has_bets)) next

  league_dir <- file.path(sports_dir, lcfg$dir)
  log_path <- file.path(league_dir, "history", "bets_log.csv")
  if (!file.exists(log_path)) next

  msg("Processing %s ...", league_key)

  log <- read_csv(log_path,
    show_col_types = FALSE,
    col_types = cols(info = "c", pnl = "d", win = "l")
  ) |>
    mutate(date_match = as.Date(date_match), info = as.character(info))

  # Normalise sex values
  sex_norm <- c(kk = "male", kvk = "female")
  log <- log |> mutate(sex = coalesce(sex_norm[sex], sex))

  league_unsettled <- log |> filter(is.na(win))
  if (nrow(league_unsettled) == 0) {
    msg("  No unsettled bets.")
    next
  }

  sexes <- if (is.list(lcfg$sex)) unlist(lcfg$sex) else lcfg$sex

  # Load results from processed data.csv, supplemented by fresh livesport-data
  all_results <- NULL
  for (sex in sexes) {
    # Primary: processed data.csv / results.csv
    for (candidate in c("data.csv", "results.csv")) {
      path <- file.path(league_dir, "data", sex, candidate)
      if (!file.exists(path)) next

      d <- tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) NULL)
      if (is.null(d)) next

      d <- normalise_columns(d)
      required <- c("date", "home", "away", "home_score", "away_score")
      if (!all(required %in% names(d))) next

      d <- d |>
        mutate(date = as.Date(date), sex = sex) |>
        select(date, home, away, home_score, away_score, sex)
      all_results <- bind_rows(all_results, d)
      break
    }

    # Supplement: fresh livesport-data (may have newer results not yet processed)
    ls_results <- tryCatch(
      load_livesport_results(lcfg$sport, lcfg$country, sex),
      error = function(e) NULL
    )
    if (!is.null(ls_results) && nrow(ls_results) > 0) {
      ls_results <- ls_results |>
        mutate(sex = sex) |>
        select(date, home, away, home_score, away_score, sex)
      all_results <- bind_rows(all_results, ls_results) |>
        distinct(date, home, away, sex, .keep_all = TRUE)
    }
  }

  if (is.null(all_results)) {
    msg("  No results data found.")
    total_pending <- total_pending + nrow(league_unsettled)
    next
  }

  # Join and settle
  matched <- league_unsettled |>
    left_join(all_results, by = c("date_match" = "date", "home", "away", "sex"))

  has_result <- matched |> filter(!is.na(home_score))
  no_result <- matched |> filter(is.na(home_score))
  total_pending <- total_pending + nrow(no_result)

  if (nrow(has_result) == 0) {
    msg("  %d unsettled, no results yet.", nrow(league_unsettled))
    next
  }

  settled <- compute_settlement(has_result) |>
    filter(!is.na(win)) |>
    select(
      date_match, sport, country, sex, market, home, away, outcome,
      odds, bet_amount, info, home_score, away_score, win, pnl
    )

  if (nrow(settled) == 0) next

  msg("  Settled %d bet(s). PnL: %.0f kr", nrow(settled), sum(settled$pnl))

  # Write back if not dry run
  if (!dry_run) {
    dedup_keys <- c(
      "date_match", "sport", "country", "sex", "market",
      "home", "away", "outcome", "info"
    )

    settled_slim <- settled |>
      select(all_of(dedup_keys), win_new = win, pnl_new = pnl)

    log <- log |>
      left_join(settled_slim, by = dedup_keys) |>
      mutate(
        win = coalesce(as.logical(win_new), as.logical(win)),
        pnl = coalesce(pnl_new, pnl)
      ) |>
      select(-win_new, -pnl_new)

    write_csv(log, log_path)
    msg("  Wrote %s", log_path)

    # Dual-write to Parquet store
    tryCatch(
      {
        source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)
        for (s in unique(log$sex)) {
          store_bets(log[log$sex == s, ], lcfg$sport, lcfg$country, sex = s, sports_dir)
        }
        store_bets(log, lcfg$sport, lcfg$country, sex = "all", sports_dir)
      },
      error = function(e) msg("  Store sync failed: %s", e$message)
    )
  }

  all_settled[[length(all_settled) + 1]] <- settled
}


# ═══════════════════════════════════════════════════════════════════════════════
# Output
# ═══════════════════════════════════════════════════════════════════════════════

combined <- if (length(all_settled) > 0) bind_rows(all_settled) else NULL

if (!is.null(combined) && nrow(combined) > 0) {
  summary_info <- list(
    count     = nrow(combined),
    wins      = sum(combined$win, na.rm = TRUE),
    losses    = sum(!combined$win, na.rm = TRUE),
    total_pnl = round(sum(combined$pnl, na.rm = TRUE))
  )
} else {
  combined <- list()
  summary_info <- list(count = 0L, wins = 0L, losses = 0L, total_pnl = 0)
}

result <- list(
  timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  dry_run = dry_run,
  settled = combined,
  summary = summary_info,
  still_pending = total_pending
)

cat(toJSON(result, auto_unbox = TRUE, pretty = TRUE, na = "null"))
cat("\n")

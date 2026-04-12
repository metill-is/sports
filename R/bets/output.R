#' Output utilities: display, bankroll computation, and ledger deduplication
#'
#' The Sports pipeline generates recommendations only — it never writes
#' to bets_log.csv. That is the exclusive responsibility of the bet placer
#' (lengjan-bets). See betting-system-rules.md.

box::use(
  dplyr[select, mutate, filter, anti_join, all_of, case_when, bind_rows, collect],
  tidyr[pivot_wider],
  readr[read_csv]
)

#' Compute current bankroll from initial pool and placed bet history
#'
#' All rows in bets_log.csv are placed bets (rule B1). Bankroll is:
#'   initial_pool + sum(settled PnL) - sum(unsettled bet amounts)
#'
#' @param initial_pool Starting bankroll amount
#' @param sports_dir Absolute path to Sports/ root
#' @return Current available bankroll
#' @export
compute_bankroll <- function(initial_pool, sports_dir, epoch = NULL) {
  filter_bets <- function(bets) {
    out <- bets |> dplyr::filter(sex != "all")
    if (!is.null(epoch)) {
      out <- out |> dplyr::filter(as.Date(date_recommended) >= as.Date(epoch))
    }
    out
  }

  # Try Parquet store first (faster for many leagues)
  store_path <- file.path(sports_dir, "store", "bets")
  if (dir.exists(store_path) && requireNamespace("arrow", quietly = TRUE)) {
    result <- tryCatch(
      {
        all_bets <- arrow::open_dataset(store_path) |>
          dplyr::collect() |>
          filter_bets()
        if (nrow(all_bets) > 0) {
          settled_pnl <- sum(all_bets$pnl[!is.na(all_bets$pnl)], na.rm = TRUE)
          outstanding <- sum(all_bets$bet_amount[is.na(all_bets$win)], na.rm = TRUE)
          initial_pool + settled_pnl - outstanding
        } else {
          NULL
        }
      },
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  # Fall back to CSV glob
  logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
  if (length(logs) == 0) {
    return(initial_pool)
  }

  all_bets <- do.call(rbind, lapply(logs, \(f) {
    read_csv(f, show_col_types = FALSE)
  })) |> filter_bets()

  if (nrow(all_bets) == 0) {
    return(initial_pool)
  }

  settled_pnl <- sum(all_bets$pnl[!is.na(all_bets$pnl)], na.rm = TRUE)
  outstanding <- sum(all_bets$bet_amount[is.na(all_bets$win)], na.rm = TRUE)

  initial_pool + settled_pnl - outstanding
}

#' Print a market's results in wide (display) format
#'
#' Pivots the outcome column into separate columns (e.g., home/tie/away).
#' Extra columns like `change` or `limit` are preserved automatically.
#'
#' @param results Tibble from a market module (long form with outcome column)
#' @param market_name Display label (e.g., "1x2", "Handicap", "Totals")
#' @export
print_market <- function(results, market_name) {
  cat("\n===", market_name, "===\n\n")
  if (is.null(results) || nrow(results) == 0) {
    cat("  No value bets found.\n")
    return(invisible(NULL))
  }

  # Keep only display-relevant columns before pivoting
  keep_cols <- intersect(
    names(results),
    c(
      "date", "division", "league", "booker", "heima", "gestir", "change", "limit",
      "outcome", "text"
    )
  )

  display <- results |>
    select(all_of(keep_cols)) |>
    pivot_wider(
      names_from = outcome,
      values_from = text,
      values_fill = ""
    )

  print(display, n = Inf)
  invisible(display)
}

#' Remove bets already in the ledger from recommendations
#'
#' Prevents re-recommending bets that have already been placed on Lengjan.
#' The ledger (bets_log.csv) is the source of truth for placed bets.
#'
#' @param results Tibble from a market module (or NULL)
#' @param cfg Config list (from bets.yml)
#' @param sport_dir Root directory of the sport project
#' @param sex Sex label
#' @param market Market label ("outcome", "handicap", "totals")
#' @param info_col Column in results holding the line info (e.g., "change", "limit")
#' @return Filtered results with already-placed bets removed
#' @export
dedup_against_log <- function(results, cfg, sport_dir, sex, market, info_col = NULL) {
  if (is.null(results) || nrow(results) == 0) {
    return(results)
  }
  if (!isTRUE(cfg$history$enabled)) {
    return(results)
  }

  log_path <- file.path(sport_dir, cfg$history$path, "bets_log.csv")
  if (!file.exists(log_path)) {
    return(results)
  }

  existing <- read_csv(log_path, show_col_types = FALSE) |>
    filter(market == !!market, sex == !!sex) |>
    mutate(info = replace(as.character(info), is.na(info), ""))

  if (nrow(existing) == 0) {
    return(results)
  }

  # Build info key from results to match against CSV's info column
  if (!is.null(info_col) && info_col %in% names(results)) {
    info_vals <- as.character(results[[info_col]])
  } else {
    info_vals <- rep(NA_character_, nrow(results))
  }
  info_vals <- replace(info_vals, is.na(info_vals), "")

  results$.dedup_info <- info_vals
  out <- anti_join(
    results, existing,
    by = c(
      "date" = "date_match", "heima" = "home", "gestir" = "away",
      "outcome", ".dedup_info" = "info"
    )
  )
  out$.dedup_info <- NULL

  n_removed <- nrow(results) - nrow(out)
  if (n_removed > 0) {
    cat("  (", n_removed, "already-placed", market, "bet(s) filtered out)\n")
  }

  out
}


#' Remove already-placed bets from combined recommendations
#'
#' Global dedup pass applied when writing recommendations.csv. Checks ALL
#' bets_log.csv files and removes any recommendations that have already been
#' placed. This catches stale recommendations carried forward from previous
#' pipeline runs of other leagues.
#'
#' @param recs Combined recommendations tibble
#' @param sports_dir Absolute path to Sports/ root
#' @return Filtered recommendations
#' @export
dedup_recommendations <- function(recs, sports_dir) {
  if (nrow(recs) == 0) {
    return(recs)
  }

  # Try Parquet store first
  store_path <- file.path(sports_dir, "store", "bets")
  all_bets <- NULL
  if (dir.exists(store_path) && requireNamespace("arrow", quietly = TRUE)) {
    all_bets <- tryCatch(
      arrow::open_dataset(store_path) |> collect() |> filter(sex != "all"),
      error = function(e) NULL
    )
  }

  # Fall back to CSV glob
  if (is.null(all_bets) || nrow(all_bets) == 0) {
    logs <- Sys.glob(file.path(sports_dir, "*", "*", "history", "bets_log.csv"))
    if (length(logs) == 0) {
      return(recs)
    }
    all_bets <- bind_rows(lapply(logs, \(f) read_csv(f, show_col_types = FALSE))) |>
      filter(sex != "all")
  }

  if (nrow(all_bets) == 0) {
    return(recs)
  }

  # Build info key: handicap → change, totals → limit, outcome → ""
  recs$.info <- case_when(
    recs$market == "handicap" ~ as.character(recs$change),
    recs$market == "totals" ~ as.character(recs$limit),
    TRUE ~ ""
  )
  recs$.info[is.na(recs$.info)] <- ""
  all_bets$.info <- replace(as.character(all_bets$info), is.na(all_bets$info), "")

  out <- anti_join(
    recs, all_bets,
    by = c("sport", "country", "sex",
      "date" = "date_match", "heima" = "home", "gestir" = "away",
      "market", "outcome", ".info"
    )
  )

  n_removed <- nrow(recs) - nrow(out)
  if (n_removed > 0) {
    cat(sprintf("  Global dedup: %d already-placed bet(s) removed.\n", n_removed))
  }

  out$.info <- NULL
  out
}

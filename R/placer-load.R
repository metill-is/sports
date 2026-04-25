#' @include storage.R
NULL

#' Load recommendations for placement.
#'
#' Reads `data/decisions/recommendations/` for the most recent `run_date`
#' partition (or `target_date` when supplied), filters to upcoming matches,
#' and optionally restricts to the league keys requested.
#'
#' @param root Data root.
#' @param leagues Optional character vector of league keys (e.g.
#'   `"football_iceland"`). NULL = all.
#' @param today_only Keep only matches today.
#' @param target_date Specific match_date to keep. Mutually exclusive with
#'   `today_only` and overrides the future-only filter.
#' @param run_date Specific run_date partition to read. NULL = most recent.
#' @return Tibble matching `schemas()$recommendations` (post-filter).
#' @export
load_recommendations <- function(root,
                                 leagues = NULL,
                                 today_only = FALSE,
                                 target_date = NULL,
                                 run_date = NULL) {
  # `read_table` already returns an empty tibble when the partition directory
  # is absent (storage.R:266), so the previous tryCatch was hiding genuine
  # read errors (corrupt Parquet, schema drift). Let real errors propagate.
  recs <- read_table("recommendations", root = root)
  if (nrow(recs) == 0L) {
    return(empty_recommendations_for_placement())
  }

  # Restrict to the requested run_date partition (or the most recent when
  # NULL). Without this filter the placer would consider stale Kelly sizes
  # from prior runs alongside the current run's recommendations.
  if (!is.null(run_date)) {
    recs <- recs[as.character(recs$run_date) == as.character(run_date), , drop = FALSE]
  } else if ("run_date" %in% names(recs) && nrow(recs) > 0L) {
    recs <- recs[recs$run_date == max(recs$run_date), , drop = FALSE]
  }

  if (!is.null(target_date)) {
    recs <- recs[recs$match_date == as.Date(target_date), , drop = FALSE]
  } else if (today_only) {
    recs <- recs[recs$match_date == Sys.Date(), , drop = FALSE]
  } else {
    recs <- recs[recs$match_date >= Sys.Date(), , drop = FALSE]
  }

  if (!is.null(leagues)) {
    keep <- paste0(recs$sport, "_", recs$country) %in% leagues
    recs <- recs[keep, , drop = FALSE]
  }

  recs
}

#' Anti-join recommendations against the placed-bet ledger.
#'
#' Removes any recommendation already represented in `data/decisions/ledger/`
#' (matched on sport, country, sex, match_date, home_team, away_team, market,
#' outcome, line). The placer is the only writer to the ledger, so this is
#' the canonical "haven't placed it yet" filter.
#'
#' @param recs Tibble from `load_recommendations()`.
#' @param root Data root.
#' @return Tibble of recommendations not yet placed.
#' @export
dedup_against_ledger <- function(recs, root) {
  if (nrow(recs) == 0L) {
    return(recs)
  }

  # An unreadable ledger is recoverable (worst case: re-attempt a duplicate
  # bet), but it's still a real fault that operators should see.
  led <- tryCatch(
    read_table("ledger", root = root),
    error = function(e) {
      cli::cli_warn(c(
        "dedup_against_ledger: ledger read failed; skipping dedup",
        "i" = "{conditionMessage(e)}"
      ))
      tibble::tibble()
    }
  )
  if (nrow(led) == 0L) {
    return(recs)
  }

  key_cols <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome", "line"
  )
  if (!all(key_cols %in% names(led))) {
    cli::cli_warn(
      "dedup_against_ledger: ledger missing key column(s); skipping dedup"
    )
    return(recs)
  }

  # dplyr::anti_join handles NA-equality on the join (two NA `line` values
  # are treated as matching), and is type-safe against future Date format
  # changes that would break the previous string-concat key.
  dplyr::anti_join(recs, led[, key_cols, drop = FALSE], by = key_cols)
}

#' @keywords internal
#' @noRd
empty_recommendations_for_placement <- function() {
  tibble::tibble(
    run_id = as.POSIXct(character(), tz = "UTC"),
    sport = character(), country = character(), sex = character(),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), p = numeric(), odds = numeric(),
    ev = numeric(), kelly = numeric(), bet_amount = numeric()
  )
}

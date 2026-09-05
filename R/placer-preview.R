#' @include placer-load.R
NULL

#' Preview pending recommendations without opening a browser.
#'
#' Loads recommendations from the unified Parquet store, dedups against the
#' ledger, prints a tabular summary via [cli::cli_*], and returns the
#' tibble for programmatic use. Never opens chromote — useful for
#' "what would I bet today?" without committing real money.
#'
#' @inheritParams load_recommendations
#' @return Tibble of pending bets (post-dedup), with columns matching
#'   [load_recommendations()] output.
#' @export
preview_pending <- function(leagues = NULL,
                            today_only = FALSE,
                            target_date = NULL,
                            root = here::here("data"),
                            leagues_cfg = NULL) {
  recs <- load_recommendations(
    root = root, leagues = leagues,
    today_only = today_only, target_date = target_date,
    leagues_cfg = leagues_cfg
  )
  if (nrow(recs) == 0L) {
    cli::cli_alert_info("No pending recommendations.")
    return(invisible(recs))
  }

  recs <- dedup_against_ledger(recs, root = root)
  if (nrow(recs) == 0L) {
    cli::cli_alert_info("All recommendations already placed.")
    return(invisible(recs))
  }

  cli::cli_h1("Pending recommendations ({nrow(recs)} total)")
  cli::cli_alert_info("Total stake: {sum(recs$bet_amount)} ISK")

  for (i in seq_len(nrow(recs))) {
    r <- recs[i, ]
    cli::cli_text(
      "  {.field {r$sport}/{r$country}/{r$sex}} ",
      "{r$home_team} vs {r$away_team} ({r$match_date}): ",
      "{r$market}/{r$outcome} @ {r$odds} for {r$bet_amount} ISK ",
      "(p={signif(r$p, 3)}, ev={signif(r$ev, 3)})"
    )
  }

  invisible(recs)
}

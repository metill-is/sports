#!/usr/bin/env Rscript
# scripts/place_bets.R — CLI entrypoint for the placer.
#
# Usage:
#   Rscript scripts/place_bets.R                # dry-run, all leagues
#   Rscript scripts/place_bets.R --live         # places (with per-bet confirm)
#   Rscript scripts/place_bets.R --live --no-confirm
#   Rscript scripts/place_bets.R --live --today
#   Rscript scripts/place_bets.R --live --league football_iceland
#   Rscript scripts/place_bets.R --live --show-browser

suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name) any(args == paste0("--", name))
get_arg <- function(name) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) NULL else args[[i + 1L]]
}

dry_run <- !get_flag("live")
interactive <- !get_flag("no-confirm")
today_only <- get_flag("today")
headless <- !get_flag("show-browser")
leagues <- get_arg("league")
target_date <- if (!is.null(get_arg("date"))) as.Date(get_arg("date")) else NULL

results <- place_bets(
  leagues     = leagues,
  dry_run     = dry_run,
  interactive = interactive,
  today_only  = today_only,
  target_date = target_date,
  headless    = headless
)

if (nrow(results) > 0L && "status" %in% names(results)) {
  cat("\n=== Results ===\n")
  cols_to_show <- intersect(
    c("home_team", "away_team", "market", "outcome", "odds", "bet_amount", "status"),
    names(results)
  )
  print(results[, cols_to_show])
}

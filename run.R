#!/usr/bin/env Rscript
#' Lengjan Bet Placement — Entry Point
#'
#' Usage:
#'   Rscript run.R                          # Dry run, all leagues
#'   Rscript run.R --live                   # Place bets (with per-bet confirmation)
#'   Rscript run.R --live --no-confirm      # Place bets (no confirmation prompts)
#'   Rscript run.R --league football_england  # Specific league only
#'   Rscript run.R --dry-run --league handball_iceland
#'
#' Environment:
#'   LENGJAN_USER and LENGJAN_PASS must be set in .Renviron

library(here)

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)

dry_run <- !("--live" %in% args)
interactive <- !("--no-confirm" %in% args)

league_idx <- which(args == "--league")
leagues <- if (length(league_idx) > 0 && league_idx < length(args)) {
  args[league_idx + 1]
} else {
  NULL
}

# Run the pipeline
box::use(R/pipeline[run_bets])

results <- run_bets(
  leagues = leagues,
  dry_run = dry_run,
  interactive = interactive,
  sports_dir = here("../Sports"),
  odds_dir = here("../lengjan-odds")
)

if (nrow(results) > 0) {
  cat("\n=== Results ===\n")
  print(results[, c("home", "away", "market", "outcome", "odds",
                     "bet_amount", "placement_status")])
}

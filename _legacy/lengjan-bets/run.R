#!/usr/bin/env Rscript
#' Lengjan Bet Placement — Entry Point
#'
#' Usage:
#'   Rscript run.R                          # Dry run, all leagues
#'   Rscript run.R --live                   # Place bets (with per-bet confirmation)
#'   Rscript run.R --live --no-confirm      # Place bets (no confirmation prompts)
#'   Rscript run.R --league football_england  # Specific league only
#'   Rscript run.R --dry-run --league handball_iceland
#'   Rscript run.R --live --today              # Only today's matches
#'   Rscript run.R --live --show-browser       # Watch Chrome click through (default: headless)
#'
#' Environment:
#'   LENGJAN_USER and LENGJAN_PASS must be set in .Renviron

library(here)

# Parse CLI arguments
args <- commandArgs(trailingOnly = TRUE)

today_only <- "--today" %in% args
dry_run <- !("--live" %in% args) && !today_only
interactive <- !("--no-confirm" %in% args) && !today_only
headless <- !("--show-browser" %in% args)

league_idx <- which(args == "--league")
leagues <- if (length(league_idx) > 0 && league_idx < length(args)) {
  args[league_idx + 1]
} else {
  NULL
}

date_idx <- which(args == "--date")
target_date <- if (length(date_idx) > 0 && date_idx < length(args)) {
  as.Date(args[date_idx + 1])
} else {
  NULL
}

# Run the pipeline
box::use(R / pipeline[run_bets])

results <- run_bets(
  leagues = leagues,
  dry_run = dry_run,
  interactive = interactive,
  today_only = today_only,
  target_date = target_date,
  headless = headless,
  sports_dir = here("../Sports"),
  odds_dir = here("../lengjan-odds")
)

if (nrow(results) > 0 && "placement_status" %in% names(results)) {
  cat("\n=== Results ===\n")
  print(results[, c(
    "home", "away", "market", "outcome", "odds",
    "bet_amount", "placement_status"
  )])
}

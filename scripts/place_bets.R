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

# Pin a UTF-8 locale before load_all() so Icelandic team/competition names in
# the package source are never mangled under a C locale. Mirrors scripts/0N_*.R.
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
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

# Commit the ledger immediately if any bets were actually placed (L1 invariant:
# a row means money was committed on Lengjan). See commit 121710d for the
# 2026-05-08 incident that motivated this guard.
n_placed <- if ("status" %in% names(results)) {
  sum(results$status == "placed", na.rm = TRUE)
} else {
  0L
}
if (n_placed > 0L && !dry_run) {
  cli::cli_h2("Committing ledger to git")
  commit_msg <- sprintf(
    "data(ledger): placer run %s -- %d bet(s) placed",
    format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    n_placed
  )
  res <- commit_ledger_changes(here::here(), commit_msg)
  switch(res$status,
    committed = cli::cli_alert_success("Ledger committed: {res$message}"),
    nothing_to_commit = cli::cli_alert_info(
      "No ledger changes to commit (already clean)."
    ),
    failed = {
      cli::cli_alert_danger("Ledger commit failed.")
      cli::cli_alert_danger("git output:")
      writeLines(res$output, stderr())
      cli::cli_alert_danger(paste(
        "Bets WERE placed on Lengjan but the ledger row(s) are NOT committed.",
        "Commit data/decisions/ledger/ MANUALLY before doing anything else."
      ))
      quit(save = "no", status = 1L)
    }
  )
}

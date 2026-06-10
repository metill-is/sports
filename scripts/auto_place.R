#!/usr/bin/env Rscript
# scripts/auto_place.R -- unattended low-footprint placer.
# LOCAL ONLY -- never referenced by CI (test-placer-ci-isolation.R enforces).
# Driven by the launchd agent is.metill.sports.autoplace.
#
# Flow: jitter -> daytime guard -> run_auto_place (kill/lock/sync/gate/cap/place)
# -> ledger commit (L1: real-money rows never sit uncommitted)
# -> status recorded for the health layer. Relies on the placer's existing
# P1-P4 rules, sample_delay() pacing, and the daily/per-match caps.

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
root <- here::here("data")

Sys.sleep(stats::runif(1, 0, 1200)) # 0-20 min jitter; irregular timing

hr <- as.integer(format(Sys.time(), "%H"))
if (hr < 9L || hr >= 22L) {
  cli::cli_alert_info("Outside daytime window ({hr}:00); skipping.")
  quit(save = "no", status = 0L)
}

rec <- tryCatch(
  run_auto_place(root = root),
  error = function(e) {
    cli::cli_alert_danger("run_auto_place errored: {conditionMessage(e)}")
    record_placement_status(
      paste0("failed: ", conditionMessage(e)),
      root = root
    )
  }
)
cli::cli_alert_info("auto_place: {rec$status}")

# Commit any ledger rows this run produced -- or a prior crashed run left
# behind. L1 invariant: a row means money was committed on Lengjan; an
# uncommitted row is destroyed by the next git reset. See commit 121710d
# (2026-05-08) and the recovered auto-place rows of 2026-06-10.
n_placed <- rec$n_placed
n_placed <- if (is.null(n_placed) || is.na(n_placed)) 0L else as.integer(n_placed)
commit_msg <- sprintf(
  "data(ledger): auto-place run %s -- %s",
  format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
  if (n_placed > 0L) sprintf("%d bet(s) placed", n_placed) else "rescue sweep"
)
res <- commit_ledger_changes(here::here(), commit_msg)
switch(res$status,
  committed = cli::cli_alert_success("Ledger committed: {res$message}"),
  nothing_to_commit = invisible(NULL),
  failed = {
    cli::cli_alert_danger("Ledger commit failed; git output:")
    writeLines(res$output, stderr())
    cli::cli_alert_danger(paste(
      "Bets may be placed on Lengjan with ledger row(s) NOT committed.",
      "Commit data/decisions/ledger/ MANUALLY before doing anything else."
    ))
    record_placement_status("failed:ledger_commit", root = root)
    quit(save = "no", status = 1L)
  }
)

if (identical(rec$status, "sync_failed") || startsWith(rec$status, "failed")) {
  quit(save = "no", status = 1L)
}

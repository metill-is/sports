#!/usr/bin/env Rscript
# scripts/auto_place.R -- unattended low-footprint placer.
# LOCAL ONLY -- never referenced by CI (test-placer-ci-isolation.R enforces).
# Driven by the launchd agent is.metill.sports.autoplace.
#
# Flow: jitter -> daytime guard -> run_auto_place (kill/lock/sync/gate/cap/place)
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

rec <- run_auto_place(root = root)
cli::cli_alert_info("auto_place: {rec$status}")
if (identical(rec$status, "sync_failed")) {
  quit(save = "no", status = 1L)
}

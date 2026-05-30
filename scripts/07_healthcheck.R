#!/usr/bin/env Rscript
# scripts/07_healthcheck.R --
# Read-only pipeline health snapshot. Composes pipeline_health() into
# data/health/status.json and prints a one-screen summary.
#
# Always exits 0 (this is observability, not a gate). The healthcheck.yml
# workflow reads status.json's `overall` and fails the run on FAIL so that
# GitHub's native failure email fires. Safe to run locally any time --
# strictly read-only over committed Parquet; never writes the ledger.
#
# Usage:
#   Rscript scripts/07_healthcheck.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

options(width = 120)

h <- pipeline_health()
status_path <- here::here("data", "health", "status.json")
write_health_status(h, status_path)

overall <- overall_health_status(h)

cli::cli_h1("Pipeline health: {overall}")
print(as.data.frame(h), row.names = FALSE)

breaches <- h[h$status %in% c("WARN", "FAIL"), , drop = FALSE]
if (nrow(breaches) > 0L) {
  cli::cli_h2("Breaches")
  for (i in seq_len(nrow(breaches))) {
    r <- breaches[i, ]
    msg <- sprintf(
      "[%s] %s (%s): %s (want %s)",
      r$status, r$check, r$scope, r$value, r$threshold
    )
    if (identical(r$status, "FAIL")) {
      cli::cli_alert_danger("{msg}")
    } else {
      cli::cli_alert_warning("{msg}")
    }
  }
}

cli::cli_alert_info("Wrote {status_path}")

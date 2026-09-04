#!/usr/bin/env Rscript
# scripts/05_publish.R --
# Generate publish JSONs (data/publish/<sport>/iceland/{karla,kvenna}/)
# for active (league, sex) pairs.
#
# Usage:
#   Rscript scripts/05_publish.R
#   Rscript scripts/05_publish.R --league football_iceland

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

leagues <- load_leagues()

cli::cli_h1("Publish ({nrow(targets)} (league, sex) pairs)")
res <- run_publish_targets(targets, leagues)
cli::cli_alert_success(
  "Publish complete: {res$published}/{nrow(targets)} cell{?s} published"
)

# Exit non-zero when ANY target failed, not only when all did. A partially
# failed publish that exits 0 is the warn-and-exit-0 shape basketball and
# handball hid in for months. The successful cells are already written, so a
# red run still ships football's output.
if (nrow(res$failed) > 0L) {
  cli::cli_alert_danger("{nrow(res$failed)} publish target{?s} failed:")
  print(as.data.frame(res$failed))
  quit(save = "no", status = 1L)
}

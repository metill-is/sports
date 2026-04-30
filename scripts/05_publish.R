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
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  league_def <- leagues[[row$key]]
  static <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  betting <- league_def$betting

  cli::cli_h2("{row$key} ({row$sex})")
  publish_one(static, betting, row$key, row$sex)
}
cli::cli_alert_success("Publish complete")

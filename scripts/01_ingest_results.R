#!/usr/bin/env Rscript
# scripts/01_ingest_results.R --
# Scrape federation results + schedules for active leagues.
#
# Writes data/facts/results/ and data/facts/schedules/ via upsert_table()
# (idempotent merge -- safe to re-run).
#
# Usage:
#   Rscript scripts/01_ingest_results.R                    # all active leagues
#   Rscript scripts/01_ingest_results.R --league football_iceland

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
leagues <- load_leagues()
active <- filter_leagues(leagues, active_only = TRUE)
if (!is.null(opts$league)) {
  if (!opts$league %in% names(active)) {
    stop("--league '", opts$league, "' is not active.")
  }
  active <- active[opts$league]
}

active_path <- here::here("config", "active_competitions.json")
if (!fs::file_exists(active_path)) {
  stop(
    "config/active_competitions.json missing. ",
    "Run scripts/00_active_competitions.R first."
  )
}

cli::cli_h1("Ingest results + schedules ({length(active)} leagues)")
for (key in names(active)) {
  static <- active[[key]][c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  cli::cli_h2("{key}")
  ingest_one_league(static, key, active_path)
}
cli::cli_alert_success("Ingest complete")

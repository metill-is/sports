#!/usr/bin/env Rscript
# Backfill data/facts/{results,schedules}/ from federation scrapers.
#
# Runs all 3 active Icelandic-league ingest sources end-to-end. Intended as a
# one-shot re-run — each scraper is idempotent per (sport, sex, season)
# partition, so re-running overwrites the affected partitions in place.
#
# Wall-clock ~10 min dominated by HSÍ handball (chromote-backed, ~10s per
# tournament page) and KSÍ football (paginated ~1 req/s). KKÍ basketball is
# sub-second per XLSX.
#
# Usage:
#   Rscript scripts/backfill_ingest.R

suppressPackageStartupMessages(devtools::load_all(here::here()))

leagues <- load_leagues()

# Historical coverage requested per scraper:
# - KSÍ football: 2021-2026 men; 2025-2026 women (only IDs currently known)
# - KKÍ basketball: 2021-2026 both sexes both divisions
# - HSÍ handball: 2021-2026; div2 + female historical gaps are known (see
#   ingest-hsi-handball.R comments) — pass the full range and let the scraper
#   no-op on missing IDs.
seasons <- 2021:2026

for (key in names(leagues)) {
  league <- leagues[[key]]
  if (!isTRUE(league$active)) next
  cli::cli_h1(key)

  for (sex in league$sexes) {
    cli::cli_h2("{sex}")
    tryCatch(
      ingest_league(league, sex, seasons = seasons),
      error = function(e) {
        cli::cli_alert_danger("Failed {key}/{sex}: {conditionMessage(e)}")
      }
    )
  }
}

cli::cli_alert_success("Backfill complete.")

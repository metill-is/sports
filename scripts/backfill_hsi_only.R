#!/usr/bin/env Rscript
# HSI-only backfill — targeted re-run to exercise the retry + upsert fixes.
#
# Iterates handball_iceland × (male, female) × seasons 2021-2025. The HSI
# scraper no-ops on (sex, division, season) triples with no known tournament
# ID, so passing the full range is safe.
#
# Wall-clock ~8-15 min (chromote-backed, ~15-60s per tournament page).
#
# Usage:
#   nohup Rscript scripts/backfill_hsi_only.R > /tmp/hsi-backfill.log 2>&1 & disown

suppressPackageStartupMessages(devtools::load_all(here::here()))

leagues <- load_leagues()
league <- leagues[["handball_iceland"]]
stopifnot(!is.null(league))

seasons <- 2021:2025

for (sex in league$sexes) {
  cli::cli_h1("handball_iceland / {sex}")
  tryCatch(
    ingest_league(league, sex, seasons = seasons),
    error = function(e) {
      cli::cli_alert_danger("Failed handball_iceland/{sex}: {conditionMessage(e)}")
    }
  )
}

cli::cli_alert_success("HSI backfill complete.")

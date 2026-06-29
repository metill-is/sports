#!/usr/bin/env Rscript
# scripts/wc/ingest.R --
# Refresh the international-results facts store for the 2026 World Cup forecast.
#
# Downloads the martj42 bulk international-results CSV (the canonical intl
# results dataset, 1872-->present, which also lists the upcoming WC fixtures as
# NA-score rows) and ingests it into the Parquet facts store under
# sport=football / country=world / sex=male -- the path prepare_data() reads.
# Run this BEFORE scripts/wc/fit.R.
#
# Usage:
#   Rscript scripts/wc/ingest.R
#
# data/wc/raw/ is gitignored (regenerable); the committed artefact is the
# facts-store Parquet this writes. The World Cup Forecast workflow diffs those
# Parquets to decide whether martj42 brought anything new worth refitting.
#
# shootouts.csv from the same upstream is NOT downloaded -- the WC simulator
# (R/wc-simulate.R) models extra-time / shootouts as more 90' at the same
# strengths, so it never consults the historical shootout table.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

martj42_url <- paste0(
  "https://raw.githubusercontent.com/",
  "martj42/international_results/master/results.csv"
)
csv_path <- here::here("data", "wc", "raw", "results.csv")
dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)

cli::cli_alert_info("Downloading martj42 international results -> {.path {csv_path}}")
utils::download.file(martj42_url, csv_path, mode = "wb", quiet = TRUE)

# Fail loudly on a bad download (404 page, redirect, schema change) rather than
# feeding garbage into the facts store. This runs unattended in CI, so the
# failure email is the only signal a refresh silently broke.
header <- readLines(csv_path, n = 1L, warn = FALSE)
if (!startsWith(header, "date,home_team,away_team")) {
  cli::cli_abort(c(
    "Unexpected results.csv header -- download failed or martj42 schema changed.",
    "i" = "Got: {.val {header}}"
  ))
}

wc_ingest_internationals(csv_path = csv_path)

# Penalty-shootout winners. The simulator models ET / shootouts as more 90'
# play, so a drawn knockout has no winner in the facts store. martj42 publishes
# the shootout winner in a parallel shootouts.csv (its `winner` column); pull it
# (validated; a 404/redirect leaves any cached raw file in place) and merge it
# with the manual overlay's pen_winner into the committed data/wc/shootouts.csv,
# which the pin-builder consults for level knockouts.
shootouts_url <- paste0(
  "https://raw.githubusercontent.com/",
  "martj42/international_results/master/shootouts.csv"
)
shootouts_path <- here::here("data", "wc", "raw", "shootouts.csv")
shootouts_tmp <- paste0(shootouts_path, ".tmp")
ok <- tryCatch(
  {
    utils::download.file(shootouts_url, shootouts_tmp, mode = "wb", quiet = TRUE)
    startsWith(
      readLines(shootouts_tmp, n = 1L, warn = FALSE), "date,home_team,away_team,winner"
    )
  },
  error = function(e) FALSE
)
if (isTRUE(ok)) {
  file.rename(shootouts_tmp, shootouts_path)
} else {
  cli::cli_warn("martj42 shootouts download failed/changed; using any cached raw file.")
  if (file.exists(shootouts_tmp)) unlink(shootouts_tmp)
}
wc_ingest_shootouts(wc_structure(), shootouts_csv = shootouts_path)

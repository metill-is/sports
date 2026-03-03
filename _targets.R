# Lengjan Odds Scraper — {targets} pipeline
#
# Usage:
#   targets::tar_make()        # Run full pipeline
#   targets::tar_read(odds)    # Read latest scraped odds
#   targets::tar_visnetwork()  # Visualise pipeline DAG

library(targets)

tar_option_set(
  packages = c(
    "dplyr", "tibble", "readr", "stringr", "lubridate",
    "rvest", "glue", "yaml", "here"
  )
)

# Source all R/ files so functions are available to targets
tar_source("R/")

list(
  # 1. Load competition config — re-runs if competitions.yml changes
  tar_target(
    config,
    load_competitions(here::here("config", "competitions.yml"))
  ),

  # 2. Scrape fresh odds — ALWAYS runs (code doesn't change, but odds do)
  #    If the scraped data is identical to last run, downstream targets are skipped
  tar_target(
    odds,
    scrape_all(config),
    cue = tar_cue(mode = "always")
  ),

  # 3. Accumulate into data/ — only runs if odds hash changed
  #    Reads existing CSVs, appends new rows, deduplicates, writes back
  tar_target(
    data_files,
    accumulate_odds(odds),
    format = "file"
  )
)

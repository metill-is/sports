# Lengjan Odds Scraper — {targets} pipeline
#
# Creates per-sport targets dynamically from competitions.yml.
# Each sport gets: scrape (always runs) → accumulate (skips if unchanged).
#
# Usage:
#   targets::tar_make()        # Run full pipeline
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

# Read config at pipeline-definition time to create per-sport targets
sport_keys <- names(yaml::read_yaml("config/competitions.yml"))

# Build target list: config file → config → per-sport (scrape → accumulate)
targets <- list(
  # Track competitions.yml as a file — re-hashes when file content changes
  tar_target(
    config_file,
    here::here("config", "competitions.yml"),
    format = "file"
  ),
  # Parse config — re-runs when config_file hash changes
  tar_target(
    config,
    load_competitions(config_file)
  )
)

for (key in sport_keys) {
  targets <- c(targets, list(
    # Scrape fresh odds — ALWAYS runs (code doesn't change, but odds do)
    tar_target_raw(
      name = paste0("odds_", key),
      command = substitute(
        scrape_sport(config, k),
        list(k = key)
      ),
      cue = tar_cue(mode = "always")
    ),
    # Accumulate into data/{sport}/ — only runs if odds hash changed
    tar_target_raw(
      name = paste0("data_", key),
      command = substitute(
        accumulate_sport_odds(odds, k),
        list(odds = as.symbol(paste0("odds_", key)), k = key)
      ),
      format = "file"
    )
  ))
}

targets

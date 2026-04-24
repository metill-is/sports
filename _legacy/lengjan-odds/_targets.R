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
all_sport_keys <- names(yaml::yaml.load(readr::read_file("config/competitions.yml")))

# Filter by active competitions if schedule scan has been run
active_path <- "config/active_competitions.json"
if (file.exists(active_path)) {
  active <- jsonlite::fromJSON(active_path)
  active_keys <- names(which(vapply(active$active, isTRUE, logical(1))))
  sport_keys <- intersect(all_sport_keys, active_keys)
  skipped <- setdiff(all_sport_keys, sport_keys)
  message(
    "Active filter (", active$generated_at, ", ", active$lookahead_days, "d lookahead): ",
    "scraping ", paste(sport_keys, collapse = ", "),
    if (length(skipped) > 0) paste0(" | skipping ", paste(skipped, collapse = ", ")) else ""
  )
} else {
  sport_keys <- all_sport_keys
  message("No active filter found — scraping all ", length(sport_keys), " competitions")
}

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

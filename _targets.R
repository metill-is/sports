# Livesport Data Scraper — {targets} pipeline
#
# Per-competition targets, each scraping all leagues/divisions.
# Always re-runs (data changes daily, not code).
#
# Usage:
#   targets::tar_make()        # Run full pipeline
#   targets::tar_visnetwork()  # Visualise pipeline DAG

library(targets)

tar_option_set(
  packages = c("dplyr", "glue", "here", "readr", "rvest", "yaml")
)

tar_source("R/")

# Read config at definition time
config_raw <- yaml::yaml.load(readr::read_file("config/competitions.yml"))
comp_keys <- setdiff(names(config_raw), "defaults")

# Filter by active competitions if schedule scan has provided one
active_path <- "config/active_competitions.json"
if (file.exists(active_path)) {
  active <- jsonlite::fromJSON(active_path)
  active_keys <- names(which(vapply(active$active, isTRUE, logical(1))))
  comp_keys <- intersect(comp_keys, active_keys)
  message(
    "Active filter: scraping ", paste(comp_keys, collapse = ", ")
  )
} else {
  message("No active filter — scraping all ", length(comp_keys), " competitions")
}

# Build targets
targets <- list(
  tar_target(
    config_file,
    here::here("config", "competitions.yml"),
    format = "file"
  ),
  tar_target(
    config,
    load_competitions(config_file)
  )
)

for (key in comp_keys) {
  targets <- c(targets, list(
    tar_target_raw(
      name = paste0("data_", key),
      command = substitute(
        scrape_competition(config, k),
        list(k = key)
      ),
      cue = tar_cue(mode = "always")
    )
  ))
}

targets

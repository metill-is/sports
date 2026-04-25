# _targets.R -- Sports pipeline DAG.
#
# Layers (in dependency order):
#   1. config + active_competitions filter
#   2. ingest_<league>             (federation results + schedules)
#   3. odds_<league>               (Lengjan odds, schedule-aware)
#   4. fit_<league>_<sex>          (Stan posteriors)        <- Task 4
#   5. decide_<league>_<sex>       (recommendations)        <- Task 5
#   6. publish_<league>_<sex>      (JSONs)                  <- Task 5
#
# Run: Rscript -e 'targets::tar_make()'                       (everything)
# Or:  Rscript -e 'targets::tar_make(starts_with("odds_"))'   (just odds)

library(targets)

# Set UTF-8 locale before sourcing R/. Some R/ files use \uXXXX escapes for
# Icelandic strings (e.g. R/ingest-hsi-handball.R::HSI_MONTH_MAP) that fail to
# re-translate under a C locale -- which targets' callr-spawned worker sessions
# default to. Without this, tar_source() / tar_make() error with
# "internal parser error" when reading those files.
Sys.setlocale("LC_ALL", "en_US.UTF-8")

tar_option_set(
  packages = c(
    "arrow", "dplyr", "tibble", "purrr", "readr", "stringr", "lubridate",
    "rvest", "yaml", "jsonlite", "here", "cli", "fs"
  ),
  format = "rds"
)

# Source R/ files. Ingest source modules (ingest-{ksi,hsi,kki,lengjan}.R) call
# register_ingest_source() at load time, so R/ingest.R (which defines that
# helper) must be sourced before the modules. tar_source() goes alphabetical,
# so we source ingest.R explicitly first, then let tar_source() pick up the
# rest (it is idempotent -- re-sourcing ingest.R is harmless).
source(here::here("R", "ingest.R"))
tar_source(here::here("R"))

# Read leagues at DAG definition time so we can generate per-league targets.
leagues_definition <- load_leagues()
active_keys <- names(filter_leagues(leagues_definition, active_only = TRUE))
lengjan_keys <- names(filter_leagues(
  leagues_definition,
  active_only = TRUE,
  has_lengjan = TRUE
))

# Static targets (config, active filter).
static_targets <- list(
  tar_target(
    leagues_file,
    here::here("config", "leagues.yml"),
    format = "file"
  ),
  tar_target(
    leagues_config,
    load_leagues(leagues_file)
  ),
  tar_target(
    bankroll_file,
    here::here("config", "bankroll.yml"),
    format = "file"
  ),
  tar_target(
    bankroll,
    load_bankroll(bankroll_file)
  ),
  tar_target(
    active_competitions,
    generate_active_competitions(
      leagues_config,
      lookahead_days = 14L
    ),
    format = "file",
    cue = tar_cue(mode = "always")
  )
)

# Per-league ingest targets -- federation results + schedules.
ingest_targets <- lapply(active_keys, function(key) {
  tar_target_raw(
    name = paste0("ingest_", key),
    command = substitute(
      ingest_one_league(leagues_config, k, active_competitions),
      list(k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

# Per-league odds targets -- Lengjan, schedule-aware.
odds_targets <- lapply(lengjan_keys, function(key) {
  tar_target_raw(
    name = paste0("odds_", key),
    command = substitute(
      ingest_one_lengjan(leagues_config, k, active_competitions),
      list(k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

c(static_targets, ingest_targets, odds_targets)

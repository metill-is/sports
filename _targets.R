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

# Per-league config slices.
#
# leagues.yml is a coarse-grained source: every change rebuilds `leagues_config`
# and would invalidate every downstream target that takes `leagues_config` as
# a hash input. Splitting per-league config into three concern-scoped slices
# lets targets' content-based hashing isolate cache invalidation: editing
# `lengjan.competitions` rebuilds only `league_lengjan_<key>` (and its
# downstream odds + decide), leaving `league_static_<key>` byte-identical
# and so the 30-90 minute Stan fits stay cached.
#
# - league_static_<key>:  sport, country, sexes, active, stan_model, data_source
# - league_lengjan_<key>: lengjan (competitions + team_names)
# - league_betting_<key>: betting (kelly_frac, ev_threshold, markets, scoring, ...)
slice_targets <- unlist(lapply(active_keys, function(key) {
  list(
    tar_target_raw(
      name = paste0("league_static_", key),
      command = substitute(
        leagues_config[[k]][c(
          "sport", "country", "sexes", "active", "stan_model", "data_source"
        )],
        list(k = key)
      )
    ),
    tar_target_raw(
      name = paste0("league_lengjan_", key),
      command = substitute(leagues_config[[k]]$lengjan, list(k = key))
    ),
    tar_target_raw(
      name = paste0("league_betting_", key),
      command = substitute(leagues_config[[k]]$betting, list(k = key))
    )
  )
}), recursive = FALSE)

# Per-league ingest targets -- federation results + schedules.
ingest_targets <- lapply(active_keys, function(key) {
  static_dep <- as.symbol(paste0("league_static_", key))
  tar_target_raw(
    name = paste0("ingest_", key),
    command = substitute(
      ingest_one_league(static, k, active_competitions),
      list(static = static_dep, k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

# Per-league odds targets -- Lengjan, schedule-aware.
odds_targets <- lapply(lengjan_keys, function(key) {
  static_dep <- as.symbol(paste0("league_static_", key))
  lengjan_dep <- as.symbol(paste0("league_lengjan_", key))
  tar_target_raw(
    name = paste0("odds_", key),
    command = substitute(
      ingest_one_lengjan(static, lengjan, k, active_competitions),
      list(static = static_dep, lengjan = lengjan_dep, k = key)
    ),
    cue = tar_cue(mode = "always")
  )
})

# Per-(league x sex) fit targets -- Stan posteriors. Depends only on the
# static slice (sport/country/stan_model) plus the ingest dep, NOT on lengjan
# or betting -- those don't affect the model so they shouldn't bust the
# cache.
fit_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("fit_", key, "_", sex)
    static_dep <- as.symbol(paste0("league_static_", key))
    ingest_dep <- as.symbol(paste0("ingest_", key))
    fit_targets[[length(fit_targets) + 1L]] <- tar_target_raw(
      name = target_name,
      command = substitute(
        fit_one(static, s, ingest_dep),
        list(static = static_dep, s = sex, ingest_dep = ingest_dep)
      )
    )
  }
}

# Per-(league x sex) decide targets -- Kelly + portfolio + calibration.
# Depends on all three slices (static + lengjan for team_names + betting for
# kelly_frac/ev_threshold) plus fit + odds.
decide_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("decide_", key, "_", sex)
    static_dep <- as.symbol(paste0("league_static_", key))
    lengjan_dep <- as.symbol(paste0("league_lengjan_", key))
    betting_dep <- as.symbol(paste0("league_betting_", key))
    fit_dep <- as.symbol(paste0("fit_", key, "_", sex))
    if (key %in% lengjan_keys) {
      odds_dep <- as.symbol(paste0("odds_", key))
      cmd <- substitute(
        decide_one(static, lengjan, betting, s, bankroll, fit_dep, odds_dep),
        list(
          static = static_dep, lengjan = lengjan_dep, betting = betting_dep,
          s = sex, fit_dep = fit_dep, odds_dep = odds_dep
        )
      )
    } else {
      cmd <- substitute(
        decide_one(static, lengjan, betting, s, bankroll, fit_dep),
        list(
          static = static_dep, lengjan = lengjan_dep, betting = betting_dep,
          s = sex, fit_dep = fit_dep
        )
      )
    }
    decide_targets[[length(decide_targets) + 1L]] <- tar_target_raw(
      name = target_name, command = cmd
    )
  }
}

# Per-(league x sex) publish targets -- JSONs.
# Depends on static + betting (publishers read sport/country for paths and
# betting.scoring for tie thresholds), plus fit + decide.
publish_targets <- list()
for (key in active_keys) {
  league_def <- leagues_definition[[key]]
  for (sex in league_def$sexes) {
    target_name <- paste0("publish_", key, "_", sex)
    static_dep <- as.symbol(paste0("league_static_", key))
    betting_dep <- as.symbol(paste0("league_betting_", key))
    fit_dep <- as.symbol(paste0("fit_", key, "_", sex))
    decide_dep <- as.symbol(paste0("decide_", key, "_", sex))
    publish_targets[[length(publish_targets) + 1L]] <- tar_target_raw(
      name = target_name,
      command = substitute(
        publish_one(static, betting, k, s, fit_dep, decide_dep),
        list(
          static = static_dep, betting = betting_dep, k = key, s = sex,
          fit_dep = fit_dep, decide_dep = decide_dep
        )
      )
    )
  }
}

c(
  static_targets, slice_targets, ingest_targets, odds_targets, fit_targets,
  decide_targets, publish_targets
)

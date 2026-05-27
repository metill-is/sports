#!/usr/bin/env Rscript
# scripts/03_fit.R --
# Fit Stan posteriors for (league x sex) pairs that have new completed
# games since the last fit.
#
# Writes data/beliefs/latest/ (overwrite) and either
# data/beliefs/archive/ (basketball + handball) or data/beliefs/extracts/
# (football iceland — accretive per fit_date; archive/ is skipped post
# Phase 3b, see R/model-league.R::fit_league).
#
# Usage:
#   Rscript scripts/03_fit.R                                    # all needing refit
#   Rscript scripts/03_fit.R --league football_iceland --sex male
#   Rscript scripts/03_fit.R --force                            # refit everything

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

leagues <- load_leagues()

cli::cli_h1("Fit Stan models ({nrow(targets)} candidate (league, sex) pairs)")
fitted <- 0L
skipped <- 0L
for (i in seq_len(nrow(targets))) {
  row <- targets[i, ]
  league_def <- leagues[[row$key]]
  static <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]

  if (!opts$force) {
    if (!needs_refit(static, row$sex)) {
      cli::cli_alert_info("Skipping {row$key} ({row$sex}): no new games since last fit.")
      skipped <- skipped + 1L
      next
    }
    if (!has_upcoming_games(static, row$sex)) {
      cli::cli_alert_info("Skipping {row$key} ({row$sex}): no upcoming games in the next 14 days.")
      skipped <- skipped + 1L
      next
    }
  }

  cli::cli_h2("{row$key} ({row$sex})")
  fit_one(static, row$sex)
  fitted <- fitted + 1L
}
cli::cli_alert_success("Fit complete: {fitted} fitted, {skipped} skipped")

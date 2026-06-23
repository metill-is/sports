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
league_named <- !is.null(opts$league)
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

  # --force refits in-season leagues even without new games, but a bulk
  # force still skips paused (off-season) leagues -- an explicit --league
  # overrides that. See fit_skip_reason() for the full rule set.
  skip <- fit_skip_reason(static, row$sex, opts$force, league_named)
  if (!is.null(skip)) {
    cli::cli_alert_info("Skipping {row$key} ({row$sex}): {skip}.")
    skipped <- skipped + 1L
    next
  }

  cli::cli_h2("{row$key} ({row$sex})")
  fit_one(static, row$sex)
  fitted <- fitted + 1L
}
cli::cli_alert_success("Fit complete: {fitted} fitted, {skipped} skipped")

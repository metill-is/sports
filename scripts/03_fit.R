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
# --force refits in-season leagues even without new games, but a bulk force
# still skips paused (off-season) leagues -- an explicit --league overrides
# that. See fit_skip_reason() for the full rule set, and run_fit_targets() for
# why one target's abort must not reach the next.
res <- run_fit_targets(targets, leagues, force = opts$force, league_named = league_named)
fitted <- res$fitted
cli::cli_alert_success(
  "Fit complete: {fitted} fitted, {res$skipped} skipped, {nrow(res$failed)} failed"
)

# Retention. data/beliefs/extracts/ is git-tracked and committed by fit.yml on
# every run, and it is the SOLE publish input, so it cannot simply be ignored.
# Left unpruned it grew to 1.3 GB over 99 partitions (~22 MB per football fit,
# ~24 fits a month). actions/checkout has no fetch-depth here, so every one of
# the nine workflows pays that working-tree size on each run.
#
# Runs only when something was actually fitted: a skip-only run has written no
# new partition, so there is nothing to age out and no reason to touch the tree.
if (fitted > 0L) {
  pruned <- prune_extracts(dry_run = FALSE)
  if (nrow(pruned) > 0L) {
    cli::cli_alert_info(
      "Pruned {nrow(pruned)} old extract partition{?s} \\
       ({round(sum(pruned$bytes) / 1024^2)} MB)."
    )
  }
}

# Exit non-zero when ANY target failed, not only when all did. A partially
# failed fit that exits 0 is the warn-and-exit-0 shape basketball and handball
# hid in for months. This runs AFTER the retention pass so a red run still
# prunes, and fit.yml's commit step carries `if: always()` so the posteriors
# that DID fit are committed rather than thrown away with the run.
if (nrow(res$failed) > 0L) {
  cli::cli_alert_danger("{nrow(res$failed)} fit target{?s} failed:")
  print(as.data.frame(res$failed))
  quit(save = "no", status = 1L)
}

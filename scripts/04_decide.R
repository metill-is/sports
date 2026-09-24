#!/usr/bin/env Rscript
# scripts/04_decide.R --
# Run decide layer (Kelly + portfolio + calibration) for active
# (league, sex) pairs. Reads beliefs_latest + today's odds, writes
# data/decisions/candidates/ and data/decisions/recommendations/.
#
# Usage:
#   Rscript scripts/04_decide.R
#   Rscript scripts/04_decide.R --league football_iceland --sex female

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("scripts", "_lib.R"))

opts <- parse_pipeline_args()
targets <- resolve_targets(opts)

if (nrow(targets) == 0L) {
  cli::cli_alert_warning("No (league, sex) pairs match the filters; nothing to do.")
  quit(save = "no", status = 0L)
}

bankroll <- load_bankroll(here::here("config", "bankroll.yml"))
leagues <- load_leagues()

cli::cli_h1("Decide ({nrow(targets)} (league, sex) pairs)")
# One (league, sex) cell's error -- e.g. a paper league's decide -- must not
# stop the cells after it: resolve_targets() walks config order, which puts
# football, the live money path, last. run_per_league() contains each failure;
# the run still ends red below, and decide-publish.yml publishes regardless.
decide_row <- function(i) {
  row <- targets[i, ]
  league_def <- leagues[[row$key]]
  static <- league_def[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  lengjan <- league_def$lengjan
  betting <- league_def$betting

  cli::cli_h2("{row$key} ({row$sex})")
  decide_one(static, lengjan, betting, row$sex, bankroll)
  invisible(NULL)
}
res <- run_per_league(
  stats::setNames(seq_len(nrow(targets)), paste0(targets$key, " (", targets$sex, ")")),
  decide_row,
  what = "decide"
)

# Exit non-zero when ANY cell failed, not only when all did (INT-2): the
# surviving cells' recommendations are already written.
if (nrow(res$failed) > 0L) {
  cli::cli_alert_danger("{nrow(res$failed)} decide target{?s} failed:")
  print(as.data.frame(res$failed))
  quit(save = "no", status = 1L)
}
cli::cli_alert_success("Decide complete")

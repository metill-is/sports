#!/usr/bin/env Rscript
# scripts/0Nb_backtest.R --
# Replay historical betting decisions against results and write tidy
# backtest artefacts to data/backtest/ for the Quarto report. Read-only on
# data/decisions/ -- never touches the ledger or the money path.
#
# Usage:
#   Rscript scripts/0Nb_backtest.R                       # football iceland (default), both stake models
#   Rscript scripts/0Nb_backtest.R --strategy kept --stake rolling
#   Rscript scripts/0Nb_backtest.R --league all          # widen to every sport (bball/handball resume autumn)
#   Rscript scripts/0Nb_backtest.R --include-bug-era      # include pre-2026-05-14 spread-bug-contaminated runs
#   Rscript scripts/0Nb_backtest.R --league football_iceland --from 2026-04-25
invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
strategy <- get_flag("strategy", "kept")
stake <- get_flag("stake", "both")
league <- get_flag("league", "football_iceland")
from <- get_flag("from")
to <- get_flag("to")
# Default: exclude the spread sign-flip bug era (run_date < 2026-05-14). The
# forensic review showed those contaminated bets carried the whole headline.
exclude_pre_fix <- !("--include-bug-era" %in% args)

# Default scope is football only -- this backtest informs football specifically,
# and basketball/handball are on seasonal pause. `--league all` widens to every
# sport; `--league <key>` targets one league.
sport_filter <- if (identical(league, "all")) NULL else sub("_.*$", "", league)

results <- sports::read_table("results")
initial_pool <- sports::load_bankroll()$initial_pool

universe <- sports::bt_load_universe(
  strategy = strategy, leagues = sport_filter, from = from, to = to,
  exclude_pre_fix = exclude_pre_fix
)
if (nrow(universe) == 0L) {
  cli::cli_alert_warning("No bets in universe for the given filters; nothing to do.")
  quit(status = 0L)
}

stake_models <- if (stake == "both") c("rolling", "fixed") else stake
per_bet <- list()
for (sm in stake_models) {
  rule <- if (sm == "rolling") sports::stake_rolling else sports::stake_fixed
  r <- sports::bt_run(universe, results,
    stake_rule = rule,
    initial_pool = initial_pool
  )
  r$stake_model <- sm
  per_bet[[sm]] <- r
}
per_bet <- dplyr::bind_rows(per_bet)

out_dir <- here::here("data", "backtest")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(per_bet, file.path(out_dir, "per_bet.parquet"))

metrics <- dplyr::bind_rows(lapply(stake_models, function(sm) {
  d <- per_bet[per_bet$stake_model == sm, ]
  m <- sports::bt_metrics(d)
  m$stake_model <- sm
  m
}))
arrow::write_parquet(metrics, file.path(out_dir, "metrics.parquet"))

calib <- sports::bt_calibration(per_bet[per_bet$stake_model == stake_models[[1]], ])
arrow::write_parquet(calib, file.path(out_dir, "calibration.parquet"))

baselines <- sports::bt_baselines(
  leagues = sport_filter, from = from, to = to, exclude_pre_fix = exclude_pre_fix
)
arrow::write_parquet(baselines, file.path(out_dir, "baselines.parquet"))

cli::cli_h2("Backtest summary")
print(metrics)
cli::cli_alert_info("Wrote artefacts to {.path {out_dir}}")

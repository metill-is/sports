#!/usr/bin/env Rscript
# Fit all active-league x both-sex combos and write beliefs/.
#
# Wall-clock ~30-60 min dominated by football Iceland (BVP ~10-15 min/sex)
# and handball (~5-8 min/sex). Basketball ~5 min/sex.
#
# Usage:
#   Rscript scripts/fit_all.R                      # MCMC 1000+1000 iters, seed 42
#   Rscript scripts/fit_all.R --iter 200 --seed 7  # quick smoke

suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL, parse = identity) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) {
    return(default)
  }
  parse(args[[i + 1L]])
}
iter <- get_flag("iter", 1000L, as.integer)
seed <- get_flag("seed", 42L, as.integer)

leagues <- load_leagues()
active <- leagues[vapply(leagues, function(l) isTRUE(l$active), logical(1))]

# Lock fit_date to the start of the run. Without this, an overnight
# backfill straddles midnight and writes some buckets to fit_date=D and
# others to fit_date=D+1, splitting one logical run across two archive
# partitions.
run_date <- Sys.Date()
t0 <- Sys.time()
for (key in names(active)) {
  league <- active[[key]]
  for (sex in league$sexes) {
    cli::cli_h1("{key} / {sex}")
    tryCatch(
      {
        beliefs <- fit_league(
          league = league, sex = sex,
          fit_date = run_date,
          iter_warmup = iter, iter_sampling = iter, seed = seed
        )
        cli::cli_alert_success("wrote {nrow(beliefs)} beliefs rows")
      },
      error = function(e) {
        cli::cli_alert_danger("FAILED {key}/{sex}: {conditionMessage(e)}")
      }
    )
  }
}
cli::cli_alert_info("Total wall-clock: {round(difftime(Sys.time(), t0, units = 'mins'), 1)} min")

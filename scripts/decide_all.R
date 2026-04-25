#!/usr/bin/env Rscript
# Run decide_league for all active-league x both-sex combos and write
# data/decisions/{candidates,recommendations}/. Wall-clock fast (~seconds)
# since decide layer doesn't refit Stan.
#
# Usage:
#   Rscript scripts/decide_all.R
#   Rscript scripts/decide_all.R --run-date 2026-04-25  # historical replay

suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL, parse = identity) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) {
    return(default)
  }
  parse(args[[i + 1L]])
}
run_date <- get_flag("run-date", Sys.Date(), as.Date)

leagues <- load_leagues()
active <- leagues[vapply(leagues, function(l) isTRUE(l$active), logical(1))]
bankroll <- load_bankroll()

t0 <- Sys.time()
for (key in names(active)) {
  league <- active[[key]]
  for (sex in league$sexes) {
    cli::cli_h1("{key} / {sex}")
    tryCatch(
      {
        recs <- decide_league(
          league = league, sex = sex,
          run_date = run_date,
          bankroll = bankroll
        )
        cli::cli_alert_success("wrote {nrow(recs)} recommendations")
      },
      error = function(e) {
        cli::cli_alert_danger("FAILED {key}/{sex}: {conditionMessage(e)}")
      }
    )
  }
}
cli::cli_alert_info("Total wall-clock: {round(difftime(Sys.time(), t0, units = 'mins'), 2)} min")

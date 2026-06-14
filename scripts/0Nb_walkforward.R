#!/usr/bin/env Rscript
# scripts/0Nb_walkforward.R --
# Leak-free walk-forward OOS validator for football iceland. Re-fits the model
# as-of each cutoff, re-decides from pre-cutoff odds, scores OOS Brier/log-loss
# (primary) + PnL (secondary). Read-only on the money path; NEVER on CI.
#
# Each cutoff is a full Stan fit (~hours for a season sweep). Run detached:
#   nohup Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round \
#       > /tmp/wf.log 2>&1 & disown
#
# Usage:
#   Rscript scripts/0Nb_walkforward.R --sex male --as-of 2026-05-15
#   Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round
#   Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round --horizon 10

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

sex <- get_flag("sex")
season_str <- get_flag("season")
as_of_str <- get_flag("as-of")
per_round <- has_flag("per-round")
horizon_days <- as.integer(get_flag("horizon", "14"))

if (is.null(sex)) stop("--sex required (male or female)", call. = FALSE)

# Football iceland only (engine stays general; handball is a deliberate later
# scope flip per spec Component C / plan P2).
league_key <- "football_iceland"
league <- load_leagues()[[league_key]]
tie_threshold <- league$betting$scoring$tie_threshold %||% 0

results <- read_table("results",
  filter = list(sport = league$sport, country = league$country, sex = sex)
)
odds <- read_table("odds",
  filter = list(sport = league$sport, country = league$country)
)
ledger <- tryCatch(read_table("ledger"), error = function(e) NULL)

# G1: each cutoff is round N's completion date, which is STRICTLY before round
# N+1's matches; bt_wf_filter_oos then scores (cutoff, cutoff+horizon] so a
# day-d match (trained on under match_date <= d) is never bet.
cutoffs <- if (isTRUE(per_round)) {
  if (is.null(season_str)) stop("--per-round requires --season YYYY", call. = FALSE)
  season <- as.integer(season_str)
  dates <- as.Date(character())
  for (n in seq_len(50L)) {
    cd <- suppressWarnings(suppressMessages(
      compute_round_cutoff_date(results, season = season, round_cutoff = n, quiet = TRUE)
    ))
    if (is.null(cd)) break
    dates <- c(dates, cd)
  }
  if (length(dates) == 0L) stop("No completed rounds for season ", season, call. = FALSE)
  dates
} else {
  if (is.null(as_of_str)) stop("--as-of YYYY-MM-DD required (or --per-round --season YYYY)", call. = FALSE)
  d <- as.Date(as_of_str)
  if (is.na(d)) stop("--as-of: could not parse '", as_of_str, "'", call. = FALSE)
  d
}

cli::cli_h1("Walk-forward {league_key}/{sex}: {length(cutoffs)} cutoff(s), horizon={horizon_days}d")
wf <- bt_walkforward(
  sex = sex, cutoffs = cutoffs, horizon_days = horizon_days,
  results = results, odds = odds, ledger = ledger,
  tie_threshold = tie_threshold
)

out_dir <- here::here("data", "backtest", "walkforward")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(wf$bets, file.path(out_dir, paste0("bets_", sex, ".parquet")))
arrow::write_parquet(wf$scores, file.path(out_dir, paste0("scores_", sex, ".parquet")))
print(wf$scores)
print(wf$pnl)
cli::cli_alert_success("Walk-forward complete: {nrow(wf$bets)} OOS bets scored")

#' Unified betting pipeline — basketball/iceland
#'
#' Thin wrapper around the shared betting orchestrator.
#'
#' Usage:
#'   Rscript R/run_bets.R

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  ../../../R/bets/run[run_betting_pipeline],
  yaml[read_yaml],
  here[here]
)

cfg <- read_yaml(here("config", "bets.yml"))
run_betting_pipeline(cfg, sport_dir = here())

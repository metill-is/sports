#' Unified Lengjan betting pipeline — football/england
#'
#' Thin wrapper around the shared betting orchestrator.
#'
#' Usage:
#'   Rscript R/run_bets.R              # use existing odds CSVs
#'   Rscript R/run_bets.R --scrape     # scrape fresh odds first

box::use(
  ../../../R/bets/run[run_betting_pipeline],
  yaml[read_yaml],
  here[here]
)

# --- CLI args ----------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
do_scrape <- "--scrape" %in% args

# --- Config ------------------------------------------------------------------

cfg <- read_yaml(here("config", "bets.yml"))

# --- Optional scrape ---------------------------------------------------------

if (do_scrape) {
  cat("Scraping fresh odds from Lengjan...\n")
  status <- system2("Rscript", args = here("R", "get_odds.R"))
  if (status != 0) {
    warning("Odds scraping failed (exit code ", status, "). Using existing CSVs.")
  }
  cat("\n")
}

# --- Run pipeline ------------------------------------------------------------

run_betting_pipeline(cfg, sport_dir = here())

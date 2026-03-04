#' Scrape Lengjan odds for handball Iceland
#'
#' Thin wrapper around the shared scraper at Sports/R/lengjan/.
#' Writes CSVs to the same paths as before (odds/iceland/{sex}/league_N.csv)
#' so downstream consumers (check_odds_lengjan.R) are unaffected.
#'
#' The shared scraper also attempts handicap + totals odds from match detail
#' pages (saved as league_N_handicap.csv, league_N_totals.csv).

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

# box::use() resolves paths relative to the calling file, so we source()
# the Sports-level entry point which has its own box::use() calls.
# here::here() anchors to handball/iceland/ (.Rproj); double dirname() gives Sports/
sports_dir <- dirname(dirname(here::here()))
withr::with_dir(sports_dir, source("scrape_odds.R"))

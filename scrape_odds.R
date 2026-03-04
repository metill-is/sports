#' Standalone entry point for Lengjan odds scraping
#'
#' Run from Sports/ directory:
#'   Rscript -e 'source("scrape_odds.R")'

box::use(R/lengjan/scrape_all[scrape_all_odds])

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

scrape_all_odds("handball_iceland")

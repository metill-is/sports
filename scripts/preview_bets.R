#!/usr/bin/env Rscript
# scripts/preview_bets.R — preview pending bets without browser.
#
# Usage:
#   Rscript scripts/preview_bets.R
#   Rscript scripts/preview_bets.R --league football_iceland
#   Rscript scripts/preview_bets.R --today

# Pin a UTF-8 locale before load_all() so Icelandic team/competition names in
# the package source are never mangled under a C locale. Mirrors scripts/0N_*.R.
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here()))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name) any(args == paste0("--", name))
get_arg <- function(name) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) NULL else args[[i + 1L]]
}

today_only <- get_flag("today")
leagues <- get_arg("league")
target_date <- if (!is.null(get_arg("date"))) as.Date(get_arg("date")) else NULL

invisible(preview_pending(
  leagues     = leagues,
  today_only  = today_only,
  target_date = target_date
))

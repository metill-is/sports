#!/usr/bin/env Rscript
# scripts/00_active_competitions.R --
# Generate config/active_competitions.json from leagues.yml + schedules.
#
# Was the `active_competitions` target in _targets.R. Run before ingest
# and odds scripts.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()
generate_active_competitions(leagues, lookahead_days = 14L)

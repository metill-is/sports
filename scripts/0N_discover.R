#!/usr/bin/env Rscript
# scripts/0N_discover.R --
# Discover Lengjan competitions we model but do not yet scrape.
#
# Reads Lengjan's public JSON program (no login, no browser) for each active
# (sport, country) at betting.mode "scrape" or above, diffs its competitions
# against config/leagues.yml, and writes data/discovery/proposals.json +
# SUMMARY.md. Read-only on our data (results only) -- never touches the ledger
# or the placer. CI-safe.
#
# Usage:
#   Rscript scripts/0N_discover.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()

findings <- tryCatch(
  discover_new_competitions(leagues),
  error = function(e) {
    cli::cli_alert_warning("Discovery failed: {conditionMessage(e)}")
    NULL
  }
)

if (is.null(findings)) {
  cli::cli_alert_warning("No discovery output this run; keeping last proposal.")
  quit(save = "no", status = 0L)
}

path <- write_discovery_proposal(findings)
n <- length(findings$competitions)
cli::cli_alert_success(
  "Discovery wrote {n} proposed competition(s) to {path} ({findings$unmodelled_offered_count} unmodelled offered)."
)

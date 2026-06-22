#!/usr/bin/env Rscript
# scripts/0N_discover.R --
# Discover Lengjan competitions we model but do not yet scrape.
#
# Reads each active modelled (sport, country)'s "Veldu deild" dropdown, diffs
# against config/leagues.yml, and writes data/discovery/proposals.json +
# SUMMARY.md. Read-only on Lengjan (public dropdown, no login) and on our data
# (results only) -- never touches the ledger or the placer. CI-safe.
#
# Usage:
#   Rscript scripts/0N_discover.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

leagues <- load_leagues()

session <- chromote::ChromoteSession$new()
try(session$default_timeout <- 30, silent = TRUE)
on.exit(session$close(), add = TRUE)

findings <- tryCatch(
  discover_new_competitions(leagues, session = session),
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
  "Discovery wrote {n} proposed competition(s) to {path} ",
  "({findings$unmodelled_offered_count} unmodelled offered)."
)

#!/usr/bin/env Rscript
# Re-normalise stored results + schedules to leagues.yml data_source.team_aliases.
#
# ingest_league() aliases newly fetched rows only. upsert_table() keys on team
# names, so rows already on disk keep the source spelling until this runs --
# run it in the same change that adds an alias
# (tests/testthat/test-ingest-integration.R stays red until you do).
#
# The work is renormalise_team_aliases() (R/ingest.R): it plans every
# partition first and writes only if none would collapse two rows onto one
# natural key, so an abort leaves the store untouched. Idempotent: a second
# run finds nothing to do.
#
# Usage:
#   Rscript scripts/renormalise_team_aliases.R          # dry run: print the row diff
#   Rscript scripts/renormalise_team_aliases.R --apply  # rewrite the partitions

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

apply_changes <- "--apply" %in% commandArgs(trailingOnly = TRUE)
diff <- renormalise_team_aliases(load_leagues(), apply = apply_changes)

groups <- split(diff, interaction(
  diff$league, diff$table, diff$sex, diff$season,
  drop = TRUE, lex.order = TRUE
))
for (g in groups) {
  cli::cli_h2(
    "{g$league[1]} {g$table[1]} sex={g$sex[1]} season={g$season[1]}: {nrow(g)} row{?s}"
  )
  print(
    as.data.frame(g[order(g$match_date), c(
      "match_date", "division", "home_before", "home_after",
      "away_before", "away_after"
    )]),
    row.names = FALSE
  )
}

n_rows <- nrow(diff)
if (n_rows == 0L) {
  cli::cli_alert_success("Nothing to re-normalise.")
} else if (apply_changes) {
  cli::cli_alert_success("Rewrote {n_rows} row{?s}.")
} else {
  cli::cli_alert_info("Dry run: {n_rows} row{?s} would change. Re-run with --apply.")
}

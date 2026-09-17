#!/usr/bin/env Rscript
# Re-normalise stored results + schedules to leagues.yml data_source.team_aliases.
#
# ingest_league() aliases newly fetched rows only. upsert_table() keys on team
# names, so rows already on disk keep the source spelling until this runs --
# run it in the same change that adds an alias
# (tests/testthat/test-ingest-integration.R stays red until you do).
#
# Only (sex, season) partitions that contain an alias key are touched, and
# each is replaced whole through write_table()'s staged rename. Idempotent:
# a second run finds nothing to do.
#
# Usage:
#   Rscript scripts/renormalise_team_aliases.R          # dry run: print the row diff
#   Rscript scripts/renormalise_team_aliases.R --apply  # rewrite the partitions

suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

apply_changes <- "--apply" %in% commandArgs(trailingOnly = TRUE)
leagues <- load_leagues()
n_rows <- 0L

for (key in names(leagues)) {
  lg <- leagues[[key]]
  aliases <- lg$data_source$team_aliases
  if (length(aliases) == 0L) next

  for (table in c("results", "schedules")) {
    rows <- read_table(
      table,
      filter = list(sport = lg$sport, country = lg$country)
    )
    if (nrow(rows) == 0L) next
    stale <- rows$home_team %in% names(aliases) |
      rows$away_team %in% names(aliases)
    if (!any(stale)) next

    parts <- unique(rows[stale, c("sex", "season")])
    for (i in seq_len(nrow(parts))) {
      sex <- parts$sex[[i]]
      season <- parts$season[[i]]
      before <- rows[rows$sex == sex & rows$season == season, ]
      after <- .apply_team_aliases(before, aliases)

      # A row stored under both spellings would collapse onto one natural key.
      key_cols <- natural_key_for(table)
      if (anyDuplicated(after[, key_cols]) > 0L) {
        cli::cli_abort(
          "{key} {table} {sex}/{season}: aliasing creates duplicate natural keys."
        )
      }

      moved <- before$home_team != after$home_team |
        before$away_team != after$away_team
      n_rows <- n_rows + sum(moved)
      cli::cli_h2(
        "{key} {table} sex={sex} season={season}: {sum(moved)} of {nrow(before)} rows"
      )
      diff <- data.frame(
        match_date = before$match_date[moved],
        division = before$division[moved],
        home_before = before$home_team[moved],
        home_after = after$home_team[moved],
        away_before = before$away_team[moved],
        away_after = after$away_team[moved]
      )
      print(diff[order(diff$match_date), ], row.names = FALSE)

      if (apply_changes) write_table(after, table)
    }
  }
}

if (n_rows == 0L) {
  cli::cli_alert_success("Nothing to re-normalise.")
} else if (apply_changes) {
  cli::cli_alert_success("Rewrote {n_rows} row{?s}.")
} else {
  cli::cli_alert_info("Dry run: {n_rows} row{?s} would change. Re-run with --apply.")
}

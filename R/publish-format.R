# ---- The regular-season boundary, and the two round counters -----------------
#
# There is ONE boundary function in the repo and it lives here: the extractor's
# cut and the publisher's cut must be the same cut, or standings and
# final_positions disagree about which matches count.
#
# WHY a cut is needed at all: basketball EMBEDS its post-season in the league
# division. KKI packages urslitakeppni as extra rounds inside the SAME
# season_id (R/ingest-kki-basketball.R:23-24), so `division == "BD"` carries
# both. Measured 2026-09-04 on season 2026: male BD 132 regular of 162, male 1D
# 132 of 159, female BD 90 of 137. Without the cut those post-season points feed
# .compute_base_points_2dt() and the published league table is simulated on
# them -- silently wrong, not a visible error. Handball does not need this: its
# playoff is a separate division.
#
# WHY n_rounds is derived and NOT 2 * (n_teams - 1): Icelandic women's handball
# plays a TRIPLE round robin -- 8 teams, 84 matches, 21 rounds, 28 pairs meeting
# 3x -- against a formula value of 14. The general derivation is a team's
# appearance count, which is also what R/derive-round.R stamps into
# `results$round`.
#
# PRECEDENCE is fixed: a configured `expected_meetings` WINS, the schedule
# derivation is the fallback. Both values are always returned so WS12's
# check_publish_format_agreement() can WARN when a federation changes format
# under a stale config, rather than silently publishing the wrong one.
#
# The "last round with >= floor(n/2) matches" heuristic is REJECTED: per-round
# counts fluctuate with postponements, so the qualifying set is non-contiguous
# in all four basketball cells (female 1D reads 3,5,5,5,4,5,6,5,6,4,6,5,4,5,4,
# 5,7,5,2,2,2,1,1,1).
#
# ---- WHY the boundary is a ROUND CUT and not the KKI stage dimension --------
#
# KKI does label the two stages (finding N7: league 190 carries 300475
# Deildarkeppni / 306658 Urslitakeppni; 191: 300472/306497; 189: 300530/306645
# plus A ridill 305952 and B ridill 305951; 231: 300529/306557), so
# `stage == "Deildarkeppni"` looks like the obvious filter. It is not available
# and would not be sufficient:
#
# 1. `stage` is NOT in the data and cannot be put there from here. Adding it to
#    `schemas()$results` is a schema migration: `validate_against_schema()`
#    (R/storage.R) hard-fails on a missing schema column, which breaks every
#    writer plus the exact-column-set assertion in
#    tests/testthat/test-ingest-kki.R. Plan A deferred it for that reason.
# 2. It would only ever cover basketball. Handball's post-season is a SEPARATE
#    division (`PO` -- verified in data/facts/results season 2026: male 20 rows,
#    female 16), already excluded by the division filter. A round rule has to
#    exist regardless, so `stage` would be a second, partly-overlapping
#    mechanism rather than a replacement.
# 3. The round cut is locally checkable and a vendor stage label is not: the
#    pair-meeting count is an assertion this repo can re-derive from its own
#    parquet (tests/testthat/test-iceland-division-helpers.R does), whereas a
#    federation label can only be trusted.
# 4. `round` already means the right thing. `derive_league_round()`
#    (R/derive-round.R) sets it to each team's cumulative appearance index
#    within (sport, country, sex, season, division), taking the max of the two
#    sides -- which is exactly the matchweek axis a boundary cuts on.
#
# FOLLOW-UP: if those KKI stage ids are ever ingested, `stage` becomes the
# primary source and this round cut becomes its fallback and cross-check --
# not the other way round.

# Rows of `df` in one (season, division set) cell. NULL/0-row in, same out.
# @noRd
.publish_cell_rows <- function(df, season, division_codes) {
  if (is.null(df) || nrow(df) == 0L) {
    return(df)
  }
  df[df$season == season & df$division %in% division_codes, , drop = FALSE]
}

# Every team appearance in `df`, home and away, as one character vector.
# @noRd
.publish_appearances <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(character())
  }
  c(df$home_team, df$away_team)
}

#' Drop post-season rows from a results slice
#'
#' Keeps rows at or below the regular season's last round. Cup rows carry
#' `round = NA` by design (R/derive-round.R leaves `division == "CUP"` unset,
#' because knockout dates are bracket rounds, not matchweeks) and are KEPT --
#' dropping them would empty every cup cell.
#'
#' The identity when `n_rounds` is `NA`: an unresolved boundary must not silently
#' delete data.
#'
#' @param results Results tibble carrying `round`.
#' @param n_rounds Last regular-season round, or `NA_integer_`.
#' @return `results`, filtered.
#' @noRd
.regular_season_results <- function(results, n_rounds) {
  if (is.null(results) || nrow(results) == 0L) {
    return(results)
  }
  if (length(n_rounds) != 1L || is.na(n_rounds)) {
    return(results)
  }
  if (!"round" %in% names(results)) {
    cli::cli_abort(
      "{.fn .regular_season_results} needs a {.field round} column.",
      call = NULL
    )
  }
  results[is.na(results$round) | results$round <= n_rounds, , drop = FALSE]
}

#' How many rounds the regular season has
#'
#' Two independent derivations, both always returned:
#'
#' * `n_rounds_config` -- `expected_meetings * (n_teams - 1)`, from
#'   `config/leagues.yml::<key>.publish_divisions`. This is the authority where
#'   it is set (basketball BD/1D 2, handball male 2, handball female 3).
#' * `n_rounds_schedule` -- the largest number of matches any one team has
#'   played (after the config cut) plus has left scheduled. Fills the gap for a
#'   cell with no configured meetings, e.g. the irregular 11-team female 1D.
#'
#' Verified 2026-09-04: basketball male BD 22 both ways, female BD 18 both ways,
#' female 1D 24 off the schedule alone; synthetic handball female OD 3 * (4 - 1)
#' = 9, never 2 * (4 - 1) = 6.
#'
#' @param results Played rows (needs `season`, `division`, `round`,
#'   `home_team`, `away_team`).
#' @param schedules Forward fixtures, same key columns.
#' @param season Season to slice to.
#' @param division_codes Division codes in this publish cell.
#' @param end_date Publish cutoff; only fixtures strictly after it are "left to
#'   play".
#' @param expected_meetings Configured meetings per pair, or `NULL`.
#' @param is_cup A knockout has no rounds in the league sense at all.
#' @return `list(n_rounds, source, n_rounds_config, n_rounds_schedule,
#'   n_teams)`. `source` is one of `"config"`, `"schedule"`, `"none"`,
#'   `"not_applicable"`.
#' @noRd
.publish_n_rounds <- function(results, schedules, season, division_codes,
                              end_date, expected_meetings = NULL,
                              is_cup = FALSE) {
  played <- .publish_cell_rows(results, season, division_codes)
  upcoming <- .publish_cell_rows(schedules, season, division_codes)
  if (!is.null(upcoming) && nrow(upcoming) > 0L) {
    upcoming <- upcoming[upcoming$match_date > end_date, , drop = FALSE]
  }

  n_teams <- length(unique(c(
    .publish_appearances(played), .publish_appearances(upcoming)
  )))

  shape <- function(n_rounds, source, cfg, sched) {
    list(
      n_rounds = n_rounds, source = source,
      n_rounds_config = cfg, n_rounds_schedule = sched,
      n_teams = n_teams
    )
  }

  # is_cup wins over a configured expected_meetings: the config key may be set
  # league-wide, but a bracket still has no matchweeks.
  if (isTRUE(is_cup)) {
    return(shape(NA_integer_, "not_applicable", NA_integer_, NA_integer_))
  }

  cfg <- if (!is.null(expected_meetings) && !is.na(expected_meetings) &&
    n_teams >= 2L) {
    as.integer(expected_meetings * (n_teams - 1L))
  } else {
    NA_integer_
  }

  # The config cut is applied BEFORE counting appearances, so a cell whose
  # post-season is embedded in the division does not inflate its own schedule
  # derivation with playoff matches.
  apps <- c(
    .publish_appearances(.regular_season_results(played, cfg)),
    .publish_appearances(upcoming)
  )
  sched <- if (length(apps) == 0L) NA_integer_ else as.integer(max(table(apps)))

  if (!is.na(cfg)) {
    shape(cfg, "config", cfg, sched)
  } else if (!is.na(sched)) {
    shape(sched, "schedule", cfg, sched)
  } else {
    shape(NA_integer_, "none", cfg, sched)
  }
}

#' Which round the cell is currently on
#'
#' The FLOOR over the cell's teams, not the ceiling: a team with a game in hand
#' must not make the whole division look a round further on than it is. This
#' reproduces football's published `meta.round` exactly (the inline
#' `min(count by team)` the publisher has always used) while additionally
#' excluding post-season rows, which is what makes it correct for basketball.
#'
#' @param results Results tibble.
#' @param season Season to slice to.
#' @param division_codes Division codes in this publish cell.
#' @param n_rounds From [`.publish_n_rounds()`]; `NA` disables both the cut and
#'   the clamp.
#' @return Integer, clamped into `[0, n_rounds]` when `n_rounds` is finite.
#' @noRd
.publish_round <- function(results, season, division_codes, n_rounds) {
  rows <- .publish_cell_rows(results, season, division_codes)
  rows <- .regular_season_results(rows, n_rounds)
  apps <- .publish_appearances(rows)
  if (length(apps) == 0L) {
    return(0L)
  }
  out <- as.integer(min(table(apps)))
  if (length(n_rounds) == 1L && !is.na(n_rounds)) {
    out <- max(0L, min(out, as.integer(n_rounds)))
  }
  out
}

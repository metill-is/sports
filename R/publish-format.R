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

#' Apply the regular-season boundary, when there is one
#'
#' The gate is the point. `n_rounds` has two sources and only one of them is a
#' BOUNDARY:
#'
#' * `"config"` -- a stated `regular_season_rounds`, or
#'   `expected_meetings * (n_teams - 1)`. Either way it is an independent fact
#'   about the competition format. Played rows past it are post-season, and
#'   basketball's embedded urslitakeppni is exactly that (male BD 162 -> 132,
#'   female 1D 98 -> 89).
#' * `"schedule"` -- derived FROM the played and scheduled rows themselves.
#'   Cutting those same rows by it is circular: it can never identify a
#'   post-season row, and it CAN delete regular-season rows wherever `round` is
#'   stamped on a different axis from appearance counting.
#'
#' Measured 2026-09-04: the schedule branch is the identity on real data in
#' every cell (all nine football cells, and basketball female 1D 98 -> 98 while
#' that cell still had no configured boundary), so the gate costs nothing
#' there; on the synthetic fixture the ungated filter deleted one played match
#' from six football cells and one basketball cell.
#'
#' The FORWARD half of the cut (`.regular_season_game_nrs_2dt()`) is NOT gated:
#' capping how many fixtures are left to play is a question about season
#' length, which both sources answer.
#'
#' @param results Results tibble carrying `round`.
#' @param format A [`.publish_n_rounds()`] result.
#' @return `results`, filtered when the boundary is authoritative.
#' @noRd
.regular_season_cut <- function(results, format) {
  .regular_season_results(results, format$cut)
}

#' How many rounds the regular season has
#'
#' Two independent derivations, both always returned:
#'
#' * `n_rounds_config` -- the CONFIGURED boundary, from
#'   `config/leagues.yml::<key>.publish_divisions`. Two keys can supply it and
#'   `regular_season_rounds` wins: it states the last regular round outright,
#'   while `expected_meetings` states meetings per pair and the boundary is
#'   derived as `expected_meetings * (n_teams - 1)` (basketball BD/1D 2,
#'   handball male 2, handball female 3).
#' * `n_rounds_schedule` -- the largest number of matches any one team has
#'   played (after the config cut) plus has left scheduled. The fallback for a
#'   cell that configures neither key.
#'
#' WHY `regular_season_rounds` exists at all. Basketball female 1. deild plays
#' 10 teams over rounds 1-18 and then an embedded 4-team promotion playoff over
#' rounds 19-24 which brings in an ELEVENTH team (measured on
#' `data/facts/results` season 2026: 89 rows over 10 teams inside the cut, 9
#' rows over 4 teams past it, Hamar/Thor appearing only in the playoff). No
#' meetings-per-pair constant survives that -- the pair table inside the cut is
#' 44 pairs twice and one pair once, and `n_teams` counted over the whole
#' season is 11 -- so the number cannot be derived and has to be stated. Left
#' to the schedule fallback the cell published its playoff as regular season,
#' `n_rounds` 24, and a `meta.round` of 6 (the floor over appearances, i.e.
#' the playoff-only team's 6 games) for a season that had finished.
#'
#' Verified 2026-09-04: basketball male BD 22 both ways, female BD 18 both ways;
#' synthetic handball female OD 3 * (4 - 1) = 9, never 2 * (4 - 1) = 6.
#' Verified 2026-09-05: female 1D 24 off the schedule alone (meta.round 6), 18
#' with the boundary stated (98 rows -> 89, meta.round 17).
#'
#' @param results Played rows (needs `season`, `division`, `round`,
#'   `home_team`, `away_team`).
#' @param schedules Forward fixtures, same key columns.
#' @param season Season to slice to.
#' @param division_codes Division codes in this publish cell.
#' @param end_date Publish cutoff; only fixtures strictly after it are "left to
#'   play".
#' @param expected_meetings Configured meetings per pair, or `NULL`.
#' @param regular_season_rounds The last regular-season round, stated outright,
#'   or `NULL`. Takes precedence over `expected_meetings`; both yield
#'   `source == "config"`, and both set the `cut`.
#' @param is_cup A knockout has no rounds in the league sense at all.
#' @return `list(n_rounds, source, n_rounds_config, n_rounds_schedule,
#'   n_teams, cut)`. `source` is one of `"config"`, `"schedule"`, `"none"`,
#'   `"not_applicable"`.
#' @noRd
.publish_n_rounds <- function(results, schedules, season, division_codes,
                              end_date, expected_meetings = NULL,
                              regular_season_rounds = NULL,
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
      n_teams = n_teams,
      # The BOUNDARY, which is not the same thing as the LENGTH. Only a
      # CONFIGURED boundary -- `regular_season_rounds`, or the
      # `expected_meetings` derivation -- is evidence that played rows past it
      # are post-season; see `.regular_season_cut()`.
      cut = if (identical(source, "config")) as.integer(n_rounds) else NA_integer_
    )
  }

  # is_cup wins over a configured expected_meetings: the config key may be set
  # league-wide, but a bracket still has no matchweeks.
  if (isTRUE(is_cup)) {
    return(shape(NA_integer_, "not_applicable", NA_integer_, NA_integer_))
  }

  # The stated boundary is the authority. It is a fact about the federation's
  # calendar, whereas the meetings derivation is a formula over whatever teams
  # happen to appear -- and the cell that needs it is precisely the one whose
  # team set is inflated by the post-season it is meant to cut.
  cfg <- if (!is.null(regular_season_rounds) &&
    !is.na(regular_season_rounds)) {
    as.integer(regular_season_rounds)
  } else if (!is.null(expected_meetings) && !is.na(expected_meetings) &&
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
#' @param n_rounds From [`.publish_n_rounds()`]; the CLAMP. `NA` disables it.
#' @param cut The boundary to drop post-season rows at, normally
#'   `.publish_n_rounds()$cut`. Defaults to `n_rounds`, which is the same thing
#'   whenever the boundary is configured. See [`.regular_season_cut()`] for why
#'   the two are not always equal.
#' @return Integer, clamped into `[0, n_rounds]` when `n_rounds` is finite.
#' @noRd
.publish_round <- function(results, season, division_codes, n_rounds,
                           cut = n_rounds) {
  rows <- .publish_cell_rows(results, season, division_codes)
  rows <- .regular_season_results(rows, cut)
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


# ---- meta.json v2 ------------------------------------------------------------
#
# WHY the producer assembles this and the consumer does not: the league format
# and the points scheme are facts about the competition that the producer can
# see in the data and the consumer cannot. metill-platform used to compute
# `total_rounds = 2 * (n_teams - 1)` and `max_points = round * 3`; both are
# wrong for basketball (2 points a win, embedded post-season) and the first is
# wrong for women's handball (a triple round robin). Spec section 12.
#
# The v1 block is copied VERBATIM. Its ten keys are what every live football
# cell publishes today, and the golden manifest hashes key ORDER as well as
# values, so re-ordering them would be a silent payload change.

#' Assemble a `meta.json` payload
#'
#' Appends the v2 block to the v1 keys the publisher already built. Key order
#' is part of the contract -- see the note above.
#'
#' The D3 relabel (design section 15) is carried HERE, in the payload, rather
#' than in a template: for basketball and handball the league table decides the
#' *deildarmeistari* and the Islandsmeistari comes out of an unmodelled
#' urslitakeppni, so `season_scope` and `postseason` exist precisely so a
#' consumer cannot reuse football's champion copy for a sport where it is false.
#'
#' @param base The v1 key block, already ordered
#'   (`sport` .. `n_draws`, plus `split` on a split-season cell).
#' @param profile [`sport_publish_profile()`] for this sport.
#' @param format A [`.publish_n_rounds()`] result.
#' @param division_cfg `list(qualify, relegation_slots, expected_meetings)`,
#'   assembled by the caller from the `.iceland_division_*()` accessors
#'   indexed by division code. `qualify` is `NULL` or
#'   `list(slots, label_is)`; an absent one publishes `qualify: null` and
#'   suppresses `p_qualify` downstream, because four bb/hb cells have four
#'   different post-season structures and no per-division integer expresses
#'   them (Plan B ID-B15).
#' @return Named list, written verbatim by `write_json_consistent()`.
#' @noRd
.build_publish_meta <- function(base, profile, format, division_cfg) {
  stopifnot(is.list(base), is.list(profile), is.list(format))
  n_rounds <- if (is.null(format$n_rounds)) NA_integer_ else as.integer(format$n_rounds)
  round <- if (is.null(base$round)) NA_integer_ else as.integer(base$round)

  # The guard the whole workstream exists for: a payload whose round exceeds
  # its season length renders "Umferdir eftir" as a negative number. The
  # producer refuses to write it rather than leaving the consumer to clamp.
  if (!is.na(n_rounds) && !is.na(round) && round > n_rounds) {
    cli::cli_abort(
      c(
        "{.field round} ({round}) exceeds {.field n_rounds} ({n_rounds}) for \
         {base$sport} {base$sex} {base$division}.",
        "i" = "Source of {.field n_rounds} was {.val {format$source}}.",
        "i" = "A published cell with round > n_rounds renders a negative \
               {.q Umfer\u00f0ir eftir}."
      ),
      call = NULL
    )
  }

  relegation_slots <- division_cfg$relegation_slots
  if (is.null(relegation_slots)) relegation_slots <- NA_integer_

  c(
    base,
    list(
      n_rounds        = n_rounds,
      n_rounds_source = as.character(format$source),
      units           = profile$units,
      points          = profile$points,
      season_scope    = profile$season_scope,
      postseason      = profile$postseason,
      qualify         = division_cfg$qualify,
      relegation      = list(slots = as.integer(relegation_slots))
    )
  )
}


# ---- The placement summary ---------------------------------------------------
#
# One builder for all three sports, replacing the football-only `top_six` block
# and the mean-over-iterations form the retired per-sport 2DT publisher used.
#
# p_qualify is the GENERIC replacement for p_top_six, and it is emitted only
# where a division actually configures a qualification cut. It does not
# transfer to basketball or handball: measured on season 2026, male Bonusdeild
# takes 8 of 12 through, male 1. deild 8 of 12, female Bonusdeild 10 of 10 and
# female 1. deild 4 of 11. Four cells, four structures, and the women's top
# flight takes EVERY team through -- no per-division integer expresses that,
# and shipping one would be the "top-six number wearing a playoff label"
# failure D3 exists to prevent (Plan B ID-B15). So bb/hb configure no
# `qualify` and publish no p_qualify.

#' Headline per-team probabilities for `final_positions.json`
#'
#' @param final_positions Tibble with `team`, `placement`, `probability`.
#' @param n_teams Teams in the cell; only used by the legacy relegation rule.
#' @param basis `"final_table"` or `"regular_season_table"`. `p_winner` is a
#'   claim about the season's CHAMPION, so it is emitted only on a final table
#'   -- a basketball league table decides the *deildarmeistari* and the
#'   Islandsmeistari comes out of an unmodelled urslitakeppni (design 15).
#' @param qualify `NULL`, or `list(slots, label_is)` from
#'   `.iceland_division_qualify()`. `NULL` publishes no `p_qualify`.
#' @param relegation_slots Teams relegated, or `NA_integer_`. `NA` keeps
#'   football's published expression (`placement >= n_teams - 1`) verbatim;
#'   `0` publishes zeros, present-and-zero rather than a missing key, so no
#'   consumer needs a null branch for a bottom-tier division.
#' @param emit_top_six_alias Football only. `p_top_six` is a DEPRECATED ALIAS
#'   kept because metill-platform reads it today; it is the literal
#'   `placement <= 6L` rule, NOT a function of `qualify`, so the five football
#'   cells with no configured cut keep publishing it. Removed in the follow-up
#'   commit whose only job is that removal, once the platform reads p_qualify.
#' @return Tibble: `team`, `p_qualify` (when configured), `p_top_of_table`,
#'   `p_winner` (final tables only), `p_top_six` (football only),
#'   `p_relegation`.
#' @noRd
.build_placement_summary <- function(final_positions, n_teams, basis,
                                     qualify = NULL,
                                     relegation_slots = NA_integer_,
                                     emit_top_six_alias = FALSE) {
  stopifnot(basis %in% c("final_table", "regular_season_table"))
  qualify_slots <- if (is.null(qualify)) NA_integer_ else as.integer(qualify$slots)
  releg <- if (is.null(relegation_slots)) NA_integer_ else as.integer(relegation_slots)
  releg_floor <- if (is.na(releg)) {
    as.integer(n_teams) - 1L
  } else {
    as.integer(n_teams) - releg + 1L
  }

  out <- final_positions |>
    dplyr::summarise(
      p_qualify = sum(.data$probability[.data$placement <= qualify_slots]),
      p_top_of_table = sum(.data$probability[.data$placement == 1L]),
      p_winner = sum(.data$probability[.data$placement == 1L]),
      p_top_six = sum(.data$probability[.data$placement <= 6L]),
      p_relegation = if (identical(releg, 0L)) {
        0
      } else {
        sum(.data$probability[.data$placement >= releg_floor])
      },
      .by = "team"
    )

  keep <- c(
    "team",
    if (!is.na(qualify_slots)) "p_qualify",
    "p_top_of_table",
    if (identical(basis, "final_table")) "p_winner",
    if (isTRUE(emit_top_six_alias)) "p_top_six",
    "p_relegation"
  )
  out[, keep, drop = FALSE]
}

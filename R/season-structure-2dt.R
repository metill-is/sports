#' @include publish-format.R extract-football-iceland.R publish-iceland-2dt-helpers.R
NULL

# Season, format and remaining fixtures for the 2DT sports (spec 2026-09-16
# §5-§6). The extractor and the publisher both call these, so the two layers
# cannot disagree about which season is current or how long it is.

.is_set_2dt <- function(x) {
  length(x) == 1L && !is.na(x)
}

# The current season of one division: the latest season with a played result
# on or before `end_date`, or with a fixture scheduled after it.
#
# Results alone (the old rule, F8) keep a finished season current until its
# successor's first match, so a pre-season forecast showed last season's
# table. Football keeps that rule (D5): KSI publishes its schedule in halves.
#
# `hold` pins a division (config `preseason_hold`): its schedule is ignored and
# the result never passes the held season, even once the next season starts.
.current_season_2dt <- function(results, schedules, end_date, division,
                                hold = NA_integer_) {
  end_date <- as.Date(end_date)
  seasons <- integer()
  if (!is.null(results) && nrow(results) > 0L) {
    played <- results$division %in% division &
      !is.na(results$match_date) & results$match_date <= end_date &
      !is.na(results$home_score) & !is.na(results$away_score)
    seasons <- c(seasons, results$season[played])
  }
  held <- .is_set_2dt(hold)
  if (!held && !is.null(schedules) && nrow(schedules) > 0L) {
    ahead <- schedules$division %in% division &
      !is.na(schedules$match_date) & schedules$match_date > end_date
    seasons <- c(seasons, schedules$season[ahead])
  }
  seasons <- seasons[!is.na(seasons)]
  season <- if (length(seasons) == 0L) {
    as.integer(format(end_date, "%Y"))
  } else {
    as.integer(max(seasons))
  }
  if (held) {
    season <- min(season, as.integer(hold))
  }
  season
}

# ---- Format -------------------------------------------------------------------

# When the season's own fixture list is trusted as a format statement: it must
# name (nearly) every pairing, agree with itself on one meetings count, and
# give every team about the same number of games. A partial list -- a few
# dated fixtures, or a regular season with its play-offs appended -- fails at
# least one test and falls through to config.
MULTIPLICITY_SCHEDULE_COVERAGE <- 0.9
MULTIPLICITY_SCHEDULE_AGREEMENT <- 0.75
MULTIPLICITY_SCHEDULE_BALANCE <- 0.1

# Meetings per pairing for one division's season (spec 2026-09-16 §6).
#
# Order: the season's own fixtures, then config `expected_meetings`, then the
# last completed season's results. The season's own fixtures come first
# because formats change between seasons: women's Olisdeild went from a triple
# round robin of 8 (2026) to a double of 10 (2027) while config and history
# both still said 3 (F17).
#
# A schedule that AGREES with a stated `expected_meetings` -- or that has no
# config to agree or disagree with -- is trusted outright. A schedule that
# DISAGREES with a stated config is trusted only when the division's roster
# size changed since the last completed prior season, the exact signal F17's
# 8 -> 10 jump carried. `.schedule_meetings_2dt()`'s own gates (coverage,
# agreement, balance) catch most partial lists, but not all: three balanced,
# fully-covering return legs (every team gets exactly one, e.g. a
# double-round-robin caught one match day in) can clear all three at once. On
# an unchanged roster that disagreeing reading is far more likely such a
# partial list than a real reform, and needs a config edit to be believed, so
# it falls through to config instead of overriding it.
#
# Config comes before history, the reverse of the spec. History is read with
# football's `.division_rr_multiplicity_pfi()`, whose max over pairs reads
# basketball's embedded urslitakeppni as 4-5 meetings; a stated format beats
# that guess, and on every configured cell the two agree anyway.
#
# A stated `regular_season_rounds` means no meetings constant describes the
# cell (basketball female 1D); the meetings stay unknown and the remaining
# fixtures are the schedule's.
.division_format_2dt <- function(results, schedules, season, division,
                                 expected_meetings = NA_integer_,
                                 regular_season_rounds = NA_integer_) {
  if (.is_set_2dt(regular_season_rounds)) {
    return(list(meetings = NA_integer_, source = "not_applicable"))
  }
  from_schedule <- .schedule_meetings_2dt(results, schedules, season, division)
  if (!is.na(from_schedule)) {
    agrees <- !.is_set_2dt(expected_meetings) ||
      as.integer(expected_meetings) == from_schedule
    if (agrees || .division_size_changed_2dt(results, schedules, season, division)) {
      return(list(meetings = from_schedule, source = "schedule"))
    }
  }
  if (.is_set_2dt(expected_meetings)) {
    return(list(meetings = as.integer(expected_meetings), source = "config"))
  }
  prior <- .division_rr_multiplicity_pfi(results, season, division)
  if (!is.na(prior)) {
    return(list(meetings = as.integer(prior), source = "prior_results"))
  }
  list(meetings = NA_integer_, source = "none")
}

# Whether the division's roster size in `season` (teams named in that
# season's results or schedule rows, played or not) differs from its roster
# size in the last completed prior season (teams named in that season's
# PLAYED results). With no prior season to compare against there is nothing
# to confirm a change against, so this reports no change -- and, in
# `.division_format_2dt()`, config wins.
.division_size_changed_2dt <- function(results, schedules, season, division) {
  teams_of <- function(df, seasons, played_only = FALSE) {
    if (is.null(df) || nrow(df) == 0L) {
      return(character())
    }
    rows <- df$division == division & df$season %in% seasons
    if (played_only) {
      rows <- rows & !is.na(df$home_score) & !is.na(df$away_score)
    }
    unique(c(df$home_team[rows], df$away_team[rows]))
  }
  current <- unique(c(
    teams_of(results, season), teams_of(schedules, season)
  ))
  prior_seasons <- if (is.null(results) || nrow(results) == 0L) {
    integer()
  } else {
    played <- results$division == division & results$season < season &
      !is.na(results$home_score) & !is.na(results$away_score)
    results$season[played]
  }
  if (length(prior_seasons) == 0L) {
    return(FALSE)
  }
  prior <- teams_of(results, max(prior_seasons), played_only = TRUE)
  length(current) != length(prior)
}

# The modal meetings count over the season's played and scheduled fixtures
# (de-duplicated on date: the current season's schedule keeps its played
# rows), or NA when the list is not a trustworthy format statement.
.schedule_meetings_2dt <- function(results, schedules, season, division) {
  cols <- c("home_team", "away_team", "match_date")
  pick <- function(df) {
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }
    df[df$season == season & df$division == division &
      !is.na(df$match_date), cols, drop = FALSE]
  }
  fx <- dplyr::distinct(dplyr::bind_rows(pick(results), pick(schedules)))
  if (nrow(fx) == 0L) {
    return(NA_integer_)
  }
  teams <- unique(c(fx$home_team, fx$away_team))
  n_pairs <- length(teams) * (length(teams) - 1L) / 2L
  pair <- paste(
    pmin(fx$home_team, fx$away_team), pmax(fx$home_team, fx$away_team),
    sep = "|"
  )
  meetings <- as.integer(table(pair))
  if (n_pairs < 1L || length(meetings) / n_pairs < MULTIPLICITY_SCHEDULE_COVERAGE) {
    return(NA_integer_)
  }
  counts <- table(meetings)
  top <- max(counts)
  if (top / length(meetings) < MULTIPLICITY_SCHEDULE_AGREEMENT) {
    return(NA_integer_)
  }
  games <- as.integer(table(c(fx$home_team, fx$away_team)))
  slack <- max(1L, as.integer(floor(MULTIPLICITY_SCHEDULE_BALANCE * max(games))))
  if (max(games) - min(games) > slack) {
    return(NA_integer_)
  }
  # Agreement >= MULTIPLICITY_SCHEDULE_AGREEMENT (0.75) means the modal count
  # holds more than half the pairs, so it is unique -- two counts could not
  # both clear a majority share. max() here just extracts that one value from
  # the (length-one) subset; it is not resolving a tie.
  max(as.integer(names(counts)[counts == top]))
}

# ---- Remaining fixtures ------------------------------------------------------

# Every fixture of the season still to be played, derived structurally
# (football's approach, R/extract-football-iceland.R): each pairing meets
# `meetings` times in all, each side hosting at most ceiling(meetings / 2).
#
# Scheduled fixtures are taken first, in date order, because their venues are
# real; a scheduled game beyond the pairing's meetings (an embedded play-off)
# or on a venue already used up is skipped. What the schedule does not cover
# is generated, alternating venues (F16). This replaces the per-team cap that
# counted last season's games (F9) and any dependence on Stan's 14-day window.
#
# Unknown `meetings` returns the schedule as is.
.remaining_fixtures_2dt <- function(teams, played, scheduled, meetings) {
  teams <- sort(unique(as.character(teams)))
  sched <- if (is.null(scheduled) || nrow(scheduled) == 0L) {
    tibble::tibble(
      home_team = character(), away_team = character(),
      match_date = as.Date(character())
    )
  } else {
    scheduled[
      scheduled$home_team %in% teams & scheduled$away_team %in% teams,
      c("home_team", "away_team", "match_date"),
      drop = FALSE
    ]
  }
  sched <- sched[order(sched$match_date), , drop = FALSE]
  if (!.is_set_2dt(meetings)) {
    return(tibble::tibble(home_team = sched$home_team, away_team = sched$away_team))
  }
  if (length(teams) < 2L) {
    return(tibble::tibble(home_team = character(), away_team = character()))
  }

  m <- as.integer(meetings)
  cap <- (m + 1L) %/% 2L
  key <- function(h, a) paste(h, a, sep = "|")
  played_n <- if (is.null(played) || nrow(played) == 0L) {
    integer()
  } else {
    table(key(played$home_team, played$away_team))
  }
  n_played <- function(h, a) {
    k <- key(h, a)
    if (k %in% names(played_n)) as.integer(played_n[[k]]) else 0L
  }
  sched_pair <- key(
    pmin(sched$home_team, sched$away_team),
    pmax(sched$home_team, sched$away_team)
  )

  out_h <- character()
  out_a <- character()
  pairs <- utils::combn(teams, 2L)
  for (j in seq_len(ncol(pairs))) {
    a <- pairs[1L, j]
    b <- pairs[2L, j]
    n_ab <- n_played(a, b)
    n_ba <- n_played(b, a)
    for (r in which(sched_pair == key(a, b))) {
      if (n_ab + n_ba >= m) {
        break
      }
      if (sched$home_team[r] == a && n_ab < cap) {
        out_h <- c(out_h, a)
        out_a <- c(out_a, b)
        n_ab <- n_ab + 1L
      } else if (sched$home_team[r] == b && n_ba < cap) {
        out_h <- c(out_h, b)
        out_a <- c(out_a, a)
        n_ba <- n_ba + 1L
      }
    }
    while (n_ab + n_ba < m) {
      if (n_ab <= n_ba && n_ab < cap) {
        out_h <- c(out_h, a)
        out_a <- c(out_a, b)
        n_ab <- n_ab + 1L
      } else {
        out_h <- c(out_h, b)
        out_a <- c(out_a, a)
        n_ba <- n_ba + 1L
      }
    }
  }
  tibble::tibble(home_team = out_h, away_team = out_a)
}

# ---- Base table ----------------------------------------------------------------

# The realised table the season simulation starts from: every division team,
# played or not, with points on the published 2DT scheme.
#
# A stray unscored row in `played` (NA home/away score -- a postponed or
# not-yet-played fixture that ended up in the played list) is dropped before
# tabulating rather than summed: `sum()` on any NA propagates, and the
# subsequent `coalesce(., 0L)` -- meant only for teams with no rows at all --
# would then silently zero out an otherwise-decisive team's goal difference
# and goals for.
.base_standings_2dt <- function(played, teams, has_ties = FALSE,
                                tie_threshold = 0) {
  teams <- sort(unique(as.character(teams)))
  if (!is.null(played)) {
    played <- played[
      !is.na(played$home_score) & !is.na(played$away_score), ,
      drop = FALSE
    ]
  }
  if (is.null(played) || nrow(played) == 0L) {
    return(tibble::tibble(
      team = teams, base_points = 0L, base_gd = 0L, base_gf = 0L
    ))
  }
  side <- function(name) {
    is_home <- identical(name, "home")
    tibble::tibble(
      team = if (is_home) played$home_team else played$away_team,
      gf = if (is_home) played$home_score else played$away_score,
      ga = if (is_home) played$away_score else played$home_score,
      pts = .points_2dt(
        played$home_score, played$away_score, name,
        has_ties = has_ties, tie_threshold = tie_threshold
      )
    )
  }
  realised <- dplyr::bind_rows(side("home"), side("away")) |>
    dplyr::summarise(
      base_points = sum(.data$pts),
      base_gd = sum(.data$gf - .data$ga),
      base_gf = sum(.data$gf),
      .by = "team"
    )
  tibble::tibble(team = teams) |>
    dplyr::left_join(realised, by = "team") |>
    dplyr::transmute(
      team = .data$team,
      base_points = as.integer(dplyr::coalesce(.data$base_points, 0L)),
      base_gd = as.integer(dplyr::coalesce(.data$base_gd, 0L)),
      base_gf = as.integer(dplyr::coalesce(.data$base_gf, 0L))
    )
}

# The regular-season boundary, proven on real federation data.
#
# Basketball EMBEDS its post-season in the league division: KKI packages
# urslitakeppni as extra rounds inside the SAME season_id
# (R/ingest-kki-basketball.R:23-24), so `division == "BD"` carries both. Without
# a cut, .compute_base_points_2dt() simulates the league table on post-season
# points -- a silently wrong table, not a visible error. Handball does not have
# this problem: its playoff is a separate division.
#
# n_rounds is DERIVED, and 2 * (n_teams - 1) is not the derivation: Icelandic
# women's handball plays a TRIPLE round robin (8 teams, 84 matches, 21 rounds),
# against a formula value of 14. Configured `expected_meetings` wins; the
# schedule derivation is the fallback; both are always returned so
# check_publish_format_agreement() can WARN when they disagree.

.overhang_cell <- function(sex, division) {
  d <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "playoff-overhang.parquet")
  )
  d[d$sex == sex & d$division == division, , drop = FALSE]
}

# ---- Block A: real basketball data, the embedded post-season -----------------

test_that("configured expected_meetings cuts basketball's embedded post-season", {
  # male BD: 162 rows, 12 teams, 2 meetings -> 22 rounds -> 132 regular + 30 PO
  bd_male <- .overhang_cell("male", "BD")
  expect_equal(nrow(bd_male), 162L)

  n <- .publish_n_rounds(
    results = bd_male, schedules = bd_male[0, , drop = FALSE],
    season = 2026L, division_codes = "BD",
    end_date = as.Date("2026-06-01"), expected_meetings = 2L
  )
  expect_equal(n$n_rounds, 22L)
  expect_equal(n$source, "config")
  expect_equal(n$n_teams, 12L)
  expect_equal(n$n_rounds_config, 22L)
  # Both paths are always returned, and here they agree -- so WS12's format
  # agreement check has nothing to warn about for this cell.
  expect_equal(n$n_rounds_schedule, 22L)

  expect_equal(nrow(.regular_season_results(bd_male, 22L)), 132L)
  expect_equal(.publish_round(bd_male, 2026L, "BD", 22L), 22L)

  # The bug this closes: standings.played is 35 for this cell, so a meta.round
  # of 35 against a 22-round season renders "Umferdir eftir" as -13.
  expect_true(max(bd_male$round) > 22L)
})

test_that("the cut holds across all three configured basketball cells", {
  cells <- list(
    list(sex = "male",   div = "1D", rows = 159L, teams = 12L, rounds = 22L, regular = 132L),
    list(sex = "female", div = "BD", rows = 137L, teams = 10L, rounds = 18L, regular =  90L)
  )
  for (cell in cells) {
    x <- .overhang_cell(cell$sex, cell$div)
    expect_equal(nrow(x), cell$rows, info = cell$div)
    n <- .publish_n_rounds(
      results = x, schedules = x[0, , drop = FALSE],
      season = 2026L, division_codes = cell$div,
      end_date = as.Date("2026-06-01"), expected_meetings = 2L
    )
    expect_equal(n$n_rounds, cell$rounds, info = cell$div)
    expect_equal(n$source, "config", info = cell$div)
    expect_equal(n$n_teams, cell$teams, info = cell$div)
    expect_equal(
      nrow(.regular_season_results(x, cell$rounds)), cell$regular,
      info = cell$div
    )
    expect_equal(
      .publish_round(x, 2026L, cell$div, cell$rounds), cell$rounds,
      info = cell$div
    )
  }
})

test_that("an unconfigured cell falls back to the schedule derivation", {
  # female 1D: 11 teams, no configured expected_meetings -- the deliberately
  # irregular cell (per-round counts fluctuate: 3,5,5,5,4,5,6,5,6,4,...).
  x <- .overhang_cell("female", "1D")
  expect_equal(nrow(x), 98L)

  n <- .publish_n_rounds(
    results = x, schedules = x[0, , drop = FALSE],
    season = 2026L, division_codes = "1D",
    end_date = as.Date("2026-06-01"), expected_meetings = NULL
  )
  expect_equal(n$source, "schedule")
  expect_equal(n$n_rounds, 24L)
  expect_equal(n$n_teams, 11L)
  expect_true(is.na(n$n_rounds_config))
  expect_equal(n$n_rounds_schedule, 24L)
  # The round floor, not the ceiling: the least-progressed team has played 6.
  expect_equal(.publish_round(x, 2026L, "1D", n$n_rounds), 6L)
})

# ---- Block B: the synthetic facts fixture ------------------------------------

test_that("the women's handball triple round robin is honoured", {
  root <- fixture_facts_root()
  results <- read_table("results", root = root)
  schedules <- read_table("schedules", root = root)

  hb_f <- results[
    results$sport == "handball" & results$sex == "female" &
      results$division == "OD" & results$season == 2100L, ,
    drop = FALSE
  ]
  sc_f <- schedules[
    schedules$sport == "handball" & schedules$sex == "female" &
      schedules$division == "OD" & schedules$season == 2100L, ,
    drop = FALSE
  ]

  n <- .publish_n_rounds(
    results = hb_f, schedules = sc_f, season = 2100L,
    division_codes = "OD", end_date = FIXTURE_END_DATE,
    expected_meetings = 3L
  )
  expect_equal(n$n_teams, 4L)
  expect_equal(n$source, "config")
  # 3 * (4 - 1) = 9, NOT 2 * (4 - 1) = 6.
  expect_equal(n$n_rounds, 9L)
  expect_false(identical(n$n_rounds, 6L))
  expect_equal(.publish_round(hb_f, 2100L, "OD", n$n_rounds), 3L)
})

test_that("a double round robin resolves to 2 * (n_teams - 1)", {
  root <- fixture_facts_root()
  results <- read_table("results", root = root)
  schedules <- read_table("schedules", root = root)

  hb_m <- results[
    results$sport == "handball" & results$sex == "male" &
      results$division == "OD" & results$season == 2100L, ,
    drop = FALSE
  ]
  sc_m <- schedules[
    schedules$sport == "handball" & schedules$sex == "male" &
      schedules$division == "OD" & schedules$season == 2100L, ,
    drop = FALSE
  ]

  n <- .publish_n_rounds(
    results = hb_m, schedules = sc_m, season = 2100L,
    division_codes = "OD", end_date = FIXTURE_END_DATE,
    expected_meetings = 2L
  )
  expect_equal(n$source, "config")
  expect_equal(n$n_rounds, 6L)
  expect_equal(.publish_round(hb_m, 2100L, "OD", n$n_rounds), 3L)
})

test_that("the schedule derivation counts appearances, never schedules$round", {
  root <- fixture_facts_root()
  results <- read_table("results", root = root)
  schedules <- read_table("schedules", root = root)

  fb <- results[
    results$sport == "football" & results$sex == "male" &
      results$division == "BD" & results$season == 2100L, ,
    drop = FALSE
  ]
  sc <- schedules[
    schedules$sport == "football" & schedules$sex == "male" &
      schedules$division == "BD" & schedules$season == 2100L, ,
    drop = FALSE
  ]
  expect_equal(nrow(fb), 66L)
  expect_equal(nrow(sc), 3L)

  n <- .publish_n_rounds(
    results = fb, schedules = sc, season = 2100L,
    division_codes = "BD", end_date = FIXTURE_END_DATE,
    expected_meetings = NULL
  )
  expect_equal(n$source, "schedule")
  expect_equal(n$n_teams, 12L)
  # Every team has played 11; the fixture's 3 forward fixtures reuse two teams
  # (01 v 02, 03 v 04, 02 v 03), so the most-scheduled team reaches 13.
  expect_equal(n$n_rounds, 13L)
  # The fixture stamps schedules$round 90/91/92 precisely to catch a derivation
  # that reads the column instead of counting appearances.
  expect_false(identical(n$n_rounds, 92L))
  expect_true(all(sc$round >= 90L))

  # round is the FLOOR over teams -- the value football's meta.json publishes.
  expect_equal(.publish_round(fb, 2100L, "BD", n$n_rounds), 11L)
})

# ---- Block C: the edges ------------------------------------------------------

test_that("a cup is not_applicable and its round is a bracket floor", {
  root <- fixture_facts_root()
  results <- read_table("results", root = root)

  cup <- results[
    results$sport == "football" & results$sex == "male" &
      results$division == "CUP" & results$season == 2100L, ,
    drop = FALSE
  ]
  n <- .publish_n_rounds(
    results = cup, schedules = cup[0, , drop = FALSE], season = 2100L,
    division_codes = "CUP", end_date = FIXTURE_END_DATE,
    expected_meetings = 2L, is_cup = TRUE
  )
  expect_true(is.na(n$n_rounds))
  expect_equal(n$source, "not_applicable")
  expect_true(is.na(n$n_rounds_config))
  expect_true(is.na(n$n_rounds_schedule))
  # is_cup wins over a configured expected_meetings: a knockout has no rounds
  # in the league sense at all.
  expect_equal(n$n_teams, 4L)

  # With n_rounds NA the cut is the identity, so round is the min appearance
  # count over the bracket -- 3 in this all-play-all synthetic cup.
  expect_equal(.publish_round(cup, 2100L, "CUP", n$n_rounds), 3L)
})

test_that("a real knockout bracket reports round 1 while a first-round loser remains", {
  # The shape data/publish/football/iceland/karla-bikar/meta.json publishes
  # today (round 1): real CUP rows carry round = NA (R/derive-round.R's cup
  # carve-out), and the teams knocked out in round one appear exactly once.
  cup <- tibble::tibble(
    match_date = as.Date(c("2100-01-02", "2100-01-02", "2100-01-09")),
    home_team = c("A", "C", "A"),
    away_team = c("B", "D", "C"),
    division = "CUP", season = 2100L, round = NA_integer_
  )
  expect_equal(.publish_round(cup, 2100L, "CUP", NA_integer_), 1L)
  # NA rounds survive the cut -- otherwise every cup row would be dropped.
  expect_equal(nrow(.regular_season_results(cup, 22L)), 3L)
})

test_that("an empty cell resolves to none rather than aborting", {
  empty <- tibble::tibble(
    match_date = as.Date(character()), home_team = character(),
    away_team = character(), division = character(),
    season = integer(), round = integer()
  )
  n <- .publish_n_rounds(
    results = empty, schedules = empty, season = 2026L,
    division_codes = "BD", end_date = as.Date("2026-06-01"),
    expected_meetings = 2L
  )
  expect_equal(n$n_teams, 0L)
  expect_true(is.na(n$n_rounds))
  expect_equal(n$source, "none")
  expect_true(is.na(n$n_rounds_config))
  expect_true(is.na(n$n_rounds_schedule))
  expect_equal(.publish_round(empty, 2026L, "BD", NA_integer_), 0L)
})

test_that(".regular_season_results is the identity when n_rounds is unknown", {
  x <- .overhang_cell("male", "BD")
  expect_equal(nrow(.regular_season_results(x, NA_integer_)), nrow(x))
  expect_identical(.regular_season_results(x, NA_integer_), x)
})

# ---- Block D: meta.json v2 assembly ------------------------------------------
#
# Key ORDER is asserted, not just membership: publish_json_digest() hashes
# jsonlite::toJSON() of the parsed list, so order is part of the payload
# identity and a silent re-order would otherwise slip past the golden net as a
# "regenerate the hashes" chore.

.meta_base <- function(division = "BD", round = 11L, split = TRUE) {
  base <- list(
    sport        = "football",
    sex          = "male",
    league       = "Besta deild",
    division     = division,
    is_cup       = FALSE,
    season       = 2100L,
    generated_at = "2100-01-15T00:00:00+0000",
    fit_date     = "2100-01-01",
    round        = round,
    n_draws      = 50L
  )
  if (split) {
    base$split <- list(upper = 6L, lower = 6L)
  }
  base
}

.meta_format <- function(n_rounds = 22L, source = "config") {
  list(
    n_rounds = n_rounds, source = source,
    n_rounds_config = n_rounds, n_rounds_schedule = n_rounds, n_teams = 12L
  )
}

.META_V2_KEYS <- c(
  "sport", "sex", "league", "division", "is_cup", "season", "generated_at",
  "fit_date", "round", "n_draws", "split",
  "n_rounds", "n_rounds_source", "units", "points",
  "season_scope", "postseason", "qualify", "relegation"
)

test_that("meta v2 appends to the v1 keys in a pinned order", {
  meta <- .build_publish_meta(
    base = .meta_base(),
    profile = sport_publish_profile("football"),
    format = .meta_format(),
    division_cfg = list(
      qualify = list(slots = 6L, label_is = "Efri hluti"),
      relegation_slots = NA_integer_,
      expected_meetings = NA_integer_
    )
  )
  expect_equal(names(meta), .META_V2_KEYS)

  # The v1 block is copied VERBATIM -- never re-ordered, never renamed.
  base <- .meta_base()
  for (k in names(base)) {
    expect_equal(meta[[k]], base[[k]], info = k)
  }

  expect_equal(meta$n_rounds, 22L)
  expect_equal(meta$n_rounds_source, "config")
  expect_equal(meta$units$diff_bin_width, 1L)
  expect_equal(meta$points, list(win = 3L, draw = 1L, loss = 0L))
  expect_equal(meta$season_scope, "full_season")
  expect_null(meta$postseason)
  expect_true("postseason" %in% names(meta))
  expect_equal(meta$qualify, list(slots = 6L, label_is = "Efri hluti"))
  expect_true(is.na(meta$relegation$slots))
})

test_that("a cell with no split carries no split key at all", {
  meta <- .build_publish_meta(
    base = .meta_base(division = "LD1", split = FALSE),
    profile = sport_publish_profile("football"),
    format = .meta_format(),
    division_cfg = list(
      qualify = NULL, relegation_slots = NA_integer_,
      expected_meetings = NA_integer_
    )
  )
  expect_equal(names(meta), setdiff(.META_V2_KEYS, "split"))
  # Absent qualification is a present key with a null value, never a missing
  # key: the consumer branches on null, it never probes for the field.
  expect_true("qualify" %in% names(meta))
  expect_null(meta$qualify)
})

test_that("a cup publishes a null n_rounds rather than a wrong one", {
  base <- .meta_base(division = "CUP", round = 1L, split = FALSE)
  base$is_cup <- TRUE
  meta <- .build_publish_meta(
    base = base,
    profile = sport_publish_profile("football"),
    format = .meta_format(NA_integer_, "not_applicable"),
    division_cfg = list(
      qualify = NULL, relegation_slots = NA_integer_,
      expected_meetings = NA_integer_
    )
  )
  expect_true(is.na(meta$n_rounds))
  expect_equal(meta$n_rounds_source, "not_applicable")
  # na = "null" is write_json_consistent's default (R/storage.R), so the NA
  # integer reaches the consumer as JSON null.
  expect_equal(
    as.character(jsonlite::toJSON(
      meta["n_rounds"], auto_unbox = TRUE, na = "null", null = "null"
    )),
    "{\"n_rounds\":null}"
  )
})

test_that("the D3 relabel is carried by the payload, not by the template", {
  for (sport in c("basketball", "handball")) {
    base <- .meta_base(division = "BD", round = 3L, split = FALSE)
    base$sport <- sport
    meta <- .build_publish_meta(
      base = base,
      profile = sport_publish_profile(sport),
      format = .meta_format(9L, "config"),
      division_cfg = list(
        qualify = NULL, relegation_slots = NA_integer_,
        expected_meetings = 3L
      )
    )
    expect_equal(meta$season_scope, "regular_season", info = sport)
    expect_equal(meta$postseason$name_is, "\u00darslitakeppni", info = sport)
    expect_false(meta$postseason$modelled, info = sport)
    expect_null(meta$qualify, info = sport)
  }

  football <- .build_publish_meta(
    base = .meta_base(), profile = sport_publish_profile("football"),
    format = .meta_format(),
    division_cfg = list(
      qualify = NULL, relegation_slots = NA_integer_,
      expected_meetings = NA_integer_
    )
  )
  expect_equal(football$season_scope, "full_season")
  expect_null(football$postseason)
})

test_that("basketball's draw slot serialises as null, never as zero", {
  base <- .meta_base(split = FALSE)
  base$sport <- "basketball"
  meta <- .build_publish_meta(
    base = base, profile = sport_publish_profile("basketball"),
    format = .meta_format(),
    division_cfg = list(
      qualify = NULL, relegation_slots = NA_integer_, expected_meetings = 2L
    )
  )
  expect_null(meta$points$draw)
  expect_equal(
    as.character(jsonlite::toJSON(
      meta["points"], auto_unbox = TRUE, na = "null", null = "null"
    )),
    "{\"points\":{\"win\":2,\"draw\":null,\"loss\":0}}"
  )
})

test_that("the producer refuses to emit a round past the end of the season", {
  # The whole point of the workstream: "Umferdir eftir" can never render a
  # negative number, because the producer will not write the payload that
  # would make it possible.
  expect_error(
    .build_publish_meta(
      base = .meta_base(round = 35L),
      profile = sport_publish_profile("basketball"),
      format = .meta_format(22L, "config"),
      division_cfg = list(
        qualify = NULL, relegation_slots = NA_integer_, expected_meetings = 2L
      )
    ),
    "BD"
  )
})

test_that("n_rounds >= round holds for every shape the builder emits", {
  shapes <- list(
    list(round = 11L, fmt = .meta_format(22L, "config")),
    list(round = 22L, fmt = .meta_format(22L, "config")),
    list(round = 0L, fmt = .meta_format(NA_integer_, "none")),
    list(round = 1L, fmt = .meta_format(NA_integer_, "not_applicable"))
  )
  for (s in shapes) {
    meta <- .build_publish_meta(
      base = .meta_base(round = s$round, split = FALSE),
      profile = sport_publish_profile("football"),
      format = s$fmt,
      division_cfg = list(
        qualify = NULL, relegation_slots = NA_integer_,
        expected_meetings = NA_integer_
      )
    )
    expect_true(
      isTRUE(is.na(meta$n_rounds)) || meta$n_rounds >= meta$round,
      info = paste(s$round, s$fmt$source)
    )
  }
})

# ---- Block E: the placement summary ------------------------------------------
#
# One builder for all three sports. Football keeps p_top_six as a DEPRECATED
# ALIAS of p_qualify; basketball and handball emit neither p_top_six nor
# p_winner, because their champion comes out of an urslitakeppni this model
# does not simulate (design section 15, D3).

.synthetic_final_positions <- function(n_teams = 6L) {
  teams <- sprintf("T%02d", seq_len(n_teams))
  # Row-stochastic per team: team i puts 0.5 on placement i and spreads the
  # rest uniformly, so every headline probability below has a hand-checkable
  # closed form.
  tidyr::expand_grid(team = teams, placement = seq_len(n_teams)) |>
    dplyr::mutate(
      probability = dplyr::if_else(
        .data$placement == match(.data$team, teams),
        0.5, 0.5 / (n_teams - 1L)
      )
    )
}

.p_at <- function(fp, team, keep) {
  rows <- fp[fp$team == team & keep(fp$placement), , drop = FALSE]
  sum(rows$probability)
}

test_that("football keeps p_top_six as an alias of the configured qualify cut", {
  fp <- .synthetic_final_positions(6L)
  out <- .build_placement_summary(
    fp, n_teams = 6L, basis = "final_table",
    qualify = list(slots = 6L, label_is = "Efri hluti"),
    relegation_slots = NA_integer_, emit_top_six_alias = TRUE
  )
  expect_equal(
    names(out),
    c("team", "p_qualify", "p_top_of_table", "p_winner", "p_top_six",
      "p_relegation")
  )
  expect_equal(out$p_top_six, out$p_qualify, tolerance = 1e-12)
  expect_equal(out$p_top_of_table, out$p_winner, tolerance = 1e-12)

  # p_relegation with an UNSET relegation_slots must reproduce football's
  # published expression byte for byte: placement >= n_teams - 1.
  expect_equal(
    out$p_relegation,
    vapply(
      out$team, function(t) .p_at(fp, t, function(p) p >= 6L - 1L), numeric(1),
      USE.NAMES = FALSE
    ),
    tolerance = 1e-12
  )
  expect_equal(
    out$p_top_of_table,
    vapply(
      out$team, function(t) .p_at(fp, t, function(p) p == 1L), numeric(1),
      USE.NAMES = FALSE
    ),
    tolerance = 1e-12
  )
})

test_that("a football cell with no qualify cut still publishes p_top_six", {
  # LD1/LD2/LD3 carry no `qualify` -- only Besta deild does. p_top_six is not
  # derived from qualify; it is the literal `placement <= 6L` football has
  # always published, so dropping it here would be a silent field removal on
  # six live cells.
  out <- .build_placement_summary(
    .synthetic_final_positions(6L), n_teams = 6L, basis = "final_table",
    qualify = NULL, relegation_slots = NA_integer_, emit_top_six_alias = TRUE
  )
  expect_equal(
    names(out),
    c("team", "p_top_of_table", "p_winner", "p_top_six", "p_relegation")
  )
  expect_false("p_qualify" %in% names(out))
})

test_that("a regular-season table publishes neither p_winner nor p_top_six", {
  fp <- .synthetic_final_positions(10L)
  out <- .build_placement_summary(
    fp, n_teams = 10L, basis = "regular_season_table",
    qualify = NULL, relegation_slots = NA_integer_, emit_top_six_alias = FALSE
  )
  expect_equal(names(out), c("team", "p_top_of_table", "p_relegation"))
  expect_false(any(c("p_winner", "p_top_six", "p_qualify") %in% names(out)))
  expect_equal(
    out$p_top_of_table,
    vapply(
      out$team, function(t) .p_at(fp, t, function(p) p == 1L), numeric(1),
      USE.NAMES = FALSE
    ),
    tolerance = 1e-12
  )
})

test_that("a configured qualify cut drives p_qualify on any basis", {
  fp <- .synthetic_final_positions(12L)
  out <- .build_placement_summary(
    fp, n_teams = 12L, basis = "regular_season_table",
    qualify = list(slots = 8L, label_is = "\u00darslitakeppni"),
    relegation_slots = NA_integer_, emit_top_six_alias = FALSE
  )
  expect_equal(names(out), c("team", "p_qualify", "p_top_of_table", "p_relegation"))
  expect_equal(
    out$p_qualify,
    vapply(
      out$team, function(t) .p_at(fp, t, function(p) p <= 8L), numeric(1),
      USE.NAMES = FALSE
    ),
    tolerance = 1e-12
  )
})

test_that("relegation_slots 0 publishes present-and-zero, never a missing key", {
  out <- .build_placement_summary(
    .synthetic_final_positions(8L), n_teams = 8L,
    basis = "regular_season_table", qualify = NULL,
    relegation_slots = 0L, emit_top_six_alias = FALSE
  )
  expect_true("p_relegation" %in% names(out))
  expect_equal(out$p_relegation, rep(0, 8L))
})

test_that("a configured relegation_slots replaces the hardcoded bottom two", {
  fp <- .synthetic_final_positions(12L)
  out <- .build_placement_summary(
    fp, n_teams = 12L, basis = "regular_season_table", qualify = NULL,
    relegation_slots = 3L, emit_top_six_alias = FALSE
  )
  expect_equal(
    out$p_relegation,
    vapply(
      out$team, function(t) .p_at(fp, t, function(p) p > 12L - 3L), numeric(1),
      USE.NAMES = FALSE
    ),
    tolerance = 1e-12
  )
})

test_that("an empty final_positions yields the shape, not an error", {
  empty <- sport_publish_profile("football")$empty_extracts$final_positions
  out <- .build_placement_summary(
    empty, n_teams = 0L, basis = "final_table",
    qualify = list(slots = 6L, label_is = "Efri hluti"),
    relegation_slots = NA_integer_, emit_top_six_alias = TRUE
  )
  expect_equal(nrow(out), 0L)
  expect_equal(
    names(out),
    c("team", "p_qualify", "p_top_of_table", "p_winner", "p_top_six",
      "p_relegation")
  )
})

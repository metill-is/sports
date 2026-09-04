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

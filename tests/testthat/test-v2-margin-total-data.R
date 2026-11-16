v2_rows <- function(dates, season, division, home, away, hs = NA, as = NA) {
  data.frame(
    match_date = as.Date(dates), season = season, division = division,
    home_team = home, away_team = away, home_score = hs, away_score = as
  )
}

test_that("weeks count from each season's first match and seasons are consecutive", {
  train <- v2_rows(
    c("2024-09-01", "2024-09-07", "2024-09-08", "2025-09-10"), c(2025, 2025, 2025, 2026),
    "OD", c("A", "B", "A", "B"), c("B", "C", "C", "A"), c(30, 28, 25, 31), c(27, 28, 29, 30)
  )
  v <- build_v2_data(train, train[0, ], teams = c("A", "B", "C"))

  expect_equal(v$wk, c(1L, 1L, 2L, 1L))
  expect_equal(v$seasons, 2025:2026)
  expect_equal(v$season, c(1L, 1L, 1L, 2L))
  expect_equal(v$W, 2L)
  expect_equal(c(v$N, v$N_pred, v$K, v$NS), c(4L, 0L, 3L, 2L))
  expect_equal(v$y_home - v$y_away, c(3, 0, -4, 1))
})

test_that("team-season ids cover exactly the pairs that occur and map back", {
  train <- v2_rows(
    c("2024-09-01", "2025-09-10"), c(2025, 2026), "OD", c("A", "B"), c("B", "C"), 1, 0
  )
  v <- build_v2_data(train, train[0, ], teams = c("A", "B", "C"))

  # A 2025, B 2025, B 2026, C 2026: four team-seasons, C 2025 and A 2026 absent.
  expect_equal(v$TS, 4L)
  expect_equal(v$ts_team[v$ts_home], v$home)
  expect_equal(v$ts_season[v$ts_home], v$season)
  expect_equal(v$ts_team[v$ts_away], v$away)
  expect_false(any(v$ts_team == 3L & v$ts_season == 1L))
})

test_that("a fixture in a season with no played match gets its own season index and state", {
  train <- v2_rows("2024-09-01", 2025, "OD", "A", "B", 30, 27)
  pred <- v2_rows("2025-09-20", 2026, "OD", "B", "A")
  v <- build_v2_data(train, pred, teams = c("A", "B"))

  expect_equal(v$NS, 2L)
  expect_equal(v$season_pred, 2L)
  expect_equal(v$wk_pred, 1L)
  expect_equal(v$TS, 4L)
  expect_false(v$ts_home_pred %in% c(v$ts_home, v$ts_away))
})

test_that("a supplied season calendar fixes the week index across cutoffs", {
  train <- v2_rows("2024-09-15", 2025, "OD", "A", "B", 30, 27)
  start <- c(`2025` = as.Date("2024-09-01"))

  v <- build_v2_data(train, train[0, ], teams = c("A", "B"), season_start = start, n_weeks = 30L)

  expect_equal(v$wk, 3L)
  expect_equal(v$W, 30L)
  expect_error(
    build_v2_data(train, train[0, ], teams = c("A", "B"), season_start = start, n_weeks = 2L),
    "exceeds"
  )
  expect_error(
    build_v2_data(train, train[0, ], teams = c("A", "B"), season_start = c(`2025` = as.Date("2024-10-01"))),
    "before its season"
  )
})

test_that("division_map folds a play-off code into its league", {
  train <- v2_rows(
    c("2025-01-10", "2025-04-01", "2025-01-11"), 2025, c("OD", "PO", "G66"),
    c("A", "A", "C"), c("B", "B", "D"), 1, 0
  )
  teams <- c("A", "B", "C", "D")

  plain <- build_v2_data(train, train[0, ], teams)
  mapped <- build_v2_data(train, train[0, ], teams, division_map = c(PO = "OD"))

  expect_equal(plain$divisions, c("G66", "OD", "PO"))
  expect_equal(mapped$divisions, c("G66", "OD"))
  expect_equal(mapped$div, c(2L, 2L, 1L))
})

test_that("a team outside the registry aborts instead of indexing as NA", {
  train <- v2_rows("2024-09-01", 2025, "OD", "A", "Z", 1, 0)
  expect_error(build_v2_data(train, train[0, ], teams = c("A", "B")), "Teams missing")
})

test_that("a season nobody played keeps its place between the seasons around it", {
  # WHY: the season level is an AR(1) over the season index, so two seasons that are two
  # years apart must be two steps apart, not one.
  train <- v2_rows(
    c("2020-09-01", "2022-09-10", "2022-09-18"), c(2021, 2023, 2023), "OD",
    c("A", "B", "A"), c("B", "A", "B"), c(30, 28, 31), c(27, 29, 30)
  )
  v <- build_v2_data(train, train[0, ], teams = c("A", "B"))

  expect_equal(v$seasons, 2021:2023)
  expect_equal(v$NS, 3L)
  expect_equal(v$season, c(1L, 3L, 3L))
  expect_equal(v$ts_season[v$ts_home], c(1L, 3L, 3L))
  # The season after the gap counts its weeks from its own first match.
  expect_equal(v$wk, c(1L, 1L, 2L))
  expect_true(is.na(v$season_start[["2022"]]))
})

suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
  library(readr)
})

source(here::here("R", "backtest", "betting_pnl", "tests", "helper.R"))
source(here::here("R", "backtest", "betting_pnl", "odds_load.R"))

test_that("load_odds_1x2 deduplicates to closest-to-kickoff scrape and pivots to long", {
  fixture_path <- tempfile(fileext = ".csv")
  readr::write_csv(fixture_odds_1x2_raw, fixture_path)

  result <- load_odds_1x2(fixture_path)

  # 1 match x 3 outcomes = 3 rows.
  expect_equal(nrow(result), 3)
  expect_setequal(result$outcome, c("H", "D", "A"))

  # Closest-to-kickoff scrape is the second row (2026-04-12 14:43).
  h_row <- result |> filter(outcome == "H")
  expect_equal(h_row$odds, 2.17)
})

test_that("load_odds_1x2 produces canonical match_id from (date, home, away)", {
  fixture_path <- tempfile(fileext = ".csv")
  readr::write_csv(fixture_odds_1x2_raw, fixture_path)

  result <- load_odds_1x2(fixture_path)
  expect_true(all(result$match_id == "2026-04-12|Fram|ÍA"))
})

test_that("load_odds_handicap parses H-A change into signed home line", {
  # change = "0-1" -> home starts 0, away starts 1 -> home handicap = 0 - 1 = -1.
  # change = "1-0" -> home starts 1, away starts 0 -> home handicap = +1.
  # change = "2-0" -> home handicap = +2.
  fixture_path <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tribble(
    ~date,         ~league,  ~home,  ~away, ~change, ~o_home, ~o_draw, ~o_away, ~scraped_at,
    "2026-04-12",  "BD",     "Fram", "ÍA",  "0-1",   3.42,    4.25,    1.61,    "2026-04-10T20:41:26Z",
    "2026-04-12",  "BD",     "Fram", "ÍA",  "1-0",   1.36,    4.75,    4.88,    "2026-04-10T20:41:26Z",
    "2026-04-12",  "BD",     "Fram", "ÍA",  "2-0",   1.12,    6.68,    9.66,    "2026-04-10T20:41:26Z"
  ), fixture_path)

  result <- load_odds_handicap(fixture_path)

  # 3 changes x 2 outcomes (home_cover, away_cover) = 6 rows.
  expect_equal(nrow(result), 6)
  expect_setequal(unique(result$line), c(-1, 1, 2))

  # home_cover at line = -1 should have odds 3.42 (the favored underdog line).
  hc_neg1 <- result |> filter(outcome == "home_cover", line == -1)
  expect_equal(hc_neg1$odds, 3.42)

  # away_cover at line = 2 has odds 9.66.
  ac_2 <- result |> filter(outcome == "away_cover", line == 2)
  expect_equal(ac_2$odds, 9.66)
})

test_that("join_results attaches home_goals / away_goals / won per bet row", {
  # 3 bet rows on Fram-ÍA with actual result 2-1 (home win, total 3, goal_diff +1).
  odds <- tibble(
    match_id = "m1_FramIA",
    date = "2026-04-12",
    home = "Fram",
    away = "ÍA",
    market = c("1x2", "totals", "handicap"),
    outcome = c("H", "over", "home_cover"),
    line = c(NA, 2.5, -0.5),
    odds = c(2.17, 1.85, 1.95)
  )
  result <- join_results(odds, fixture_results)

  expect_equal(result$home_goals, c(2, 2, 2))
  expect_equal(result$away_goals, c(1, 1, 1))
  # 1x2 H: home_win -> TRUE
  # totals over 2.5: 2+1=3 > 2.5 -> TRUE
  # handicap home_cover line=-0.5: 1 + (-0.5) = 0.5 > 0 -> TRUE
  expect_equal(result$won, c(TRUE, TRUE, TRUE))
})

test_that("join_results sets won = NA for unplayed matches", {
  odds <- tibble(
    match_id = "2026-04-23|FutureH|FutureA",
    date = "2026-04-23",
    home = "FutureH",
    away = "FutureA",
    market = "1x2",
    outcome = "H",
    line = NA_real_,
    odds = 2.0
  )
  result <- join_results(odds, fixture_results)
  expect_true(is.na(result$won))
  expect_true(is.na(result$home_goals))
})

test_that("join_results sets won = NA for handicap pushes (integer-line ties)", {
  # goal_diff=+1 with line=-1 -> goal_diff + line = 0 -> push.
  odds <- tibble(
    match_id = "m1_FramIA",
    date = "2026-04-12",
    home = "Fram",
    away = "ÍA",
    market = "handicap",
    outcome = c("home_cover", "away_cover"),
    line = c(-1, -1),
    odds = c(2.0, 2.0)
  )
  result <- join_results(odds, fixture_results)
  expect_true(all(is.na(result$won)))
})

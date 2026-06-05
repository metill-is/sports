# Tests for derive_league_round() -- populates the schema `round` column with the
# league matchweek (umferð). A round = each team's cumulative appearance index
# within its (sport,country,sex,season,division) cell, NOT a dense rank of dates:
# a single round is often played across several calendar dates (e.g. Besta deild
# round 1 in 2026 ran 04-10 + 04-12), so all those fixtures must share a round.
# Cup rounds stay NA (knockout brackets, not weekly matchweeks).

# 6 fixtures, 4 teams, three full rounds, each round split across two dates.
round_robin_fixture <- function() {
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    division = "BD",
    match_date = as.Date(c(
      "2026-04-10", "2026-04-12", # round 1, two dates
      "2026-04-17", "2026-04-19", # round 2, two dates
      "2026-04-24", "2026-04-26" # round 3, two dates
    )),
    home_team = c("A", "C", "A", "B", "A", "B"),
    away_team = c("B", "D", "C", "D", "D", "C"),
    round = NA_integer_
  )
}

test_that("a round played across several dates shares one round number", {
  out <- derive_league_round(round_robin_fixture())
  # 04-10 (A-B) and 04-12 (C-D) are both round 1 despite different dates
  expect_equal(out$round[out$match_date == as.Date("2026-04-10")], 1L)
  expect_equal(out$round[out$match_date == as.Date("2026-04-12")], 1L)
})

test_that("round equals the teams' cumulative appearance (matchweek)", {
  out <- derive_league_round(round_robin_fixture())
  expect_equal(out$round, c(1L, 1L, 2L, 2L, 3L, 3L))
  expect_equal(min(out$round), 1L)
})

test_that("round is monotonic non-decreasing with match_date within a cell", {
  out <- derive_league_round(round_robin_fixture())
  out <- out[order(out$match_date), ]
  expect_false(is.unsorted(out$round))
})

test_that("max league round equals games-played, not distinct dates", {
  # 6 fixtures over 6 distinct dates but only 3 rounds -- date-rank would give 6
  out <- derive_league_round(round_robin_fixture())
  expect_equal(max(out$round), 3L)
})

test_that("a postponed fixture takes the max appearance of its two sides", {
  # D's round-2 match is postponed; D plays its round-3 fixture (A-D) before it.
  df <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    division = "BD",
    match_date = as.Date(c(
      "2026-04-10", "2026-04-10", # round 1: A-B, C-D
      "2026-04-17", # round 2: A-C  (B-D postponed)
      "2026-04-24", # round 3: A-D  (A app3, D app2)
      "2026-04-26" # the postponed B-D played late (B app2, D app3)
    )),
    home_team = c("A", "C", "A", "A", "B"),
    away_team = c("B", "D", "C", "D", "D"),
    round = NA_integer_
  )
  out <- derive_league_round(df)
  # A-D: A has played 3, D has played 2 -> max = 3
  ad <- out[out$home_team == "A" & out$away_team == "D", ]
  expect_equal(ad$round, 3L)
})

test_that("cup rows keep round NA (bracket rounds, not weekly matchweeks)", {
  df <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    division = c("BD", "CUP", "CUP"),
    match_date = as.Date(c("2026-04-10", "2026-04-12", "2026-05-01")),
    home_team = c("A", "A", "B"), away_team = c("B", "X", "Y"),
    round = NA_integer_
  )
  out <- derive_league_round(df)
  expect_true(all(is.na(out$round[out$division == "CUP"])))
  expect_equal(out$round[out$division == "BD"], 1L)
})

test_that("round is integer typed (matches int32 schema)", {
  expect_type(derive_league_round(round_robin_fixture())$round, "integer")
})

test_that("appearance counting is independent across divisions, sex, season", {
  df <- tibble::tibble(
    sport = "football", country = "iceland",
    sex = c("male", "male", "female", "female"),
    season = c(2026L, 2026L, 2026L, 2025L),
    division = c("BD", "LD1", "BD", "BD"),
    match_date = as.Date(c("2026-05-01", "2026-05-01", "2026-05-02", "2025-06-01")),
    home_team = c("A", "E", "F", "G"),
    away_team = c("B", "H", "I", "J"),
    round = NA_integer_
  )
  out <- derive_league_round(df)
  expect_equal(out$round, c(1L, 1L, 1L, 1L))
})

test_that("existing non-round columns are preserved unchanged and in order", {
  fx <- round_robin_fixture()
  out <- derive_league_round(fx)
  expect_equal(out$home_team, fx$home_team)
  expect_equal(out$match_date, fx$match_date)
  expect_equal(nrow(out), nrow(fx))
})

# tests/testthat/test-backtest-divisions.R

test_that("bt_attach_division joins division + round from results on the match key", {
  bets <- tibble::tibble(
    sex = "male",
    match_date = as.Date(c("2026-05-20", "2026-05-21")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    p = c(0.6, 0.4)
  )
  results <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    division = "BD", round = 7L, home_score = 1L, away_score = 0L
  )
  out <- bt_attach_division(bets, results)
  expect_equal(out$division, c("BD", "unknown"))
  expect_equal(out$round[1], 7L)
  expect_true(is.na(out$round[2]))
  expect_equal(nrow(out), 2L)
})

test_that("bt_attach_division de-duplicates results to one division per match", {
  bets <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B", p = 0.5
  )
  results <- tibble::tibble(
    sex = "male", match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    division = c("BD", "BD"), round = c(7L, 7L),
    home_score = c(1L, 1L), away_score = c(0L, 0L)
  )
  expect_equal(nrow(bt_attach_division(bets, results)), 1L)
})

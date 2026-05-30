# tests/testthat/test-backtest-engine.R
bt_eng_universe <- function() {
  tibble::tibble(
    run_id = "2026-05-01", run_date = as.Date("2026-05-01"),
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-05-02"),
    home_team = c("A", "C"), away_team = c("B", "D"),
    market = "moneyline", outcome = c("home", "away"),
    line = NA_real_, p = 0.5, odds = c(2.0, 3.0),
    ev = 0.1, kelly_raw = 0.1, kelly = 0.10,
    bet_amount_recorded = NA_real_, stage = "kept", strategy = "kept"
  )
}
bt_eng_results <- function() {
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-05-02"),
    home_team = c("A", "C"), away_team = c("B", "D"),
    home_score = c(2L, 0L), away_score = c(1L, 1L) # A home win; D away win
  )
}

test_that("bt_run settles via compute_settlement and applies fixed stakes", {
  out <- bt_run(bt_eng_universe(), bt_eng_results(),
    stake_rule = stake_fixed, initial_pool = 10000
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(out$win)) # both bets win
  # stake = 0.10 * 10000 = 1000 each; pnl = 1000*(odds-1)
  expect_equal(sort(out$pnl), sort(c(1000 * 1.0, 1000 * 2.0)))
  expect_equal(max(out$pool_after), 10000 + 3000) # cumulative
})

test_that("bt_run excludes bets with no matching result (pending)", {
  u <- bt_eng_universe()
  res <- bt_eng_results()[1, ] # only A vs B has a result
  out <- bt_run(u, res, stake_rule = stake_fixed, initial_pool = 10000)
  expect_equal(nrow(out), 1L)
  expect_equal(attr(out, "pending"), 1L)
})

test_that("bt_run returns empty-with-columns for an empty universe", {
  out <- bt_run(bt_load_universe(root = withr::local_tempdir()),
    bt_eng_results(),
    initial_pool = 10000
  )
  expect_equal(nrow(out), 0L)
  expect_true(all(c("stake", "pnl", "pool_after", "cum_pnl") %in% names(out)))
})

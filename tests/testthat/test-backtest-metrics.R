# tests/testthat/test-backtest-metrics.R
settled_fixture <- function() {
  tibble::tibble(
    sport = "football", market = c("moneyline", "moneyline", "total"),
    sex = "male", p = c(0.6, 0.4, 0.5),
    match_date = as.Date(c("2026-05-02", "2026-05-03", "2026-05-04")),
    odds = c(2.0, 3.0, 1.9),
    win = c(TRUE, FALSE, TRUE),
    stake = c(1000, 1000, 1000),
    pnl = c(1000, -1000, 900)
  )
}

test_that("bt_metrics computes ROI, hit-rate, yield, drawdown", {
  m <- bt_metrics(settled_fixture())
  expect_equal(m$n_bets, 3L)
  expect_equal(m$total_staked, 3000)
  expect_equal(m$total_pnl, 900)
  expect_equal(m$roi, 900 / 3000)
  expect_equal(m$yield, 900 / 3)
  expect_equal(m$hit_rate, 2 / 3)
  expect_equal(m$max_drawdown, -1000) # after +1000 then -1000 trough
})

test_that("bt_metrics groups by a dimension", {
  m <- bt_metrics(settled_fixture(), by = "market")
  expect_setequal(m$market, c("moneyline", "total"))
  ml <- m[m$market == "moneyline", ]
  expect_equal(ml$n_bets, 2L)
  expect_equal(ml$total_pnl, 0)
})

test_that("bt_calibration bins predicted p against realised frequency", {
  cal <- bt_calibration(settled_fixture(), n_bins = 2)
  expect_true(all(c("mean_p", "realised_freq", "n") %in% names(cal)))
  expect_equal(sum(cal$n), 3L)
})

test_that("bt_metrics returns empty tibble on empty input", {
  expect_equal(nrow(bt_metrics(settled_fixture()[0, ])), 0L)
})

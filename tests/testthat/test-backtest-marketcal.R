# tests/testthat/test-backtest-marketcal.R

mc_fixture <- function() {
  tibble::tibble(
    sex = "male", market = "moneyline",
    match_date = as.Date("2026-05-01") + rep(0:3, each = 3),
    home_team = rep(c("A", "C", "E", "G"), each = 3),
    away_team = rep(c("B", "D", "F", "H"), each = 3),
    outcome = rep(c("home", "draw", "away"), 4),
    p = rep(c(0.5, 0.3, 0.2), 4),
    q_market = rep(c(0.55, 0.25, 0.2), 4),
    y = c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1),
    overround = 1.05
  )
}

test_that("bt_market_calibration reports the reliability of q_market (not the model p)", {
  cal <- bt_market_calibration(mc_fixture(), n_bins = 5)
  expect_true(all(c("mean_p", "realised_freq", "n", "lo", "hi", "band_lo", "band_hi") %in% names(cal)))
  expect_true(all(
    abs(cal$mean_p - 0.55) < 1e-6 | abs(cal$mean_p - 0.25) < 1e-6 | abs(cal$mean_p - 0.2) < 1e-6
  ))
  expect_equal(sum(cal$n), 12L)
})

test_that("bt_market_bias aggregates realised-minus-q_market bias per outcome", {
  scored <- tibble::tibble(
    market = "moneyline", outcome = rep(c("home", "draw", "away"), 4),
    q_market = rep(c(0.5, 0.25, 0.25), 4),
    y = c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0)
  )
  bias <- bt_market_bias(scored, by = "market")
  draw <- bias[bias$outcome == "draw", ]
  expect_equal(draw$mean_q, 0.25)
  expect_equal(draw$realised, 0.5)
  expect_equal(draw$bias, 0.25)
  expect_equal(draw$n, 4L)
})

test_that("bt_disagreement bands gap = p - q_market and reports who is right", {
  scored <- tibble::tibble(
    market = "moneyline",
    p = c(0.80, 0.50, 0.20, 0.51),
    q_market = c(0.50, 0.50, 0.50, 0.50),
    y = c(1, 0, 0, 1)
  )
  d <- bt_disagreement(scored)
  expect_true(all(c("band", "n", "mean_p", "mean_q", "realised") %in% names(d)))
  expect_true("model>>mkt" %in% as.character(d$band))
  expect_true("model<<mkt" %in% as.character(d$band))
  agree <- d[as.character(d$band) == "agree", ]
  expect_equal(agree$n, 2L)
})

test_that("bt_disagreement accepts a `by` stratifier", {
  scored <- tibble::tibble(
    market = c("moneyline", "total"),
    p = c(0.8, 0.2), q_market = c(0.5, 0.5), y = c(1, 0)
  )
  d <- bt_disagreement(scored, by = "market")
  expect_true(all(c("market", "band") %in% names(d)))
})

test_that("bt_line_stability measures how often a price moves across snapshots", {
  odds <- tibble::tibble(
    match_date = as.Date("2026-05-20"),
    home_team = "A", away_team = "B",
    market = "moneyline", outcome = c("home", "home", "away", "away"),
    line = NA_real_,
    odds = c(2.10, 2.30, 1.70, 1.70),
    scraped_at = as.POSIXct(
      c(
        "2026-05-18 09:00", "2026-05-19 09:00",
        "2026-05-18 09:00", "2026-05-19 09:00"
      ),
      tz = "UTC"
    )
  )
  s <- bt_line_stability(odds)
  expect_equal(s$n_series, 2L)
  expect_equal(s$pct_moved, 0.5)
  expect_equal(s$mean_distinct_prices, 1.5)
})

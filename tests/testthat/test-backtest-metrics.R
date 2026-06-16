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

# ---- P2: stratification primitives -------------------------------------------

ci_devig_fixture <- function() {
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date(c(
      "2026-05-01", "2026-05-01", "2026-05-02", "2026-05-02",
      "2026-05-03", "2026-05-03", "2026-05-04", "2026-05-04"
    )),
    home_team = c("A", "A", "C", "C", "E", "E", "G", "G"),
    away_team = c("B", "B", "D", "D", "F", "F", "H", "H"),
    market = c("total", "total", "total", "total", "moneyline", "moneyline", "moneyline", "moneyline"),
    line = c(2.5, 2.5, 2.5, 2.5, NA, NA, NA, NA),
    outcome = c("over", "under", "over", "under", "home", "away", "home", "away"),
    p = c(0.6, 0.4, 0.55, 0.45, 0.7, 0.3, 0.65, 0.35),
    q_market = c(0.58, 0.42, 0.52, 0.48, 0.68, 0.32, 0.6, 0.4),
    y = c(1, 0, 0, 1, 1, 0, 0, 1),
    overround = 1.05
  )
}

test_that("bt_skill_ci stays backward-compatible (numeric vector) with no `by`", {
  ci <- bt_skill_ci(ci_devig_fixture(), R = 200, seed = 1L)
  expect_type(ci, "double")
  expect_length(ci, 3L)
  expect_true(ci[1] <= ci[2] && ci[2] <= ci[3])
})

test_that("bt_skill_ci returns a per-stratum tibble with `by`", {
  out <- bt_skill_ci(ci_devig_fixture(), by = "market", R = 200, seed = 1L)
  expect_setequal(out$market, c("total", "moneyline"))
  expect_true(all(c("skill_lo", "skill_mid", "skill_hi") %in% names(out)))
  expect_true(all(out$skill_lo <= out$skill_mid & out$skill_mid <= out$skill_hi))
})

test_that("bt_brier_decomp splits Brier into reliability/resolution/uncertainty", {
  scored <- tibble::tibble(p = c(0.2, 0.2, 0.8, 0.8), y = c(0, 1, 1, 1))
  d <- bt_brier_decomp(scored, n_bins = 2)
  expect_equal(d$uncertainty, 0.75 * 0.25)
  expect_equal(d$reliability, 0.065)
  expect_equal(d$resolution, 0.0625)
  expect_equal(d$brier, 0.19)
  expect_equal(d$brier, d$reliability - d$resolution + d$uncertainty, tolerance = 1e-9)
})

test_that("bt_brier_decomp groups by a dimension", {
  scored <- tibble::tibble(
    market = c("a", "a", "b", "b"),
    p = c(0.2, 0.8, 0.3, 0.7), y = c(0, 1, 0, 1)
  )
  d <- bt_brier_decomp(scored, n_bins = 2, by = "market")
  expect_setequal(d$market, c("a", "b"))
  expect_equal(nrow(d), 2L)
})

test_that("bt_calibration_bands adds Jeffreys intervals + a consistency band", {
  settled <- tibble::tibble(p = rep(0.5, 100), win = rep(c(TRUE, FALSE), 50))
  cal <- bt_calibration_bands(settled, n_bins = 1)
  expect_equal(cal$n, 100L)
  expect_equal(cal$mean_p, 0.5)
  expect_equal(cal$realised_freq, 0.5)
  expect_true(cal$lo > 0 && cal$lo < 0.5 && cal$hi > 0.5 && cal$hi < 1)
  expect_true(cal$band_lo <= 0.5 && cal$band_hi >= 0.5)
})

test_that("bt_calibration_bands flags a miscalibrated bin (realised outside the band)", {
  settled <- tibble::tibble(p = rep(0.92, 100), win = rep(c(TRUE, FALSE), 50))
  cal <- bt_calibration_bands(settled, n_bins = 10)
  cal <- cal[cal$n > 0, ]
  expect_true(cal$realised_freq < cal$band_lo)
})

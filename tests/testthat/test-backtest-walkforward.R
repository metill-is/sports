# tests/testthat/test-backtest-walkforward.R

test_that("bt_oos_scores computes Brier and log-loss over (p, win)", {
  settled <- tibble::tibble(
    p   = c(0.9, 0.1, 0.5, 0.8),
    win = c(TRUE, FALSE, TRUE, FALSE)
  )
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 4L)
  expect_equal(
    s$brier,
    mean((c(0.9, 0.1, 0.5, 0.8) - c(1, 0, 1, 0))^2)
  )
  expect_equal(
    s$log_loss,
    -mean(c(log(0.9), log(0.9), log(0.5), log(0.2)))
  )
})

test_that("bt_oos_scores clamps p away from 0/1 so log-loss is finite", {
  settled <- tibble::tibble(p = c(0, 1), win = c(FALSE, TRUE))
  s <- bt_oos_scores(settled)
  expect_true(is.finite(s$log_loss))
  expect_true(is.finite(s$brier))
})

test_that("bt_oos_scores returns NA-row for empty input", {
  s <- bt_oos_scores(tibble::tibble(p = numeric(), win = logical()))
  expect_equal(s$n, 0L)
  expect_true(is.na(s$brier))
  expect_true(is.na(s$log_loss))
})

test_that("bt_oos_scores drops rows with NA win (unsettled) before scoring", {
  settled <- tibble::tibble(p = c(0.7, 0.4), win = c(TRUE, NA))
  s <- bt_oos_scores(settled)
  expect_equal(s$n, 1L)
  expect_equal(s$brier, (0.7 - 1)^2)
})

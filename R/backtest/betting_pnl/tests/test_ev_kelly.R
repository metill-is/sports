suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
})

source(here::here("R", "backtest", "betting_pnl", "ev_kelly.R"))

test_that("compute_ev_kelly computes EV and Kelly fraction", {
  df <- tibble(
    p_model = c(0.55, 0.30, 0.70, 0.50),
    odds    = c(2.50, 2.00, 1.50, 2.00)
  )
  result <- compute_ev_kelly(df,
    bankroll = 10000, kelly_frac = 0.10,
    min_ev = 0.03, min_bet = 200
  )

  # Row 1: p=0.55, o=2.5 -> ev = 0.55*2.5 - 1 = 0.375. kelly = 0.375/1.5 = 0.25.
  #        stake = 10000*0.10*0.25 = 250. Passes both filters.
  expect_equal(result$ev[1], 0.375, tolerance = 1e-10)
  expect_equal(result$kelly_f[1], 0.25, tolerance = 1e-10)
  expect_equal(result$stake[1], 250)
  expect_true(result$pass[1])

  # Row 2: p=0.30, o=2.0 -> ev = -0.4. kelly_f clamped to 0. Doesn't pass.
  expect_equal(result$ev[2], -0.4, tolerance = 1e-10)
  expect_equal(result$kelly_f[2], 0)
  expect_equal(result$stake[2], 0)
  expect_false(result$pass[2])

  # Row 4: p=0.50, o=2.0 -> ev = 0. Fails min_ev = 0.03.
  expect_equal(result$ev[4], 0, tolerance = 1e-10)
  expect_false(result$pass[4])
})

test_that("compute_ev_kelly respects min_bet filter (Lengjan minimum = 200)", {
  # Small edge: p=0.52, o=2.0 -> ev = 0.04, kelly = 0.04/1 = 0.04.
  # At bankroll=10000, kelly_frac=0.10 -> stake = 40. Fails min_bet=200 even though ev >= 0.03.
  df <- tibble(p_model = 0.52, odds = 2.0)
  result <- compute_ev_kelly(df,
    bankroll = 10000, kelly_frac = 0.10,
    min_ev = 0.03, min_bet = 200
  )
  expect_equal(result$stake[1], 40)
  expect_false(result$pass[1])

  # Bigger bankroll lets the same bet pass.
  result2 <- compute_ev_kelly(df,
    bankroll = 100000, kelly_frac = 0.10,
    min_ev = 0.03, min_bet = 200
  )
  expect_equal(result2$stake[1], 400)
  expect_true(result2$pass[1])
})

test_that("compute_ev_kelly clamps kelly_f to [0, 1]", {
  # At p=1.0, o=1.5: kelly = (1*1.5 - 1)/(1.5 - 1) = 1. No clamp needed but defensive.
  df <- tibble(p_model = 1.0, odds = 1.5)
  result <- compute_ev_kelly(df,
    bankroll = 10000, kelly_frac = 0.10,
    min_ev = 0.03, min_bet = 200
  )
  expect_equal(result$kelly_f[1], 1.0, tolerance = 1e-10)
})

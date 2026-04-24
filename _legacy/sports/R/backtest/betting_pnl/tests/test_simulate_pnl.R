suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
})

source(here::here("R", "backtest", "betting_pnl", "simulate_pnl.R"))

test_that("compute_pnl returns per-bet PnL for win/lose/NA", {
  df <- tibble(
    stake = c(250, 250, 250, 250),
    odds  = c(2.5, 2.5, 2.5, 2.5),
    won   = c(TRUE, FALSE, NA, TRUE)
  )
  result <- compute_pnl(df)
  expect_equal(result$pnl[1], 375) # 250 * (2.5 - 1) = 375
  expect_equal(result$pnl[2], -250) # -stake
  expect_true(is.na(result$pnl[3])) # unplayed
  expect_equal(result$pnl[4], 375)
})

test_that("compute_pnl returns 0 for rows with stake = 0 (non-bet)", {
  df <- tibble(
    stake = c(250, 0, 250),
    odds  = c(2.5, 2.5, 2.5),
    won   = c(TRUE, TRUE, FALSE)
  )
  result <- compute_pnl(df)
  expect_equal(result$pnl[1], 375)
  expect_equal(result$pnl[2], 0)
  expect_equal(result$pnl[3], -250)
})

test_that("summarise_pnl aggregates by (variant, market, mode)", {
  bets <- tibble(
    variant = c("v3", "v3", "bvp", "bvp"),
    market  = c("1x2", "totals", "1x2", "1x2"),
    mode    = "in_sample",
    stake   = c(250, 300, 400, 200),
    odds    = c(2.5, 1.8, 2.0, 3.0),
    won     = c(TRUE, FALSE, TRUE, NA)
  )
  bets$pnl <- compute_pnl(bets)$pnl

  summary <- summarise_pnl(bets)

  v3_1x2 <- summary |> filter(variant == "v3", market == "1x2")
  expect_equal(v3_1x2$n_bets, 1L)
  expect_equal(v3_1x2$total_stake, 250)
  expect_equal(v3_1x2$total_pnl, 375)

  bvp_1x2 <- summary |> filter(variant == "bvp", market == "1x2")
  # 400 won (+400) + 200 NA (excluded from aggregate).
  expect_equal(bvp_1x2$n_bets, 1L)
  expect_equal(bvp_1x2$total_stake, 400)
  expect_equal(bvp_1x2$total_pnl, 400)
})

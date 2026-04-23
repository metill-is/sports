# Tests for market probability extraction from posterior (S, D) draws.
suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
})

source(here::here("R", "backtest", "betting_pnl", "tests", "helper.R"))
source(here::here("R", "backtest", "betting_pnl", "market_probs.R"))

test_that("compute_1x2_probs tallies home/draw/away from goal_diff", {
  # From fixture_posterior_draws, bvp, m1_FramIA:
  #   goal_diff = 1, 0, 2, -1  ->  P(home)=2/4, P(draw)=1/4, P(away)=1/4.
  result <- compute_1x2_probs(fixture_posterior_draws, draw_threshold = 0.5)

  bvp_m1 <- result |> filter(variant == "bvp", match_id == "m1_FramIA")
  expect_equal(bvp_m1$p_model[bvp_m1$outcome == "H"], 0.50)
  expect_equal(bvp_m1$p_model[bvp_m1$outcome == "D"], 0.25)
  expect_equal(bvp_m1$p_model[bvp_m1$outcome == "A"], 0.25)
  expect_equal(sum(bvp_m1$p_model), 1.0, tolerance = 1e-10)
})

test_that("compute_1x2_probs uses continuous draw threshold for Student-t", {
  # v3, m1_FramIA: goal_diff = 0.8, -0.2, 1.6, -0.7.
  # At threshold 0.5: |D| <= 0.5 only for -0.2 -> P(draw) = 1/4.
  # D > 0.5: 0.8, 1.6 -> P(home) = 2/4.
  # D < -0.5: -0.7 -> P(away) = 1/4.
  result <- compute_1x2_probs(fixture_posterior_draws, draw_threshold = 0.5)

  v3_m1 <- result |> filter(variant == "v3", match_id == "m1_FramIA")
  expect_equal(v3_m1$p_model[v3_m1$outcome == "H"], 0.50)
  expect_equal(v3_m1$p_model[v3_m1$outcome == "D"], 0.25)
  expect_equal(v3_m1$p_model[v3_m1$outcome == "A"], 0.25)
})

test_that("compute_totals_probs tallies P(S > line) per line", {
  # bvp, m1_FramIA: total_goals = 3, 2, 4, 1.
  # Line 1.5: P(S > 1.5) = 3/4.
  # Line 2.5: P(S > 2.5) = 2/4 (3, 4).
  # Line 3.5: P(S > 3.5) = 1/4 (4).
  result <- compute_totals_probs(fixture_posterior_draws, lines = c(1.5, 2.5, 3.5))

  bvp_m1 <- result |> filter(variant == "bvp", match_id == "m1_FramIA")
  expect_equal(bvp_m1$p_model[bvp_m1$line == 1.5 & bvp_m1$outcome == "over"], 0.75)
  expect_equal(bvp_m1$p_model[bvp_m1$line == 2.5 & bvp_m1$outcome == "over"], 0.50)
  expect_equal(bvp_m1$p_model[bvp_m1$line == 3.5 & bvp_m1$outcome == "over"], 0.25)
  expect_equal(bvp_m1$p_model[bvp_m1$line == 2.5 & bvp_m1$outcome == "under"], 0.50)
})

test_that("compute_handicap_probs tallies P(goal_diff + line > 0)", {
  # BVP m1_FramIA: D = 1, 0, 2, -1. Handicap line +0.5:
  #   D + 0.5 > 0 for D = 1, 0, 2 -> 3/4 home covers; -1 -> 1/4 away covers.
  # Handicap line -0.5:
  #   D - 0.5 > 0 for D = 1, 2 -> 2/4 home covers.
  result <- compute_handicap_probs(fixture_posterior_draws, lines = c(0.5, -0.5))

  bvp_m1 <- result |> filter(variant == "bvp", match_id == "m1_FramIA")
  expect_equal(bvp_m1$p_model[bvp_m1$line == 0.5 & bvp_m1$outcome == "home_cover"], 0.75)
  expect_equal(bvp_m1$p_model[bvp_m1$line == 0.5 & bvp_m1$outcome == "away_cover"], 0.25)
  expect_equal(bvp_m1$p_model[bvp_m1$line == -0.5 & bvp_m1$outcome == "home_cover"], 0.50)
})

test_that("compute_all_market_probs returns a single long frame with market column", {
  result <- compute_all_market_probs(
    fixture_posterior_draws,
    totals_lines = c(2.5),
    handicap_lines = c(0.5),
    draw_threshold = 0.5
  )

  # 2 variants x 2 matches x (3 1x2 + 2 totals + 2 handicap) = 28 rows.
  expect_equal(nrow(result), 28)
  expect_setequal(unique(result$market), c("1x2", "totals", "handicap"))
  expect_true(all(c("variant", "match_id", "market", "outcome", "line", "p_model") %in% names(result)))
})

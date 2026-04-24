suppressPackageStartupMessages({
  library(here)
  library(testthat)
  library(dplyr)
  library(tibble)
})

source(here::here("R", "backtest", "betting_pnl", "disagreement.R"))

test_that("classify_disagreement tags both/v3_only/bvp_only bets", {
  # Three bet keys:
  #   (m1, 1x2, H, NA):        v3 passes (250), bvp passes (400)  -> both
  #   (m1, totals, over, 2.5): v3 passes (300), bvp doesn't pass  -> v3_only
  #   (m2, 1x2, H, NA):        v3 doesn't pass, bvp passes (500)  -> bvp_only
  df <- tibble(
    match_id = c("m1", "m1", "m1", "m1", "m2", "m2"),
    market   = c("1x2", "totals", "1x2", "totals", "1x2", "1x2"),
    outcome  = c("H", "over", "H", "over", "H", "H"),
    line     = c(NA, 2.5, NA, 2.5, NA, NA),
    variant  = c("v3", "v3", "bvp", "bvp", "v3", "bvp"),
    stake    = c(250, 300, 400, 0, 0, 500),
    pass     = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  result <- classify_disagreement(df, variants = c("v3", "bvp"))

  expect_equal(nrow(result), 3)

  both_row <- result |> filter(match_id == "m1", market == "1x2")
  expect_equal(both_row$tag, "both")
  expect_equal(both_row$v3_stake, 250)
  expect_equal(both_row$bvp_stake, 400)

  v3_only_row <- result |> filter(match_id == "m1", market == "totals")
  expect_equal(v3_only_row$tag, "v3_only")
  expect_equal(v3_only_row$v3_stake, 300)
  expect_equal(v3_only_row$bvp_stake, 0)

  bvp_only_row <- result |> filter(match_id == "m2")
  expect_equal(bvp_only_row$tag, "bvp_only")
  expect_equal(bvp_only_row$v3_stake, 0)
  expect_equal(bvp_only_row$bvp_stake, 500)
})

test_that("classify_disagreement handles line column correctly (multiple totals lines)", {
  # Same (match, market, outcome) but different lines must be distinct keys.
  df <- tibble(
    match_id = c("m1", "m1", "m1", "m1"),
    market   = c("totals", "totals", "totals", "totals"),
    outcome  = c("over", "over", "over", "over"),
    line     = c(2.5, 3.5, 2.5, 3.5),
    variant  = c("v3", "v3", "bvp", "bvp"),
    stake    = c(200, 0, 250, 300),
    pass     = c(TRUE, FALSE, TRUE, TRUE)
  )

  result <- classify_disagreement(df, variants = c("v3", "bvp"))
  expect_equal(nrow(result), 2)

  expect_equal(result$tag[result$line == 2.5], "both")
  expect_equal(result$tag[result$line == 3.5], "bvp_only")
})

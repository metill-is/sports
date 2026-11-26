bins_of <- function(margins, bw = 1L, low = -20L, high = 20L, game_nr = 1L) {
  goals <- tibble::tibble(
    game_nr = game_nr,
    home_score = pmax(margins, 0) + 20,
    away_score = pmax(-margins, 0) + 20
  )
  .bin_goal_diff_distribution_2dt(goals, bucket_width = bw, range_low = low, range_high = high)
}

test_that("a whole-number margin is its own bucket", {
  out <- bins_of(-3:3)
  expect_equal(out$diff, -3:3)
  expect_equal(out$p, rep(1 / 7, 7))
})

test_that("uniform whole-number margins give flat buckets, not a sawtooth", {
  out <- bins_of(rep(-9:9, 50))
  expect_equal(out$diff, -9:9)
  expect_equal(max(out$p) - min(out$p), 0)
})

test_that("buckets are half-open, [k - 0.5, k + 0.5) to k, never half-to-even", {
  out <- bins_of(c(0.49, 0.5, -0.5, -0.51, 1.5, 2.5))
  expect_equal(out$diff, c(-1L, 0L, 1L, 2L, 3L))
  expect_equal(out$p, c(1, 2, 1, 1, 1) / 6)
})

test_that("wider buckets are half-open around their centres too", {
  out <- bins_of(c(2.4, 2.5, -2.5, -2.6), bw = 5L, low = -50L, high = 50L)
  expect_equal(out$diff, c(-5L, 0L, 5L))
  expect_equal(out$p, c(0.25, 0.5, 0.25))
})

test_that("margins beyond the range are clamped to its ends", {
  out <- bins_of(c(25, -31, 19))
  expect_equal(out$diff, c(-20L, 19L, 20L))
  expect_equal(out$p, rep(1 / 3, 3))
})

test_that("each game's probabilities sum to one", {
  goals <- rbind(
    tibble::tibble(game_nr = 1L, home_score = c(21, 22, 23), away_score = 20),
    tibble::tibble(game_nr = 2L, home_score = 20, away_score = c(25, 26))
  )
  out <- .bin_goal_diff_distribution_2dt(goals, bucket_width = 1L, range_low = -20L, range_high = 20L)
  expect_equal(as.numeric(tapply(out$p, out$game_nr, sum)), c(1, 1))
})

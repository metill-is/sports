test_that("the rounded pmf is a proper distribution and matches the normal CDF", {
  k <- -60:60
  p <- v2_integer_pmf(k, loc = rep(1.3, length(k)), scale = rep(6, length(k)))

  expect_equal(sum(p), 1, tolerance = 1e-10)
  expect_equal(p[k == 2], pnorm(2.5, 1.3, 6) - pnorm(1.5, 1.3, 6))
})

test_that("a Student-t predictive puts more mass in the tails than the Gaussian", {
  gauss <- v2_integer_pmf(25, loc = 0, scale = 6)
  heavy <- v2_integer_pmf(25, loc = 0, scale = 6, df = 4)

  expect_gt(heavy, gauss)
  expect_equal(heavy, pt(25.5 / 6, 4) - pt(24.5 / 6, 4))
})

test_that("posterior draws are averaged per match, with one scale per draw", {
  loc <- rbind(c(0, 5), c(2, 5))
  p <- v2_integer_pmf(c(0, 5), loc = loc, scale = c(4, 8))

  by_hand <- c(
    mean(c(pnorm(0.5, 0, 4) - pnorm(-0.5, 0, 4), pnorm(0.5, 2, 8) - pnorm(-0.5, 2, 8))),
    mean(c(pnorm(5.5, 5, 4) - pnorm(4.5, 5, 4), pnorm(5.5, 5, 8) - pnorm(4.5, 5, 8)))
  )
  expect_equal(p, by_hand)
})

test_that("zero_shift moves the level-score probability on the logit scale and keeps total mass one", {
  k <- -60:60
  loc <- rep(0.8, length(k))
  scale <- rep(5, length(k))
  base <- v2_integer_pmf(k, loc, scale)
  shifted <- v2_integer_pmf(k, loc, scale, zero_shift = 0.4)

  expect_equal(sum(shifted), 1, tolerance = 1e-10)
  expect_equal(qlogis(shifted[k == 0]) - qlogis(base[k == 0]), 0.4)
  # Every non-zero margin shrinks by the same factor: the shape away from zero is kept.
  # WHY base > 1e-12: at the far ends of the support both masses underflow to 0 / 0.
  ratio <- (shifted / base)[k != 0 & base > 1e-12]
  expect_equal(max(ratio) - min(ratio), 0, tolerance = 1e-12)
  expect_lt(ratio[1], 1)
})

test_that("outcome probabilities agree with the pmf at zero and sum to one", {
  loc <- c(-3, 0, 4.5)
  scale <- c(5, 5, 6)
  o <- v2_outcome_probs(loc, scale, ties = TRUE)

  expect_equal(o$p_draw, v2_integer_pmf(c(0, 0, 0), loc, scale))
  expect_equal(o$p_home + o$p_draw + o$p_away, rep(1, 3))
  expect_equal(o$p_home[2], o$p_away[2])
  expect_gt(o$p_home[3], o$p_away[3])

  shifted <- v2_outcome_probs(loc, scale, ties = TRUE, zero_shift = 0.5)
  expect_equal(shifted$p_home + shifted$p_draw + shifted$p_away, rep(1, 3))
  expect_true(all(shifted$p_draw > o$p_draw))
  expect_equal(shifted$p_home / shifted$p_away, o$p_home / o$p_away)
})

test_that("without ties the home side wins on a positive continuous margin", {
  o <- v2_outcome_probs(c(0, 6), c(12, 12), ties = FALSE)

  expect_equal(o$p_draw, c(0, 0))
  expect_equal(o$p_home, c(0.5, pnorm(6 / 12)))
})

test_that("the zero-shift estimate recovers a simulated inflation", {
  set.seed(11)
  p0 <- runif(40000, 0.03, 0.09)
  is_tie <- runif(40000) < plogis(qlogis(p0) + 0.5)
  est <- v2_fit_zero_shift(p0, is_tie)

  expect_lt(abs(est[["shift"]] - 0.5), 3 * est[["se"]])
  expect_equal(est[["ties"]], sum(is_tie))
  expect_lt(est[["se"]], 0.05)
})

test_that("basketball final-result probabilities resolve regulation ties through overtime", {
  r <- v2_basketball_result_probs(loc = c(0, 8), scale = c(12, 12), zero_shift = 0.3)

  expect_equal(r$p_home + r$p_away, c(1, 1))
  expect_equal(r$p_home_reg + r$p_draw_reg + r$p_away_reg, c(1, 1))
  expect_equal(r$p_home[1], 0.5)
  expect_equal(r$p_home_ot[1], 0.5)
  # Overtime is a short game: the favourite is favoured, but by less than over 40 minutes.
  reg_two_way <- r$p_home_reg[2] / (r$p_home_reg[2] + r$p_away_reg[2])
  expect_gt(r$p_home_ot[2], 0.5)
  expect_lt(r$p_home_ot[2], reg_two_way)
  expect_equal(r$p_home[2], r$p_home_reg[2] + r$p_draw_reg[2] * r$p_home_ot[2])
})

test_that("proportional de-vigging returns probabilities and the overround", {
  q <- v2_devig(rbind(c(1.8, 9, 2.4), c(1.5, 2.5, 100)))

  expect_equal(rowSums(q), c(1, 1))
  expect_equal(attr(q, "overround")[1], 1 / 1.8 + 1 / 9 + 1 / 2.4)
  expect_equal(q[1, 1], (1 / 1.8) / (1 / 1.8 + 1 / 9 + 1 / 2.4))
})

test_that("sampled scores are whole, non-negative and reproduce the margin and total", {
  set.seed(5)
  n_draw <- 20000
  loc_m <- matrix(rep(c(-2.4, 0.3, 7.5), each = n_draw), n_draw, 3)
  loc_t <- matrix(rep(c(52, 58.6, 61), each = n_draw), n_draw, 3)
  s <- v2_sample_scores(loc_m, 5, loc_t, 6.5)

  expect_type(s$home, "integer")
  expect_true(all(s$home >= 0L & s$away >= 0L))
  # Margin and total come back out of the two scores, so they shared parity by construction.
  expect_equal(colMeans(s$home - s$away), c(-2.4, 0.3, 7.5), tolerance = 0.05)
  expect_equal(colMeans(s$home + s$away), c(52, 58.6, 61), tolerance = 0.005)
  expect_equal(apply(s$home - s$away, 2, sd), rep(5, 3), tolerance = 0.03)
  expect_equal(apply(s$home + s$away, 2, sd), rep(6.5, 3), tolerance = 0.03)
})

test_that("sampled level scores match the analytic draw probability, with and without a shift", {
  set.seed(6)
  n_draw <- 60000
  loc_m <- matrix(rep(c(0.5, 4), each = n_draw), n_draw, 2)
  loc_t <- matrix(56, n_draw, 2)

  plain <- v2_sample_scores(loc_m, 5, loc_t, 6.5)
  expect_equal(colMeans(plain$home == plain$away), v2_outcome_probs(c(0.5, 4), c(5, 5))$p_draw, tolerance = 0.05)

  for (shift in c(0.4, -0.4)) {
    moved <- v2_sample_scores(loc_m, 5, loc_t, 6.5, zero_shift = shift)
    want <- v2_outcome_probs(c(0.5, 4), c(5, 5), zero_shift = shift)
    expect_equal(colMeans(moved$home == moved$away), want$p_draw, tolerance = 0.05)
    expect_equal(colMeans(moved$home > moved$away), want$p_home, tolerance = 0.03)
  }
})

test_that("rho_mt carries through to the correlation of sampled margin and total", {
  set.seed(7)
  loc_m <- matrix(1, 20000, 1)
  loc_t <- matrix(170, 20000, 1)

  none <- v2_sample_scores(loc_m, 13, loc_t, 17)
  some <- v2_sample_scores(loc_m, 13, loc_t, 17, rho_mt = 0.3)

  expect_lt(abs(cor(none$home - none$away, none$home + none$away)), 0.03)
  expect_equal(as.numeric(cor(some$home - some$away, some$home + some$away)), 0.3, tolerance = 0.1)
})

test_that("a per-match zero_shift vector moves each match by its own amount", {
  p <- v2_outcome_probs(c(0, 0), c(5, 5), zero_shift = c(0, 0.6))
  base <- v2_outcome_probs(0, 5)$p_draw

  expect_equal(p$p_draw[1], base)
  expect_equal(qlogis(p$p_draw[2]) - qlogis(base), 0.6)
  expect_equal(v2_integer_pmf(c(0, 0), c(0, 0), c(5, 5), zero_shift = c(0, 0.6)), p$p_draw)
})

test_that("with posterior draws the overtime share is weighted draw by draw", {
  # Two equally weighted draws of the expected regulation margin: a level game and a
  # clear home favourite. Overtime mostly follows the level-game draw, so the final
  # result is the average over draws of regulation win + level * overtime win, not the
  # product of the two averages.
  loc <- matrix(c(0, 20), nrow = 2)
  scale <- matrix(12, nrow = 2, ncol = 1)

  r <- v2_basketball_result_probs(loc, scale)
  per_draw <- rbind(v2_basketball_result_probs(0, 12), v2_basketball_result_probs(20, 12))

  expect_equal(r$p_home, mean(per_draw$p_home))
  expect_equal(r$p_away, mean(per_draw$p_away))
  expect_equal(r$p_home, r$p_home_reg + r$p_draw_reg * r$p_home_ot)
  expect_equal(r$p_home + r$p_away, 1)
})

test_that("a margin that cannot be level puts nothing on zero and still sums to one", {
  k <- -90:90
  loc <- rep(3, length(k))
  scale <- rep(12, length(k))

  no_tie <- v2_integer_pmf(k, loc, scale, ties = FALSE)
  rounded <- v2_integer_pmf(k, loc, scale)

  expect_equal(no_tie[k == 0], 0)
  expect_equal(sum(no_tie), 1, tolerance = 1e-9)
  expect_equal(no_tie[k != 0], rounded[k != 0] / (1 - rounded[k == 0]))
  expect_error(v2_integer_pmf(1, 3, 12, ties = FALSE, zero_shift = 0.3), "zero_shift")
})

test_that("a game too lopsided to reach overtime still has a final result", {
  # WHY: with an expected margin this large the probability of a level score underflows
  # to zero in every draw, and the overtime share is a ratio with that sum below it.
  r <- v2_basketball_result_probs(c(500, -500), c(12, 12))

  expect_false(anyNA(unlist(r)))
  expect_equal(r$p_draw_reg, c(0, 0))
  expect_equal(r$p_home, c(1, 0))
  expect_equal(r$p_away, c(0, 1))

  draws <- v2_basketball_result_probs(matrix(c(500, 520), nrow = 2), matrix(12, nrow = 2, ncol = 1))
  expect_equal(draws$p_home, 1)
  expect_equal(draws$p_home + draws$p_away, 1)
})

test_that("the share of draws on an observed margin can leave out the draws on a level score", {
  draws <- c(-0.2, 0.3, 1.2, 0.9, 2.6, -3.1)

  # Two of six draws round to 1; the half count keeps a margin no draw hit off zero.
  expect_equal(v2_draws_pmf(draws, 1), (2 + 0.5) / (6 + 1))
  expect_equal(v2_draws_pmf(draws, 7), 0.5 / (6 + 1))
  # A final score that cannot be level: the two level draws are set aside.
  expect_equal(v2_draws_pmf(draws, 1, ties = FALSE), (2 + 0.5) / (4 + 1))
  expect_equal(v2_draws_pmf(draws, 0, ties = FALSE), 0)
  expect_gt(v2_draws_pmf(draws, 1, ties = FALSE), v2_draws_pmf(draws, 1))
})

test_that("a scale or df that fits neither the draws nor the matches is refused", {
  # WHY: with a draws-by-matches location a scale vector is one value per draw. A scale
  # given per match would be spread over the draws in a cycle, silently.
  loc <- matrix(c(-3, 3), nrow = 4, ncol = 2, byrow = TRUE)
  expect_error(v2_cdf(c(0, 0), loc, c(5, 50)), "scale")
  expect_error(v2_cdf(c(0, 0), c(-3, 3), c(5, 50, 7)), "scale")
  expect_error(v2_cdf(c(0, 0), loc, 5, df = c(3, 30)), "df")
  expect_error(v2_cdf(c(0, 0), c(-3, 3), 5, df = c(3, 30)), "df")

  # Accepted: one scale per draw, one per match for a single predictive, a scalar, a matrix.
  expect_equal(v2_cdf(c(0, 0), loc, c(5, 5, 50, 50)), colMeans(rbind(stats::pnorm(c(3, -3) / 5), stats::pnorm(c(3, -3) / 5), stats::pnorm(c(3, -3) / 50), stats::pnorm(c(3, -3) / 50))))
  expect_equal(v2_cdf(c(0, 0), c(-3, 3), c(5, 50)), stats::pnorm(c(3 / 5, -3 / 50)))
  expect_equal(v2_cdf(c(0, 0), loc, 5), stats::pnorm(c(3, -3) / 5))
  expect_equal(v2_cdf(c(0, 0), loc, matrix(5, 4, 2), df = rep(10, 4)), stats::pt(c(3, -3) / 5, df = 10))
})

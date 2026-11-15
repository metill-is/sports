test_that("match rates follow the model's linear predictor", {
  fit <- nt_test_fit(
    theta = c(A = 0.4, B = -0.1), delta = c(A = 0.05, B = -0.02),
    mu = 0.2, home_for = 0.15, home_against = -0.2
  )
  neutral <- nt_match_rates(fit, "A", "B", venue = 0)
  expect_equal(neutral$home[1, 1], exp(0.2 + 0.03 + 0.5), ignore_attr = TRUE)
  expect_equal(neutral$away[1, 1], exp(0.2 + 0.03 - 0.5), ignore_attr = TRUE)
  at_home <- nt_match_rates(fit, "A", "B", venue = 1)
  expect_equal(at_home$home[1, 1], exp(0.2 + 0.03 + 0.5 + 0.15), ignore_attr = TRUE)
  expect_equal(at_home$away[1, 1], exp(0.2 + 0.03 - 0.5 - 0.2), ignore_attr = TRUE)
})

test_that("a neutral match is symmetric in the listing order", {
  fit <- nt_test_fit(c(A = 0.4, B = -0.1), delta = c(A = 0.05, B = -0.02))
  ab <- nt_match_rates(fit, "A", "B")
  ba <- nt_match_rates(fit, "B", "A")
  expect_equal(ab$home, ba$away, ignore_attr = TRUE)
  expect_equal(ab$away, ba$home, ignore_attr = TRUE)
})

test_that("the friendly multiplier damps the strength gap in friendlies only", {
  fit <- nt_test_fit(c(A = 0.6, B = 0), kappa = 0.5)
  r <- nt_match_rates(fit, c("A", "A"), c("B", "B"), friendly = c(FALSE, TRUE))
  expect_equal(r$home[1, ], exp(c(0.6, 0.3)), ignore_attr = TRUE)
  expect_equal(r$away[1, ], exp(c(-0.6, -0.3)), ignore_attr = TRUE)
})

test_that("match rates reject a team the fit does not know", {
  fit <- nt_test_fit(c(A = 0, B = 0))
  expect_error(nt_match_rates(fit, "A", "Atlantis"), "Atlantis")
})

test_that("scoreline probabilities are the posterior mixture of Poisson products", {
  rates <- list(home = matrix(c(1, 2), 2, 1), away = matrix(c(0.5, 1.5), 2, 1))
  grid <- nt_scoreline_probs(rates, max_goals = 15L)
  expect_equal(sum(grid), 1, tolerance = 1e-8)
  want <- 0.5 * (dpois(2, 1) * dpois(1, 0.5) + dpois(2, 2) * dpois(1, 1.5))
  expect_equal(grid[1, 3, 2], want)
})

test_that("outcome probabilities and expected goals come from the grid", {
  fit <- nt_test_fit(c(A = 0.2, B = 0.2), mu = log(1.4))
  grid <- nt_scoreline_probs(nt_match_rates(fit, c("A", "A"), c("B", "B"), venue = c(0, 0)), 15L)
  p <- nt_outcome_probs(grid)
  expect_equal(p$p_home, p$p_away)
  expect_equal(p$p_home + p$p_draw + p$p_away, c(1, 1))
  expect_equal(p$eg_home, c(1.4, 1.4), tolerance = 1e-6)
})

test_that("half-life helpers invert each other and scale as 1 / sd", {
  sd3 <- nt_rw_sd_for_half_life(3)
  expect_equal(nt_implied_half_life(sd3), 3)
  expect_equal(nt_implied_half_life(2 * sd3), 1.5)
})

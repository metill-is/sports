test_that("attack / defence terms reproduce the v2 margin and total means for every pairing", {
  set.seed(21)
  k <- 6
  net <- rnorm(k, 0, 4)
  pace <- 28 + rnorm(k, 0, 1.5)
  ha <- 0.7 + rnorm(k, 0, 0.3)
  p <- v2_as_attack_defence(net, pace, ha, sigma_m = 4.8, sigma_t = 6.6)

  for (h in 1:k) {
    for (a in setdiff(1:k, h)) {
      mu_home <- p$mean_goals + p$offense[1, h] + p$home_advantage_off[1, h] - p$defense[1, a]
      mu_away <- p$mean_goals + p$offense[1, a] - p$defense[1, h] - p$home_advantage_def[1, h]
      expect_equal(mu_home - mu_away, ha[h] + net[h] - net[a])
      expect_equal(mu_home + mu_away, pace[h] + pace[a])
    }
  }
})

test_that("the single score sd and correlation reproduce the margin sd and the total sd", {
  p <- v2_as_attack_defence(c(1, -1), c(28, 29), c(0.7, 0.7), sigma_m = 4.8, sigma_t = 6.6)

  expect_equal(sqrt(2 * p$sigma^2 * (1 - p$rho)), 4.8)
  expect_equal(sqrt(2 * p$sigma^2 * (1 + p$rho)), 6.6)
  expect_gt(p$rho, 0)
})

test_that("the existing 2DT score draw, given the adapter's terms, has the v2 predictive", {
  set.seed(22)
  p <- v2_as_attack_defence(c(3, -2), c(29.5, 27), c(0.8, 0.5), sigma_m = 4.8, sigma_t = 6.6)
  n <- 200000
  mu_home <- p$mean_goals + p$offense[1, 1] + p$home_advantage_off[1, 1] - p$defense[1, 2]
  mu_away <- p$mean_goals + p$offense[1, 2] - p$defense[1, 1] - p$home_advantage_def[1, 1]
  # nu = 1e6: the simulator's Student-t with the heavy tail switched off.
  s <- .draw_bivariate_t_2dt(rep(mu_home, n), rep(mu_away, n), p$sigma, p$sigma, p$rho, 1e6)
  margin <- s$home - s$away
  total <- s$home + s$away

  expect_equal(mean(margin), 0.8 + 3 - (-2), tolerance = 0.01)
  expect_equal(mean(total), 29.5 + 27, tolerance = 0.001)
  expect_equal(sd(margin), 4.8, tolerance = 0.01)
  expect_equal(sd(total), 6.6, tolerance = 0.01)
  expect_lt(abs(cor(margin, total)), 0.01)
})

test_that("posterior draws keep their shape and get one level, sd and correlation per draw", {
  net <- matrix(rnorm(12), nrow = 3)
  pace <- matrix(28 + rnorm(12), nrow = 3)
  ha <- matrix(0.7, nrow = 3, ncol = 4)
  p <- v2_as_attack_defence(net, pace, ha, sigma_m = c(4.7, 4.8, 4.9), sigma_t = c(6.5, 6.6, 6.7))

  expect_equal(dim(p$offense), c(3L, 4L))
  expect_equal(p$mean_goals, rowMeans(pace))
  expect_length(p$sigma, 3L)
  expect_equal(p$offense + p$defense, net)
  expect_equal(p$offense - p$defense, pace - rowMeans(pace))
})

test_that("a supplied scoring level replaces the mean over teams and keeps every margin and total", {
  set.seed(2)
  net <- matrix(stats::rnorm(12), 3)
  pace <- matrix(stats::rnorm(12, 27, 2), 3)
  ha <- matrix(0.6, 3, 4)
  level <- c(26.5, 27.2, 28)
  out <- v2_as_attack_defence(net, pace, ha, 5, 6, level = level)
  expect_equal(out$mean_goals, level)
  expect_equal(out$offense, (net + pace - level) / 2)
  mu_h <- level + out$offense[, 1] + out$home_advantage_off[, 1] - out$defense[, 2]
  mu_a <- level + out$offense[, 2] - out$defense[, 1] - out$home_advantage_def[, 1]
  expect_equal(mu_h + mu_a, pace[, 1] + pace[, 2])
  expect_equal(mu_h - mu_a, 0.6 + net[, 1] - net[, 2])
})

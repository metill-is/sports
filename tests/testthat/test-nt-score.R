test_that("1X2 scores match hand calculation", {
  s <- nt_score_outcomes(0.5, 0.3, 0.2, home_score = 2L, away_score = 0L)
  expect_equal(s$log_1x2, -log(0.5))
  expect_equal(s$rps, 0.5 * ((0.5 - 1)^2 + (0.8 - 1)^2))
  d <- nt_score_outcomes(0.5, 0.3, 0.2, 1L, 1L)
  expect_equal(d$log_1x2, -log(0.3))
  expect_equal(d$rps, 0.5 * (0.5^2 + (0.8 - 1)^2))
  a <- nt_score_outcomes(0.5, 0.3, 0.2, 0L, 1L)
  expect_equal(a$log_1x2, -log(0.2))
  expect_equal(a$rps, 0.5 * (0.5^2 + 0.8^2))
})

test_that("1X2 scores renormalise probabilities", {
  expect_equal(
    nt_score_outcomes(1, 0.6, 0.4, 3L, 1L),
    nt_score_outcomes(0.5, 0.3, 0.2, 3L, 1L)
  )
})

test_that("scoreline score reads the observed cell and rejects out-of-grid scores", {
  grid <- array(0, dim = c(2L, 3L, 3L))
  grid[1, 2, 1] <- 0.25
  grid[2, 1, 3] <- 0.1
  expect_equal(nt_score_scorelines(grid, c(1L, 0L), c(0L, 2L)), -log(c(0.25, 0.1)))
  expect_error(nt_score_scorelines(grid, c(3L, 0L), c(0L, 0L)), "exceeds")
})

test_that("paired difference agrees with a paired t test", {
  a <- c(0.9, 1.1, 0.7, 1.3, 1.0, 0.8)
  b <- c(1.0, 1.0, 0.9, 1.2, 1.4, 0.9)
  d <- nt_paired_diff(a, b)
  tt <- stats::t.test(a, b, paired = TRUE)
  expect_equal(d$diff, unname(tt$estimate))
  expect_equal(c(d$lower, d$upper), as.numeric(tt$conf.int))
  expect_equal(d$n, 6L)
})

test_that("edge calibration takes the favourite's side whichever team is listed home", {
  pred <- tibble::tibble(
    p_home = c(0.6, 0.2), p_draw = c(0.25, 0.25), p_away = c(0.15, 0.55),
    eg_home = c(1.8, 0.8), eg_away = c(0.8, 1.8)
  )
  cal <- nt_edge_calibration(pred, home_score = c(2L, 3L), away_score = c(0L, 0L), breaks = c(0, Inf))
  expect_equal(cal$n, 2L)
  expect_equal(cal$pred_fav_win, mean(c(0.6, 0.55)))
  expect_equal(cal$obs_fav_win, 0.5)
  expect_equal(cal$pred_margin, 1)
  expect_equal(cal$obs_margin, mean(c(2, -3)))
})

test_that("nt_evaluate_grid combines probabilities and the three scores", {
  fit <- nt_test_fit(c(A = 0.3, B = -0.3))
  grid <- nt_scoreline_probs(nt_match_rates(fit, "A", "B"))
  ev <- nt_evaluate_grid(grid, 1L, 1L)
  expect_named(ev, c("p_home", "p_draw", "p_away", "eg_home", "eg_away", "log_1x2", "rps", "log_score"))
  expect_equal(ev$log_1x2, -log(ev$p_draw))
  expect_equal(ev$log_score, -log(grid[1, 2, 2]))
})

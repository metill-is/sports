# `jump` (named, e.g. c(T8 = 1.4)) is added to those teams' strengths from
# period `jump_from` on.
nt_sim_prep <- function(n_teams = 8L, n_periods = 8L, per_period = 60L, seed = 11L,
                        jump = NULL, jump_from = n_periods) {
  withr::local_seed(seed)
  teams <- paste0("T", seq_len(n_teams))
  theta <- seq(0.7, -0.7, length.out = n_teams)
  n <- n_periods * per_period
  home <- sample.int(n_teams, n, replace = TRUE)
  away <- vapply(home, function(h) sample(setdiff(seq_len(n_teams), h), 1L), 1L)
  venue <- stats::rbinom(n, 1L, 0.6)
  period <- rep(seq_len(n_periods), each = per_period)
  th <- matrix(theta, n_teams, n_periods, dimnames = list(teams, NULL))
  for (tm in names(jump)) th[tm, jump_from:n_periods] <- th[tm, jump_from:n_periods] + jump[[tm]]
  gap <- th[cbind(home, period)] - th[cbind(away, period)]
  matches <- tibble::tibble(
    match_date = as.Date("2020-01-15") + 91L * (rep(seq_len(n_periods), each = per_period) - 1L),
    home_team = teams[home], away_team = teams[away],
    home_score = stats::rpois(n, exp(0.2 + gap + 0.3 * venue)),
    away_score = stats::rpois(n, exp(0.2 - gap - 0.2 * venue)),
    tournament = "FIFA World Cup qualification",
    home_nr = home, away_nr = away,
    period = period,
    venue = venue, friendly = rep(c(FALSE, TRUE), length.out = n)
  )
  list(
    prep = list(
      matches = matches, teams = teams, origin = as.Date("2020-01-01"),
      n_periods = n_periods, step_months = 3L, cutoff = as.Date("2022-01-01")
    ),
    theta = stats::setNames(theta, teams)
  )
}

test_that("the design matrices encode strength gap, openness and damping", {
  sim <- nt_sim_prep(n_teams = 4L, n_periods = 3L, per_period = 5L)
  prep <- sim$prep
  des <- .nt_design(prep, kappa = 0.5)
  n <- nrow(prep$matches)
  expect_equal(dim(des$a_u), c(2L * n, 4L * 3L))
  expect_equal(as.numeric(Matrix::rowSums(des$a_alpha)), rep(0, 2L * n))
  expect_equal(as.numeric(Matrix::rowSums(des$a_delta)), rep(2, 2L * n))
  expect_equal(des$y, c(prep$matches$home_score, prep$matches$away_score))
  m1 <- prep$matches[1, ]
  w1 <- if (m1$friendly) 0.5 else 1
  expect_equal(des$a_alpha[1, m1$home_nr], w1)
  expect_equal(des$a_alpha[1, m1$away_nr], -w1)
  expect_equal(des$a_alpha[n + 1L, m1$home_nr], -w1)
  expect_equal(des$a_u[1, (m1$home_nr - 1L) * 3L + m1$period], w1)
  expect_equal(des$a_u[1, (m1$away_nr - 1L) * 3L + m1$period], -w1)
  friendly_rows <- which(rep(prep$matches$friendly, 2L))
  expect_true(all(abs(des$a_alpha[friendly_rows, ]@x) == 0.5))
  expect_equal(des$home_for[seq_len(n)], as.numeric(prep$matches$venue == 1L))
  expect_equal(des$home_for[n + seq_len(n)], rep(0, n))
  expect_equal(des$home_against[n + seq_len(n)], as.numeric(prep$matches$venue == 1L))
})

test_that("the INLA fit recovers simulated strengths and venue effects", {
  skip_if_no_inla()
  sim <- nt_sim_prep()
  fit <- nt_fit_inla(sim$prep, n_draws = 200L, threads = "1:1")
  est <- colMeans(fit$draws$theta)
  expect_gt(stats::cor(est, sim$theta), 0.97)
  within_3_sd <- function(draws, truth) {
    abs(mean(draws) - truth) < 3 * stats::sd(draws)
  }
  expect_true(within_3_sd(fit$draws$theta[, "T1"] - fit$draws$theta[, "T8"], 1.4))
  expect_true(within_3_sd(fit$draws$home_for, 0.3))
  expect_true(within_3_sd(fit$draws$home_against, -0.2))
  expect_gt(mean(fit$draws$home_for), 0)
  expect_lt(mean(fit$draws$home_against), 0)
  expect_equal(dim(fit$draws$theta), c(200L, 8L))
  expect_true("sd u per sqrt(month)" %in% fit$hyper$parameter)
})

test_that("the posterior draws are the strengths at the end of the window", {
  skip_if_no_inla()
  # T8 starts weakest and jumps above everyone for the last three periods;
  # a draw from any earlier period would leave it below mid-table.
  sim <- nt_sim_prep(jump = c(T8 = 1.6), jump_from = 6L)
  fit <- nt_fit_inla(sim$prep, n_draws = 200L, threads = "1:1")
  th <- colMeans(fit$draws$theta)
  expect_gt(th[["T8"]], th[["T3"]])
  expect_gt(th[["T8"]] - th[["T7"]], 0.8)
})

test_that("a fixed random-walk sd is honoured, not silently ignored", {
  skip_if_no_inla()
  sim <- nt_sim_prep()
  fit <- nt_fit_inla(sim$prep, rw_sd = 0.01, n_draws = 10L, threads = "1:1", keep_result = TRUE)
  expect_false(any(grepl("^sd u", fit$hyper$parameter)))
  u_hyper <- fit$result$all.hyper$random[[2]]
  expect_equal(u_hyper$hyperid, "u")
  expect_true(u_hyper$hyper$theta$fixed)
  expect_equal(u_hyper$hyper$theta$initial, log(1 / (0.01 * sqrt(3))^2), ignore_attr = TRUE)
})

test_that("the PC priors reach INLA", {
  skip_if_no_inla()
  sim <- nt_sim_prep()
  fit <- nt_fit_inla(sim$prep, n_draws = 10L, threads = "1:1", keep_result = TRUE)
  priors <- vapply(fit$result$all.hyper$random, function(h) h$hyper$theta$prior, "")
  expect_equal(unname(priors), rep("pc.prec", 3L))
  upper <- vapply(fit$result$all.hyper$random, function(h) h$hyper$theta$param[1], 1)
  expect_equal(unname(upper), c(2, 0.1, 0.3))
})

test_that("Stan data mirrors the prepared inputs", {
  sim <- nt_sim_prep(n_teams = 4L, n_periods = 3L, per_period = 5L)
  sd <- nt_stan_data(sim$prep, c("T1", "T2"), c("T3", "T4"), venue_pred = c(1, 0))
  expect_equal(c(sd$K, sd$T, sd$N, sd$N_pred), c(4L, 3L, 15L, 2L))
  expect_equal(sd$home, sim$prep$matches$home_nr)
  expect_equal(as.vector(sd$home_pred), c(1L, 2L))
  expect_equal(as.vector(sd$venue_pred), c(1, 0))
  expect_equal(sd$damp_friendly, 0L)
  expect_error(nt_stan_data(sim$prep, "T1", "Nowhere"), "Nowhere")
  empty <- nt_stan_data(sim$prep)
  expect_equal(empty$N_pred, 0L)
  expect_length(empty$home_pred, 0L)
})

test_that("the Stan reference model is syntactically valid", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )
  mod <- cmdstanr::cmdstan_model(
    here::here("Stan", "football_national", "nt_v2.stan"),
    compile = FALSE
  )
  expect_true(suppressMessages(mod$check_syntax(quiet = TRUE)))
})

test_that("a reproducible fit runs on one thread without online optimisation and repeats its draws", {
  skip_if_no_inla()
  sim <- nt_sim_prep()
  a <- nt_fit_inla(sim$prep, n_draws = 50L, threads = "1:1", reproducible = TRUE, seed = 5L)
  b <- nt_fit_inla(sim$prep, n_draws = 50L, threads = "1:1", reproducible = TRUE, seed = 5L)
  expect_true(a$settings$reproducible)
  expect_identical(a$draws, b$draws)
  expect_error(nt_fit_inla(sim$prep, threads = "4:1", reproducible = TRUE), "one thread")
})

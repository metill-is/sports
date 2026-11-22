# Simulated league: 8 teams, 3 seasons, double round robin per season, one division.
# Most of a team's strength is specific to the season (sd 3.5 against a club level of sd
# 1.5), so a fit that attached season levels to the wrong team-season could not recover it.
simulate_v2_league <- function(seed = 3, hfa = 2.5, sigma = 5, level = 56, sigma_total = 7) {
  set.seed(seed)
  teams <- paste0("T", 1:8)
  strength <- rnorm(8, 0, 1.5)
  pace <- rnorm(8, 0, 2)
  rows <- list()
  by_season <- matrix(NA_real_, 8, 3)
  for (s in 1:3) {
    season_strength <- strength + rnorm(8, 0, 3.5)
    by_season[, s] <- season_strength
    pairs <- expand.grid(h = 1:8, a = 1:8)
    pairs <- pairs[pairs$h != pairs$a, ]
    pairs <- pairs[sample(nrow(pairs)), ]
    dates <- as.Date(sprintf("%d-09-01", 2023 + s)) + sort(sample(0:180, nrow(pairs), replace = TRUE))
    margin <- hfa + season_strength[pairs$h] - season_strength[pairs$a] + rnorm(nrow(pairs), 0, sigma)
    total <- level + pace[pairs$h] + pace[pairs$a] + rnorm(nrow(pairs), 0, sigma_total)
    rows[[s]] <- data.frame(
      match_date = dates, season = 2024L + s, division = "OD",
      home_team = teams[pairs$h], away_team = teams[pairs$a],
      home_score = (total + margin) / 2, away_score = (total - margin) / 2
    )
  }
  list(matches = do.call(rbind, rows), teams = teams, strength = strength, by_season = by_season)
}

test_that("the INLA margin fit recovers the home advantage and each team-season's strength", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  m <- sim$matches
  train <- m[m$match_date < as.Date("2026-12-01"), ]
  pred <- m[m$match_date >= as.Date("2026-12-01"), ]
  v <- build_v2_data(train, pred, sim$teams)

  fit <- fit_v2_inla(v, "margin", v2_priors("handball"), int_strategy = "eb")

  hfa <- fit$fixed["hfa", ]
  expect_lt(abs(hfa$mean - 2.5), 3 * hfa$sd)
  expect_lt(abs(sqrt(fit$sigma2) - 5), 0.6)
  # State of team k in season s = club level + season level; u_s is stored team by team.
  # A margin identifies strengths only up to a constant per season, so both sides are
  # centred within the season before they are compared.
  state <- fit$random$b$mean + matrix(fit$random$u_s$mean, nrow = v$K, byrow = TRUE)
  centre <- function(m) sweep(m, 2, colMeans(m))
  expect_gt(cor(as.numeric(centre(state)), as.numeric(centre(sim$by_season))), 0.85)

  expect_equal(nrow(fit$pred), nrow(pred))
  expect_equal(nrow(fit$fitted), nrow(train))
  expect_true(all(fit$pred$sd > 0))
  # A held-out margin is predicted better than by the league average alone.
  observed <- pred$home_score - pred$away_score
  expect_lt(mean((observed - fit$pred$mean)^2), mean((observed - mean(observed))^2))
})

test_that("the within-season walk sums to zero inside every team-season", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  fit <- fit_v2_inla(v, "margin", v2_priors("handball"), int_strategy = "eb")

  w <- matrix(fit$random$w_t$mean, nrow = v$TS, ncol = v$W, byrow = TRUE)
  expect_equal(nrow(fit$random$w_t), v$TS * v$W)
  expect_lt(max(abs(rowSums(w))), 1e-4)
})

test_that("the INLA total fit recovers the scoring level and returns one intercept per division", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  fit <- fit_v2_inla(v, "total", v2_priors("handball"), int_strategy = "eb", within = "none")

  expect_equal(rownames(fit$fixed), "alpha1")
  expect_lt(abs(fit$fixed$mean - 56), 3)
  expect_lt(abs(sqrt(fit$sigma2) - 7), 0.8)
  expect_false("w_t" %in% names(fit$random))
})

test_that("a supplied response replaces the stored scores", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  shifted <- (v$y_home - v$y_away) + 10

  base <- fit_v2_inla(v, "margin", v2_priors("handball"), int_strategy = "eb", within = "none")
  moved <- fit_v2_inla(v, "margin", v2_priors("handball"), int_strategy = "eb", within = "none", y = shifted)

  # WHY 0.1: the home advantage has a normal(0, 5) prior, so a 10-goal shift is pulled
  # back by a hair rather than reproduced to the last digit.
  expect_equal(moved$fixed$mean - base$fixed$mean, 10, tolerance = 0.1)
})

test_that("the priors and starting values the fitter states are the ones R-INLA uses", {
  # WHY: R-INLA drops a hyperparameter spec without a warning when it cannot read it from
  # the model term, and the fit still looks plausible under its default priors.
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  pr <- v2_priors("handball")

  fit <- fit_v2_inla(v, "margin", pr, within = "ar1", rho_week = 0.95, int_strategy = "eb")$fit

  used <- fit$all.hyper
  term <- function(id) Filter(function(r) identical(as.character(r$hyperid), id), used$random)[[1]]$hyper
  expect_equal(as.character(used$family[[1]]$hyper$theta1$prior), "pc.prec")
  expect_equal(as.numeric(used$family[[1]]$hyper$theta1$param), c(pr$u_sigma_m, 0.01))
  expect_equal(as.numeric(used$family[[1]]$hyper$theta1$initial), -2 * log(pr$u_sigma_m / 5))

  expect_equal(as.numeric(term("b")$theta$param), c(pr$u_b, 0.01))
  expect_equal(as.numeric(term("ha")$theta$param), c(pr$u_ha, 0.01))
  expect_equal(as.numeric(term("u_s")$theta1$param), c(pr$u_u, 0.01))
  expect_equal(as.character(term("u_s")$theta2$prior), "normal")
  expect_equal(as.numeric(term("u_s")$theta2$param), c(pr$rho_mu, 1 / pr$rho_sd^2))

  # The within-season correlation is fixed, not estimated.
  expect_equal(as.numeric(term("w_t")$theta1$param), c(pr$u_w_ar1, 0.01))
  expect_equal(as.numeric(term("w_t")$theta1$initial), -2 * log(pr$u_w_ar1 / 5))
  expect_true(term("w_t")$theta2$fixed)
  expect_equal(as.numeric(term("w_t")$theta2$initial), log(1.95 / 0.05))
  expect_false(any(grepl("Rho for w_t", rownames(fit$summary.hyperpar))))
  expect_true(any(grepl("Rho for u_s", rownames(fit$summary.hyperpar))))
})

test_that("the mode status is read from the fit and a missing status is not taken for success", {
  expect_identical(v2_inla_mode_status(list(mode = list(mode.status = 0))), 0L)
  expect_identical(v2_inla_mode_status(list(mode = list(mode.status = 3))), 3L)
  expect_identical(v2_inla_mode_status(list(mode = list())), NA_integer_)
  expect_identical(v2_inla_mode_status(list(mode = list(mode.status = NA))), NA_integer_)
})

test_that("a fit returns the status of its mode search and warns when the search failed", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  pr <- v2_priors("handball")

  expect_no_warning(ok <- fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb"))
  expect_identical(ok$mode_status, 0L)

  # WHY a replaced status: a failed search cannot be produced on demand. Only the status
  # the fitter reads is replaced; the fit itself is real.
  local_mocked_bindings(v2_inla_mode_status = function(fit) 2L)
  expect_warning(
    bad <- fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb"),
    class = "v2_inla_mode_not_found"
  )
  expect_identical(bad$mode_status, 2L)
})

# Two divisions of four teams over three seasons. Each division has its own scoring level
# and the two move in opposite directions from season to season, so the six
# division-season levels are all different: a fit that mixed up divisions, or seasons
# within a division, could not return them.
simulate_two_divisions <- function(seed = 5, sigma_total = 6) {
  set.seed(seed)
  level <- rbind(G66 = c(46, 40, 34), OD = c(56, 62, 68))
  rows <- list()
  for (dv in rownames(level)) {
    teams <- paste0(dv, 1:4)
    pace <- rnorm(4, 0, 1)
    pace <- pace - mean(pace)
    for (s in 1:3) {
      pairs <- expand.grid(h = 1:4, a = 1:4)
      pairs <- pairs[pairs$h != pairs$a, ]
      pairs <- pairs[rep(seq_len(nrow(pairs)), 2), ]
      dates <- as.Date(sprintf("%d-09-01", 2023 + s)) + sort(sample(0:180, nrow(pairs), replace = TRUE))
      total <- level[dv, s] + pace[pairs$h] + pace[pairs$a] + rnorm(nrow(pairs), 0, sigma_total)
      margin <- rnorm(nrow(pairs), 0, 5)
      rows[[paste(dv, s)]] <- data.frame(
        match_date = dates, season = 2024L + s, division = dv,
        home_team = teams[pairs$h], away_team = teams[pairs$a],
        home_score = (total + margin) / 2, away_score = (total - margin) / 2
      )
    }
  }
  matches <- do.call(rbind, rows)
  list(matches = matches, level = level, teams = sort(unique(matches$home_team)))
}

test_that("the total fit keeps each division's level on that division's own season path", {
  skip_if_no_inla()
  sim <- simulate_two_divisions()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  fit <- fit_v2_inla(v, "total", v2_priors("handball"), int_strategy = "eb", within = "none")

  expect_equal(v$divisions, rownames(sim$level))
  expect_equal(rownames(fit$fixed), c("alpha1", "alpha2"))
  # Level of division d in season s = the division's intercept + its season path, which
  # R-INLA returns division by division.
  path <- matrix(fit$random$s_s$mean, nrow = v$ND, ncol = v$NS, byrow = TRUE)
  fitted_level <- fit$fixed$mean + path
  expect_lt(max(abs(fitted_level - sim$level)), 3.5)
})

# The hyperparameter block R-INLA actually used for one model term.
used_hyper <- function(fit, id) {
  Filter(function(r) identical(as.character(r$hyperid), id), fit$all.hyper$random)[[1]]$hyper
}
v2_start <- function(u) -2 * log(u / 5)

test_that("the total model and the random-walk form run on the priors and starting values stated", {
  # WHY a second prior test: the first fits the margin model with the AR(1) form only.
  # The total model's season path, the random-walk form, the starting values of the
  # random effects and the normal priors on the intercepts are separate specifications,
  # and each can be dropped without a warning on its own.
  skip_if_no_inla()
  sim <- simulate_two_divisions()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  pr <- v2_priors("handball")

  fit <- fit_v2_inla(v, "total", pr, within = "rw1", int_strategy = "eb")$fit

  family <- fit$all.hyper$family[[1]]$hyper$theta1
  expect_equal(as.numeric(family$param), c(pr$u_sigma_t, 0.01))
  expect_equal(as.numeric(family$initial), v2_start(pr$u_sigma_t))

  season_path <- used_hyper(fit, "s_s")$theta
  expect_equal(as.character(season_path$prior), "pc.prec")
  expect_equal(as.numeric(season_path$param), c(pr$u_season, 0.01))
  expect_equal(as.numeric(season_path$initial), v2_start(pr$u_season))

  walk <- used_hyper(fit, "w_t")$theta
  expect_equal(as.character(walk$prior), "pc.prec")
  expect_equal(as.numeric(walk$param), c(pr$u_w, 0.01))
  expect_equal(as.numeric(walk$initial), v2_start(pr$u_w))

  # The club and season levels of the total model carry the same priors as the margin's.
  expect_equal(as.numeric(used_hyper(fit, "b")$theta$param), c(pr$u_b, 0.01))
  expect_equal(as.numeric(used_hyper(fit, "b")$theta$initial), v2_start(pr$u_b))
  expect_equal(as.numeric(used_hyper(fit, "u_s")$theta1$param), c(pr$u_u, 0.01))
  expect_equal(as.numeric(used_hyper(fit, "u_s")$theta1$initial), v2_start(pr$u_u))
  expect_equal(as.numeric(used_hyper(fit, "u_s")$theta2$param), c(pr$rho_mu, 1 / pr$rho_sd^2))

  fixed <- fit$all.hyper$fixed
  expect_equal(vapply(fixed, function(f) f$label, character(1)), c("alpha1", "alpha2"))
  expect_equal(vapply(fixed, function(f) f$prior.mean, numeric(1)), rep(pr$total_mean, 2))
  expect_equal(vapply(fixed, function(f) f$prior.prec, numeric(1)), rep(1 / pr$total_mean_sd^2, 2))
})

test_that("the margin model's home-advantage prior and starting values are the ones stated", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  pr <- v2_priors("handball")

  fit <- fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb")$fit

  fixed <- fit$all.hyper$fixed
  expect_equal(vapply(fixed, function(f) f$label, character(1)), "hfa")
  expect_equal(fixed[[1]]$prior.mean, 0)
  expect_equal(fixed[[1]]$prior.prec, 1 / pr$hfa_sd^2)
  expect_equal(as.numeric(used_hyper(fit, "ha")$theta$initial), v2_start(pr$u_ha))
  expect_equal(as.numeric(used_hyper(fit, "b")$theta$initial), v2_start(pr$u_b))
  expect_equal(as.numeric(used_hyper(fit, "u_s")$theta1$initial), v2_start(pr$u_u))
})

test_that("a fit that reports no mode status at all is treated as a failed search", {
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  local_mocked_bindings(v2_inla_mode_status = function(fit) NA_integer_)
  expect_warning(
    bad <- fit_v2_inla(v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb"),
    class = "v2_inla_mode_not_found"
  )
  expect_identical(bad$mode_status, NA_integer_)
})

# Set R-INLA's session option for the rest of a test and put the old value back after it.
local_internal_opt <- function(value, env = parent.frame()) {
  old <- INLA::inla.getOption("internal.opt")
  INLA::inla.setOption(internal.opt = value)
  withr::defer(INLA::inla.setOption(internal.opt = old), envir = env)
}

test_that("the fitter asks R-INLA for its online optimisation off in the call and leaves the session setting alone", {
  # WHY: with that option on, the same fit does not always return the same numbers in two
  # R processes. The fitter passes it off through `control.compute` for its own call only.
  # R-INLA 25.6.7 reads its session-wide option only when the call's is NULL (in
  # inla.problem.section(); nothing else reads it), so nothing session-wide is changed or
  # has to be put back.
  # WHY a throwing stand-in: R-INLA's fitting function cannot be called through a wrapper
  # (it evaluates the model in its caller's frame), so the stand-in records what it was
  # asked for and stops there.
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  pr <- v2_priors("handball")
  at_the_call <- NULL
  local_mocked_bindings(
    inla = function(..., control.compute = NULL) {
      at_the_call <<- list(asked = control.compute$internal.opt, session = INLA::inla.getOption("internal.opt"))
      stop("no fit today")
    },
    .package = "INLA"
  )

  for (callers_setting in c(TRUE, FALSE)) {
    local_internal_opt(callers_setting)
    at_the_call <- NULL
    expect_error(fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb"), "no fit today")
    expect_identical(at_the_call, list(asked = FALSE, session = callers_setting))
    expect_identical(INLA::inla.getOption("internal.opt"), callers_setting)
  }

  # Asked not to, the fitter passes nothing, so R-INLA goes by its session setting.
  local_internal_opt(TRUE)
  at_the_call <- NULL
  expect_error(fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb", reproducible = FALSE), "no fit today")
  expect_identical(at_the_call, list(asked = NULL, session = TRUE))
})

test_that("a fit that succeeds was made with the option off and leaves R-INLA's session setting as it was", {
  # A real fit, no stand-in: R-INLA keeps the arguments it was called with in `.args`.
  skip_if_no_inla()
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  local_internal_opt(TRUE)

  fit <- fit_v2_inla(v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb")

  expect_identical(fit$mode_status, 0L)
  expect_identical(fit$fit$.args$control.compute$internal.opt, FALSE)
  expect_true(INLA::inla.getOption("internal.opt"))
})

test_that("the same fit in two separate R processes returns the same numbers", {
  skip_if_no_inla()
  skip_if_not_installed("callr")
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)
  root <- normalizePath(testthat::test_path("..", ".."))
  fit_elsewhere <- function() {
    callr::r(function(root, v) {
      pkgload::load_all(root, quiet = TRUE)
      f <- fit_v2_inla(v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb")
      list(theta = f$theta, fitted = f$fitted, hyper = f$hyper_sd, sigma2 = f$sigma2, status = f$mode_status)
    }, args = list(root = root, v = v))
  }

  first <- fit_elsewhere()
  second <- fit_elsewhere()

  expect_identical(first$status, 0L)
  expect_identical(first, second)
})

test_that("the draws of a full-integration fit do not depend on where R's generator stands, and drawing them does not move it", {
  # WHY: R-INLA's own seed reaches its sampler of the latent field only. Which
  # hyperparameter configuration each draw comes from is picked with R's generator, so
  # under full integration the same fit drew differently in two R processes: in two
  # complete runs of the 16 full-integration fits behind the report, 3 % to 52 % of the
  # 2,000 draws of a fit differed.
  skip_if_no_inla()
  sim <- simulate_v2_league()
  m <- sim$matches
  train <- m[m$match_date < as.Date("2026-12-01"), ]
  pred <- m[m$match_date >= as.Date("2026-12-01"), ]
  v <- build_v2_data(train, pred, sim$teams)
  draws <- function(seed = 1L) {
    fit_v2_inla(v, "margin", v2_priors("handball"), within = "none", int_strategy = "auto", n_samples = 200L, seed = seed)$samples
  }

  set.seed(11)
  first <- draws()
  after_drawing <- stats::runif(1)
  set.seed(12)
  second <- draws()

  expect_identical(first, second)
  expect_false(identical(draws(seed = 2L)$sigma, first$sigma))
  # R-INLA's fit takes numbers from R's generator itself; the drawing takes none on top,
  # so the caller's stream is where the same fit without draws leaves it.
  set.seed(11)
  fit_v2_inla(v, "margin", v2_priors("handball"), within = "none", int_strategy = "auto")
  expect_identical(stats::runif(1), after_drawing)
})

# Training and prediction rows from the simulated league: the third season's matches from
# December on are predicted.
v2_train_pred <- function() {
  sim <- simulate_v2_league()
  m <- sim$matches
  train <- m[m$match_date < as.Date("2026-12-01"), ]
  pred <- m[m$match_date >= as.Date("2026-12-01"), ]
  list(train = train, pred = pred, v = build_v2_data(train, pred, sim$teams))
}

test_that("the posterior draws are of the prediction rows and of the observation sd", {
  # WHY: the draws feed the full-integration scores; draws of the wrong rows would score
  # other matches without any error.
  skip_if_no_inla()
  d <- v2_train_pred()
  fit <- fit_v2_inla(d$v, "margin", v2_priors("handball"), within = "none", int_strategy = "auto", n_samples = 1000L)

  expect_identical(dim(fit$samples$loc), c(1000L, nrow(d$pred)))
  # Each match's draws centre on its own predictive mean; the Monte Carlo error of 1,000
  # draws is about 3 % of the predictive sd.
  expect_lt(max(abs(colMeans(fit$samples$loc) - fit$pred$mean) / fit$pred$sd), 0.15)
  expect_equal(mean(fit$samples$sigma^2), fit$sigma2, tolerance = 0.05)
})

test_that("drawing after a fit on more than one thread does not warn", {
  # WHY: with a seed, R-INLA's sampler runs on one thread whatever it is asked, and warns
  # when asked for more. The fitter asks for the one thread it gets.
  skip_if_no_inla()
  d <- v2_train_pred()
  expect_no_warning(
    fit_v2_inla(d$v, "margin", v2_priors("handball"), within = "none", int_strategy = "auto", n_samples = 50L, num_threads = "2:1"),
    message = "parallel model is disabled"
  )
})

test_that("a warm start reaches R-INLA's mode search, and a cold fit starts without one", {
  skip_if_no_inla()
  d <- v2_train_pred()
  pr <- v2_priors("handball")
  cold <- fit_v2_inla(d$v, "margin", pr, within = "none", int_strategy = "eb")
  warm <- fit_v2_inla(d$v, "margin", pr, within = "none", int_strategy = "eb", theta_start = cold$theta)

  expect_null(cold$fit$.args$control.mode$theta)
  expect_equal(unname(warm$fit$.args$control.mode$theta), unname(cold$theta))
  expect_true(warm$fit$.args$control.mode$restart)
})

test_that("leave-one-out densities come back for the training rows, and only for them", {
  # R-INLA has no leave-one-out density for a row without an outcome (a fixture), so a
  # prediction row read by mistake shows up as NA.
  skip_if_no_inla()
  d <- v2_train_pred()
  fit <- fit_v2_inla(d$v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb", cpo = TRUE)

  expect_length(fit$cpo, nrow(d$train))
  expect_true(all(is.finite(fit$cpo) & fit$cpo > 0))
  expect_null(fit_v2_inla(d$v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb")$cpo)
})

test_that("without the club level the fit has no club term and no club sd", {
  skip_if_no_inla()
  d <- v2_train_pred()
  pr <- v2_priors("handball")
  with_club <- fit_v2_inla(d$v, "margin", pr, within = "none", int_strategy = "eb")
  without <- fit_v2_inla(d$v, "margin", pr, within = "none", int_strategy = "eb", club_level = FALSE)

  expect_true("b" %in% names(with_club$random))
  expect_true("sd: b" %in% with_club$hyper_sd$parameter)
  expect_false("b" %in% names(without$random))
  expect_false("sd: b" %in% without$hyper_sd$parameter)
})

test_that("hyperparameters are reported on the sd and correlation scale", {
  # R-INLA's own summary gives precisions and the season memory on the correlation scale;
  # a quantile carries through a monotone transform, so the medians must agree.
  skip_if_no_inla()
  d <- v2_train_pred()
  fit <- fit_v2_inla(d$v, "margin", v2_priors("handball"), within = "none", int_strategy = "eb")
  h <- fit$hyper_sd
  s <- fit$fit$summary.hyperpar

  expect_equal(h$q50[h$parameter == "rho: u_s"], s["Rho for u_s", "0.5quant"], tolerance = 0.02)
  expect_equal(
    h$q50[h$parameter == "sd: the Gaussian observations"],
    1 / sqrt(s["Precision for the Gaussian observations", "0.5quant"]),
    tolerance = 0.02
  )
  expect_true(all(h$q05 <= h$q50 & h$q50 <= h$q95))
})

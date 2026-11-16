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
  skip_if_not_installed("INLA")
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
  skip_if_not_installed("INLA")
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  fit <- fit_v2_inla(v, "margin", v2_priors("handball"), int_strategy = "eb")

  w <- matrix(fit$random$w_t$mean, nrow = v$TS, ncol = v$W, byrow = TRUE)
  expect_equal(nrow(fit$random$w_t), v$TS * v$W)
  expect_lt(max(abs(rowSums(w))), 1e-4)
})

test_that("the INLA total fit recovers the scoring level and returns one intercept per division", {
  skip_if_not_installed("INLA")
  sim <- simulate_v2_league()
  v <- build_v2_data(sim$matches, sim$matches[0, ], sim$teams)

  fit <- fit_v2_inla(v, "total", v2_priors("handball"), int_strategy = "eb", within = "none")

  expect_equal(rownames(fit$fixed), "alpha1")
  expect_lt(abs(fit$fixed$mean - 56), 3)
  expect_lt(abs(sqrt(fit$sigma2) - 7), 0.8)
  expect_false("w_t" %in% names(fit$random))
})

test_that("a supplied response replaces the stored scores", {
  skip_if_not_installed("INLA")
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
  skip_if_not_installed("INLA")
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
  skip_if_not_installed("INLA")
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

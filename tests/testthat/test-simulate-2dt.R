# The 2DT match generators reproduce the generated-quantities blocks of
# Stan/basketball_iceland/2d_student_t_scalarsigma.stan and
# Stan/handball_iceland/2d_student_t.stan (spec 2026-09-16 §4). Task 8's
# equivalence test pins them against a real fit; these pin the pieces.

.const_scalars <- function(n, ...) tibble::tibble(.draw = seq_len(n), ...)
.flat_side <- function(n, off = 0, def = 0, ...) {
  c(list(off = rep(off, n), def = rep(def, n)), lapply(list(...), rep, n))
}

test_that("each side's mean is the level plus its offence minus the other's defence", {
  n <- 5L
  sc <- .const_scalars(n,
    mean_goals = 80, nu = 30, sigma = 1e-9,
    alpha_rho = 0, beta_rho = 0, beta2_rho = 0, beta3_rho = 0
  )
  g <- .match_fn_basketball(.flat_side(n, off = 5, def = 5), .flat_side(n, off = -1, def = 0.5), sc)
  expect_equal(g$home, rep(80 + 5 - 0.5, n), tolerance = 1e-6)
  expect_equal(g$away, rep(80 - 1 - 5, n), tolerance = 1e-6)
})

test_that("handball scales each side by its own sigma_team", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 28, nu = 1e6, rho = 0)
  set.seed(1)
  g <- .match_fn_handball(
    .flat_side(n, sigma_team = 6), .flat_side(n, sigma_team = 3), sc
  )
  expect_lt(abs(stats::sd(g$home) - 6), 0.05)
  expect_lt(abs(stats::sd(g$away) - 3), 0.03)
  expect_lt(abs(mean(g$home) - 28), 0.05)
})

test_that("handball scores are correlated at the model's rho", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 28, nu = 1e6, rho = 0.4)
  set.seed(2)
  side <- .flat_side(n, sigma_team = 5)
  g <- .match_fn_handball(side, side, sc)
  expect_lt(abs(stats::cor(g$home, g$away) - 0.4), 0.01)
})

test_that("basketball's rho follows each fixture's strength gap and total", {
  n <- 200000L
  sc <- .const_scalars(n,
    mean_goals = 80, nu = 1e6, sigma = 10,
    alpha_rho = 0.2, beta_rho = 0.05, beta2_rho = -0.03, beta3_rho = 0.004
  )
  # d = |off_h + def_h - off_a - def_a| = 5, t = |off_h + def_h + off_a + def_a| = 5
  target <- 2 * stats::plogis(0.2 + 0.05 * 5 - 0.03 * 5 + 0.004 * 5 * 5) - 1
  set.seed(3)
  g <- .match_fn_basketball(.flat_side(n, off = 4, def = 1), .flat_side(n), sc)
  expect_lt(abs(stats::cor(g$home, g$away) - target), 0.01)
})

test_that("both scores share one mixing variable (a bivariate t, not two t's)", {
  n <- 200000L
  sc <- .const_scalars(n, mean_goals = 0, nu = 5, rho = 0)
  set.seed(4)
  side <- .flat_side(n, sigma_team = 1)
  g <- .match_fn_handball(side, side, sc)
  # Uncorrelated scores, but a shared scale makes their magnitudes move
  # together (population value ~0.21 at nu = 5).
  expect_lt(abs(stats::cor(g$home, g$away)), 0.03)
  expect_gt(stats::cor(abs(g$home), abs(g$away)), 0.1)
  # Two independent t draws -- the bug this guards against -- show nothing.
  set.seed(4)
  wrong_h <- stats::rt(n, df = 5)
  wrong_a <- stats::rt(n, df = 5)
  expect_lt(abs(stats::cor(abs(wrong_h), abs(wrong_a))), 0.03)
})

test_that("2DT points: handball draws inside the threshold, basketball never draws", {
  hb <- .points_fn_2dt(has_ties = TRUE, tie_threshold = 0.5)(c(30.3, 31, 28), c(30, 29, 30))
  expect_identical(hb$home, c(1L, 2L, 0L))
  expect_identical(hb$away, c(1L, 0L, 2L))
  bb <- .points_fn_2dt(has_ties = FALSE, tie_threshold = 0)(c(80.2, 70), c(80, 71))
  expect_identical(bb$home, c(2L, 0L))
  expect_identical(bb$away, c(0L, 2L))
})

test_that(".match_fn_2dt dispatches by sport and declares its columns", {
  expect_identical(.match_fn_2dt("handball"), .match_fn_handball)
  expect_setequal(
    attr(.match_fn_2dt("basketball"), "scalar_cols"),
    c("mean_goals", "nu", "sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho")
  )
  expect_identical(attr(.match_fn_2dt("handball"), "team_cols"), "sigma_team")
  expect_error(.match_fn_2dt("football"), "2DT")
})

test_that(".extract_sim_inputs_2dt returns the simulator's columns, draw-aligned", {
  teams <- tibble::tibble(team = c("A", "B", "C"))
  fit <- stub_fit(stub_2dt_draws(
    teams$team,
    n_pred = 1L, n_draws = 20L, n_seasons = 3L, level = 85
  ))
  bb <- .extract_sim_inputs_2dt(fit, teams, "basketball", n_seasons = 3L)
  expect_setequal(names(bb$team), c(
    "team", ".draw", "cur_offense", "cur_defense",
    "home_advantage_off", "home_advantage_def"
  ))
  expect_equal(nrow(bb$team), 60L)
  expect_setequal(names(bb$scalar), c(
    ".draw", "mean_goals_fit", "delta_mean_goals", "sigma_mean_goals", "nu",
    "sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho"
  ))
  expect_identical(bb$scalar$.draw, sort(bb$scalar$.draw))

  # cur_offense is the strength WITHOUT home advantage, i.e. offense[N_rounds].
  raw <- posterior::as_draws_df(fit$draws("cur_offense_away"))
  b <- bb$team[bb$team$team == "B", ]
  expect_equal(b$cur_offense[order(b$.draw)], raw[["cur_offense_away[2]"]])
  # The level is the LAST fitted season's.
  lv <- posterior::as_draws_df(fit$draws("mean_goals"))
  expect_equal(bb$scalar$mean_goals_fit, lv[["mean_goals[3]"]])

  hb <- .extract_sim_inputs_2dt(fit, teams, "handball", n_seasons = 3L)
  expect_true("sigma_team" %in% names(hb$team))
  expect_setequal(
    setdiff(names(hb$scalar), c(
      ".draw", "mean_goals_fit", "delta_mean_goals", "sigma_mean_goals", "nu"
    )),
    c("rho", "mean_sigma_team", "scale_sigma_team")
  )
})

test_that(".extract_sim_inputs_2dt drops team indices the team list does not cover", {
  fit <- stub_fit(stub_2dt_draws(c("A", "B", "C"), n_pred = 1L, n_draws = 5L))
  out <- .extract_sim_inputs_2dt(
    fit, tibble::tibble(team = c("A", "B")), "basketball",
    n_seasons = 2L
  )
  expect_setequal(unique(out$team$team), c("A", "B"))
})

test_that("a season the fit has not seen is stepped forward on the model's trend (F12)", {
  sc <- tibble::tibble(
    .draw = 1:3, mean_goals_fit = 80, delta_mean_goals = 2,
    sigma_mean_goals = 1.5, z_level = c(-1, 0, 1)
  )
  expect_equal(.season_level_2dt(sc, 0L)$mean_goals, c(80, 80, 80))
  expect_equal(.season_level_2dt(sc, 1L)$mean_goals, c(80.5, 82, 83.5))
  expect_equal(.season_level_2dt(sc, 2L)$mean_goals, 84 + sqrt(2) * 1.5 * c(-1, 0, 1))
  expect_error(.season_level_2dt(sc, -1L))
})

.rated_inputs <- function(n_draws = 4000L, handball = FALSE) {
  teams <- paste0("T", 1:5)
  team <- tidyr::expand_grid(team = teams, .draw = seq_len(n_draws))
  i <- match(team$team, teams)
  team$cur_offense <- c(4, 2, 0, -2, -4)[i]
  team$cur_defense <- c(2, 1, 0, -1, -2)[i]
  team$home_advantage_off <- c(1, 2, 3, 4, 5)[i]
  team$home_advantage_def <- 1
  if (handball) team$sigma_team <- 5
  scalar <- tibble::tibble(
    .draw = seq_len(n_draws), mean_sigma_team = log(4), scale_sigma_team = 0.2
  )
  list(team = team, scalar = scalar)
}

test_that("a team without history is centred on the division's bottom two (§7)", {
  set.seed(5)
  out <- .add_new_team_priors_2dt(.rated_inputs(), c(paste0("T", 1:5), "NEW"))
  new <- out$team[out$team$team == "NEW", ]
  expect_equal(nrow(new), 4000L)
  # Bottom two by offence + defence are T5 and T4: offence -3, defence -1.5.
  expect_lt(abs(mean(new$cur_offense) + 3), 0.25)
  expect_lt(abs(mean(new$cur_defense) + 1.5), 0.12)
  # Spread: 1.5 x the rated division's between-team SD.
  expect_lt(abs(stats::sd(new$cur_offense) / (1.5 * stats::sd(c(4, 2, 0, -2, -4))) - 1), 0.05)
  expect_equal(unique(new$home_advantage_off), 3)
  expect_equal(unique(new$home_advantage_def), 1)
  expect_false("sigma_team" %in% names(out$team))
})

test_that("handball newcomers draw sigma_team from the fitted hierarchy", {
  set.seed(6)
  # A division with one rated team borrows the whole league's bottom two.
  out <- .add_new_team_priors_2dt(.rated_inputs(handball = TRUE), c("T1", "NEW"))
  new <- out$team[out$team$team == "NEW", ]
  expect_lt(abs(mean(log(new$sigma_team)) - log(4)), 0.02)
  expect_lt(abs(stats::sd(log(new$sigma_team)) - 0.2), 0.01)
  expect_lt(abs(mean(new$cur_offense) + 3), 0.25)
})

test_that("a division whose teams are all rated is returned unchanged", {
  si <- .rated_inputs(10L)
  expect_identical(.add_new_team_priors_2dt(si, paste0("T", 1:5)), si)
})

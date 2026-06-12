# R/simulate-league-season.R — synthetic-data validation suite.
# Exercises the full-remaining-season league simulator against mock
# sim_inputs (no fit needed). The simulator reproduces the model's
# frozen-strength bivariate-Poisson match prediction (Stan GQ lines
# 297-307) over ALL remaining fixtures, then ranks each posterior draw's
# final table by points -> goal difference -> goals for.

# Per-team constant-strength sim_inputs (same value every draw) so that
# behaviour is dominated by the strengths under test rather than posterior
# spread. `strengths` is a named list of 4-vectors keyed by team:
# c(off, def, ha_off, ha_def).
.league_inputs <- function(strengths, n_draws,
                           mean_log_goals = log(1.5),
                           alpha_mu3 = -3, beta_mu3 = 0) {
  teams <- names(strengths)
  team_inputs <- tidyr::expand_grid(team = teams, .draw = seq_len(n_draws)) |>
    dplyr::mutate(
      cur_offense        = vapply(.data$team, function(t) strengths[[t]][1], numeric(1)),
      cur_defense        = vapply(.data$team, function(t) strengths[[t]][2], numeric(1)),
      home_advantage_off = vapply(.data$team, function(t) strengths[[t]][3], numeric(1)),
      home_advantage_def = vapply(.data$team, function(t) strengths[[t]][4], numeric(1))
    )
  scalar_inputs <- tibble::tibble(
    .draw                  = seq_len(n_draws),
    mean_log_goals         = mean_log_goals,
    alpha_mu3              = alpha_mu3,
    beta_mu3_strength_diff = beta_mu3
  )
  list(team = team_inputs, scalar = scalar_inputs)
}

.base <- function(...) {
  rows <- list(...)
  dplyr::bind_rows(lapply(rows, function(r) {
    tibble::tibble(
      team = r[[1]], base_points = as.integer(r[[2]]),
      base_gd = as.integer(r[[3]]), base_gf = as.integer(r[[4]])
    )
  }))
}

.no_fixtures <- function() {
  tibble::tibble(home_team = character(), away_team = character())
}

# ---- Ranking of the realised base table (no games left) ---------------------

test_that("with no remaining fixtures, placement is deterministic from points", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0), C = c(0, 0, 0, 0)), 50L)
  base <- .base(list("A", 10, 5, 20), list("B", 8, 3, 18), list("C", 6, 1, 12))

  out <- simulate_league_season(si$team, si$scalar, .no_fixtures(), base)
  fp <- out$final_positions

  expect_equal(fp$probability[fp$team == "A" & fp$placement == 1], 1)
  expect_equal(fp$probability[fp$team == "B" & fp$placement == 2], 1)
  expect_equal(fp$probability[fp$team == "C" & fp$placement == 3], 1)
})

test_that("goal difference breaks ties on equal points", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0)), 30L)
  base <- .base(list("A", 10, 2, 15), list("B", 10, 9, 25)) # equal points, B better GD

  out <- simulate_league_season(si$team, si$scalar, .no_fixtures(), base)
  fp <- out$final_positions
  expect_equal(fp$probability[fp$team == "B" & fp$placement == 1], 1)
  expect_equal(fp$probability[fp$team == "A" & fp$placement == 2], 1)
})

test_that("goals for breaks ties on equal points and goal difference", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0)), 30L)
  base <- .base(list("A", 10, 5, 30), list("B", 10, 5, 18)) # equal pts+GD, A more GF

  out <- simulate_league_season(si$team, si$scalar, .no_fixtures(), base)
  fp <- out$final_positions
  expect_equal(fp$probability[fp$team == "A" & fp$placement == 1], 1)
})

# ---- Probability invariants -------------------------------------------------

test_that("each team's placement probabilities sum to 1", {
  si <- .league_inputs(list(
    A = c(0.5, 0.3, 0.2, 0.1), B = c(0, 0, 0.2, 0.1),
    C = c(-0.4, -0.2, 0.2, 0.1)
  ), 200L)
  base <- .base(list("A", 6, 2, 9), list("B", 4, 0, 7), list("C", 3, -2, 5))
  fixtures <- tibble::tibble(
    home_team = c("A", "B", "C", "A", "B", "C"),
    away_team = c("B", "C", "A", "C", "A", "B")
  )
  out <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 1L)
  fp <- out$final_positions

  sums <- tapply(fp$probability, fp$team, sum)
  expect_true(all(abs(sums - 1) < 1e-9))
})

test_that("exactly one team occupies each placement in expectation (column sums to 1)", {
  si <- .league_inputs(list(
    A = c(0.5, 0.3, 0.2, 0.1), B = c(0, 0, 0.2, 0.1),
    C = c(-0.4, -0.2, 0.2, 0.1)
  ), 200L)
  base <- .base(list("A", 6, 2, 9), list("B", 4, 0, 7), list("C", 3, -2, 5))
  fixtures <- tibble::tibble(home_team = c("A", "B", "C"), away_team = c("B", "C", "A"))
  out <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 2L)
  fp <- out$final_positions

  col_sums <- tapply(fp$probability, fp$placement, sum)
  expect_true(all(abs(col_sums - 1) < 1e-9))
})

# ---- Strength behaviour -----------------------------------------------------

test_that("a vastly stronger team almost always finishes first", {
  # A starts level on points but is far stronger; over a full mini-season it
  # should win nearly every draw.
  si <- .league_inputs(list(A = c(1.5, 1.5, 0, 0), B = c(0, 0, 0, 0), C = c(0, 0, 0, 0)), 400L)
  base <- .base(list("A", 5, 0, 8), list("B", 5, 0, 8), list("C", 5, 0, 8))
  # double round robin among 3 teams = 6 fixtures
  fixtures <- tibble::tibble(
    home_team = c("A", "A", "B", "B", "C", "C"),
    away_team = c("B", "C", "A", "C", "A", "B")
  )
  out <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 3L)
  fp <- out$final_positions
  p_a_first <- fp$probability[fp$team == "A" & fp$placement == 1]
  expect_gt(p_a_first, 0.9)
})

test_that("home advantage tilts an otherwise even title race", {
  # Two equal teams, A hosts B twice and B hosts A twice -> symmetric, so the
  # title is roughly 50/50; verify neither is pinned at 0/1 (the bug symptom).
  si <- .league_inputs(list(A = c(0, 0, 0.4, 0.2), B = c(0, 0, 0.4, 0.2)), 600L)
  base <- .base(list("A", 4, 0, 6), list("B", 4, 0, 6))
  fixtures <- tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  out <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 4L)
  fp <- out$final_positions
  p_a_first <- fp$probability[fp$team == "A" & fp$placement == 1]
  expect_gt(p_a_first, 0.25)
  expect_lt(p_a_first, 0.75)
})

# ---- Reproducibility + points distribution ----------------------------------

test_that("a fixed seed yields identical output", {
  si <- .league_inputs(list(A = c(0.3, 0.1, 0.2, 0.1), B = c(0, 0, 0.2, 0.1)), 100L)
  base <- .base(list("A", 5, 1, 8), list("B", 4, 0, 7))
  fixtures <- tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  o1 <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 42L)
  o2 <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 42L)
  expect_equal(o1$final_positions, o2$final_positions)
  expect_equal(o1$points_distribution, o2$points_distribution)
})

test_that("points distribution spans base..base+3*games and sums to 1", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0)), 300L)
  base <- .base(list("A", 7, 2, 10), list("B", 5, -2, 8))
  # A plays 2 remaining games -> 7..13 points possible
  fixtures <- tibble::tibble(home_team = c("A", "B"), away_team = c("B", "A"))
  out <- simulate_league_season(si$team, si$scalar, fixtures, base, seed = 5L)
  pd <- out$points_distribution
  a <- pd[pd$team == "A", ]
  expect_gte(min(a$points), 7L)
  expect_lte(max(a$points), 13L)
  expect_equal(sum(a$probability), 1)
})

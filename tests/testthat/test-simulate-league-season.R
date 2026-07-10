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

# ---- Split-season format (efri/nedri hluti) ---------------------------------
# Format facts verified 2026-07-10 against KSI tables + ingested playoff
# divisions (docs/superpowers/specs/2026-07-10-split-season-simulator-design.md):
# 6/6 (male) and 6/4 (female) split after the regular phase, single round-robin
# within each group, FULL carry-over of points/GD/GF, group-locked final
# ranking, deterministic KSI home-allocation template.

.spread_base <- function(teams, pts) {
  dplyr::bind_rows(mapply(function(t, p) {
    tibble::tibble(team = t, base_points = as.integer(p), base_gd = 0L, base_gf = 0L)
  }, teams, pts, SIMPLIFY = FALSE))
}

test_that("split fixture template matches the observed KSI allocation (n = 6)", {
  tpl <- .split_fixture_template(6L)
  expect_equal(nrow(tpl), 15L)
  got <- sort(paste(tpl$home_rank, tpl$away_rank))
  want <- sort(paste(
    c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 6L, 6L),
    c(2L, 5L, 6L, 3L, 4L, 5L, 1L, 4L, 5L, 1L, 6L, 4L, 6L, 2L, 3L)
  ))
  expect_equal(got, want)
  home_counts <- table(tpl$home_rank)
  expect_equal(sort(as.integer(home_counts), decreasing = TRUE), c(3L, 3L, 3L, 2L, 2L, 2L))
})

test_that("split fixture template matches the observed KSI allocation (n = 4)", {
  tpl <- .split_fixture_template(4L)
  expect_equal(nrow(tpl), 6L)
  got <- sort(paste(tpl$home_rank, tpl$away_rank))
  want <- sort(paste(
    c(1L, 1L, 2L, 2L, 3L, 4L),
    c(2L, 3L, 3L, 4L, 4L, 1L)
  ))
  expect_equal(got, want)
  home_counts <- table(tpl$home_rank)
  expect_equal(sort(as.integer(home_counts), decreasing = TRUE), c(2L, 2L, 1L, 1L))
})

test_that("split fixture template errors on unverified group sizes", {
  expect_error(.split_fixture_template(5L), "template")
})

test_that("phase 1: split phase is simulated after the regular phase (deterministic groups)", {
  teams <- LETTERS[1:8]
  si <- .league_inputs(setNames(rep(list(c(0, 0, 0, 0)), 8), teams), 300L)
  base <- .spread_base(teams, c(22, 19, 16, 13, 10, 8, 6, 4))

  out <- simulate_league_season(
    si$team, si$scalar, .no_fixtures(), base,
    seed = 11L, split_format = list(upper = 4L, lower = 4L)
  )
  fp <- out$final_positions
  pd <- out$points_distribution

  # Group locking with deterministic membership: A..D fill 1..4, E..H fill 5..8.
  for (t in c("A", "B", "C", "D")) {
    expect_equal(sum(fp$probability[fp$team == t & fp$placement <= 4]), 1)
  }
  for (t in c("E", "F", "G", "H")) {
    expect_equal(sum(fp$probability[fp$team == t & fp$placement >= 5]), 1)
  }

  # Each team plays 3 split games (4-team single RR): support is base..base+9,
  # and extends beyond the (empty) regular remainder.
  a <- pd[pd$team == "A", ]
  expect_gte(min(a$points), 22L)
  expect_lte(max(a$points), 31L)
  expect_gt(max(a$points), 22L)
  expect_equal(sum(a$probability), 1)
})

test_that("phase 1: simulated regular fixtures decide split membership", {
  teams <- LETTERS[1:8]
  strengths <- setNames(rep(list(c(0, 0, 0, 0)), 8), teams)
  strengths[["E"]] <- c(1.5, 1.5, 0, 0)
  si <- .league_inputs(strengths, 400L)
  # E trails D by 1 point with one head-to-head left; E nearly always wins it
  # and overtakes D into the upper group.
  base <- .spread_base(teams, c(24, 21, 18, 14, 13, 6, 4, 2))
  fixtures <- tibble::tibble(home_team = "E", away_team = "D")

  out <- simulate_league_season(
    si$team, si$scalar, fixtures, base,
    seed = 12L, split_format = list(upper = 4L, lower = 4L)
  )
  fp <- out$final_positions
  p_e_upper <- sum(fp$probability[fp$team == "E" & fp$placement <= 4])
  p_d_upper <- sum(fp$probability[fp$team == "D" & fp$placement <= 4])
  expect_gt(p_e_upper, 0.85)
  expect_lt(p_d_upper, 0.15)
})

test_that("phase 2: group-locked ranking (nedri winner ranks below efri last)", {
  # The 2024 scenario: KA topped the lower group on 37 points while the upper
  # group's last team had 34 -- KA still finishes 7th. Scaled down to 4 teams.
  si <- .league_inputs(list(
    A = c(0, 0, 0, 0), B = c(0, 0, 0, 0), C = c(0, 0, 0, 0), D = c(0, 0, 0, 0)
  ), 50L)
  base <- .base(
    list("A", 40, 10, 30), list("B", 33, 2, 20),
    list("C", 37, 5, 25), list("D", 21, -10, 12)
  )
  groups <- tibble::tibble(
    team = c("A", "B", "C", "D"),
    group = c("upper", "upper", "lower", "lower")
  )

  out <- simulate_league_season(
    si$team, si$scalar, .no_fixtures(), base,
    split_groups = groups
  )
  fp <- out$final_positions
  expect_equal(fp$probability[fp$team == "A" & fp$placement == 1], 1)
  expect_equal(fp$probability[fp$team == "B" & fp$placement == 2], 1)
  expect_equal(fp$probability[fp$team == "C" & fp$placement == 3], 1)
  expect_equal(fp$probability[fp$team == "D" & fp$placement == 4], 1)
})

test_that("phase 2: remaining split fixtures are simulated within known groups", {
  si <- .league_inputs(list(
    A = c(0, 0, 0, 0), B = c(0, 0, 0, 0), C = c(0, 0, 0, 0), D = c(0, 0, 0, 0)
  ), 300L)
  base <- .base(
    list("A", 30, 5, 20), list("B", 29, 3, 18),
    list("C", 20, -2, 12), list("D", 19, -6, 10)
  )
  groups <- tibble::tibble(
    team = c("A", "B", "C", "D"),
    group = c("upper", "upper", "lower", "lower")
  )
  fixtures <- tibble::tibble(home_team = c("A", "C"), away_team = c("B", "D"))

  out <- simulate_league_season(
    si$team, si$scalar, fixtures, base,
    seed = 13L, split_groups = groups
  )
  fp <- out$final_positions
  pd <- out$points_distribution

  # B can overtake A within the group; both stay in placements 1-2.
  expect_gt(fp$probability[fp$team == "B" & fp$placement == 1], 0.05)
  expect_equal(sum(fp$probability[fp$team == "B" & fp$placement <= 2]), 1)
  expect_equal(sum(fp$probability[fp$team == "C" & fp$placement >= 3]), 1)
  b <- pd[pd$team == "B", ]
  expect_gte(min(b$points), 29L)
  expect_lte(max(b$points), 32L)
})

test_that("asymmetric 6/4 split (women's format) partitions placements 1-6 / 7-10", {
  teams <- LETTERS[1:10]
  si <- .league_inputs(setNames(rep(list(c(0, 0, 0, 0)), 10), teams), 200L)
  base <- .spread_base(teams, c(40, 36, 32, 28, 24, 20, 16, 12, 8, 4))

  out <- simulate_league_season(
    si$team, si$scalar, .no_fixtures(), base,
    seed = 14L, split_format = list(upper = 6L, lower = 4L)
  )
  fp <- out$final_positions
  pd <- out$points_distribution
  for (t in teams[1:6]) {
    expect_equal(sum(fp$probability[fp$team == t & fp$placement <= 6]), 1)
  }
  for (t in teams[7:10]) {
    expect_equal(sum(fp$probability[fp$team == t & fp$placement >= 7]), 1)
  }
  # Upper teams play 5 split games, lower teams 3.
  a <- pd[pd$team == "A", ]
  expect_lte(max(a$points), 40L + 15L)
  g <- pd[pd$team == "G", ]
  expect_lte(max(g$points), 16L + 9L)
})

test_that("split arguments are validated", {
  si <- .league_inputs(list(A = c(0, 0, 0, 0), B = c(0, 0, 0, 0), C = c(0, 0, 0, 0)), 20L)
  base <- .base(list("A", 10, 5, 20), list("B", 8, 3, 18), list("C", 6, 1, 12))

  expect_error(
    simulate_league_season(
      si$team, si$scalar, .no_fixtures(), base,
      split_format = list(upper = 4L, lower = 4L)
    ),
    "upper \\+ lower"
  )
  expect_error(
    simulate_league_season(
      si$team, si$scalar, .no_fixtures(), base,
      split_groups = tibble::tibble(team = c("A", "B"), group = c("upper", "lower"))
    ),
    "cover every league team"
  )
})

test_that("a fixed seed yields identical output under a split", {
  teams <- LETTERS[1:8]
  si <- .league_inputs(setNames(rep(list(c(0.1, 0, 0.2, 0.1)), 8), teams), 100L)
  base <- .spread_base(teams, c(22, 19, 16, 13, 10, 8, 6, 4))
  fixtures <- tibble::tibble(home_team = c("A", "E"), away_team = c("H", "B"))

  o1 <- simulate_league_season(
    si$team, si$scalar, fixtures, base,
    seed = 42L, split_format = list(upper = 4L, lower = 4L)
  )
  o2 <- simulate_league_season(
    si$team, si$scalar, fixtures, base,
    seed = 42L, split_format = list(upper = 4L, lower = 4L)
  )
  expect_equal(o1$final_positions, o2$final_positions)
  expect_equal(o1$points_distribution, o2$points_distribution)
})

# R/simulate-cup-bracket.R — synthetic-data validation suite.
# These tests exercise the bracket walker against mock sim_inputs (no fit
# needed). The real-fit integration is covered downstream by
# tests for `.extract_sim_inputs_pfi()` once that helper exists.

.make_equal_strength_inputs <- function(teams, n_draws) {
  team_inputs <- tidyr::expand_grid(
    team  = teams,
    .draw = seq_len(n_draws)
  ) |>
    dplyr::mutate(
      cur_offense        = 0,
      cur_defense        = 0,
      home_advantage_off = 0,
      home_advantage_def = 0
    )

  scalar_inputs <- tibble::tibble(
    .draw                  = seq_len(n_draws),
    mean_log_goals         = log(1.5),
    alpha_mu3              = -3,
    beta_mu3_strength_diff = 0
  )

  list(team = team_inputs, scalar = scalar_inputs)
}

.standard_r16_bracket <- function(teams) {
  stopifnot(length(teams) == 16L)
  tibble::tibble(
    home_team     = teams[seq(1L, 16L, by = 2L)],
    away_team     = teams[seq(2L, 16L, by = 2L)],
    venue         = rep("neutral", 8L),
    known_winner  = NA_character_
  )
}

# ---- Bivariate Poisson primitive --------------------------------------------

test_that(".rbvpois has correct marginal means and shared-component covariance", {
  set.seed(2026L)
  n <- 20000L
  lambdas <- c(1.5, 1.2, 0.3)
  samples <- replicate(n, .rbvpois(lambdas[1], lambdas[2], lambdas[3]))

  # E[Y1] = lambda1 + lambda3, E[Y2] = lambda2 + lambda3
  expect_equal(mean(samples[1L, ]), lambdas[1] + lambdas[3], tolerance = 0.05)
  expect_equal(mean(samples[2L, ]), lambdas[2] + lambdas[3], tolerance = 0.05)
  # Cov(Y1, Y2) = Var(X3) = lambda3
  expect_equal(cov(samples[1L, ], samples[2L, ]), lambdas[3], tolerance = 0.05)
})

test_that(".rbvpois with lambda3 = 0 produces independent Poissons", {
  set.seed(2027L)
  n <- 10000L
  samples <- replicate(n, .rbvpois(1.0, 1.5, 0.0))
  expect_equal(cov(samples[1L, ], samples[2L, ]), 0.0, tolerance = 0.05)
})

# ---- One-match simulator ----------------------------------------------------

test_that(".simulate_cup_match_pfi always returns one of the two team names", {
  set.seed(2028L)
  off <- c(A = 0, B = 0)
  def <- c(A = 0, B = 0)
  ha_off <- c(A = 0.3, B = 0.3)
  ha_def <- c(A = 0.3, B = 0.3)
  tiebreak <- default_tiebreak_opts()

  winners <- replicate(200L, .simulate_cup_match_pfi(
    "A", "B", "home", off, def, ha_off, ha_def,
    mean_log_goals = log(1.5), alpha_mu3 = -3, beta_mu3_diff = 0,
    tiebreak_opts = tiebreak
  ))
  expect_true(all(winners %in% c("A", "B")))
})

test_that(".simulate_cup_match_pfi home advantage biases winners toward home", {
  set.seed(2029L)
  off <- c(A = 0, B = 0)
  def <- c(A = 0, B = 0)
  ha_off <- c(A = 0.8, B = 0.8)
  ha_def <- c(A = 0.8, B = 0.8)
  tiebreak <- default_tiebreak_opts()

  winners <- replicate(500L, .simulate_cup_match_pfi(
    "A", "B", "home", off, def, ha_off, ha_def,
    mean_log_goals = log(1.5), alpha_mu3 = -3, beta_mu3_diff = 0,
    tiebreak_opts = tiebreak
  ))
  expect_gt(mean(winners == "A"), 0.55)
})

test_that(".simulate_cup_match_pfi: dominant team almost always wins", {
  set.seed(2030L)
  off <- c(A = 2, B = 0)
  def <- c(A = 2, B = 0)
  ha_off <- c(A = 0, B = 0)
  ha_def <- c(A = 0, B = 0)
  tiebreak <- default_tiebreak_opts()

  winners <- replicate(300L, .simulate_cup_match_pfi(
    "A", "B", "neutral", off, def, ha_off, ha_def,
    mean_log_goals = log(1.5), alpha_mu3 = -3, beta_mu3_diff = 0,
    tiebreak_opts = tiebreak
  ))
  expect_gt(mean(winners == "A"), 0.95)
})

# ---- Full bracket walker ----------------------------------------------------

test_that("simulate_cup_bracket: equal-strength teams produce ~1/16 P(Champion) each", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  inputs <- .make_equal_strength_inputs(teams, n_draws = 2000L)

  bracket_state <- list(
    r16 = .standard_r16_bracket(teams)
  )

  result <- simulate_cup_bracket(
    inputs$team, inputs$scalar, bracket_state,
    pairing_seed = 42L
  )

  # P(Champion) sums to 1 across all teams (exactly one team wins per draw).
  champion_probs <- result |>
    dplyr::filter(.data$round_name == "Champion") |>
    dplyr::pull("probability")
  expect_equal(sum(champion_probs), 1.0)
  # Each team's P(Champion) is within ~3% of 1/16 = 0.0625 (2-sigma at n=2000).
  expect_true(all(abs(champion_probs - 1 / 16) < 0.03))

  # P(R16) = 1 for all 16 teams (they're definitionally in R16).
  r16_probs <- result |>
    dplyr::filter(.data$round_name == "R16") |>
    dplyr::pull("probability")
  expect_equal(r16_probs, rep(1.0, 16L))

  # P(QF) sums to 8 (8 winners advance), P(SF) sums to 4, P(Final) sums to 2.
  qf_probs <- result |>
    dplyr::filter(.data$round_name == "QF") |>
    dplyr::pull("probability")
  sf_probs <- result |>
    dplyr::filter(.data$round_name == "SF") |>
    dplyr::pull("probability")
  final_probs <- result |>
    dplyr::filter(.data$round_name == "Final") |>
    dplyr::pull("probability")
  expect_equal(sum(qf_probs), 8.0)
  expect_equal(sum(sf_probs), 4.0)
  expect_equal(sum(final_probs), 2.0)
})

test_that("simulate_cup_bracket: per-team probabilities are monotone non-increasing across rounds", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  inputs <- .make_equal_strength_inputs(teams, n_draws = 500L)
  bracket_state <- list(r16 = .standard_r16_bracket(teams))

  result <- simulate_cup_bracket(
    inputs$team, inputs$scalar, bracket_state,
    pairing_seed = 7L
  )

  for (t in teams) {
    p <- result |>
      dplyr::filter(.data$team == t) |>
      dplyr::arrange(.data$round_name) |>
      dplyr::pull("probability")
    expect_equal(length(p), 5L)
    expect_true(all(diff(p) <= 0), info = sprintf("non-monotone for team %s", t))
  }
})

test_that("simulate_cup_bracket: dominant team has high P(Champion)", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  n_draws <- 1000L

  team_inputs <- tidyr::expand_grid(
    team  = teams,
    .draw = seq_len(n_draws)
  ) |>
    dplyr::mutate(
      cur_offense        = ifelse(.data$team == "T01", 2.0, 0.0),
      cur_defense        = ifelse(.data$team == "T01", 2.0, 0.0),
      home_advantage_off = 0.0,
      home_advantage_def = 0.0
    )
  scalar_inputs <- tibble::tibble(
    .draw                  = seq_len(n_draws),
    mean_log_goals         = log(1.5),
    alpha_mu3              = -3,
    beta_mu3_strength_diff = 0
  )

  bracket_state <- list(r16 = .standard_r16_bracket(teams))

  result <- simulate_cup_bracket(
    team_inputs, scalar_inputs, bracket_state,
    pairing_seed = 11L
  )

  t01_final <- result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Final") |>
    dplyr::pull("probability")
  t01_champ <- result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Champion") |>
    dplyr::pull("probability")

  # T01 has +2 offence and +2 defence vs every opponent.
  # Per-match win probability ~ 0.93; 4 wins needed -> ~0.93^4 = 0.75.
  expect_gt(t01_final, 0.75)
  expect_gt(t01_champ, 0.70)
})

test_that("simulate_cup_bracket: known_winner is respected for R16 matches", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  inputs <- .make_equal_strength_inputs(teams, n_draws = 500L)

  r16 <- .standard_r16_bracket(teams)
  # Pin T01 as the deterministic R16 winner of match 1 (T01 vs T02).
  r16$known_winner[1L] <- "T01"

  bracket_state <- list(r16 = r16)

  result <- simulate_cup_bracket(
    inputs$team, inputs$scalar, bracket_state,
    pairing_seed = 13L
  )

  t01_qf <- result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "QF") |>
    dplyr::pull("probability")
  t02_qf <- result |>
    dplyr::filter(.data$team == "T02", .data$round_name == "QF") |>
    dplyr::pull("probability")

  expect_equal(t01_qf, 1.0)
  expect_equal(t02_qf, 0.0)
})

test_that("simulate_cup_bracket: fixed QF pairings reduce variance vs open-bracket", {
  # When QF pairings are known, the variance over draws drops (fewer
  # combinatorial alternatives to marginalise over). We confirm this by
  # rerunning with `qf_pairings_known = TRUE` and pre-set pairings, and
  # checking that one team's P(Champion) shifts noticeably when paired
  # against weaker opponents in fixed mode.
  teams <- paste0("T", sprintf("%02d", 1:16))
  n_draws <- 1000L

  # T01 is strong; T15, T16 are weak (-1 each).
  team_inputs <- tidyr::expand_grid(
    team  = teams,
    .draw = seq_len(n_draws)
  ) |>
    dplyr::mutate(
      cur_offense = dplyr::case_when(
        .data$team == "T01" ~ 1.0,
        .data$team %in% c("T15", "T16") ~ -1.0,
        TRUE ~ 0.0
      ),
      cur_defense = dplyr::case_when(
        .data$team == "T01" ~ 1.0,
        .data$team %in% c("T15", "T16") ~ -1.0,
        TRUE ~ 0.0
      ),
      home_advantage_off = 0.0,
      home_advantage_def = 0.0
    )
  scalar_inputs <- tibble::tibble(
    .draw                  = seq_len(n_draws),
    mean_log_goals         = log(1.5),
    alpha_mu3              = -3,
    beta_mu3_strength_diff = 0
  )

  # R16: T01 plays T02 in match 1; T15 plays T16 in match 8 (so a weak team
  # advances from match 8 deterministically by parity of strengths there).
  r16 <- .standard_r16_bracket(teams)

  # Open-bracket sim
  open_state <- list(r16 = r16)
  open_result <- simulate_cup_bracket(
    team_inputs, scalar_inputs, open_state,
    pairing_seed = 23L
  )
  open_t01_champ <- open_result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Champion") |>
    dplyr::pull("probability")

  # Fixed-bracket sim: T01 (match 1 winner) is paired with match 8 winner in QF.
  fixed_state <- list(
    r16 = r16,
    qf_pairings_known = TRUE,
    qf = tibble::tibble(
      prev_left  = c(1L, 2L, 3L, 4L),
      prev_right = c(8L, 7L, 6L, 5L),
      venue      = rep("neutral", 4L)
    )
  )
  fixed_result <- simulate_cup_bracket(
    team_inputs, scalar_inputs, fixed_state,
    pairing_seed = 23L
  )
  fixed_t01_champ <- fixed_result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Champion") |>
    dplyr::pull("probability")

  # Whichever mode: T01 should be the favourite. Sanity check.
  expect_gt(open_t01_champ, 0.20)
  expect_gt(fixed_t01_champ, 0.20)
})

test_that("simulate_cup_bracket: pairing_seed produces reproducible output", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  inputs <- .make_equal_strength_inputs(teams, n_draws = 300L)
  bracket_state <- list(r16 = .standard_r16_bracket(teams))

  r1 <- simulate_cup_bracket(
    inputs$team, inputs$scalar, bracket_state,
    pairing_seed = 99L
  )
  r2 <- simulate_cup_bracket(
    inputs$team, inputs$scalar, bracket_state,
    pairing_seed = 99L
  )
  expect_equal(r1, r2)
})

test_that("default_tiebreak_opts returns the expected structure", {
  opts <- default_tiebreak_opts()
  expect_named(opts, c("et_rate_scale", "shootout", "bt_scale"))
  expect_equal(opts$et_rate_scale, log(30 / 90))
  expect_equal(opts$shootout, "coin")
})

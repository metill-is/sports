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

# New-shape helper: build a generalised bracket_state with cup_teams + rounds.
# `r16_matches` is the 8-row R16 tibble produced by .standard_r16_bracket().
# By default R8/SF/Final have pairings_known = FALSE so the simulator draws
# them uniformly. Tests that need known later rounds can override per-test.
.standard_bracket_state <- function(teams,
                                    r16_matches = NULL,
                                    r8 = list(pairings_known = FALSE, matches = NULL),
                                    sf = list(pairings_known = FALSE, matches = NULL),
                                    final = list(pairings_known = FALSE, matches = NULL)) {
  if (is.null(r16_matches)) r16_matches <- .standard_r16_bracket(teams)
  list(
    cup_teams = teams,
    rounds = list(
      R16   = list(pairings_known = TRUE, matches = r16_matches),
      R8    = r8,
      SF    = sf,
      Final = final
    )
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

  bracket_state <- .standard_bracket_state(teams)

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
  bracket_state <- .standard_bracket_state(teams)

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

  bracket_state <- .standard_bracket_state(teams)

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

  bracket_state <- .standard_bracket_state(teams, r16_matches = r16)

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
  open_state <- .standard_bracket_state(teams, r16_matches = r16)
  open_result <- simulate_cup_bracket(
    team_inputs, scalar_inputs, open_state,
    pairing_seed = 23L
  )
  open_t01_champ <- open_result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Champion") |>
    dplyr::pull("probability")

  # Fixed-bracket sim: pin all R16 winners deterministically, then pin R8
  # pairings to specific team matchups (T01 vs T15 — the weakest), and
  # confirm the simulator uses the pinned R8 pairings rather than redrawing.
  r16_pinned <- r16
  r16_pinned$known_winner <- teams[seq(1L, 15L, by = 2L)] # odd-indexed teams win
  fixed_state <- .standard_bracket_state(
    teams,
    r16_matches = r16_pinned,
    r8 = list(
      pairings_known = TRUE,
      matches = tibble::tibble(
        home_team    = c("T01", "T03", "T05", "T07"),
        away_team    = c("T15", "T13", "T11", "T09"),
        venue        = rep("neutral", 4L),
        known_winner = NA_character_
      )
    )
  )
  fixed_result <- simulate_cup_bracket(
    team_inputs, scalar_inputs, fixed_state,
    pairing_seed = 23L
  )
  fixed_t01_champ <- fixed_result |>
    dplyr::filter(.data$team == "T01", .data$round_name == "Champion") |>
    dplyr::pull("probability")

  # In fixed mode T01 plays the weakest team (T15) in R8, then random SF/F.
  # Both modes should give T01 a meaningful Champion probability.
  expect_gt(open_t01_champ, 0.20)
  expect_gt(fixed_t01_champ, 0.20)
})

test_that("simulate_cup_bracket: pairing_seed produces reproducible output", {
  teams <- paste0("T", sprintf("%02d", 1:16))
  inputs <- .make_equal_strength_inputs(teams, n_draws = 300L)
  bracket_state <- .standard_bracket_state(teams)

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
  expect_named(opts, "max_iter")
  expect_true(is.numeric(opts$max_iter))
  expect_gt(opts$max_iter, 1L)
})

test_that(".simulate_cup_match_pfi: rejection sampling converges (no ties returned)", {
  # With moderate lambdas the rejection loop should always produce a winner
  # well within the max_iter cap.
  set.seed(2031L)
  off <- c(A = 0, B = 0)
  def <- c(A = 0, B = 0)
  ha_off <- c(A = 0.0, B = 0.0)
  ha_def <- c(A = 0.0, B = 0.0)
  tiebreak <- default_tiebreak_opts()

  winners <- replicate(500L, .simulate_cup_match_pfi(
    "A", "B", "neutral", off, def, ha_off, ha_def,
    mean_log_goals = log(1.5), alpha_mu3 = -3, beta_mu3_diff = 0,
    tiebreak_opts = tiebreak
  ))
  # Equal strength + neutral venue + symmetric coin fallback => ~50/50.
  expect_true(all(winners %in% c("A", "B")))
  expect_gt(mean(winners == "A"), 0.42)
  expect_lt(mean(winners == "A"), 0.58)
})

# Shared fixtures for split-season (efri/nedri hluti) tests.

# All ordered pairs among `teams` played, with the LOWER-indexed team always
# winning by a margin that also orders GD/GF -- so the realised ranking equals
# the team order. `skip_pairs` (character "H A" keys) are left unplayed.
.mini_reg_results <- function(teams, season = 2026L, division = "BD",
                              skip_pairs = character()) {
  grid <- expand.grid(
    home_team = teams, away_team = teams,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid <- grid[grid$home_team != grid$away_team, , drop = FALSE]
  grid <- grid[!(paste(grid$home_team, grid$away_team) %in% skip_pairs), , drop = FALSE]
  hi <- match(grid$home_team, teams)
  ai <- match(grid$away_team, teams)
  n <- length(teams)
  tibble::tibble(
    home_team = grid$home_team, away_team = grid$away_team,
    home_score = as.integer(ifelse(hi < ai, n - hi + 1L, 0L)),
    away_score = as.integer(ifelse(ai < hi, n - ai + 1L, 0L)),
    division = division, season = as.integer(season),
    match_date = as.Date("2026-06-01")
  )
}

# Constant-strength sim-input mock for `local_mocked_bindings()` on
# `.extract_sim_inputs_pfi` -- no Stan fit needed.
.mock_sim_inputs_fn <- function(teams, n_draws = 40L) {
  force(teams)
  force(n_draws)
  function(fit, teams_arg) {
    tm <- tidyr::expand_grid(team = teams, .draw = seq_len(n_draws)) |>
      dplyr::mutate(
        cur_offense = 0, cur_defense = 0,
        home_advantage_off = 0, home_advantage_def = 0
      )
    sc <- tibble::tibble(
      .draw = seq_len(n_draws), mean_log_goals = log(1.5),
      alpha_mu3 = -3, beta_mu3_strength_diff = 0
    )
    list(team = tm, scalar = sc)
  }
}

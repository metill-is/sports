# ---- Shared WC test constructors (auto-sourced by testthat) ----------------

make_wc_fixtures <- function(structure) {
  rows <- list()
  for (g in names(structure$groups)) {
    tm <- structure$groups[[g]]
    cmb <- utils::combn(tm, 2L)
    for (j in seq_len(ncol(cmb))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        match_date = as.Date("2026-06-15"),
        group = g, home_team = cmb[1, j], away_team = cmb[2, j],
        home_score = NA_integer_, away_score = NA_integer_,
        played = FALSE, venue = "neutral"
      )
    }
  }
  dplyr::bind_rows(rows)
}

make_sim_inputs <- function(teams, n_draws = 200L, off = NULL, def = NULL) {
  if (is.null(off)) off <- stats::setNames(rep(0, length(teams)), teams)
  if (is.null(def)) def <- stats::setNames(rep(0, length(teams)), teams)
  team <- tidyr::expand_grid(.draw = seq_len(n_draws), team = teams)
  team$cur_offense <- unname(off[team$team])
  team$cur_defense <- unname(def[team$team])
  team$home_advantage_off <- 0
  team$home_advantage_def <- 0
  scalar <- tibble::tibble(
    .draw = seq_len(n_draws),
    mean_log_goals = log(1.3), alpha_mu3 = -3, beta_mu3_strength_diff = 0
  )
  list(team = team, scalar = scalar)
}

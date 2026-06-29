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

# Synthetic schedule/results constructors for the knockout-fixtures helper.
# A group pairing = both teams in the same group (one of the 72); a knockout
# fixture = any cross-group pairing. The exact identities are irrelevant to the
# helper under test — only the within-group vs cross-group split matters.
make_group_results <- function(structure) {
  rows <- list()
  for (g in names(structure$groups)) {
    cmb <- utils::combn(structure$groups[[g]], 2L)
    for (j in seq_len(ncol(cmb))) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        match_date = as.Date("2026-06-20"),
        home_team = cmb[1, j], away_team = cmb[2, j]
      )
    }
  }
  dplyr::bind_rows(rows)
}

make_r32_schedule <- function(structure) {
  # 16 cross-group pairs. Teams are grouped in blocks of 4 (A=1:4, B=5:8, ...), so
  # pairing team i with team i+24 always crosses groups (groups 1-6 vs 7-12).
  teams <- unlist(structure$groups, use.names = FALSE)
  tibble::tibble(
    match_date = as.Date("2026-06-28") + rep(0:7, each = 2L),
    home_team = teams[1:16],
    away_team = teams[25:40]
  )
}

# A fully-played, deterministic group stage: in every group team 1 beats all,
# team 2 beats 3 & 4, team 3 beats 4 (9/6/3/0 pts, distinct GD), so each group's
# top two are certain (one-hot occupancy) for the R32 slots that feed off them.
# (Third-placed teams tie across groups; the best-thirds pick is left to the
# allocator — irrelevant to the 2A/2B-fed R32 matches used in tests.)
make_group_results_scored <- function(structure) {
  rows <- list()
  for (g in names(structure$groups)) {
    tm <- structure$groups[[g]]
    games <- list(
      c(tm[1], tm[2], 1L, 0L), c(tm[1], tm[3], 2L, 0L), c(tm[1], tm[4], 3L, 0L),
      c(tm[2], tm[3], 1L, 0L), c(tm[2], tm[4], 2L, 0L),
      c(tm[3], tm[4], 1L, 0L)
    )
    for (gm in games) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        match_date = as.Date("2026-06-20"), group = g,
        home_team = gm[[1]], away_team = gm[[2]],
        home_score = as.integer(gm[[3]]), away_score = as.integer(gm[[4]]),
        played = TRUE, venue = "neutral"
      )
    }
  }
  dplyr::bind_rows(rows)
}

# One-hot R32 slot occupancy (the certain, post-group state) from a given
# assignment: a_names[i] / b_names[i] occupy slot a / b of the i-th R32 match
# (bracket order, match 73..88). Returns 16 x nt matrices like simulate_world_cup.
make_certain_occ <- function(structure, a_names, b_names) {
  teams <- unlist(structure$groups, use.names = FALSE)
  tidx <- stats::setNames(seq_along(teams), teams)
  nt <- length(teams)
  oa <- matrix(0, 16L, nt)
  ob <- matrix(0, 16L, nt)
  for (i in 1:16) {
    oa[i, tidx[[a_names[i]]]] <- 1
    ob[i, tidx[[b_names[i]]]] <- 1
  }
  list(occ_a = oa, occ_b = ob)
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

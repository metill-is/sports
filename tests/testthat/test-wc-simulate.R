# ---- Local constructors (self-contained) -----------------------------------

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

# ---- Group table -----------------------------------------------------------

test_that(".wc_group_table ranks an unambiguous group correctly", {
  teams <- c("A1", "A2", "A3", "A4")
  # A1 beats all; A2 beats A3,A4; A3 beats A4; A4 loses all.
  m <- tibble::tribble(
    ~home_team, ~away_team, ~home_score, ~away_score,
    "A1", "A2", 2L, 0L,
    "A1", "A3", 2L, 0L,
    "A1", "A4", 2L, 0L,
    "A2", "A3", 1L, 0L,
    "A2", "A4", 1L, 0L,
    "A3", "A4", 1L, 0L
  )
  tbl <- .wc_group_table(teams, m)
  expect_equal(tbl$team, c("A1", "A2", "A3", "A4"))
  expect_equal(tbl$rank, 1:4)
  expect_equal(tbl$pts, c(9, 6, 3, 0))
})

# ---- Thirds allocation -----------------------------------------------------

test_that(".wc_bipartite_thirds returns a valid eligibility-respecting matching", {
  s <- wc_structure()
  elig <- stats::setNames(s$third_slots$eligible, as.character(s$third_slots$match_no))
  combo <- c("A", "B", "C", "D", "E", "F", "G", "H")
  alloc <- .wc_bipartite_thirds(combo, s$third_slots)
  expect_setequal(unlist(alloc), combo)
  for (m in names(alloc)) {
    expect_true(alloc[[m]] %in% elig[[m]])
  }
})

test_that(".wc_allocate_thirds uses the FIFA Annex-C table when present", {
  s <- wc_structure()
  skip_if(is.null(s$third_allocation), "allocation CSV not present")
  combo <- c("A", "B", "C", "D", "E", "F", "G", "H")
  alloc <- .wc_allocate_thirds(combo, s)
  row <- s$third_allocation[s$third_allocation$combo == "ABCDEFGH", ]
  expect_equal(alloc[["74"]], row$m74)
  expect_equal(alloc[["80"]], row$m80)
  expect_equal(alloc[["87"]], row$m87)
})

# ---- Tournament simulation invariants --------------------------------------

test_that("simulate_world_cup covers all 48 teams with coherent probabilities", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 150L)
  fx <- make_wc_fixtures(s)

  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)

  expect_setequal(out$group_probs$team, teams)
  expect_setequal(unique(out$placement_probs$team), teams)

  # Each team's group-finish distribution sums to 1.
  finish_sums <- with(out$group_probs, p_first + p_second + p_third + p_fourth)
  expect_true(all(abs(finish_sums - 1) < 1e-9))

  # Within each group, exactly one team wins per draw -> P(first) sums to 1.
  by_group <- tapply(out$group_probs$p_first, out$group_probs$group, sum)
  expect_true(all(abs(by_group - 1) < 1e-9))

  # Exactly one champion per draw.
  champ <- out$placement_probs[out$placement_probs$round_name == "Champion", ]
  expect_equal(sum(champ$probability), 1, tolerance = 1e-9)

  # 32 teams qualify each draw -> sum of P(reach R32) == 32.
  r32 <- out$placement_probs[out$placement_probs$round_name == "R32", ]
  expect_equal(sum(r32$probability), 32, tolerance = 1e-9)
})

test_that("placement probabilities are cumulative (non-increasing by round)", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 120L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 2L)

  lvl <- c("R32", "R16", "QF", "SF", "Final", "Champion")
  for (tm in unique(out$placement_probs$team)) {
    p <- out$placement_probs[out$placement_probs$team == tm, ]
    p <- p[match(lvl, as.character(p$round_name)), ]
    expect_true(all(diff(p$probability) <= 1e-9),
      info = paste("non-monotone for", tm)
    )
  }
})

test_that("a dominant team wins the tournament far more often", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  off <- stats::setNames(rep(0, length(teams)), teams)
  def <- stats::setNames(rep(0, length(teams)), teams)
  off["Spain"] <- 3
  def["Spain"] <- 1.5
  si <- make_sim_inputs(teams, n_draws = 200L, off = off, def = def)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 3L)

  champ <- out$placement_probs[out$placement_probs$round_name == "Champion", ]
  p_spain <- champ$probability[champ$team == "Spain"]
  expect_gt(p_spain, 0.5)
  # And Spain should be the clear favourite.
  expect_equal(champ$team[which.max(champ$probability)], "Spain")
})

test_that("predictions, projected tables and bracket model are coherent", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 100L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 5L)

  # Upcoming-match predictions: one row per fixture, 1X2 sums to 1.
  expect_equal(nrow(out$predictions), nrow(fx))
  s1x2 <- with(out$predictions, p_home + p_draw + p_away)
  expect_true(all(abs(s1x2 - 1) < 1e-9))
  expect_true(all(out$predictions$eg_home > 0 & out$predictions$eg_away > 0))

  # Projected final tables: total projected points per group in [12, 18].
  by_group_pts <- tapply(out$group_probs$proj_points, out$group_probs$group, sum)
  expect_true(all(by_group_pts >= 12 - 1e-6 & by_group_pts <= 18 + 1e-6))

  # No matches played -> performance all-zero.
  expect_true(all(out$performance$n_played == 0L))

  # Win matrix: 48x48, complementary off-diagonal (no ties in knockout).
  W <- out$bracket_model$W
  expect_equal(dim(W), c(48L, 48L))
  od <- W + t(W)
  diag(od) <- 1
  expect_true(all(abs(od - 1) < 1e-9))

  # R32 occupancy: each of the 32 slots is filled (rows sum to 1).
  expect_true(all(abs(rowSums(out$bracket_model$occ_a) - 1) < 1e-9))
  expect_true(all(abs(rowSums(out$bracket_model$occ_b) - 1) < 1e-9))
})

test_that("wc_forward_bracket: monotone, sums to one, and pins force a winner", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 100L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 6L)
  bm <- out$bracket_model

  base <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams)
  champ <- base$placement[base$placement$round_name == "Champion", ]
  expect_equal(sum(champ$probability), 1, tolerance = 1e-9)
  expect_equal(sum(base$reach$R32), 32, tolerance = 1e-9)

  lvl <- c("R32", "R16", "QF", "SF", "Final", "Champion")
  for (tm in teams[1:6]) {
    p <- base$placement[base$placement$team == tm, ]
    p <- p[match(lvl, as.character(p$round_name)), ]
    expect_true(all(diff(p$probability) <= 1e-9))
  }

  # Pin the final winner -> that team is champion w.p. 1.
  x <- which(teams == "Spain")
  pinned <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams,
    pins = list("104" = x)
  )
  pc <- pinned$placement[pinned$placement$round_name == "Champion", ]
  expect_equal(pc$probability[pc$team == "Spain"], 1, tolerance = 1e-9)
  expect_equal(sum(pc$probability), 1, tolerance = 1e-9)
})

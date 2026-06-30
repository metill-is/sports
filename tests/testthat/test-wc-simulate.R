# Shared constructors (make_wc_fixtures, make_sim_inputs) live in helper-wc.R.

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

# ---- Knockout-result conditioning (Phase 2) --------------------------------

test_that("simulate_world_cup conditions placement on a played knockout result", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  fx <- make_group_results_scored(s) # group stage fully played -> certain top 2
  si <- make_sim_inputs(teams, n_draws = 60L)

  base <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 11L)
  expect_length(base$bracket_model$played, 0L) # inert without knockout results

  # match 73 (first R32) is 2A vs 2B — certain now the group stage is played.
  occ_a <- base$bracket_model$occ_a
  occ_b <- base$bracket_model$occ_b
  ia <- which(occ_a[1, ] >= 0.9995)
  ib <- which(occ_b[1, ] >= 0.9995)
  expect_length(ia, 1L)
  expect_length(ib, 1L)

  kr <- tibble::tibble(
    match_date = as.Date("2026-07-01"),
    home_team = teams[ia], away_team = teams[ib], home_score = 2L, away_score = 1L
  )
  out <- simulate_world_cup(si$team, si$scalar, fx, s,
    pairing_seed = 11L, knockout_results = kr
  )

  r16 <- out$placement_probs[out$placement_probs$round_name == "R16", ]
  expect_equal(r16$probability[r16$team == teams[ia]], 1, tolerance = 1e-9)
  expect_equal(r16$probability[r16$team == teams[ib]], 0, tolerance = 1e-9)

  expect_length(out$bracket_model$played, 1L)
  expect_equal(out$bracket_model$played[[1L]]$winner, ia)
  expect_equal(out$bracket_model$played[[1L]]$loser, ib)
})

# ---- Knockout match predictions --------------------------------------------

test_that(".wc_knockout_round_of maps played-knockout count to the round name", {
  expect_equal(.wc_knockout_round_of(0L), "R32")
  expect_equal(.wc_knockout_round_of(15L), "R32")
  expect_equal(.wc_knockout_round_of(16L), "R16")
  expect_equal(.wc_knockout_round_of(23L), "R16")
  expect_equal(.wc_knockout_round_of(24L), "QF")
  expect_equal(.wc_knockout_round_of(28L), "SF")
  expect_equal(.wc_knockout_round_of(30L), "Final")
})

# make_group_results / make_r32_schedule live in helper-wc.R (shared with publish).

test_that(".wc_knockout_fixtures_from returns cross-group upcoming fixtures as R32", {
  s <- wc_structure()
  results <- make_group_results(s) # 72 group fixtures played, 0 knockout
  schedule <- make_r32_schedule(s) # 16 cross-group upcoming

  kfx <- .wc_knockout_fixtures_from(schedule, results, s)

  expect_equal(nrow(kfx), 16L)
  expect_true(all(kfx$round == "R32"))
  expect_true(all(kfx$played == FALSE))
  expect_true(all(kfx$venue == "neutral"))
  # every fixture is cross-group (not one of the 72 group pairings)
  expect_true(all(s$group_of[kfx$home_team] != s$group_of[kfx$away_team]))
})

test_that(".wc_knockout_fixtures_from excludes group pairings still in the schedule", {
  s <- wc_structure()
  results <- make_group_results(s)
  schedule <- dplyr::bind_rows(
    .head <- tibble::tibble( # a stray group pairing (same group) must be dropped
      match_date = as.Date("2026-06-28"),
      home_team = s$groups$A[1], away_team = s$groups$A[2]
    ),
    make_r32_schedule(s)
  )
  kfx <- .wc_knockout_fixtures_from(schedule, results, s)
  expect_equal(nrow(kfx), 16L) # the group pairing is excluded
})

test_that(".wc_knockout_fixtures_from rolls the round forward as knockout results land", {
  s <- wc_structure()
  r32 <- make_r32_schedule(s)
  # All 16 R32 now played (added to results); the schedule holds 8 R16 fixtures.
  results <- dplyr::bind_rows(make_group_results(s), r32)
  gw <- vapply(s$groups, `[`, character(1), 1L)
  schedule <- tibble::tibble(
    match_date = as.Date("2026-07-05") + rep(0:3, each = 2L),
    home_team = gw[1:8], away_team = gw[c(9:12, 1:4)]
  )
  kfx <- .wc_knockout_fixtures_from(schedule, results, s)
  expect_equal(nrow(kfx), 8L)
  expect_true(all(kfx$round == "R16"))
})

test_that("wc_knockout_predictions emits the card contract plus round + p_advance", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 200L) # equal strengths
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 7L)
  W <- out$bracket_model$W

  kfx <- .wc_knockout_fixtures_from(make_r32_schedule(s), make_group_results(s), s)
  pr <- wc_knockout_predictions(kfx, si$team, si$scalar, s, W)

  expect_equal(nrow(pr), nrow(kfx))
  # same card contract as the group predictions
  s1x2 <- with(pr, p_home + p_draw + p_away)
  expect_true(all(abs(s1x2 - 1) < 1e-9))
  expect_true(all(pr$eg_home > 0 & pr$eg_away > 0))
  expect_true("goal_diff_distribution" %in% names(pr))
  for (i in seq_len(min(5L, nrow(pr)))) {
    gdd <- pr$goal_diff_distribution[[i]]
    expect_equal(sum(gdd$p), 1, tolerance = 1e-9)
    expect_equal(sum(gdd$p[gdd$diff > 0]), pr$p_home[i], tolerance = 1e-9)
  }
  # knockout-specific fields
  expect_true(all(pr$round == "R32"))
  expect_true(all(pr$p_advance >= 0 & pr$p_advance <= 1))
  expect_true(all(is.na(pr$group)))
  # equal strengths => progression is a coin flip
  expect_equal(mean(pr$p_advance), 0.5, tolerance = 0.05)
})

test_that("wc_knockout_predictions handles a single fixture (the Final)", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 120L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 9L)

  one <- tibble::tibble(
    round = "Final", match_date = as.Date("2026-07-19"),
    home_team = teams[1], away_team = teams[40], # cross-group (A vs J)
    played = FALSE, venue = "neutral"
  )
  pr <- wc_knockout_predictions(one, si$team, si$scalar, s, out$bracket_model$W)

  expect_equal(nrow(pr), 1L)
  expect_equal(pr$round, "Final")
  expect_equal(pr$p_home + pr$p_draw + pr$p_away, 1, tolerance = 1e-9)
  expect_true(pr$p_advance >= 0 && pr$p_advance <= 1)
  expect_s3_class(pr$goal_diff_distribution[[1]], "tbl_df")
})

# ---- Goal-diff distribution (fixture-card strip contract) -------------------

test_that("simulate_world_cup predictions carry a goal-diff distribution", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  si <- make_sim_inputs(unlist(s$groups, use.names = FALSE), n_draws = 100L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)
  pr <- out$predictions
  expect_true("goal_diff_distribution" %in% names(pr))
  expect_length(pr$goal_diff_distribution, nrow(pr))
  for (i in seq_len(min(5L, nrow(pr)))) {
    gdd <- pr$goal_diff_distribution[[i]]
    expect_s3_class(gdd, "tbl_df")
    expect_named(gdd, c("diff", "p"))
    expect_false(is.unsorted(gdd$diff))
    expect_equal(sum(gdd$p), 1, tolerance = 1e-9)
    # Consistency with the 1X2 probabilities (same draws, same tabulation).
    expect_equal(sum(gdd$p[gdd$diff > 0]), pr$p_home[i], tolerance = 1e-9)
    expect_equal(sum(gdd$p[gdd$diff == 0]), pr$p_draw[i], tolerance = 1e-9)
    expect_equal(sum(gdd$p[gdd$diff < 0]), pr$p_away[i], tolerance = 1e-9)
  }
})

# ---- Head-to-head (joint MC) ------------------------------------------------

test_that("wc_forward_bracket emits Third/Fourth consistent with the SF-loser pool", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 100L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 6L)
  bm <- out$bracket_model

  fb <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams)
  third <- fb$placement[fb$placement$round_name == "Third", ]
  fourth <- fb$placement[fb$placement$round_name == "Fourth", ]

  # exactly one bronze winner and one fourth-placed team across the field
  expect_equal(sum(third$probability), 1, tolerance = 1e-9)
  expect_equal(sum(fourth$probability), 1, tolerance = 1e-9)

  # per-team identity: Third + Fourth == reach(SF) - reach(Final) (the SF-loser pool)
  pool <- fb$reach$SF - fb$reach$Final
  third <- third[match(teams, third$team), ]
  fourth <- fourth[match(teams, fourth$team), ]
  expect_equal(third$probability + fourth$probability, pool, tolerance = 1e-9)
  expect_true(all(third$probability >= -1e-12))
})

test_that("wc_forward_bracket third_pin forces the bronze winner", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 80L)
  fx <- make_wc_fixtures(s)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 6L)
  bm <- out$bracket_model
  x <- which(teams == "Brazil")
  fb <- wc_forward_bracket(bm$W, bm$occ_a, bm$occ_b, s$bracket, teams, third_pin = x)
  third <- fb$placement[fb$placement$round_name == "Third", ]
  expect_equal(third$probability[third$team == "Brazil"], 1, tolerance = 1e-9)
  expect_equal(sum(third$probability), 1, tolerance = 1e-9)
})

test_that("wc_head_to_head returns coherent all-pairs probabilities", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  nt <- length(teams)
  si <- make_sim_inputs(teams, n_draws = 40L)
  fx <- make_wc_fixtures(s)

  h2h <- wc_head_to_head(si$team, si$scalar, fx, s,
    k_replays = 30L, pairing_seed = 1L
  )

  expect_equal(h2h$teams, teams)
  expect_length(h2h$pairs, nt * (nt - 1L) / 2L) # 1128

  for (pr in h2h$pairs[c(1L, 17L, 500L, 1128L)]) {
    # indices are 0-based, ordered i < j, in range
    expect_true(pr$i >= 0L && pr$j <= nt - 1L && pr$i < pr$j)
    # the three-way split is a valid distribution (implied p_j_farther >= 0)
    expect_gte(pr$p_i_farther, 0)
    expect_gte(pr$p_tie, 0)
    expect_lte(pr$p_i_farther + pr$p_tie, 1 + 1e-9)
    # meeting prob decomposes into the per-round meeting probs (identity holds
    # exactly pre-rounding; allow for independent 4-decimal rounding of each)
    mbr <- unlist(pr$meet_by_round)
    expect_lt(abs(pr$p_meet - sum(mbr)), 1e-3)
    # R32 meetings only possible for same-R32-match pairs; tournament-wide most
    # pairs can't, but the field always exists
    expect_true(all(mbr >= 0))
    if (!is.null(pr$p_i_wins_if_meet)) {
      expect_gte(pr$p_i_wins_if_meet, 0)
      expect_lte(pr$p_i_wins_if_meet, 1)
    }
  }

  # furthest-round marginals are proper pmfs (levels 0..6)
  expect_length(h2h$team_marginals, nt)
  for (m in h2h$team_marginals[c(1L, 24L, nt)]) {
    expect_length(m, 7L)
    expect_lt(abs(sum(m) - 1), 1e-3) # 7 components, each 4-decimal rounded
  }

  # equal strengths => head-to-head is a coin flip when teams meet
  meets <- Filter(function(pr) !is.null(pr$p_i_wins_if_meet) && pr$p_meet > 0.01, h2h$pairs)
  if (length(meets) > 0L) {
    win <- vapply(meets, function(pr) pr$p_i_wins_if_meet, numeric(1))
    expect_equal(mean(win), 0.5, tolerance = 0.05)
  }
})

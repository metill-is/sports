# R/extract-football-iceland.R — cup bracket.json payload builder.
#
# Exercises `.build_cup_bracket_payload_pfi()` against a tiny synthetic
# bracket_state (4 alive teams, 2 known SF pairings, Final undrawn) and a
# small synthetic sim_inputs — no 4000-draw fit, fully deterministic. The
# payload must mirror the World Cup `bracket.json` contract (wc-publish.R)
# so the metill-platform interactive what-if tree (`cup-bracket.js`) can
# consume cup brackets through the same renderer.

# ---- Synthetic fixtures -----------------------------------------------------

# 4 alive semifinalists with distinct strengths so the W matrix is non-trivial
# and orientable. A is strongest, D weakest.
.cup_bracket_sim_inputs <- function(n_draws = 6L) {
  teams <- c("Alpha", "Bravo", "Charlie", "Delta")
  off <- c(Alpha = 0.40, Bravo = 0.10, Charlie = -0.10, Delta = -0.40)
  def <- c(Alpha = 0.40, Bravo = 0.10, Charlie = -0.10, Delta = -0.40)

  team_inputs <- tidyr::expand_grid(team = teams, .draw = seq_len(n_draws)) |>
    dplyr::mutate(
      cur_offense        = off[.data$team],
      cur_defense        = def[.data$team],
      home_advantage_off = 0.3,
      home_advantage_def = 0.3
    )
  scalar_inputs <- tibble::tibble(
    .draw                  = seq_len(n_draws),
    mean_log_goals         = log(1.5),
    alpha_mu3              = -3,
    beta_mu3_strength_diff = 0
  )
  list(team = team_inputs, scalar = scalar_inputs)
}

# Frontier-at-SF bracket_state: R16 + R8 decided (collapsed into cup_teams
# here — the helper only reads the alive frontier), SF pairings known but
# undecided, Final undrawn. Pairings: Alpha–Delta, Bravo–Charlie.
.cup_bracket_state_sf <- function() {
  list(
    cup_teams = c("Alpha", "Bravo", "Charlie", "Delta"),
    rounds = list(
      R16 = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team = paste0("x", 1:8), away_team = paste0("y", 1:8),
        venue = "home", known_winner = paste0("x", 1:8)
      )),
      R8 = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team = c("Alpha", "Bravo", "Charlie", "Delta"),
        away_team = c("p", "q", "r", "s"),
        venue = "home",
        known_winner = c("Alpha", "Bravo", "Charlie", "Delta")
      )),
      SF = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team    = c("Alpha", "Bravo"),
        away_team    = c("Delta", "Charlie"),
        venue        = "home",
        known_winner = c(NA_character_, NA_character_)
      )),
      Final = list(pairings_known = FALSE, matches = NULL)
    )
  )
}

# ---- Contract tests ---------------------------------------------------------

test_that("cup bracket payload has the 9 keys (8 WC keys + completed)", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  expect_setequal(
    names(bj),
    c(
      "generated_at", "n_draws", "teams", "teams_is", "matchup", "matches",
      "r32", "completed", "played"
    )
  )
})

test_that("teams are the 4 alive SF participants, teams_is mirrors teams", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  expect_setequal(bj$teams, c("Alpha", "Bravo", "Charlie", "Delta"))
  # Icelandic clubs: display names == canonical names.
  expect_identical(bj$teams_is, bj$teams)
})

test_that("matchup is a square nt x nt matrix, diag 0, off-diag in (0,1)", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  n <- length(bj$teams)
  W <- bj$matchup
  expect_equal(nrow(W), n)
  expect_equal(ncol(W), n)
  expect_true(all(diag(W) == 0))
  off <- W[row(W) != col(W)]
  expect_true(all(off > 0 & off < 1))
})

test_that("W is oriented: stronger team has higher win prob", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  idx <- stats::setNames(seq_along(bj$teams), bj$teams)
  # Alpha (strongest) beats Delta (weakest) > 50%.
  expect_gt(bj$matchup[idx[["Alpha"]], idx[["Delta"]]], 0.5)
  # P(a beats b) + P(b beats a) == 1 (conditional-on-decisive).
  expect_equal(
    bj$matchup[idx[["Alpha"]], idx[["Delta"]]] +
      bj$matchup[idx[["Delta"]], idx[["Alpha"]]],
    1,
    tolerance = 1e-6
  )
})

test_that("leaf (SF) matchup cells use the home venue; non-leaf (Final) cells stay neutral", {
  si <- .cup_bracket_sim_inputs()
  bs_home <- .cup_bracket_state_sf() # SF venue = "home"
  bs_neutral <- .cup_bracket_state_sf()
  bs_neutral$rounds$SF$matches$venue <- "neutral"

  bj_home <- .build_cup_bracket_payload_pfi(
    bs_home, si,
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  bj_neu <- .build_cup_bracket_payload_pfi(
    bs_neutral, si,
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  idx <- stats::setNames(seq_along(bj_home$teams), bj_home$teams)

  # SF leaves carry the host's home advantage -> the home team's win prob is
  # strictly higher than at neutral. Asserted on SF2 = Bravo (home) v Charlie;
  # SF1's Alpha v Delta is saturated at ~1.0 (no headroom to shift).
  expect_gt(
    bj_home$matchup[idx[["Bravo"]], idx[["Charlie"]]],
    bj_neu$matchup[idx[["Bravo"]], idx[["Charlie"]]]
  )
  # The reverse cell stays complementary (conditional-on-decisive).
  expect_equal(
    bj_home$matchup[idx[["Bravo"]], idx[["Charlie"]]] +
      bj_home$matchup[idx[["Charlie"]], idx[["Bravo"]]],
    1,
    tolerance = 1e-6
  )
  # A Final cross-pairing (Alpha v Bravo) is not a leaf -> neutral, unchanged.
  expect_equal(
    bj_home$matchup[idx[["Alpha"]], idx[["Bravo"]]],
    bj_neu$matchup[idx[["Alpha"]], idx[["Bravo"]]],
    tolerance = 1e-9
  )
})

test_that("matches: exactly one Final (root) with W-prefixed feeders", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  finals <- Filter(function(m) m$round == "Final", bj$matches)
  expect_length(finals, 1L)
  expect_true(all(grepl("^W", c(finals[[1]]$feeder_a, finals[[1]]$feeder_b))))
  # Leaves are the two SF matches.
  sfs <- Filter(function(m) m$round == "SF", bj$matches)
  expect_length(sfs, 2L)
  expect_length(bj$matches, 3L)
})

test_that("r32: every leaf match_no present with degenerate p==1 occupancy", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  idx <- stats::setNames(seq_along(bj$teams), bj$teams) # 1-based
  sf_match_nos <- vapply(
    Filter(function(m) m$round == "SF", bj$matches),
    function(m) m$match_no, integer(1)
  )
  r32_nos <- vapply(bj$r32, function(e) e$match_no, integer(1))
  expect_setequal(r32_nos, sf_match_nos)

  for (e in bj$r32) {
    # Each side is a single-entry degenerate occupancy with p == 1.
    expect_length(e$a, 1L)
    expect_length(e$b, 1L)
    expect_equal(e$a[[1]]$p, 1)
    expect_equal(e$b[[1]]$p, 1)
    # 0-based indices point at the actual SF pairing participants.
    occ_a_team <- bj$teams[e$a[[1]]$i + 1L]
    occ_b_team <- bj$teams[e$b[[1]]$i + 1L]
    expect_true(occ_a_team %in% bj$teams)
    expect_true(occ_b_team %in% bj$teams)
  }
})

test_that("r32 occupancy matches the synthetic SF pairings exactly", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  # Collect (home, away) pairs implied by r32 occupancy.
  pairs <- lapply(bj$r32, function(e) {
    c(bj$teams[e$a[[1]]$i + 1L], bj$teams[e$b[[1]]$i + 1L])
  })
  pair_set <- vapply(pairs, function(p) paste(sort(p), collapse = "|"), character(1))
  expect_setequal(
    pair_set,
    c(
      paste(sort(c("Alpha", "Delta")), collapse = "|"),
      paste(sort(c("Bravo", "Charlie")), collapse = "|")
    )
  )
})

test_that(".build_cup_completed_pfi emits decided pre-frontier matches with scores", {
  bs <- list(
    cup_teams = c("Alpha", "Bravo", "Charlie", "Delta"),
    rounds = list(
      R16 = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team    = c("Alpha", "Echo", "Charlie", "Golf"),
        away_team    = c("Foxtrot", "Bravo", "Hotel", "Delta"),
        venue        = "home",
        known_winner = c("Alpha", "Bravo", "Charlie", "Delta")
      )),
      R8 = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team    = c("Alpha", "Charlie"),
        away_team    = c("Bravo", "Delta"),
        venue        = "home",
        known_winner = c("Alpha", "Charlie")
      )),
      SF = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team = "Alpha", away_team = "Charlie",
        venue = "home", known_winner = NA_character_ # frontier (undecided)
      )),
      Final = list(pairings_known = FALSE, matches = NULL)
    )
  )
  results <- tibble::tibble(
    division = "CUP", season = 2026L,
    home_team = c("Alpha", "Echo", "Charlie", "Golf"),
    away_team = c("Foxtrot", "Bravo", "Hotel", "Delta"),
    home_score = c(3L, 0L, 2L, 1L),
    away_score = c(0L, 1L, 0L, 2L)
  )
  comp <- .build_cup_completed_pfi(bs, results, season = 2026L)
  # 4 R16 + 2 QF decided matches, SF is the frontier (excluded)
  expect_length(comp, 6L)
  expect_setequal(unique(vapply(comp, `[[`, "", "round")), c("R16", "QF"))
  a <- Filter(function(x) x$home == "Alpha" && x$round == "R16", comp)[[1]]
  expect_identical(a$winner, "Alpha")
  expect_identical(a$home_score, 3L)
  expect_identical(a$away_score, 0L)
  # score oriented to the bracket's home/away even when results row is flipped:
  b <- Filter(function(x) x$winner == "Bravo", comp)[[1]] # bracket home=Echo away=Bravo
  expect_identical(b$home, "Echo")
  expect_identical(b$away, "Bravo")
  expect_identical(b$home_score, 0L)
  expect_identical(b$away_score, 1L)
})

test_that("payload carries an additive completed[] without changing live keys", {
  bs <- .cup_bracket_state_sf() # existing fixture: SF frontier, R16+R8 decided
  si <- .cup_bracket_sim_inputs()
  results <- tibble::tibble(
    division = "CUP", season = 2026L,
    home_team = bs$rounds$R16$matches$home_team,
    away_team = bs$rounds$R16$matches$away_team,
    home_score = rep(2L, nrow(bs$rounds$R16$matches)),
    away_score = rep(0L, nrow(bs$rounds$R16$matches))
  )
  base <- .build_cup_bracket_payload_pfi(bs, si, "2026-06-27T00:00:00Z", 6L)
  ext <- .build_cup_bracket_payload_pfi(bs, si, "2026-06-27T00:00:00Z", 6L,
    results = results, season = 2026L
  )
  # live keys identical
  expect_identical(ext$teams, base$teams)
  expect_identical(ext$matchup, base$matchup)
  expect_identical(ext$matches, base$matches)
  expect_identical(ext$r32, base$r32)
  # additive completed present + non-empty
  expect_true("completed" %in% names(ext))
  expect_true(length(ext$completed) >= 1L)
  expect_true(all(vapply(ext$completed, function(x) !is.null(x$winner), TRUE)))
})

# ---- Half-played frontier (2026-07-04 incident) -----------------------------
# The 2026 Mjólkurbikar SF1 (Breiðablik 3-0 Víkingur R., played 28 Jun) was
# published as a pending pairing: the payload treated every frontier match as
# live, and completed[] stopped strictly before the frontier round. A decided
# frontier match must (a) pin its matchup cells to 1/0, (b) appear in the
# WC-contract `played[]`, and (c) appear in `completed[]`.

# SF1 decided (Alpha beat Delta), SF2 undecided — the live frontier is still SF.
.cup_bracket_state_sf_half <- function() {
  bs <- .cup_bracket_state_sf()
  bs$rounds$SF$matches$known_winner <- c("Alpha", NA_character_)
  bs
}

.cup_half_results <- function(sf1_score = c(3L, 0L)) {
  tibble::tibble(
    division = "CUP", season = 2026L,
    home_team = "Alpha", away_team = "Delta",
    home_score = sf1_score[1], away_score = sf1_score[2]
  )
}

test_that("a decided frontier leaf pins its matchup cells to 1/0", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf_half(), .cup_bracket_sim_inputs(),
    generated_at = "2026-07-04T00:00:00Z", n_draws = 6L,
    results = .cup_half_results(), season = 2026L
  )
  idx <- stats::setNames(seq_along(bj$teams), bj$teams)
  expect_equal(bj$matchup[idx[["Alpha"]], idx[["Delta"]]], 1)
  expect_equal(bj$matchup[idx[["Delta"]], idx[["Alpha"]]], 0)
  # The undecided leaf keeps a genuine home-venue probability.
  p_sf2 <- bj$matchup[idx[["Bravo"]], idx[["Charlie"]]]
  expect_true(p_sf2 > 0 && p_sf2 < 1)
})

test_that("played[] carries decided frontier matches under the WC contract", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf_half(), .cup_bracket_sim_inputs(),
    generated_at = "2026-07-04T00:00:00Z", n_draws = 6L,
    results = .cup_half_results(), season = 2026L
  )
  expect_length(bj$played, 1L)
  p <- bj$played[[1]]
  sf1_no <- Filter(
    function(m) m$round == "SF", bj$matches
  )[[1]]$match_no
  expect_identical(p$match_no, sf1_no)
  # 0-based indices into teams, mirroring wc-publish.R.
  expect_identical(bj$teams[p$winner + 1L], "Alpha")
  expect_identical(bj$teams[p$loser + 1L], "Delta")
  expect_identical(p$winner_score, 3L)
  expect_identical(p$loser_score, 0L)
  expect_false(p$shootout)
})

test_that("played[] is present but empty when no frontier match is decided", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-07-04T00:00:00Z", n_draws = 6L,
    results = .cup_half_results(), season = 2026L
  )
  expect_true("played" %in% names(bj))
  expect_length(bj$played, 0L)
})

test_that("a frontier match decided beyond 90' gets shootout = TRUE", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf_half(), .cup_bracket_sim_inputs(),
    generated_at = "2026-07-04T00:00:00Z", n_draws = 6L,
    results = .cup_half_results(sf1_score = c(2L, 2L)), season = 2026L
  )
  expect_length(bj$played, 1L)
  expect_true(bj$played[[1]]$shootout)
  expect_identical(bj$played[[1]]$winner_score, 2L)
  expect_identical(bj$played[[1]]$loser_score, 2L)
})

test_that("completed[] includes decided matches of the frontier round", {
  comp <- .build_cup_completed_pfi(
    .cup_bracket_state_sf_half(), .cup_half_results(),
    season = 2026L
  )
  sf <- Filter(function(x) x$round == "SF", comp)
  expect_length(sf, 1L)
  expect_identical(sf[[1]]$home, "Alpha")
  expect_identical(sf[[1]]$away, "Delta")
  expect_identical(sf[[1]]$home_score, 3L)
  expect_identical(sf[[1]]$away_score, 0L)
  expect_identical(sf[[1]]$winner, "Alpha")
  # Pre-frontier rounds still emit (scores NULL when absent from results).
  expect_length(Filter(function(x) x$round == "R16", comp), 8L)
  expect_length(Filter(function(x) x$round == "QF", comp), 4L)
})

test_that("completed[] emits decided matches when the entry round is the frontier", {
  bs <- list(
    cup_teams = c("Alpha", "Bravo", "Charlie", "Delta"),
    rounds = list(
      R16 = list(pairings_known = TRUE, matches = tibble::tibble(
        home_team    = c("Alpha", "Charlie", "Echo", "Golf"),
        away_team    = c("Bravo", "Delta", "Foxtrot", "Hotel"),
        venue        = "home",
        known_winner = c("Alpha", "Charlie", NA, NA)
      )),
      R8 = list(pairings_known = FALSE, matches = NULL),
      SF = list(pairings_known = FALSE, matches = NULL),
      Final = list(pairings_known = FALSE, matches = NULL)
    )
  )
  results <- tibble::tibble(
    division = "CUP", season = 2026L,
    home_team = c("Alpha", "Charlie"), away_team = c("Bravo", "Delta"),
    home_score = c(2L, 1L), away_score = c(0L, 0L)
  )
  comp <- .build_cup_completed_pfi(bs, results, season = 2026L)
  expect_length(comp, 2L)
  expect_setequal(vapply(comp, `[[`, "", "winner"), c("Alpha", "Charlie"))
})

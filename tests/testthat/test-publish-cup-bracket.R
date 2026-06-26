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

test_that("cup bracket payload has the 7 WC keys", {
  bj <- .build_cup_bracket_payload_pfi(
    .cup_bracket_state_sf(), .cup_bracket_sim_inputs(),
    generated_at = "2026-06-25T00:00:00Z", n_draws = 6L
  )
  expect_setequal(
    names(bj),
    c("generated_at", "n_draws", "teams", "teams_is", "matchup", "matches", "r32")
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

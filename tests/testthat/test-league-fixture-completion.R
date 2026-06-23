# R/extract-football-iceland.R — league fixture-completion suite.
#
# Regression cover for the "leader pinned at 100%" bug: KSÍ only publishes the
# first single round-robin's dated fixtures, so the schedule store misses every
# return-leg fixture. The season simulator, fed only the schedule's remaining
# fixtures, then integrates over almost no games and reports the current leader
# as champion with probability 1. The fix derives the remaining fixtures
# structurally for double round-robin divisions (every unplayed ordered pair),
# keyed off a per-division round-robin multiplicity inferred from the most
# recent completed season.

# ---- .complete_double_rr_remaining_pfi --------------------------------------
# Pure structural enumeration: all ordered (home, away) pairs among `teams`
# (home != away), minus the ordered pairs already played.

test_that("complete double round-robin among 3 teams with no games played is 6 fixtures", {
  out <- .complete_double_rr_remaining_pfi(c("A", "B", "C"), character())
  expect_equal(nrow(out), 6L)
  expect_setequal(
    paste(out$home_team, out$away_team),
    c("A B", "A C", "B A", "B C", "C A", "C B")
  )
})

test_that("complete double round-robin excludes already-played ordered pairs and keeps the reverse leg", {
  # First single round-robin played, one orientation each: A>B, B>C, C>A.
  played <- c("A B", "B C", "C A")
  out <- .complete_double_rr_remaining_pfi(c("A", "B", "C"), played)
  # Remaining = the three return legs (reverse orientation).
  expect_setequal(paste(out$home_team, out$away_team), c("B A", "C B", "A C"))
})

test_that("complete double round-robin returns both legs for a 2-team league", {
  out <- .complete_double_rr_remaining_pfi(c("A", "B"), character())
  expect_setequal(paste(out$home_team, out$away_team), c("A B", "B A"))
})

test_that("complete double round-robin is empty for fewer than two teams", {
  expect_equal(nrow(.complete_double_rr_remaining_pfi(c("A"), character())), 0L)
  expect_equal(nrow(.complete_double_rr_remaining_pfi(character(), character())), 0L)
})

# ---- .division_rr_multiplicity_pfi ------------------------------------------
# Infer round-robin multiplicity (1 = single, 2 = double) from the MOST RECENT
# COMPLETED PRIOR season. Current-season data cannot be used: mid-season a
# double round-robin has played each pair at most once, so it would look single.

.rr_results <- function(season, division, pairs) {
  # pairs: character vector of "H A" ordered-pair keys.
  ha <- do.call(rbind, strsplit(pairs, " ", fixed = TRUE))
  tibble::tibble(
    home_team = ha[, 1], away_team = ha[, 2],
    home_score = 1L, away_score = 0L,
    division = division, season = as.integer(season),
    match_date = as.Date("2025-05-01")
  )
}

test_that("multiplicity is 2 when the prior season played each pair home and away", {
  prior <- .rr_results(2025L, "BD", c("A B", "B A", "A C", "C A", "B C", "C B"))
  expect_equal(.division_rr_multiplicity_pfi(prior, current_season = 2026L, division = "BD"), 2L)
})

test_that("multiplicity is 1 when the prior season played each pair once", {
  prior <- .rr_results(2025L, "LD2", c("A B", "A C", "B C"))
  expect_equal(.division_rr_multiplicity_pfi(prior, current_season = 2026L, division = "LD2"), 1L)
})

test_that("multiplicity ignores the in-progress current season (uses prior only)", {
  prior <- .rr_results(2025L, "BD", c("A B", "B A", "A C", "C A", "B C", "C B")) # double
  current <- .rr_results(2026L, "BD", c("A B", "A C", "B C")) # first leg only -> looks single
  res <- dplyr::bind_rows(prior, current)
  expect_equal(.division_rr_multiplicity_pfi(res, current_season = 2026L, division = "BD"), 2L)
})

test_that("multiplicity uses the most recent completed prior season", {
  old <- .rr_results(2024L, "LD2", c("A B", "A C", "B C")) # single
  recent <- .rr_results(2025L, "LD2", c("A B", "B A", "A C", "C A", "B C", "C B")) # double
  res <- dplyr::bind_rows(old, recent)
  expect_equal(.division_rr_multiplicity_pfi(res, current_season = 2026L, division = "LD2"), 2L)
})

test_that("multiplicity is NA when no prior completed season exists for the division", {
  only_current <- .rr_results(2026L, "BD", c("A B", "A C"))
  expect_true(is.na(.division_rr_multiplicity_pfi(only_current, current_season = 2026L, division = "BD")))
})

# ---- .league_base_and_remaining_pfi: structural completion ------------------
# The bug reproduction. With a double-RR multiplicity and an INCOMPLETE schedule
# (KSÍ has not yet published the return-leg fixtures), the remaining-fixture set
# must be the full structural remainder, NOT the schedule's truncated subset.

test_that("multiplicity=2 derives the full remaining double round-robin, ignoring an incomplete schedule", {
  # 4 teams, first single round-robin (6 games, one orientation each) all played.
  played <- tibble::tibble(
    home_team = c("A", "C", "A", "B", "A", "B"),
    away_team = c("B", "D", "C", "D", "D", "C"),
    home_score = c(2L, 1L, 1L, 0L, 3L, 1L),
    away_score = c(0L, 0L, 1L, 0L, 1L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date("2026-05-01") + 0:5
  )
  ctt <- tibble::tibble(team = c("A", "B", "C", "D"))
  # KSÍ has published NO return-leg fixtures yet: the schedule has nothing left.
  empty_schedule <- tibble::tibble(
    home_team = character(), away_team = character(),
    division = character(), match_date = as.Date(character())
  )

  out <- .league_base_and_remaining_pfi(played, ctt, empty_schedule, "BD", multiplicity = 2L)

  # Full double RR = 12 ordered pairs; 6 first-leg pairs played -> 6 return legs.
  expect_equal(nrow(out$remaining_fixtures), 6L)
  expect_setequal(
    paste(out$remaining_fixtures$home_team, out$remaining_fixtures$away_team),
    c("B A", "D C", "C A", "D B", "D A", "C B")
  )
})

test_that("multiplicity=1 (or NA) falls back to the schedule-derived remaining fixtures", {
  # Single round-robin division: the schedule IS the complete remaining set,
  # so structural double-RR completion must NOT kick in.
  played <- tibble::tibble(
    home_team = "A", away_team = "B",
    home_score = 1L, away_score = 0L,
    division = "LD2", season = 2026L, match_date = as.Date("2026-05-01")
  )
  ctt <- tibble::tibble(team = c("A", "B", "C"))
  schedule <- tibble::tibble(
    home_team = c("A", "B"), away_team = c("C", "C"), division = "LD2",
    match_date = as.Date(c("2026-05-08", "2026-05-09"))
  )
  out <- .league_base_and_remaining_pfi(played, ctt, schedule, "LD2", multiplicity = 1L)
  # Schedule path: only the 2 scheduled fixtures remain (A vs C, B vs C).
  expect_equal(nrow(out$remaining_fixtures), 2L)
  expect_setequal(
    paste(out$remaining_fixtures$home_team, out$remaining_fixtures$away_team),
    c("A C", "B C")
  )
})

# sport_publish_profile() is the SOLE author of the per-sport publish facts
# (SC-1). These assertions are the contract: everything downstream reads the
# registry instead of re-deriving, so if a value here is wrong it is wrong
# everywhere at once.

test_that("points, tie handling and the predicted_matches shape are per-sport", {
  expect_equal(
    sport_publish_profile("football")$points,
    list(win = 3L, draw = 1L, loss = 0L)
  )
  expect_equal(
    sport_publish_profile("handball")$points,
    list(win = 2L, draw = 1L, loss = 0L)
  )
  # draw is NULL, not 0L, so meta.points.draw serialises as JSON null and no
  # consumer infers a drawn basketball game is possible.
  expect_equal(
    sport_publish_profile("basketball")$points,
    list(win = 2L, draw = NULL, loss = 0L)
  )
  expect_null(sport_publish_profile("basketball")$points$draw)

  expect_equal(
    sport_publish_profile("football")$predicted_matches_shape,
    "scoreline_counts"
  )
  for (sport in c("basketball", "handball")) {
    expect_equal(
      sport_publish_profile(sport)$predicted_matches_shape,
      "match_summary"
    )
  }
})

test_that("has_ties and tie_threshold agree with config/leagues.yml", {
  leagues <- load_leagues()
  keys <- c(
    football = "football_iceland",
    basketball = "basketball_iceland",
    handball = "handball_iceland"
  )
  for (sport in names(keys)) {
    scoring <- leagues[[keys[[sport]]]]$betting$scoring
    profile <- sport_publish_profile(sport)
    expect_identical(profile$has_ties, isTRUE(scoring$has_ties), info = sport)
    expect_equal(profile$tie_threshold, scoring$tie_threshold, info = sport)
  }
  expect_false(sport_publish_profile("basketball")$has_ties)
  expect_true(sport_publish_profile("handball")$has_ties)
  expect_true(sport_publish_profile("football")$has_ties)
  expect_equal(sport_publish_profile("basketball")$tie_threshold, 0)
})

test_that("required / optional extracts match what each extractor writes", {
  football <- sport_publish_profile("football")
  expect_setequal(
    football$required_extracts,
    c(
      "predicted_matches", "team_strengths_quantiles",
      "round_strengths_quantiles", "home_advantage_quantiles",
      "final_positions", "points_distribution"
    )
  )
  for (sport in c("basketball", "handball")) {
    # The 2DT extractor writes the same six division-keyed parquets football
    # does since WS8, round_strengths_quantiles included.
    expect_setequal(
      sport_publish_profile(sport)$required_extracts,
      c(
        "predicted_matches", "team_strengths_quantiles",
        "round_strengths_quantiles", "home_advantage_quantiles",
        "final_positions", "points_distribution"
      )
    )
  }
})

test_that("fit_meta is optional for every sport", {
  # THE load-bearing assertion: required_extracts drives the reader's
  # partition-completeness check, so a newly-required file would mark every
  # existing on-disk football partition incomplete and stop the live publish.
  for (sport in c("football", "basketball", "handball")) {
    profile <- sport_publish_profile(sport)
    expect_true("fit_meta" %in% profile$optional_extracts, info = sport)
    expect_false("fit_meta" %in% profile$required_extracts, info = sport)
  }
})

test_that("round_strengths_quantiles and tournament_placements are placed per sport", {
  football <- sport_publish_profile("football")
  expect_true("round_strengths_quantiles" %in% football$required_extracts)
  expect_true("tournament_placements" %in% football$optional_extracts)

  for (sport in c("basketball", "handball")) {
    profile <- sport_publish_profile(sport)
    # REQUIRED for the 2DT sports too since WS8: all three Stan models declare
    # the same `array[N_rounds] vector[K] offense`/`defense` surface, the
    # extractor writes it, and there is no pre-contract bb/hb partition on disk
    # for the requirement to strand.
    expect_true(
      "round_strengths_quantiles" %in% profile$required_extracts,
      info = sport
    )
    expect_false(
      "round_strengths_quantiles" %in% profile$optional_extracts,
      info = sport
    )
    expect_setequal(profile$required_extracts, football$required_extracts)
    expect_false(
      "tournament_placements" %in%
        c(profile$required_extracts, profile$optional_extracts),
      info = sport
    )
  }
})

test_that("empty_extracts covers every file type with a 0-row tibble", {
  for (sport in c("football", "basketball", "handball")) {
    profile <- sport_publish_profile(sport)
    expect_setequal(
      names(profile$empty_extracts),
      union(profile$required_extracts, profile$optional_extracts)
    )
    rows <- vapply(profile$empty_extracts, nrow, integer(1))
    expect_true(all(rows == 0L), info = sport)
    expect_true(
      all(vapply(profile$empty_extracts, tibble::is_tibble, logical(1))),
      info = sport
    )
  }
})

test_that("the football-only surfaces are football-only", {
  football_only <- c(
    "round_predictions_history", "xg", "cup_bracket", "split",
    "preseason_strengths"
  )
  expect_true(all(football_only %in% sport_publish_profile("football")$surfaces))
  for (sport in c("basketball", "handball")) {
    expect_length(
      intersect(football_only, sport_publish_profile(sport)$surfaces), 0L
    )
  }

  common <- c(
    "standings", "standings_history", "team_strengths",
    "team_strengths_history", "final_positions", "final_positions_history",
    "points_distribution", "home_advantage", "next_games", "meta"
  )
  for (sport in c("football", "basketball", "handball")) {
    expect_true(
      all(common %in% sport_publish_profile(sport)$surfaces),
      info = sport
    )
  }
})

test_that("units, season scope, postseason and placement basis are per-sport", {
  expect_equal(
    sport_publish_profile("basketball")$units,
    list(strength = "points", home_advantage = "points", diff_bin_width = 5L)
  )
  expect_equal(
    sport_publish_profile("handball")$units,
    list(strength = "goals", home_advantage = "goals", diff_bin_width = 2L)
  )
  expect_equal(
    sport_publish_profile("football")$units,
    list(
      strength = "log_goals", home_advantage = "goal_multiplier",
      diff_bin_width = 1L
    )
  )

  expect_equal(sport_publish_profile("football")$season_scope, "full_season")
  expect_null(sport_publish_profile("football")$postseason)
  expect_equal(sport_publish_profile("football")$placement_basis, "final_table")

  for (sport in c("basketball", "handball")) {
    profile <- sport_publish_profile(sport)
    expect_equal(profile$season_scope, "regular_season", info = sport)
    expect_equal(profile$placement_basis, "regular_season_table", info = sport)
    expect_equal(
      profile$postseason,
      list(name_is = "\u00darslitakeppni", modelled = FALSE),
      info = sport
    )
  }
})

test_that("value_link records the link each extractor already applied", {
  # Football stores raw strengths and an ALREADY-exponentiated home advantage
  # with the total halved; the 2DT sports store the parameter itself. Getting
  # this backwards is the B5 bug (a 12.07-point home edge published as 417.8).
  expect_equal(
    sport_publish_profile("football")$value_link[["home_advantage"]], "exp"
  )
  expect_equal(
    sport_publish_profile("football")$value_link[["home_advantage_total"]],
    "exp_half"
  )
  expect_equal(
    sport_publish_profile("football")$value_link[["team_strength"]], "identity"
  )
  for (sport in c("basketball", "handball")) {
    expect_true(
      all(sport_publish_profile(sport)$value_link == "identity"),
      info = sport
    )
  }
})

test_that("an unknown sport aborts and names the known ones", {
  expect_error(sport_publish_profile("cricket"), "cricket")
  expect_error(sport_publish_profile("cricket"), "football")
  expect_error(sport_publish_profile(NA_character_))
  expect_error(sport_publish_profile(c("football", "handball")))
})

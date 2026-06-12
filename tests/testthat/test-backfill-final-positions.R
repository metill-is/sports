test_that(".league_base_and_remaining_pfi builds the realised table and unplayed fixtures", {
  played <- tibble::tibble(
    home_team = c("A", "B", "C"), away_team = c("B", "C", "A"),
    home_score = c(2L, 0L, 1L), away_score = c(0L, 0L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-05-01", "2026-05-02", "2026-05-03"))
  )
  current_top_teams <- tibble::tibble(team = c("A", "B", "C"))
  schedule <- tibble::tibble(
    home_team = c("A", "B", "C", "A"), away_team = c("C", "A", "B", "B"),
    division = c("BD", "BD", "BD", "LD1"),
    match_date = as.Date(c("2026-05-10", "2026-05-11", "2026-05-12", "2026-05-10"))
  )

  out <- .league_base_and_remaining_pfi(played, current_top_teams, schedule, "BD")

  a <- out$base_standings[out$base_standings$team == "A", ]
  expect_equal(a$base_points, 4L) # win vs B (3) + draw vs C (1)
  expect_equal(a$base_gd, 2L) # +2 vs B, 0 vs C
  expect_equal(a$base_gf, 3L) # 2 vs B + 1 vs C
  # remaining: BD-only, both teams known, deduped, the LD1 row excluded
  expect_equal(nrow(out$remaining_fixtures), 3L)
  expect_true(all(c("home_team", "away_team") %in% names(out$remaining_fixtures)))
})

test_that(".league_base_and_remaining_pfi drops reschedule ghosts (same ordered pair twice)", {
  played <- tibble::tibble(
    home_team = character(), away_team = character(),
    home_score = integer(), away_score = integer(),
    division = character(), season = integer(), match_date = as.Date(character())
  )
  ctt <- tibble::tibble(team = c("A", "B"))
  schedule <- tibble::tibble(
    home_team = c("A", "A"), away_team = c("B", "B"), division = "BD",
    match_date = as.Date(c("2026-06-01", "2026-06-08")) # ghost + reschedule
  )
  out <- .league_base_and_remaining_pfi(played, ctt, schedule, "BD")
  expect_equal(nrow(out$remaining_fixtures), 1L) # deduped to the later date
})

test_that("completed_bd_rounds enumerates 1..R_max with ascending cutoff dates", {
  # 2 BD teams playing 3 synchronised rounds -> 3 completed rounds.
  results <- tibble::tibble(
    home_team = c("A", "B", "A"), away_team = c("B", "A", "B"),
    home_score = c(1L, 1L, 2L), away_score = c(0L, 1L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-04-10", "2026-04-17", "2026-04-24"))
  )
  rounds <- completed_bd_rounds(results, season = 2026L)
  expect_equal(rounds$round, 1:3)
  expect_true(all(diff(as.integer(rounds$cutoff_date)) > 0))
  expect_equal(rounds$cutoff_date[1], as.Date("2026-04-10"))
})

test_that("build_round_final_positions stamps as_of/round/division and rows sum to 1 per team", {
  # Mock the strength extraction so the test needs no Stan fit.
  local_mocked_bindings(
    .extract_sim_inputs_pfi = function(fit, teams) {
      tm <- tidyr::expand_grid(team = c("A", "B", "C"), .draw = 1:40) |>
        dplyr::mutate(
          cur_offense = 0, cur_defense = 0,
          home_advantage_off = 0, home_advantage_def = 0
        )
      sc <- tibble::tibble(
        .draw = 1:40, mean_log_goals = log(1.5),
        alpha_mu3 = -3, beta_mu3_strength_diff = 0
      )
      list(team = tm, scalar = sc)
    },
    .package = "sports"
  )
  results <- tibble::tibble(
    home_team = c("A", "B"), away_team = c("B", "C"),
    home_score = c(2L, 0L), away_score = c(0L, 1L),
    division = "BD", season = 2026L,
    match_date = as.Date(c("2026-05-01", "2026-05-01"))
  )
  schedule <- tibble::tibble(
    home_team = c("C", "A"), away_team = c("A", "C"), division = "BD",
    match_date = as.Date(c("2026-05-08", "2026-05-15"))
  )
  recs <- build_round_final_positions(
    fit = NULL, prep = list(teams = tibble::tibble(team = c("A", "B", "C"))),
    results = results, season_schedule = schedule,
    round_idx = 1L, cutoff_date = as.Date("2026-05-01"),
    season = 2026L, target_divs = "BD", generated_at = "2026-06-11T00:00:00+0000"
  )
  expect_setequal(
    names(recs),
    c(
      "as_of", "generated_at", "round", "season", "division",
      "team", "placement", "probability"
    )
  )
  expect_true(all(recs$as_of == "2026-05-01"))
  expect_true(all(recs$division == "BD"))
  # round label = the division's own games-played count (min over its teams):
  # A and C each played 1, B played 2 -> min = 1.
  expect_true(all(recs$round == 1L))
  sums <- tapply(recs$probability, recs$team, sum)
  expect_true(all(abs(sums - 1) < 1e-9))
})

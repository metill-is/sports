# The per-round strength trajectory is SPORT-NEUTRAL. It used to live in
# R/publish-football-iceland.R under a `_pfi` name, which was a lie the moment
# the 2DT extractor needed it: football
# (Stan/football_iceland/bivariate_poisson_no_inflation.stan:188,195) and both
# 2DT models (Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164)
# declare the identical `array[N_rounds] vector[K] offense` / `defense` plus
# `home_advantage_off` / `_def`, so one implementation serves all three.
#
# These assertions run against a hand-built fit whose every element is pinned,
# so the (round, team) -> offense[global_round, team_nr] mapping is checked as
# an exact identity rather than a plausible-looking shape.

traj_case <- function(n_draws = 5L) {
  teams <- tibble::tibble(team = c("A", "B", "C"), team_nr = 1:3)

  # A double round robin over three teams: six matches, four appearances each,
  # one match per date so every team's appearance index is unambiguous.
  results <- tibble::tibble(
    home_team  = c("A", "A", "B", "B", "C", "C"),
    away_team  = c("B", "C", "C", "A", "A", "B"),
    match_date = as.Date("2100-01-01") + 0:5,
    division   = "BD",
    season     = 2100L,
    home_score = c(90L, 91L, 92L, 93L, 94L, 95L),
    away_score = c(80L, 81L, 82L, 83L, 84L, 85L)
  )

  n_rounds <- 4L
  k <- 3L
  # offense[r, k] = 10 * r + k, defense[r, k] = 100 + 10 * r + k. Column-major
  # flattening (first index fastest) is what cmdstanr emits for
  # `array[N_rounds] vector[K]`.
  idx <- expand.grid(r = seq_len(n_rounds), k = seq_len(k))
  mat <- function(values) {
    m <- matrix(rep(values, each = n_draws), nrow = n_draws)
    m
  }
  offense <- mat(10 * idx$r + idx$k)
  colnames(offense) <- sprintf("offense[%d,%d]", idx$r, idx$k)
  defense <- mat(100 + 10 * idx$r + idx$k)
  colnames(defense) <- sprintf("defense[%d,%d]", idx$r, idx$k)

  ha_off <- mat(seq_len(k))
  colnames(ha_off) <- sprintf("home_advantage_off[%d]", seq_len(k))
  ha_def <- mat(0.5 * seq_len(k))
  colnames(ha_def) <- sprintf("home_advantage_def[%d]", seq_len(k))

  lp <- matrix(-1, nrow = n_draws, ncol = 1L)
  colnames(lp) <- "lp__"

  list(
    fit = stub_fit(list(
      offense = offense, defense = defense,
      home_advantage_off = ha_off, home_advantage_def = ha_def,
      lp__ = lp
    )),
    teams = teams,
    results = results,
    current_top_teams = tibble::tibble(team = c("A", "B", "C")),
    n_draws = n_draws
  )
}

test_that(".compute_team_strength_trajectory exists under its neutral name", {
  expect_true(is.function(.compute_team_strength_trajectory))
})

test_that("the trajectory maps each matchweek to the fit's own round index", {
  cs <- traj_case()

  out <- .compute_team_strength_trajectory(
    fit = cs$fit,
    results = cs$results,
    teams = cs$teams,
    current_top_teams = cs$current_top_teams,
    current_season = 2100L,
    top_div = "BD"
  )

  expect_setequal(
    names(out),
    c("round", ".draw", "team", "component", "location", "value")
  )
  # Three teams x four appearances each.
  expect_equal(sort(unique(out$round)), 1:4)
  expect_setequal(unique(out$team), c("A", "B", "C"))
  expect_setequal(unique(out$component), c("offence", "defence", "total"))
  expect_setequal(unique(out$location), c("home", "away", "avg"))
  # 12 (team, matchweek) pairs x 9 grid cells x n_draws.
  expect_equal(nrow(out), 12L * 9L * cs$n_draws)

  # B is team_nr 2; its third appearance is the fourth match, so the fit row
  # read must be offense[3, 2] = 32, plus home_advantage_off[2] = 2.
  pick <- out[
    out$team == "B" & out$round == 3L &
      out$component == "offence" & out$location == "home",
  ]
  expect_equal(nrow(pick), cs$n_draws)
  expect_equal(unique(pick$value), 34, tolerance = 0)

  # And the away side is the bare parameter.
  away <- out[
    out$team == "B" & out$round == 3L &
      out$component == "offence" & out$location == "away",
  ]
  expect_equal(unique(away$value), 32, tolerance = 0)

  # total/avg is the mean of the two sides of (offence + defence).
  tot <- out[
    out$team == "B" & out$round == 3L &
      out$component == "total" & out$location == "avg",
  ]
  # offence_home 34 + defence_home (132 + 1) + offence_away 32 +
  # defence_away 132, averaged over the two sides.
  expect_equal(unique(tot$value), (34 + 133 + 32 + 132) / 2, tolerance = 0)
})

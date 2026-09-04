# The 2DT extractor used to take a single hardcoded `top_div` ("BD" / "OD"), so
# a two-division publish cell was unreachable: the second tier had no extracts
# at all. This file pins the multi-division contract -- every division-keyed
# parquet carries its own `division`, and the two divisions' league tables are
# computed from their OWN teams and their OWN base points.
#
# The named hazard is `.compute_final_positions_2dt()`, which filters
# `posterior_goals` internally on the division it is handed. A loop that passed
# the wrong `base_points` would still return a plausible-looking table -- one
# division's placements simulated on the other's points -- so the assertions
# below check the team sets are DISJOINT and equal to their own division's, not
# merely that a division column exists.

extract_2dt_cell <- function(sport, sex, env = parent.frame(),
                             extra_results = NULL, extra_schedules = NULL) {
  root <- fixture_facts_root(env = env)
  if (!is.null(extra_results)) {
    write_table(extra_results, "results", root = root)
  }
  if (!is.null(extra_schedules)) {
    write_table(extra_schedules, "schedules", root = root)
  }
  key <- paste0(sport, "_iceland")
  league <- load_leagues()[[key]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(
    withr::local_tempdir(.local_envir = env), "extracts"
  )
  fn <- switch(sport,
    basketball = extract_basketball_iceland,
    handball = extract_handball_iceland
  )
  suppressMessages(fn(
    fit = st$fit, league = league, sex = sex,
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  list(
    root = root, key = key, league = league, st = st, sport = sport, sex = sex,
    partition = file.path(
      extracts_root, paste0("sport=", sport), "country=iceland",
      paste0("sex=", sex),
      paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

read_part <- function(cell, ft) {
  arrow::read_parquet(file.path(cell$partition, paste0(ft, ".parquet")))
}

test_that("every division-keyed 2DT parquet carries the configured divisions", {
  cell <- extract_2dt_cell("basketball", "male")
  divs <- .iceland_division_codes(cell$key, "male")
  expect_setequal(divs, c("BD", "1D"))

  for (ft in c(
    "predicted_matches", "team_strengths_quantiles",
    "home_advantage_quantiles", "final_positions", "points_distribution"
  )) {
    df <- read_part(cell, ft)
    expect_true("division" %in% names(df), info = ft)
    expect_setequal(unique(df$division), divs)
  }
})

test_that("each division's table is simulated on its own teams", {
  cell <- extract_2dt_cell("basketball", "male")

  fp <- read_part(cell, "final_positions")
  fp_bd <- fp[fp$division == "BD", ]
  fp_1d <- fp[fp$division == "1D", ]
  expect_length(intersect(fp_bd$team, fp_1d$team), 0L)
  expect_setequal(
    unique(fp_bd$team), fixture_division_teams("basketball", "male", "BD")
  )
  expect_setequal(
    unique(fp_1d$team), fixture_division_teams("basketball", "male", "1D")
  )
  # Placements run 1..n_teams within the division, not across the cell.
  expect_setequal(unique(fp_bd$placement), seq_len(4L))
  expect_setequal(unique(fp_1d$placement), seq_len(6L))

  pd <- read_part(cell, "points_distribution")
  totals <- pd |>
    dplyr::summarise(p = sum(.data$probability), .by = c("division", "team"))
  expect_equal(totals$p, rep(1, nrow(totals)), tolerance = 1e-8)
  expect_length(
    intersect(
      pd$team[pd$division == "BD"], pd$team[pd$division == "1D"]
    ),
    0L
  )
})

test_that("the handball cell loops over OD and G66 the same way", {
  cell <- extract_2dt_cell("handball", "female")
  divs <- .iceland_division_codes(cell$key, "female")
  expect_setequal(divs, c("OD", "G66"))

  fp <- read_part(cell, "final_positions")
  expect_setequal(unique(fp$division), divs)
  expect_setequal(
    unique(fp$team[fp$division == "OD"]),
    fixture_division_teams("handball", "female", "OD")
  )
  expect_setequal(
    unique(fp$team[fp$division == "G66"]),
    fixture_division_teams("handball", "female", "G66")
  )

  ts <- read_part(cell, "team_strengths_quantiles")
  expect_setequal(unique(ts$division), divs)
  expect_setequal(
    unique(ts$team[ts$division == "G66"]),
    fixture_division_teams("handball", "female", "G66")
  )
})

test_that("a team with no fixture in the window keeps its place in the table", {
  # posterior_goals only covers the model's 14-day prediction window. Keying the
  # league table off it silently dropped every team not playing inside that
  # window -- so a 6-team division published a 4-team final_positions whose
  # probabilities summed to one over the wrong support. The dropped team's
  # realised points still rank it.
  posterior_goals <- tibble::tibble(
    .draw = rep(1:4, each = 2L),
    division = "BD",
    home_team = rep(c("A", "B"), times = 4L),
    away_team = rep(c("B", "A"), times = 4L),
    home_score = 100,
    away_score = 90
  )
  base_points <- tibble::tibble(
    team = c("A", "B", "Z"), base_points = c(2L, 0L, 40L)
  )
  top_teams <- tibble::tibble(team = c("A", "B", "Z"))

  fp <- .compute_final_positions_2dt(
    posterior_goals, "BD", base_points,
    has_ties = FALSE, tie_threshold = 0, current_top_teams = top_teams
  )
  expect_setequal(unique(fp$team), c("A", "B", "Z"))
  expect_setequal(unique(fp$placement), 1:3)
  # Z never plays again but sits 40 points clear, so it wins every draw.
  expect_equal(fp$probability[fp$team == "Z" & fp$placement == 1L], 1)

  pd <- .compute_points_distribution_2dt(
    posterior_goals, "BD", base_points,
    has_ties = FALSE, tie_threshold = 0, current_top_teams = top_teams
  )
  expect_equal(pd$points[pd$team == "Z"], 40)
  expect_equal(pd$probability[pd$team == "Z"], 1)
})

test_that("a division whose fixtures have all been played still gets a table", {
  # The end-of-season case: no upcoming fixture anywhere in this division, but
  # the cell's OTHER division is still playing, so the posterior has draws.
  posterior_goals <- tibble::tibble(
    .draw = 1:4,
    division = "1D",
    home_team = "X", away_team = "Y",
    home_score = 100, away_score = 90
  )
  base_points <- tibble::tibble(team = c("A", "B"), base_points = c(6L, 2L))

  fp <- .compute_final_positions_2dt(
    posterior_goals, "BD", base_points,
    has_ties = FALSE, tie_threshold = 0,
    current_top_teams = tibble::tibble(team = c("A", "B"))
  )
  expect_setequal(unique(fp$team), c("A", "B"))
  expect_equal(fp$probability[fp$team == "A" & fp$placement == 1L], 1)
})

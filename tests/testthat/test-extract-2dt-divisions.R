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
  # write_table() REPLACES a partition, so injected rows must be unioned with
  # what the fixture already wrote rather than written on their own.
  if (!is.null(extra_results)) {
    write_table(
      dplyr::bind_rows(read_table("results", root = root), extra_results),
      "results",
      root = root
    )
  }
  if (!is.null(extra_schedules)) {
    write_table(
      dplyr::bind_rows(read_table("schedules", root = root), extra_schedules),
      "schedules",
      root = root
    )
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

# ---- The regular-season cut (D3) --------------------------------------------
#
# Basketball EMBEDS its urslitakeppni in the league division: KKI packages the
# playoffs as extra rounds inside the SAME season_id
# (R/ingest-kki-basketball.R:23-24), so `division == "BD"` carries both.
# Measured on data/facts/results season 2026: male BD 162 rows of which 132 are
# the 22-round regular season, female BD 137 of which 90 are the 18-round one.
# Without the cut those post-season points feed .compute_base_points_2dt() and
# the published table is simulated on them -- a wrong table, not an error.

bb_postseason_results <- function(rounds = 7:10) {
  teams <- fixture_division_teams("basketball", "male", "BD")
  # BB M BD 04 is the fixture's WEAKEST team (it loses every regular-season
  # match). Three post-season wins plus a fourth over the leader would hand it
  # placement 1 if the playoff rounds reached the table.
  tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2100L,
    match_date = as.Date("2100-01-08") + seq_along(rounds),
    home_team = teams[4L],
    away_team = teams[c(1L, 2L, 3L, 1L)][seq_along(rounds)],
    home_score = 120L,
    away_score = 70L,
    division = "BD",
    round = as.integer(rounds)
  )
}

test_that("played post-season rounds never reach the 2DT league table", {
  teams <- fixture_division_teams("basketball", "male", "BD")
  cell <- extract_2dt_cell(
    "basketball", "male",
    extra_results = bb_postseason_results()
  )

  pd <- read_part(cell, "points_distribution")
  pd_04 <- pd[pd$division == "BD" & pd$team == teams[4L], ]
  # The weakest team finished the regular season on 0 points and has exactly
  # one fixture left in the prediction window, so 2 is its ceiling. With the
  # four playoff rounds counted its BASE alone is 8, and its support is 8..10.
  expect_lte(max(pd_04$points), 2)

  fp <- read_part(cell, "final_positions")
  bd <- fp[fp$division == "BD" & fp$placement == 1L, ]
  # 0 points with a ceiling of 2 cannot win a division whose leader is on 6.
  # Uncounted, the injected playoff wins put it on 8 and it takes the title in
  # 40 % of draws.
  expect_equal(bd$probability[bd$team == teams[4L]], 0)
  expect_gt(bd$probability[bd$team == teams[1L]], 0.5)
})

test_that("upcoming post-season fixtures publish but score no points", {
  teams <- fixture_division_teams("basketball", "male", "BD")
  # The fixture already schedules three BD matches on 16/18/20 Jan. These push
  # the leader from 3 regular-season appearances to 8, well past the
  # 2 * (4 - 1) = 6-round boundary.
  extra <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2100L,
    match_date = as.Date(c(
      "2100-01-22", "2100-01-24", "2100-01-26", "2100-01-28"
    )),
    home_team = teams[1L],
    away_team = teams[c(3L, 4L, 2L, 3L)],
    division = "BD",
    round = c(93L, 94L, 95L, 96L),
    kickoff_time = "19:15"
  )
  cell <- extract_2dt_cell("basketball", "male", extra_schedules = extra)

  pm <- read_part(cell, "predicted_matches")
  pm_bd <- pm[pm$division == "BD", ]
  # A next game is a next game: every scheduled fixture is still published.
  expect_equal(nrow(pm_bd), 7L)
  expect_true(all(
    as.Date(c("2100-01-26", "2100-01-28")) %in% as.Date(pm_bd$match_date)
  ))

  pd <- read_part(cell, "points_distribution")
  pd_01 <- pd[pd$division == "BD" & pd$team == teams[1L], ]
  # 6 realised points plus at most three COUNTING fixtures at 2 points each.
  # Uncapped it is five fixtures, so 16.
  expect_lte(max(pd_01$points), 12)
})

test_that("handball needs no cut because its post-season is its own division", {
  # This is the whole justification for basketball being the only sport that
  # needs the round cut, and it is a property of the federations, not of this
  # code -- so it is asserted against live git-tracked results. If HSI ever
  # folds the playoff back into OD the way KKI folds urslitakeppni into BD,
  # this goes red and handball needs the same treatment.
  results <- read_table("results", root = testthat::test_path("..", "..", "data"))
  hb <- results[results$sport == "handball" & results$country == "iceland", ]
  expect_gt(nrow(hb), 0L)

  for (sex_key in c("male", "female")) {
    codes <- .iceland_division_codes("handball_iceland", sex_key)
    divisions <- unique(hb$division[hb$sex == sex_key])
    # PO exists in the data and is NOT one of the published divisions, so the
    # division filter alone already excludes it.
    expect_true("PO" %in% divisions, info = sex_key)
    expect_false("PO" %in% codes, info = sex_key)
  }
})

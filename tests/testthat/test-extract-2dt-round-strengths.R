# round_strengths_quantiles.parquet for basketball and handball.
#
# An earlier analysis recorded that a per-round strength trajectory was
# football-specific and unreachable for the 2DT sports. That was simply wrong:
# Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164 declares
# `array[N_rounds] vector[K] offense` / `defense` as a deterministic random walk
# (:168-173), and R/model-prepare.R already builds N_rounds / round1 / round2.
# The surface was always there; nothing read it.
#
# The hazard this file exists to catch is the INDEX. The fit's round index is
# each team's cumulative APPEARANCE index over the whole modelled results set --
# not a division matchweek, not a calendar round. A division whose teams entered
# at different global rounds (the fixture's 1D teams have played twice as many
# matches as its BD teams) reads a different `offense[r, k]` per team for the
# same published matchweek, so the two indices are asserted separately below.

rs_cell <- function(sport, sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
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

test_that("both 2DT sports write round_strengths_quantiles.parquet", {
  for (sport in c("basketball", "handball")) {
    cell <- rs_cell(sport, "male")
    path <- file.path(cell$partition, "round_strengths_quantiles.parquet")
    expect_true(file.exists(path), info = sport)

    df <- arrow::read_parquet(path)
    expect_setequal(
      names(df),
      c("round", "team", "component", "location", "quantile", "value", "division")
    )
    expect_setequal(unique(df$division), .iceland_division_codes(cell$key, "male"))
  }
})

test_that("the trajectory covers the full grid with no gaps and no NAs", {
  cell <- rs_cell("basketball", "male")
  df <- arrow::read_parquet(
    file.path(cell$partition, "round_strengths_quantiles.parquet")
  )

  expect_setequal(unique(df$component), c("offence", "defence", "total"))
  expect_setequal(unique(df$location), c("home", "away", "avg"))
  expect_setequal(unique(df$quantile), seq_len(99L))
  expect_false(anyNA(df$value))

  for (div in unique(df$division)) {
    d <- df[df$division == div, ]
    # Rounds run 1..N with no gaps -- a hole means a team's matchweek failed to
    # map onto a fit round.
    expect_equal(sort(unique(d$round)), seq_len(max(d$round)), info = div)
    dupes <- d |>
      dplyr::count(
        .data$round, .data$team, .data$component, .data$location, .data$quantile
      ) |>
      dplyr::filter(.data$n > 1L)
    expect_equal(nrow(dupes), 0L, info = div)
  }
})

# A CONTRACT LOCK, not a TDD cycle: this identity already held before
# round_strengths_quantiles existed, and it is pinned here because it is the
# assumption the whole surface rests on.
test_that("the trajectory's global round index IS prepare_data's round1/round2", {
  # The load-bearing identity. `.compute_team_strength_trajectory()` derives each
  # team's global round with a `row_number()` over its date-ordered matches and
  # uses it to address `offense[r, k]`. R/model-prepare.R:212-221 builds
  # `round1`/`round2` the same way. If the two ever disagree the trajectory reads
  # a neighbouring round and nothing else in the pipeline notices.
  root <- fixture_facts_root()
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root))

  results <- read_table(
    "results",
    root = root,
    filter = list(sport = "basketball", country = "iceland", sex = "male")
  )
  results <- results[
    !is.na(results$match_date) & results$match_date <= FIXTURE_END_DATE, ,
    drop = FALSE
  ]
  results <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
  results <- results[order(results$match_date), , drop = FALSE]
  results$game_nr <- seq_len(nrow(results))

  long <- dplyr::bind_rows(
    dplyr::transmute(results,
      game_nr = .data$game_nr, match_date = .data$match_date,
      team = .data$home_team, side = "home"
    ),
    dplyr::transmute(results,
      game_nr = .data$game_nr, match_date = .data$match_date,
      team = .data$away_team, side = "away"
    )
  ) |>
    dplyr::arrange(.data$team, .data$match_date) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(global_round = dplyr::row_number()) |>
    dplyr::ungroup()

  home <- long[long$side == "home", c("game_nr", "global_round")]
  away <- long[long$side == "away", c("game_nr", "global_round")]
  home <- home[order(home$game_nr), ]
  away <- away[order(away$game_nr), ]

  expect_equal(as.integer(home$global_round), st$prep$stan_data$round1)
  expect_equal(as.integer(away$global_round), st$prep$stan_data$round2)
  expect_equal(
    max(c(home$global_round, away$global_round)),
    st$prep$stan_data$N_rounds
  )
})

test_that("the final matchweek reproduces the fit's own cur_strength", {
  # THE identity: for a team whose appearance count equals the fit's N_rounds,
  # its last published matchweek must read offense[N_rounds, k] -- which is
  # exactly what `cur_strength` is built from
  # (2d_student_t_scalarsigma.stan:279-289). Task 2 made stub_2dt_draws derive
  # cur_* from the walk precisely so this is an equality and not a coincidence.
  cell <- rs_cell("basketball", "male")
  prep <- cell$st$prep
  n_rounds <- prep$stan_data$N_rounds

  df <- arrow::read_parquet(
    file.path(cell$partition, "round_strengths_quantiles.parquet")
  )
  ts <- arrow::read_parquet(
    file.path(cell$partition, "team_strengths_quantiles.parquet")
  )

  appearances <- table(c(
    prep$stan_data$team1, prep$stan_data$team2
  ))
  top_k <- as.integer(names(appearances)[which.max(appearances)])
  # Fail loudly if the fixture's shape changes, rather than silently comparing
  # the wrong quantities at a round the team never reached.
  expect_equal(as.integer(max(appearances)), n_rounds)
  team <- prep$teams$team[top_k]

  traj <- df[
    df$team == team & df$component == "total" & df$location == "avg", ,
    drop = FALSE
  ]
  expect_gt(nrow(traj), 0L)
  last_round <- max(traj$round)
  traj_median <- traj$value[traj$round == last_round & traj$quantile == 50L]

  cur <- ts[
    ts$team == team & ts$component == "total" & ts$location == "avg" &
      ts$quantile == 50L,
  ]
  expect_equal(length(traj_median), 1L)
  expect_equal(traj_median, unique(cur$value), tolerance = 1e-6)
})

test_that("post-season matchweeks are cut out of the published trajectory", {
  # The trajectory is the REGULAR season's. A basketball team's matchweeks past
  # the boundary are bracket games and must not appear, for the same reason they
  # must not reach the league table.
  teams <- fixture_division_teams("basketball", "male", "BD")
  root <- fixture_facts_root()
  extra <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2100L,
    match_date = as.Date("2100-01-08") + 1:4,
    home_team = teams[4L], away_team = teams[c(1L, 2L, 3L, 1L)],
    home_score = 120L, away_score = 70L,
    division = "BD", round = 7:10L
  )
  write_table(
    dplyr::bind_rows(read_table("results", root = root), extra),
    "results",
    root = root
  )

  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root))
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  suppressMessages(extract_basketball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  df <- arrow::read_parquet(file.path(
    extracts_root, "sport=basketball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d")),
    "round_strengths_quantiles.parquet"
  ))

  bd <- df[df$division == "BD", ]
  # 2 * (4 - 1) = 6 regular-season rounds; the injected bracket sits at 7..10.
  expect_lte(max(bd$round), 6L)
  expect_equal(max(bd$round[bd$team == teams[4L]]), 3L)
})

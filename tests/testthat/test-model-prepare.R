# Helper: materialise the mini fixture into a temporary hive-partitioned root,
# mimicking data/facts/results/sport=X/country=Y/sex=Z/season=N/*.parquet
setup_mini_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  results <- arrow::read_parquet(testthat::test_path(
    "fixtures", "model",
    "mini_results.parquet"
  ))
  schedules <- arrow::read_parquet(testthat::test_path(
    "fixtures", "model",
    "mini_schedules.parquet"
  ))
  write_table(results, "results", root = tmp)
  write_table(schedules, "schedules", root = tmp)
  tmp
}

test_that("prepare_data builds a stan_data list with all expected fields", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  out <- prepare_data(league,
    sex = "male", end_date = as.Date("2026-04-24"),
    schedule_horizon_days = 60L, root = root
  )

  expect_type(out, "list")
  expect_named(out, c("stan_data", "pred_d", "teams"), ignore.order = TRUE)

  sd <- out$stan_data
  required <- c(
    "K", "N", "N_pred", "N_rounds", "N_seasons",
    "team1", "team2", "round1", "round2",
    "time_between_matches",
    "goals1", "goals2", "division", "season", "season_first",
    "team1_pred", "team2_pred",
    "pred_timediff1", "pred_timediff2", "pred_division",
    "time_to_next_games", "top_teams", "N_top_teams"
  )
  expect_true(all(required %in% names(sd)),
    info = paste(
      "missing:",
      paste(setdiff(required, names(sd)), collapse = ", ")
    )
  )

  expect_equal(sd$K, 4L) # Alpha, Bravo, Charlie, Delta
  expect_equal(sd$N, 6L) # played matches in fixture
  expect_equal(sd$N_pred, 2L) # scheduled matches
  expect_equal(sd$N_seasons, 3L) # 2024, 2025, 2026
  expect_equal(length(sd$goals1), sd$N)
  expect_equal(length(sd$goals2), sd$N)
  expect_equal(dim(sd$time_between_matches), c(sd$K, sd$N_rounds))
  expect_true(all(sd$time_between_matches >= 0))

  # season_first flags the HOME team's first appearance in its season. Fixture
  # order:
  #   g1 2024 Alpha(H) v Bravo(A)   -> Alpha's first 2024 appearance => 1
  #   g2 2024 Bravo(H) v Charlie(A) -> Bravo already appeared (g1 away) => 0
  #   g3 2025 Charlie(H) v Alpha(A) -> Charlie's first 2025 appearance => 1
  #   g4 2025 Alpha(H) v Delta(A)   -> Alpha already appeared (g3 away) => 0
  #   g5 2026 Bravo(H) v Charlie(A) -> Bravo's first 2026 appearance => 1
  #   g6 2026 Delta(H) v Alpha(A)   -> Delta's first 2026 appearance => 1
  expect_equal(sd$season_first, c(1L, 0L, 1L, 0L, 1L, 1L))
})

test_that("prepare_data returns teams tibble with sequential team_nr", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  out <- prepare_data(league, sex = "male", end_date = as.Date("2026-04-24"), root = root)

  expect_equal(sort(out$teams$team), sort(c("Alpha", "Bravo", "Charlie", "Delta")))
  expect_equal(out$teams$team_nr, seq_len(nrow(out$teams)))
  expect_equal(out$stan_data$K, nrow(out$teams))
})

test_that("prepare_data pred_d has canonical columns and numeric team indices", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  out <- prepare_data(league, sex = "male", end_date = as.Date("2026-04-24"), root = root)

  expect_true(all(c(
    "game_nr", "match_date", "home_team", "away_team",
    "division", "home_nr", "away_nr",
    "home_timediff", "away_timediff"
  ) %in% names(out$pred_d)))
  expect_type(out$pred_d$home_nr, "integer")
  expect_type(out$pred_d$away_nr, "integer")
  expect_true(all(out$pred_d$home_nr <= out$stan_data$K))
  expect_true(all(out$pred_d$away_nr <= out$stan_data$K))
})

test_that("prepare_data filters results by end_date", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  # end_date before some of the fixture matches -> fewer matches
  out <- prepare_data(league,
    sex = "male",
    end_date = as.Date("2025-06-01"), root = root
  )
  expect_equal(out$stan_data$N, 4L) # 2024-10-12, 2024-11-03, 2025-01-15, 2025-02-20
})

test_that("prepare_data respects from_season when supplied", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  out <- prepare_data(league,
    sex = "male",
    end_date = as.Date("2026-04-24"), from_season = 2025L, root = root
  )
  expect_equal(out$stan_data$N, 4L) # drops the two 2024 matches
  expect_equal(out$stan_data$N_seasons, 2L)
})

test_that("prepare_data returns N_pred = 0 when no upcoming schedule matches", {
  root <- setup_mini_root()
  league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  # end_date far in future means schedule rows are all before end_date
  out <- prepare_data(league,
    sex = "male",
    end_date = as.Date("2026-06-30"), root = root
  )
  expect_equal(out$stan_data$N_pred, 0L)
  expect_equal(nrow(out$pred_d), 0L)
})

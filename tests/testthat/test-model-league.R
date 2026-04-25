test_that("fit_league writes beliefs_latest and beliefs_archive", {
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results, "results", root = root)
  write_table(schedules, "schedules", root = root)

  mini_league <- list(
    sport = "basketball", country = "iceland",
    sexes = c("male"), active = TRUE,
    data_source = list(
      results = "kki_basketball",
      schedule = "kki_basketball",
      odds = "lengjan_odds"
    ),
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = rep(Sys.Date() + c(3L, 7L), each = 5L),
    home_team = rep(c("Alpha", "Charlie"), each = 5L),
    away_team = rep(c("Bravo", "Delta"), each = 5L),
    draw_id = rep(1:5, times = 2L),
    home_goals = runif(10, 70, 100),
    away_goals = runif(10, 70, 100)
  )

  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    .package = "sports"
  )

  out <- fit_league(
    league   = mini_league, sex = "male",
    fit_date = Sys.Date(), root = root,
    stan_dir = here::here("Stan")
  )

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10L)
  # Plan 6: fit RDS persisted alongside beliefs/latest/.
  expect_true(file.exists(file.path(
    root, "beliefs", "fits",
    "sport=basketball", "country=iceland", "sex=male", "fit.rds"
  )))

  bl <- read_table("beliefs_latest",
    root = root,
    filter = list(sport = "basketball", country = "iceland", sex = "male")
  )
  expect_equal(nrow(bl), 10L)

  ba <- read_table("beliefs_archive",
    root = root,
    filter = list(sport = "basketball", country = "iceland", sex = "male")
  )
  expect_equal(nrow(ba), 10L)
})

test_that("fit_league (write_archive = FALSE) only writes latest", {
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results, "results", root = root)
  write_table(schedules, "schedules", root = root)

  mini_league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1L, home_goals = 85, away_goals = 80
  )

  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    .package = "sports"
  )

  fit_league(league = mini_league, sex = "male", root = root, write_archive = FALSE)

  expect_equal(nrow(read_table("beliefs_latest", root = root)), 1L)
  expect_false(
    dir.exists(file.path(root, "beliefs", "archive"))
  )
})

test_that("fit_league errors when both league_key and league supplied", {
  expect_error(
    fit_league(league_key = "basketball_iceland", league = list(), sex = "male"),
    "Exactly one of"
  )
})

test_that("fit_league errors when neither league_key nor league supplied", {
  expect_error(
    fit_league(sex = "male"),
    "Exactly one of"
  )
})

test_that("fit_league errors when the Stan model file is missing", {
  mini_league <- list(
    sport = "basketball", country = "iceland", sexes = "male",
    stan_model = "nonexistent/model.stan"
  )
  expect_error(
    fit_league(
      league = mini_league, sex = "male",
      stan_dir = withr::local_tempdir()
    ),
    "Stan model missing"
  )
})

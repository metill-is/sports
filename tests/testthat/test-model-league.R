# The mini schedule fixture is dated when it was cut, so under a wall-clock
# end_date nothing falls inside the 14-day prediction horizon and fit_league()
# now refuses to sample (no fixture inside the horizon -> nothing to predict).
# These tests exercise the write paths with a mocked sampler, so give them a
# fixture inside the horizon, relative to today (time-bomb rule).
.inside_horizon <- function(schedules, end_date = Sys.Date()) {
  schedules$match_date <- end_date + 3L
  schedules
}

test_that("fit_league writes beliefs_latest and beliefs_archive", {
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  write_table(results, "results", root = root)
  write_table(.inside_horizon(schedules), "schedules", root = root)

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
    # `fake_fit` carries only save_object(), so the real extractor would fail
    # on fit$draws(). Since WS3 that failure ABORTS rather than warning, so it
    # is mocked here: this block covers the beliefs write path, and extraction
    # has its own coverage in test-extract-{basketball,handball}-iceland.R and
    # test-extract-2dt-home-advantage-units.R.
    extract_basketball_iceland = function(...) invisible(NULL),
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
  write_table(.inside_horizon(schedules), "schedules", root = root)

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

test_that("fit_league(football iceland) skips beliefs_archive by default", {
  # Phase 3b (2026-05-04): football iceland's per-fit archive moved from
  # the long-form `beliefs/archive/` tree to `extract_football_iceland()`'s
  # summary Parquets under `beliefs/extracts/`. The legacy archive write
  # must be skipped for football iceland to keep storage minimal.
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  # Relabel as football iceland so `is_football_iceland` triggers.
  results$sport <- "football"
  schedules$sport <- "football"
  # mini_results is basketball-scale; bring scores into football range so the
  # value validator accepts the relabelled fixture (fit_model is mocked, so the
  # score magnitudes are incidental to this archive-skip test).
  results$home_score <- results$home_score %% 6L
  results$away_score <- results$away_score %% 6L
  write_table(results, "results", root = root)
  write_table(.inside_horizon(schedules), "schedules", root = root)

  mini_league <- list(
    sport = "football", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1L, home_goals = 2, away_goals = 1
  )
  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    extract_football_iceland = function(...) invisible(NULL),
    .package = "sports"
  )

  fit_league(league = mini_league, sex = "male", root = root)

  expect_equal(nrow(read_table("beliefs_latest", root = root)), 1L)
  expect_false(
    dir.exists(file.path(
      root, "beliefs", "archive",
      "sport=football", "country=iceland", "sex=male"
    ))
  )
})

test_that("fit_league(football iceland, force_archive_write = TRUE) bypasses the skip", {
  # The backfill override (scripts/03c_backfill_football_archive_2026_05.R)
  # passes force_archive_write = TRUE to fill the historical archive gap.
  # This must override the football-iceland skip introduced in Phase 3b.
  root <- withr::local_tempdir()
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "model", "mini_schedules.parquet")
  )
  results$sport <- "football"
  schedules$sport <- "football"
  # mini_results is basketball-scale; bring scores into football range so the
  # value validator accepts the relabelled fixture (fit_model is mocked, so the
  # score magnitudes are incidental to this archive-skip test).
  results$home_score <- results$home_score %% 6L
  results$away_score <- results$away_score %% 6L
  write_table(results, "results", root = root)
  write_table(.inside_horizon(schedules), "schedules", root = root)

  mini_league <- list(
    sport = "football", country = "iceland", sexes = "male",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )

  fake_beliefs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    fit_date = Sys.Date(),
    match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    draw_id = 1L, home_goals = 2, away_goals = 1
  )
  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) fake_beliefs,
    extract_football_iceland = function(...) invisible(NULL),
    .package = "sports"
  )

  fit_league(
    league = mini_league, sex = "male", root = root,
    force_archive_write = TRUE
  )

  ba <- read_table("beliefs_archive",
    root = root,
    filter = list(sport = "football", country = "iceland", sex = "male")
  )
  expect_equal(nrow(ba), 1L)
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

test_that("fit_league ABORTS when the extractor fails, rather than warning", {
  # WS3: a swallowed extract failure means the cell silently stops publishing,
  # because the extracts tree is the sole publish input. Beliefs are written
  # before the extract runs, so aborting costs nothing already computed.
  root <- withr::local_tempdir()
  mini_league <- list(
    sport = "basketball", country = "iceland",
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan",
    betting = list(scoring = list(has_ties = FALSE, tie_threshold = 0))
  )
  write_table(tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    season = 2100L, division = "BD",
    match_date = as.Date("2100-01-02") + 0:9,
    round = 1L,
    home_team = rep(c("A", "B"), 5), away_team = rep(c("B", "A"), 5),
    home_score = 80, away_score = 75
  ), "results", root = root)
  # One fixture inside the 14-day horizon after the test's end_date, so
  # fit_league() has something to predict and reaches the (mocked) extractor
  # instead of refusing to sample.
  write_table(tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male", season = 2100L,
    match_date = as.Date("2100-01-14"), home_team = "A", away_team = "B",
    division = "BD", round = 6L, kickoff_time = "19:15"
  ), "schedules", root = root)

  fake_fit <- structure(
    list(save_object = function(file) saveRDS(NULL, file)),
    class = "CmdStanMCMC"
  )
  testthat::local_mocked_bindings(
    fit_model = function(...) fake_fit,
    extract_posteriors = function(...) tibble::tibble(
      sport = "basketball", country = "iceland", sex = "male",
      fit_date = as.Date("2100-01-11"),
      match_date = as.Date("2100-02-01"),
      home_team = "A", away_team = "B", draw_id = 1:10L,
      home_goals = runif(10, 70, 100), away_goals = runif(10, 70, 100)
    ),
    extract_basketball_iceland = function(...) stop("simulated extract failure"),
    .package = "sports"
  )

  expect_error(
    fit_league(
      league = mini_league, sex = "male",
      fit_date = as.Date("2100-01-11"), root = root,
      stan_dir = here::here("Stan")
    ),
    "extract_basketball_iceland"
  )

  # Beliefs survived the abort -- the fit's work is not thrown away.
  expect_gt(nrow(read_table("beliefs_latest", root = root)), 0L)
})

# ---- A fit with nothing to predict refuses BEFORE sampling ---------------------

test_that("fit_league aborts before sampling when no fixture is inside the horizon", {
  # 2026-09-05: a forced basketball fit sampled for 100 minutes, then the
  # extractor failed on "Can't find goals1_pred, goals2_pred" -- the model had
  # been given N_pred = 0 because basketball's first fixture (29 Sept) lay
  # beyond the 14-day horizon. prepare_data() knew that before a single draw.
  env <- environment()
  root <- fixture_facts_root(env)
  schedules <- read_table("schedules", root = root)
  # Push every handball male fixture well past the horizon (time-bomb rule:
  # relative to the end_date the test passes, not to the wall clock).
  hit <- schedules$sport == "handball" & schedules$sex == "male"
  schedules$match_date[hit] <- as.Date("2100-03-01")
  write_table(schedules, "schedules", root = root)
  league <- load_leagues()[["handball_iceland"]]
  started <- Sys.time()
  expect_error(
    suppressMessages(fit_league(
      league = league, sex = "male", end_date = as.Date("2100-01-15"),
      root = root, write_archive = FALSE
    )),
    "no fixture inside the .*horizon"
  )
  # Refusing must be immediate: a guard that lets sampling start first is not
  # the guard this test is for.
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 60)
  expect_false(dir.exists(file.path(root, "beliefs", "latest", "sport=handball")))
})

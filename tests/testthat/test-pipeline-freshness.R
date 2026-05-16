test_that("needs_refit() is TRUE when no fit exists", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  expect_true(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is FALSE when fit covers all completed games", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  # needs_refit() also checks beliefs/latest/ — production state has both.
  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is TRUE when a new game was played after last fit", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "C"), away_team = c("B", "D"),
      match_date = as.Date(c("2026-04-29", "2026-05-02")),
      home_score = c(1L, 2L), away_score = c(0L, 1L),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_true(needs_refit(static, "male", root = root))
})

test_that("needs_refit() ignores unplayed games", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "C"), away_team = c("B", "D"),
      match_date = as.Date(c("2026-04-29", "2026-05-15")),
      home_score = c(1L, NA_integer_),
      away_score = c(0L, NA_integer_),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() returns TRUE when beliefs/latest/ is wiped despite archive history", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  # archive has a fit_date but latest/ is empty (post-2026-05-15 defence-in-
  # depth against latest/-wipe scenarios) — refit forced.
  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_true(needs_refit(static, "male", root = root))
})

test_that("has_upcoming_games() filters by horizon", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  sched_dir <- fs::path(
    root, "facts", "schedules",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(sched_dir)
  today <- Sys.Date()
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "B", "C"), away_team = c("X", "Y", "Z"),
      match_date = c(today + 3L, today + 30L, today - 1L),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(sched_dir, "part-0.parquet")
  )

  expect_true(has_upcoming_games(static, "male", root = root, days = 14L))
  expect_false(has_upcoming_games(static, "male", root = root, days = 1L))
})

test_that("has_upcoming_games() returns FALSE when schedule dir missing", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")
  expect_false(has_upcoming_games(static, "male", root = root))
})

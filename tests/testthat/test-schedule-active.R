test_that("generate_active_competitions marks leagues with future fixtures active", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "data"), recursive = TRUE)

  fake_schedules <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = Sys.Date() + 3L,
    home_team = "KR", away_team = "Valur",
    division = NA_character_,
    round = NA_integer_,
    kickoff_time = NA_character_
  )
  write_table(fake_schedules, "schedules", root = file.path(tmp, "data"))

  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      sexes = c("male", "female"), active = TRUE
    ),
    basketball_iceland = list(
      sport = "basketball", country = "iceland",
      sexes = c("male", "female"), active = TRUE
    )
  )

  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)
  generate_active_competitions(leagues,
    lookahead_days = 7L,
    root = file.path(tmp, "data"),
    out_path = out_path
  )

  res <- jsonlite::fromJSON(out_path)
  expect_true(res$active$football_iceland)
  expect_false(res$active$basketball_iceland)
  expect_equal(res$lookahead_days, 7L)
})

test_that("generate_active_competitions defaults to all-active on missing data", {
  tmp <- withr::local_tempdir()
  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)

  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      sexes = c("male", "female"), active = TRUE
    )
  )
  generate_active_competitions(leagues,
    lookahead_days = 7L,
    root = file.path(tmp, "data-missing"),
    out_path = out_path
  )

  res <- jsonlite::fromJSON(out_path)
  expect_true(res$active$football_iceland)
})

test_that("generate_active_competitions ignores past fixtures", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "data"), recursive = TRUE)

  fake <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male", season = 2026L,
    match_date = Sys.Date() - 7L,
    home_team = "KR", away_team = "Valur",
    division = NA_character_,
    round = NA_integer_,
    kickoff_time = NA_character_
  )
  write_table(fake, "schedules", root = file.path(tmp, "data"))

  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      sexes = c("male", "female"), active = TRUE
    )
  )
  out_path <- file.path(tmp, "config", "active_competitions.json")
  dir.create(dirname(out_path), recursive = TRUE)
  generate_active_competitions(leagues,
    lookahead_days = 7L,
    root = file.path(tmp, "data"),
    out_path = out_path
  )

  res <- jsonlite::fromJSON(out_path)
  expect_false(res$active$football_iceland)
})

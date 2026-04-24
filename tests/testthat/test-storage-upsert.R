make_result_row <- function(...) {
  defaults <- list(
    sport = "handball", country = "iceland", sex = "male",
    season = 2025L,
    match_date = as.Date("2025-01-15"),
    home_team = "Valur", away_team = "FH",
    home_score = 30L, away_score = 28L,
    division = "OD", round = NA_integer_
  )
  tibble::as_tibble(modifyList(defaults, list(...)))
}

test_that("upsert_table writing same frame twice does not duplicate rows", {
  tmp <- withr::local_tempdir()
  df <- dplyr::bind_rows(
    make_result_row(home_team = "Valur", away_team = "FH"),
    make_result_row(
      home_team = "Haukar", away_team = "IR",
      match_date = as.Date("2025-01-16")
    )
  )

  upsert_table(df, "results", root = tmp)
  upsert_table(df, "results", root = tmp)

  back <- read_table("results", root = tmp)
  expect_equal(nrow(back), 2L)
})

test_that("upsert_table preserves rows in partition absent from new frame", {
  tmp <- withr::local_tempdir()

  # First write: OD division + G66 division for same (sex, season) partition
  # (they share partition columns: sport/country/sex/season).
  initial <- dplyr::bind_rows(
    make_result_row(division = "OD", home_team = "Valur", away_team = "FH"),
    make_result_row(
      division = "G66", home_team = "Thor", away_team = "UMFT",
      match_date = as.Date("2025-01-20")
    )
  )
  upsert_table(initial, "results", root = tmp)

  # Second write: only OD rows — G66 row should be preserved.
  od_only <- make_result_row(
    division = "OD", home_team = "Valur", away_team = "FH"
  )
  upsert_table(od_only, "results", root = tmp)

  back <- read_table("results", root = tmp)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$division, c("OD", "G66"))
})

test_that("upsert_table does not touch partitions absent from the new frame", {
  tmp <- withr::local_tempdir()

  season_2024 <- make_result_row(season = 2024L)
  season_2025 <- make_result_row(season = 2025L)

  upsert_table(dplyr::bind_rows(season_2024, season_2025), "results", root = tmp)

  # Re-write only 2025 — 2024 partition must stay intact.
  upsert_table(season_2025, "results", root = tmp)

  back <- read_table("results", root = tmp)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$season, c(2024L, 2025L))
})

test_that("upsert_table dedupes on natural match key within a partition", {
  tmp <- withr::local_tempdir()

  # Initial row with one score.
  upsert_table(
    make_result_row(home_score = 25L, away_score = 22L),
    "results",
    root = tmp
  )

  # Re-upsert same match with corrected score — new row wins.
  upsert_table(
    make_result_row(home_score = 30L, away_score = 28L),
    "results",
    root = tmp
  )

  back <- read_table("results", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_score, 30L)
  expect_equal(back$away_score, 28L)
})

test_that("upsert_table is a no-op for a zero-row frame", {
  tmp <- withr::local_tempdir()
  upsert_table(make_result_row(), "results", root = tmp)

  empty <- make_result_row()[0, , drop = FALSE]
  expect_no_error(upsert_table(empty, "results", root = tmp))

  back <- read_table("results", root = tmp)
  expect_equal(nrow(back), 1L)
})

test_that("upsert_table round-trips schedules with matching dedup semantics", {
  make_sched <- function(...) {
    defaults <- list(
      sport = "handball", country = "iceland", sex = "male",
      season = 2025L,
      match_date = as.Date("2025-03-01"),
      home_team = "Valur", away_team = "FH",
      division = "OD", round = NA_integer_
    )
    tibble::as_tibble(modifyList(defaults, list(...)))
  }

  tmp <- withr::local_tempdir()
  upsert_table(make_sched(), "schedules", root = tmp)
  upsert_table(make_sched(), "schedules", root = tmp)

  back <- read_table("schedules", root = tmp)
  expect_equal(nrow(back), 1L)
})

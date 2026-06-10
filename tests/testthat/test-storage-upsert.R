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

test_that("upsert_table on odds preserves all daily snapshots (intra-day scrapes)", {
  # Pre-2026-05-15, the scrape-odds workflow's three daily runs each wrote
  # to the same (sport, country, scraped_date) partition via write_table,
  # which uses Arrow's existing_data_behavior='overwrite'. The 14:00 scrape
  # silently deleted the 08:00 snapshot, and the 20:00 scrape deleted both.
  # Switching to upsert_table with scraped_at in the natural key makes the
  # three scrapes accrete instead of overwrite. Audit 2026-05-15 §F.
  make_odds <- function(scraped_at, market = "moneyline", outcome = "home",
                        odds = 2.0, line = NA_real_) {
    tibble::tibble(
      sport      = "football",
      country    = "iceland",
      scraped_at = scraped_at,
      match_date = as.Date("2026-05-20"),
      home_team  = "KR",
      away_team  = "Vikingur",
      market     = market,
      outcome    = outcome,
      line       = line,
      odds       = odds
    )
  }

  tmp <- withr::local_tempdir()
  scrape_08 <- make_odds(as.POSIXct("2026-05-15 08:00:00", tz = "UTC"), odds = 2.10)
  scrape_14 <- make_odds(as.POSIXct("2026-05-15 14:00:00", tz = "UTC"), odds = 2.05)
  scrape_20 <- make_odds(as.POSIXct("2026-05-15 20:00:00", tz = "UTC"), odds = 1.98)

  upsert_table(scrape_08, "odds", root = tmp)
  upsert_table(scrape_14, "odds", root = tmp)
  upsert_table(scrape_20, "odds", root = tmp)

  back <- read_table("odds", root = tmp)
  expect_equal(nrow(back), 3L)
  expect_setequal(round(back$odds, 2), c(2.10, 2.05, 1.98))
})

test_that("upsert_table on odds dedupes only on same scraped_at re-runs", {
  # If a test or recovery script re-runs the exact same scrape (same
  # scraped_at), the new row should win and not duplicate.
  make_odds <- function(odds, scraped_at = as.POSIXct("2026-05-15 08:00:00", tz = "UTC")) {
    tibble::tibble(
      sport = "football", country = "iceland",
      scraped_at = scraped_at, match_date = as.Date("2026-05-20"),
      home_team = "KR", away_team = "Vikingur",
      market = "moneyline", outcome = "home",
      line = NA_real_, odds = odds
    )
  }

  tmp <- withr::local_tempdir()
  upsert_table(make_odds(2.10), "odds", root = tmp)
  upsert_table(make_odds(2.05), "odds", root = tmp) # same scraped_at, corrected odds

  back <- read_table("odds", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$odds, 2.05) # new row wins
})

test_that("upsert_table round-trips schedules with matching dedup semantics", {
  make_sched <- function(...) {
    defaults <- list(
      sport = "handball", country = "iceland", sex = "male",
      season = 2025L,
      match_date = as.Date("2025-03-01"),
      home_team = "Valur", away_team = "FH",
      division = "OD", round = NA_integer_, kickoff_time = NA_character_
    )
    tibble::as_tibble(modifyList(defaults, list(...)))
  }

  tmp <- withr::local_tempdir()
  upsert_table(make_sched(), "schedules", root = tmp)
  upsert_table(make_sched(), "schedules", root = tmp)

  back <- read_table("schedules", root = tmp)
  expect_equal(nrow(back), 1L)
})

# ---- schedules snapshot retraction (reschedule ghosts) -----------------------
# A fixture's natural key includes match_date, so when the federation moves a
# match to a new date the old-dated row no longer collides on the key and the
# plain upsert keeps both forever (the "ghost fixture" bug: Fylkir-Grótta
# published at both 2026-06-10 and 2026-06-13). `snapshot_future_by` treats
# the incoming frame as the authoritative statement of future fixtures for
# every scope-combo (e.g. division) it covers: existing future rows in those
# scopes that the new frame no longer asserts are retracted.

make_sched_row <- function(...) {
  defaults <- list(
    sport = "football", country = "iceland", sex = "male",
    season = 2026L,
    match_date = as.Date("2026-06-13"),
    home_team = "Fylkir", away_team = "Grotta",
    division = "CUP", round = NA_integer_, kickoff_time = NA_character_
  )
  tibble::as_tibble(modifyList(defaults, list(...)))
}

test_that("upsert_table snapshot_future_by retracts a rescheduled fixture's old row", {
  tmp <- withr::local_tempdir()

  # Day 1 scrape: the tie is scheduled for Jun 10.
  upsert_table(
    make_sched_row(match_date = as.Date("2026-06-10")),
    "schedules",
    root = tmp
  )

  # Day 2 scrape: KSI moved the tie to Jun 13; the scrape no longer
  # asserts the Jun 10 row.
  upsert_table(
    make_sched_row(match_date = as.Date("2026-06-13")),
    "schedules",
    root = tmp,
    snapshot_future_by = "division",
    snapshot_cutoff = as.Date("2026-06-09")
  )

  back <- read_table("schedules", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$match_date, as.Date("2026-06-13"))
})

test_that("snapshot retraction keeps past rows and divisions absent from the new frame", {
  tmp <- withr::local_tempdir()

  seed <- dplyr::bind_rows(
    # Played-date CUP row (history; must survive).
    make_sched_row(match_date = as.Date("2026-06-01")),
    # Future CUP ghost (incl. a TBD placeholder fossil; must be swept).
    make_sched_row(
      match_date = as.Date("2026-06-24"),
      home_team = "Undanúrslit", away_team = "."
    ),
    # Future BD row: division not covered by the new frame; must survive.
    make_sched_row(
      match_date = as.Date("2026-06-15"),
      home_team = "KR", away_team = "Valur", division = "BD"
    )
  )
  upsert_table(seed, "schedules", root = tmp)

  upsert_table(
    make_sched_row(match_date = as.Date("2026-06-22")),
    "schedules",
    root = tmp,
    snapshot_future_by = "division",
    snapshot_cutoff = as.Date("2026-06-09")
  )

  back <- read_table("schedules", root = tmp)
  expect_equal(nrow(back), 3L)
  got <- paste(back$division, back$match_date)
  expect_setequal(
    got,
    c("CUP 2026-06-01", "BD 2026-06-15", "CUP 2026-06-22")
  )
})

test_that("snapshot retraction defaults are inert (plain upsert unchanged)", {
  tmp <- withr::local_tempdir()
  upsert_table(
    make_sched_row(match_date = as.Date("2026-06-10")),
    "schedules",
    root = tmp
  )
  upsert_table(
    make_sched_row(match_date = as.Date("2026-06-13")),
    "schedules",
    root = tmp
  )
  back <- read_table("schedules", root = tmp)
  expect_equal(nrow(back), 2L) # legacy union semantics preserved
})

make_ledger_row <- function(...) {
  defaults <- list(
    placed_at = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    match_date = as.Date("2026-04-24"),
    sport = "basketball", country = "iceland", sex = "male",
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds_placed = 2.10, p = 0.55,
    kelly = 0.02, bet_amount = 500,
    settled = FALSE, win = NA, pnl = NA_real_
  )
  tibble::as_tibble(modifyList(defaults, list(...)))
}

test_that("write_table writes Parquet and round-trips via read_table", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row()

  write_table(row, table = "ledger", root = tmp)

  back <- read_table(table = "ledger", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_equal(back$bet_amount, 500)
})

test_that("write_table rejects rows missing a required column", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row()
  row$odds_placed <- NULL

  expect_error(write_table(row, table = "ledger", root = tmp),
    regexp = "odds_placed"
  )
})

test_that("write_table rejects wrong type for a column", {
  tmp <- withr::local_tempdir()
  row <- make_ledger_row(odds_placed = "2.10") # string not double

  expect_error(write_table(row, table = "ledger", root = tmp),
    regexp = "odds_placed"
  )
})

test_that("write_table partitions by the table's partition columns", {
  tmp <- withr::local_tempdir()
  rows <- dplyr::bind_rows(
    make_ledger_row(sport = "basketball"),
    make_ledger_row(sport = "handball")
  )

  write_table(rows, table = "ledger", root = tmp)

  dirs <- fs::dir_ls(fs::path(tmp, "decisions", "ledger"), type = "directory")
  expect_true(any(grepl("sport=basketball", dirs)))
  expect_true(any(grepl("sport=handball", dirs)))
})

test_that("read_table with predicate filters before materialising", {
  tmp <- withr::local_tempdir()
  rows <- dplyr::bind_rows(
    make_ledger_row(sport = "basketball"),
    make_ledger_row(sport = "handball")
  )
  write_table(rows, table = "ledger", root = tmp)

  b <- read_table(table = "ledger", root = tmp, filter = list(sport = "basketball"))
  expect_equal(nrow(b), 1L)
  expect_equal(b$sport, "basketball")
})

test_that("write_table round-trips odds via scraped_date virtual partition", {
  tmp <- withr::local_tempdir()
  row <- tibble::tibble(
    sport = "basketball", country = "iceland",
    scraped_at = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    match_date = as.Date("2026-04-24"),
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.10
  )
  write_table(row, "odds", root = tmp)
  back <- read_table("odds", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_s3_class(back$scraped_at, "POSIXct")
})

test_that("write_table round-trips candidates via run_date virtual partition", {
  tmp <- withr::local_tempdir()
  row <- tibble::tibble(
    run_id = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    sport = "basketball", country = "iceland", sex = "male",
    match_date = as.Date("2026-04-24"),
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, p = 0.55, odds = 2.10,
    ev = 0.155, kelly_raw = 0.02, stage = "candidate"
  )
  write_table(row, "candidates", root = tmp)
  back <- read_table("candidates", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_s3_class(back$run_id, "POSIXct")
})

test_that("write_table round-trips recommendations via run_date virtual partition", {
  tmp <- withr::local_tempdir()
  row <- tibble::tibble(
    run_id = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    sport = "basketball", country = "iceland", sex = "male",
    match_date = as.Date("2026-04-24"),
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, p = 0.55, odds = 2.10,
    ev = 0.155, kelly = 0.02, bet_amount = 500
  )
  write_table(row, "recommendations", root = tmp)
  back <- read_table("recommendations", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_s3_class(back$run_id, "POSIXct")
})

test_that("write_table errors clearly on unknown table", {
  expect_error(
    write_table(tibble::tibble(x = 1), "unknown"),
    regexp = "Unknown table"
  )
})

test_that("write_table preserves old partition data when arrow write fails", {
  # Atomicity: pre-2026-05-15 the bare write_table called write_dataset with
  # existing_data_behavior='overwrite', which deletes the partition's old
  # files BEFORE writing new ones. A process kill in that window would leave
  # the partition empty -- bitten by beliefs/latest/ after a SIGKILL'd Stan
  # fit. The staged-rename fix should leave existing data intact when the
  # new write fails. Audit 2026-05-15 §H.
  tmp <- withr::local_tempdir()
  good <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-15 08:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-20"),
    home_team = "KR", away_team = "Vikingur",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.10
  )
  write_table(good, "odds", root = tmp)
  expect_equal(nrow(read_table("odds", root = tmp)), 1L)

  testthat::local_mocked_bindings(
    write_dataset = function(...) stop("simulated mid-write crash"),
    .package = "arrow"
  )
  bad <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-15 14:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-20"),
    home_team = "KR", away_team = "Vikingur",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.05
  )
  expect_error(write_table(bad, "odds", root = tmp), "simulated mid-write crash")

  back <- read_table("odds", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$odds, 2.10)
})

test_that("write_table cleans up staging directory on success", {
  tmp <- withr::local_tempdir()
  df <- tibble::tibble(
    sport = "football", country = "iceland",
    scraped_at = as.POSIXct("2026-05-15 08:00:00", tz = "UTC"),
    match_date = as.Date("2026-05-20"),
    home_team = "KR", away_team = "Vikingur",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds = 2.10
  )
  write_table(df, "odds", root = tmp)
  stage_siblings <- list.files(
    fs::path(tmp, "facts"),
    pattern = "\\.stage\\."
  )
  expect_length(stage_siblings, 0L)
})

test_that("beliefs_archive without fit_date errors", {
  tmp <- withr::local_tempdir()
  row <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = "male",
    match_date = as.Date("2026-04-24"),
    home_team = "KR", away_team = "Stjarnan",
    draw_id = 1L, home_goals = 2.1, away_goals = 1.5
  )
  expect_error(
    write_table(row, "beliefs_archive", root = tmp),
    regexp = "fit_date"
  )
})

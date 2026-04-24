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

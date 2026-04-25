mk_ledger_row <- function(home = "KR", odds_placed = 1.85, bet_amount = 1180) {
  tibble::tibble(
    placed_at = Sys.time(),
    match_date = as.Date("2026-04-26"),
    sport = "football", country = "iceland", sex = "male",
    home_team = home, away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    odds_placed = odds_placed, p = 0.55, kelly = 0.05,
    bet_amount = bet_amount,
    settled = FALSE, win = NA, pnl = NA_real_
  )
}

test_that("append_to_ledger writes a new bet to Parquet", {
  tmp <- withr::local_tempdir()
  bet <- mk_ledger_row()
  append_to_ledger(bet, root = tmp, dual_write_csv = FALSE)

  back <- read_table("ledger", root = tmp)
  expect_equal(nrow(back), 1L)
  expect_equal(back$home_team, "KR")
  expect_equal(back$bet_amount, 1180)
})

test_that("append_to_ledger writes Parquet only by default (no CSV)", {
  tmp <- withr::local_tempdir()
  bet <- mk_ledger_row()

  # Default call: no dual_write_csv argument. Plan 6 cutover flipped the
  # default from TRUE -> FALSE, so this must NOT touch the legacy CSV path.
  append_to_ledger(bet, root = tmp)

  expect_true(dir.exists(file.path(tmp, "decisions", "ledger")))
  back <- read_table("ledger", root = tmp)
  expect_equal(nrow(back), 1L)

  legacy_csv_path <- normalizePath(
    file.path(
      tmp, "..", "_legacy", "sports",
      bet$sport, bet$country, "history", "bets_log.csv"
    ),
    mustWork = FALSE
  )
  expect_false(file.exists(legacy_csv_path))
})

test_that("append_to_ledger preserves existing rows on append", {
  tmp <- withr::local_tempdir()
  append_to_ledger(mk_ledger_row(home = "KR"), root = tmp, dual_write_csv = FALSE)
  append_to_ledger(mk_ledger_row(home = "Fram"), root = tmp, dual_write_csv = FALSE)

  back <- read_table("ledger", root = tmp)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$home_team, c("KR", "Fram"))
})

test_that("append_to_ledger CSV dual-write writes the legacy path", {
  tmp <- withr::local_tempdir()
  legacy <- withr::local_tempdir()
  bet <- mk_ledger_row()
  append_to_ledger(bet,
    root = tmp, dual_write_csv = TRUE,
    legacy_root = legacy
  )

  csv_path <- file.path(legacy, "football", "iceland", "history", "bets_log.csv")
  expect_true(file.exists(csv_path))
  back <- readr::read_csv(csv_path, show_col_types = FALSE)
  expect_equal(nrow(back), 1L)
})

test_that("append_to_ledger CSV dual-write APPENDS to an existing file", {
  tmp <- withr::local_tempdir()
  legacy <- withr::local_tempdir()
  csv_dir <- file.path(legacy, "football", "iceland", "history")
  dir.create(csv_dir, recursive = TRUE)
  csv_path <- file.path(csv_dir, "bets_log.csv")

  pre_existing <- mk_ledger_row(home = "Stjarnan")
  readr::write_csv(pre_existing, csv_path)

  append_to_ledger(mk_ledger_row(home = "KR"),
    root = tmp, dual_write_csv = TRUE,
    legacy_root = legacy
  )

  back <- readr::read_csv(csv_path, show_col_types = FALSE)
  expect_equal(nrow(back), 2L)
  expect_setequal(back$home_team, c("Stjarnan", "KR"))
})

test_that("append_to_ledger errors on missing required columns", {
  tmp <- withr::local_tempdir()
  bad <- tibble::tibble(sport = "football", country = "iceland")
  expect_error(
    append_to_ledger(bad, root = tmp, dual_write_csv = FALSE),
    "missing"
  )
})

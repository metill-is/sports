test_that("rebuild_duckdb creates a file with views over Parquet", {
  tmp <- withr::local_tempdir()
  db_path <- fs::path(tmp, "sports.duckdb")

  # Seed a small ledger Parquet
  row <- tibble::tibble(
    placed_at = as.POSIXct("2026-04-24 10:00:00", tz = "UTC"),
    match_date = as.Date("2026-04-24"),
    sport = "basketball", country = "iceland", sex = "male",
    home_team = "KR", away_team = "Stjarnan",
    market = "moneyline", outcome = "home",
    line = NA_real_, odds_placed = 2.10, p = 0.55,
    kelly = 0.02, bet_amount = 500,
    settled = TRUE, win = TRUE, pnl = 550
  )
  write_table(row, "ledger", root = tmp)

  rebuild_duckdb(root = tmp, db_path = db_path)

  expect_true(file.exists(db_path))

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  views <- DBI::dbGetQuery(con, "SELECT view_name FROM duckdb_views() WHERE schema_name = 'main'")$view_name
  expect_true("ledger" %in% views)

  sum_pnl <- DBI::dbGetQuery(con, "SELECT SUM(pnl) AS pnl FROM ledger WHERE settled")$pnl
  expect_equal(sum_pnl, 550)
})

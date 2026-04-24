# tests/testthat/test-etl-validation.R
#
# Integration tests: validate that the ETL in data/ matches the legacy
# sources in _legacy/. Skips if _legacy/ is absent (e.g. on CI).

skip_if_no_legacy <- function() {
  if (!dir.exists(here::here("_legacy", "sports"))) {
    testthat::skip("_legacy/sports/ absent; ETL hasn't run yet")
  }
}

iceland_legacy_ledgers <- function() {
  list(
    basketball_iceland = here::here("_legacy", "sports", "basketball", "iceland", "history", "bets_log.csv"),
    handball_iceland   = here::here("_legacy", "sports", "handball", "iceland", "history", "bets_log.csv"),
    football_iceland   = here::here("_legacy", "sports", "football", "iceland", "history", "bets_log.csv")
  )
}

all_legacy_ledgers <- function() {
  files <- list.files(here::here("_legacy", "sports"),
    pattern = "bets_log\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  setNames(files, basename(dirname(dirname(files))))
}

test_that("Parquet ledger total row count matches legacy (modulo sex=all)", {
  skip_if_no_legacy()

  legacy_files <- list.files(here::here("_legacy", "sports"),
    pattern = "bets_log\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  legacy_rows <- sum(vapply(legacy_files, function(f) {
    df <- readr::read_csv(f, show_col_types = FALSE)
    sum(df$sex != "all", na.rm = TRUE) + sum(is.na(df$sex))
  }, integer(1)))

  parquet_rows <- nrow(read_table("ledger"))
  expect_equal(parquet_rows, legacy_rows,
    info = sprintf(
      "parquet=%d legacy(non-all)=%d",
      parquet_rows, legacy_rows
    )
  )
})

test_that("Parquet ledger total PnL matches legacy to 0.01 ISK", {
  skip_if_no_legacy()

  legacy_files <- list.files(here::here("_legacy", "sports"),
    pattern = "bets_log\\.csv$",
    recursive = TRUE, full.names = TRUE
  )
  legacy_pnl <- sum(vapply(legacy_files, function(f) {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df <- df[df$sex != "all" | is.na(df$sex), ]
    sum(df$pnl, na.rm = TRUE)
  }, numeric(1)))

  parquet_pnl <- sum(read_table("ledger")$pnl, na.rm = TRUE)
  expect_equal(parquet_pnl, legacy_pnl,
    tolerance = 0.01,
    info = sprintf(
      "parquet=%.2f legacy=%.2f",
      parquet_pnl, legacy_pnl
    )
  )
})

test_that("Per-league PnL totals match legacy for each ledger file", {
  skip_if_no_legacy()

  legacy_files <- list.files(here::here("_legacy", "sports"),
    pattern = "bets_log\\.csv$",
    recursive = TRUE, full.names = TRUE
  )

  for (f in legacy_files) {
    # Path layout: .../_legacy/sports/{sport}/{country}/history/bets_log.csv
    # Walk from the filename end — more robust than `which(parts == "sports")`,
    # which double-matches because the legacy sub-repo directory is also called
    # "sports".
    parts <- strsplit(f, "/")[[1]]
    n <- length(parts)
    sport_path <- parts[n - 3]
    country_path <- parts[n - 2]

    legacy_df <- readr::read_csv(f, show_col_types = FALSE)
    legacy_df <- legacy_df[legacy_df$sex != "all" | is.na(legacy_df$sex), ]
    legacy_pnl <- sum(legacy_df$pnl, na.rm = TRUE)

    parquet_pnl <- sum(
      read_table("ledger",
        filter = list(sport = sport_path, country = country_path)
      )$pnl,
      na.rm = TRUE
    )

    expect_equal(parquet_pnl, legacy_pnl,
      tolerance = 0.01,
      info = sprintf(
        "%s/%s: parquet=%.2f legacy=%.2f",
        sport_path, country_path,
        parquet_pnl, legacy_pnl
      )
    )
  }
})

test_that("Odds Parquet has rows for all 3 Icelandic sports", {
  skip_if_no_legacy()

  for (sport in c("basketball", "handball", "football")) {
    o <- read_table("odds", filter = list(sport = sport))
    expect_gt(nrow(o), 0,
      label = sprintf("odds for sport=%s", sport)
    )
  }
})

test_that("DuckDB view ledger SUM(pnl) matches Parquet R read", {
  skip_if_no_legacy()

  rebuild_duckdb()
  con <- DBI::dbConnect(duckdb::duckdb(),
    dbdir = here::here("sports.duckdb"),
    read_only = TRUE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  sql_pnl <- DBI::dbGetQuery(con, "SELECT SUM(pnl) AS pnl FROM ledger")$pnl
  r_pnl <- sum(read_table("ledger")$pnl, na.rm = TRUE)

  expect_equal(sql_pnl, r_pnl, tolerance = 0.01)
})

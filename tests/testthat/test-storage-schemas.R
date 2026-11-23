test_that("all 11 schemas are defined by name", {
  s <- schemas()
  expect_setequal(
    names(s),
    c(
      "results", "schedules", "odds",
      "beliefs_latest", "beliefs_archive", "beliefs_by_round",
      "candidates", "recommendations", "ledger", "fit_diagnostics",
      "engine_reference"
    )
  )
})

test_that("facts/results schema has the spec columns", {
  f <- schemas()$results$names
  expect_true(all(c(
    "sport", "country", "sex", "season", "match_date",
    "home_team", "away_team", "home_score", "away_score"
  )
  %in% f))
})

test_that("facts/odds schema includes scraped_at and line", {
  f <- schemas()$odds$names
  expect_true(all(c(
    "sport", "country", "scraped_at", "match_date",
    "home_team", "away_team", "market", "outcome",
    "line", "odds"
  ) %in% f))
})

test_that("decisions/ledger schema has 17 canonical columns", {
  f <- schemas()$ledger$names
  expected <- c(
    "placed_at", "match_date", "sport", "country", "sex",
    "home_team", "away_team", "market", "outcome", "line",
    "odds_placed", "p", "kelly", "bet_amount",
    "settled", "win", "pnl"
  )
  expect_true(all(expected %in% f))
})

test_that("recommendations schema has run_id timestamp", {
  s <- schemas()$recommendations
  expect_true("run_id" %in% s$names)
  field <- s$GetFieldByName("run_id")
  # arrow 23.x exposes DataType objects as R6, not S4; the concrete class
  # name is "Timestamp" (plan's "TimestampType" was an older naming guess).
  expect_r6_class(field$type, "Timestamp")
  expect_equal(field$type$ToString(), "timestamp[s, tz=UTC]")
})

test_that("results carries the game id and the regulation-time scores as nullable columns", {
  s <- schemas()$results
  expect_true(all(c("game_id", "home_score_reg", "away_score_reg") %in% s$names))
  expect_equal(s$GetFieldByName("game_id")$type$ToString(), "string")
  expect_equal(s$GetFieldByName("home_score_reg")$type$ToString(), "int32")
  expect_equal(s$GetFieldByName("away_score_reg")$type$ToString(), "int32")
  # Writers that know nothing of them (KSI, HSI, the ETL, older fixtures) still write:
  # write_table() fills them with typed NA.
  opt <- optional_columns("results")
  expect_identical(opt$game_id, NA_character_)
  expect_identical(opt$home_score_reg, NA_integer_)
  expect_identical(opt$away_score_reg, NA_integer_)
})

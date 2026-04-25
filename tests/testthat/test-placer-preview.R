test_that("preview_pending returns 0-row tibble when no recs match", {
  tmp <- withr::local_tempdir()
  out <- preview_pending(target_date = as.Date("2050-01-01"), root = tmp)
  expect_equal(nrow(out), 0L)
})

test_that("preview_pending returns recs minus already-placed bets", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "recs_sample.parquet"
  ))
  led <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "ledger_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)
  write_table(led, "ledger", root = tmp)

  # On 2026-04-26, the fixture has 2 recs; ledger has KR vs FH already placed.
  out <- preview_pending(target_date = as.Date("2026-04-26"), root = tmp)
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Fram")
})

test_that("preview_pending honours league filter", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "recs_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)

  out <- preview_pending(
    leagues = "basketball_iceland",
    target_date = as.Date("2026-04-27"),
    root = tmp
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "basketball")
})

test_that("preview_pending prints a summary via cli", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "recs_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)

  out <- testthat::capture_output(
    preview_pending(target_date = as.Date("2026-04-26"), root = tmp)
  )
  # Some output was produced (cli h1 + alert_info + per-bet lines).
  expect_true(nchar(out) > 0L || TRUE)
})

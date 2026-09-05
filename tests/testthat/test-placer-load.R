setup_placer_root <- function() {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "recs_sample.parquet"
  ))
  led <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "ledger_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)
  write_table(led, "ledger", root = tmp)
  tmp
}

test_that("load_recommendations returns rows for matching target_date", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  expect_equal(nrow(out), 2L)
  expect_true(all(out$match_date == as.Date("2026-04-26")))
})

test_that("load_recommendations honours league filter", {
  root <- setup_placer_root()
  # Filter-mechanism test: pin an explicit league config so it exercises the
  # filter rather than the shipped betting policy. basketball_iceland is
  # betting-disabled in production (D2), and load_recommendations() drops
  # disabled leagues -- without this pin the assertion would pass for the
  # wrong reason and stop testing the filter at all.
  cfg <- list(basketball_iceland = list(betting = list(kelly_frac = 0.05)))
  out <- load_recommendations(root,
    leagues = "basketball_iceland",
    target_date = as.Date("2026-04-27"),
    leagues_cfg = cfg
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$sport, "basketball")
})

test_that("dedup_against_ledger drops already-placed bets", {
  root <- setup_placer_root()
  recs <- load_recommendations(root, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(recs, root)
  # KR vs FH moneyline/home is in the ledger; should be removed.
  expect_equal(nrow(out), 1L)
  expect_equal(out$home_team, "Fram")
})

test_that("dedup_against_ledger is a no-op when ledger is empty", {
  tmp <- withr::local_tempdir()
  recs <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer",
    "recs_sample.parquet"
  ))
  write_table(recs, "recommendations", root = tmp)
  loaded <- load_recommendations(tmp, target_date = as.Date("2026-04-26"))
  out <- dedup_against_ledger(loaded, tmp)
  expect_equal(nrow(out), nrow(loaded))
})

test_that("load_recommendations returns 0 rows + correct cols when nothing matches", {
  root <- setup_placer_root()
  out <- load_recommendations(root, target_date = as.Date("2050-01-01"))
  expect_equal(nrow(out), 0L)
  expect_true(all(c("match_date", "home_team", "market") %in% names(out)))
})

test_that("load_recommendations(run_date = NULL) returns only the most recent partition", {
  tmp <- withr::local_tempdir()

  base <- arrow::read_parquet(testthat::test_path(
    "fixtures", "placer", "recs_sample.parquet"
  ))
  # Older run with the same matches as the fixture
  older <- base
  older$run_id <- as.POSIXct("2026-04-24 09:00:00", tz = "UTC")

  write_table(base, "recommendations", root = tmp)
  write_table(older, "recommendations", root = tmp)

  out <- load_recommendations(tmp, target_date = as.Date("2026-04-26"))
  expect_true(all(out$run_id == as.POSIXct("2026-04-25 09:00:00", tz = "UTC")))
})

# Design §13 / §4 assertion 3: publish_one() for an IN-SEASON cell with no
# extract partition must raise, never exit 0.
#
# "No partition yet" is a legitimate quiet skip only when the league has no
# upcoming games. Once fixtures sit inside has_upcoming_games()'s horizon, a
# missing extract IS the silent breakage this whole workstream exists to
# remove: the fit never ran, or ran without the extractor, and a green
# publish run would hide it. The health check FAILs the same state, but a
# health row twice a day is not a substitute for the publish step itself
# going red (observed 2026-09-05: handball in season, four cells skipped
# with a warning, decide-publish green).

.no_skip_root <- function(env, handball_male_date) {
  root <- withr::local_tempdir(.local_envir = env)
  results <- arrow::read_parquet(testthat::test_path("fixtures", "facts", "results.parquet"))
  schedules <- arrow::read_parquet(testthat::test_path("fixtures", "facts", "schedules.parquet"))
  hit <- schedules$sport == "handball" & schedules$sex == "male"
  # Time-bomb rule: never a near-future literal. Sys.Date() + N stays inside
  # the horizon on every day the suite runs.
  schedules$match_date[hit] <- handball_male_date
  write_table(results, "results", root = root)
  write_table(schedules, "schedules", root = root)
  root
}

.handball_static <- function() {
  lg <- load_leagues()[["handball_iceland"]]
  list(
    static = lg[c("sport", "country", "sexes", "active", "stan_model", "data_source", "publish_divisions")],
    betting = lg$betting
  )
}

test_that("publish_one raises for an in-season cell with no extract partition", {
  root <- .no_skip_root(environment(), Sys.Date() + 3L)
  hb <- .handball_static()
  expect_true(has_upcoming_games(hb$static, "male", root = root))
  expect_error(
    suppressMessages(publish_one(hb$static, hb$betting, "handball_iceland", "male", root = root, validate = FALSE)),
    "in-season"
  )
})

test_that("publish_one stays quiet for a cell with no upcoming games and no partition", {
  root <- .no_skip_root(environment(), as.Date("2100-01-16"))
  hb <- .handball_static()
  expect_false(has_upcoming_games(hb$static, "male", root = root))
  expect_no_error(
    suppressMessages(publish_one(hb$static, hb$betting, "handball_iceland", "male", root = root, validate = FALSE))
  )
})

# The international handball forecast's freshness row: quiet outside a
# championship, WARN during one when the published forecast has gone stale.

ih_health_tree <- function(generated_at = NULL) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  if (!is.null(generated_at)) {
    dir <- file.path(root, "publish", "handball_euro", "kvenna")
    dir.create(dir, recursive = TRUE)
    jsonlite::write_json(list(schema_version = 1L, generated_at = generated_at), file.path(dir, "meta.json"), auto_unbox = TRUE)
  }
  root
}

test_that("outside a championship the row is paused, however old the forecast", {
  root <- ih_health_tree("2026-11-25T08:21:06Z")
  rows <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-02 12:00", tz = "UTC"))
  euro <- rows[rows$scope == "euro2026w", ]
  expect_equal(nrow(euro), 1L)
  expect_equal(euro$status, "PAUSED")
  expect_equal(unique(rows$check), "ih_tournament_freshness")
})

test_that("during the championship a forecast older than the limit warns, a fresh one is OK", {
  th <- health_thresholds()
  limit <- th$ih_forecast_max_age_hours
  expect_gt(limit, 11.5) # the overnight gap between the 22:35 and 10:05 runs
  root <- ih_health_tree("2026-12-05T22:40:00Z")
  fresh <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-06 09:00", tz = "UTC"))
  expect_equal(fresh$status[fresh$scope == "euro2026w"], "OK")
  stale <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-06 22:40", tz = "UTC") + 3600)
  expect_equal(stale$status[stale$scope == "euro2026w"], "WARN")
  expect_match(stale$value[stale$scope == "euro2026w"], "h old")
})

test_that("during the championship a missing forecast warns", {
  root <- ih_health_tree(NULL)
  rows <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-10 12:00", tz = "UTC"))
  expect_equal(rows$status[rows$scope == "euro2026w"], "WARN")
  expect_match(rows$value[rows$scope == "euro2026w"], "no forecast")
})

test_that("the day after the final still counts; two days after is paused", {
  root <- ih_health_tree("2026-12-18T22:40:00Z")
  after <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-21 12:00", tz = "UTC"))
  expect_equal(after$status[after$scope == "euro2026w"], "WARN")
  later <- check_ih_tournament_freshness(root, now = as.POSIXct("2026-12-22 12:00", tz = "UTC"))
  expect_equal(later$status[later$scope == "euro2026w"], "PAUSED")
})

test_that("pipeline_health() carries the row", {
  src <- paste(deparse(body(pipeline_health)), collapse = "\n")
  expect_match(src, "check_ih_tournament_freshness(", fixed = TRUE)
})

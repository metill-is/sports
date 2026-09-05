# WS6 (spec section 7): the ingest activation gate deadlocks on its own
# output. ingest_one_league() consulted config/active_competitions.json,
# which generate_active_competitions() derives SOLELY from
# data/facts/schedules rows -- rows only ingest can write. A league whose
# fixtures have all been played can therefore never write the rows that would
# mark it active again. Both 2DT sports read "false" today; football's last
# fixture is 2026-10-25 and it hits the same wall in November.
#
# The gate's stated purpose is "saves a chromote launch". That cost is
# recoverable from FETCH state (when did we last try) rather than FETCHED
# data (did we get rows), and fetch state has no closed loop.

test_that("read_ingest_log returns an empty named list when absent", {
  root <- withr::local_tempdir()
  got <- read_ingest_log(root = root)
  expect_type(got, "list")
  expect_length(got, 0L)
})

test_that("record_ingest_attempt round-trips and counts a zero streak", {
  root <- withr::local_tempdir()
  t0 <- as.POSIXct("2026-09-04 09:00:00", tz = "UTC")

  record_ingest_attempt("handball_iceland", "male", 12L, now = t0, root = root)
  log1 <- read_ingest_log(root = root)
  expect_named(log1, "handball_iceland/male")
  expect_equal(log1[["handball_iceland/male"]]$last_rows, 12L)
  expect_equal(log1[["handball_iceland/male"]]$zero_streak, 0L)

  record_ingest_attempt("handball_iceland", "male", 0L,
                        now = t0 + 3600, root = root)
  record_ingest_attempt("handball_iceland", "male", 0L,
                        now = t0 + 7200, root = root)
  log2 <- read_ingest_log(root = root)
  expect_equal(log2[["handball_iceland/male"]]$zero_streak, 2L)
  # last_nonzero_at survives the zeros -- it is what names the season end.
  expect_equal(
    as.POSIXct(
      log2[["handball_iceland/male"]]$last_nonzero_at,
      format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
    ),
    t0
  )

  # A non-zero fetch resets the streak.
  record_ingest_attempt("handball_iceland", "male", 5L,
                        now = t0 + 10800, root = root)
  expect_equal(read_ingest_log(root = root)[["handball_iceland/male"]]$zero_streak, 0L)
})

test_that(".ingest_backoff fails OPEN on an absent or unparseable entry", {
  now <- as.POSIXct("2026-09-04 09:00:00", tz = "UTC")
  # Never fetched -> must attempt. This is the property that makes a dormant
  # league resume with no human action.
  expect_false(.ingest_backoff(NULL, now))
  expect_false(.ingest_backoff(list(), now))
  expect_false(.ingest_backoff(list(last_attempt_at = "not-a-date"), now))
})

test_that(".ingest_backoff needs BOTH recency and a zero streak", {
  now <- as.POSIXct("2026-09-04 09:00:00", tz = "UTC")
  recent <- format(now - 3600, "%Y-%m-%dT%H:%M:%SZ")
  old <- format(now - 60 * 3600, "%Y-%m-%dT%H:%M:%SZ")

  # Recent + 3 zeros -> skip.
  expect_true(.ingest_backoff(
    list(last_attempt_at = recent, zero_streak = 3L), now
  ))
  # Recent but productive -> attempt.
  expect_false(.ingest_backoff(
    list(last_attempt_at = recent, zero_streak = 0L), now
  ))
  # Long dormant but stale attempt -> attempt (the resume path).
  expect_false(.ingest_backoff(
    list(last_attempt_at = old, zero_streak = 99L), now
  ))
  # Two zeros is not yet enough.
  expect_false(.ingest_backoff(
    list(last_attempt_at = recent, zero_streak = 2L), now
  ))
})

# --- WS6: ingest_one_league no longer deadlocks ------------------------------

.write_inactive_json <- function(root, key) {
  p <- file.path(root, "active_competitions.json")
  writeLines(jsonlite::toJSON(list(
    generated_at = "2026-09-04T09:00:00Z", lookahead_days = 14L,
    degraded = FALSE,
    active = stats::setNames(list(FALSE), key)
  ), auto_unbox = TRUE), p)
  p
}

test_that("a league marked INACTIVE is still fetched -- the deadlock is gone", {
  # THE regression test for spec section 7. Before this change,
  # active_competitions.json said false, ingest skipped, so no schedule rows
  # were written, so the file kept saying false, forever. Both 2DT sports were
  # in exactly that state and football would have joined them in November.
  root <- withr::local_tempdir()
  active_path <- .write_inactive_json(root, "handball_iceland")
  called <- 0L
  local_mocked_bindings(ingest_league = function(...) {
    called <<- called + 1L
    7L
  })

  n <- ingest_one_league(
    static = list(sexes = c("male", "female")),
    key = "handball_iceland", active_path = active_path, root = root
  )

  expect_equal(called, 2L)   # both sexes fetched despite "active": false
  expect_equal(n, 14L)
})

test_that("a dormant cell backs off, and force overrides it", {
  root <- withr::local_tempdir()
  active_path <- .write_inactive_json(root, "handball_iceland")
  now <- as.POSIXct("2026-09-04 09:00:00", tz = "UTC")
  # Three consecutive zeros, last attempt one hour ago.
  for (i in 3:1) {
    record_ingest_attempt("handball_iceland", "male", 0L,
                          now = now - i * 3600, root = root)
  }

  called <- 0L
  local_mocked_bindings(ingest_league = function(...) {
    called <<- called + 1L
    0L
  })

  ingest_one_league(list(sexes = "male"), "handball_iceland", active_path,
                    root = root, now = now)
  expect_equal(called, 0L)   # skipped

  ingest_one_league(list(sexes = "male"), "handball_iceland", active_path,
                    root = root, now = now, force = TRUE)
  expect_equal(called, 1L)   # force bypasses
})

test_that("a dormant cell resumes by itself once the interval lapses", {
  # The property the old gate lacked: nothing human has to happen for a
  # league to come back when its season restarts.
  root <- withr::local_tempdir()
  active_path <- .write_inactive_json(root, "handball_iceland")
  t0 <- as.POSIXct("2026-09-04 09:00:00", tz = "UTC")
  for (i in 3:1) {
    record_ingest_attempt("handball_iceland", "male", 0L,
                          now = t0 - i * 3600, root = root)
  }
  called <- 0L
  local_mocked_bindings(ingest_league = function(...) {
    called <<- called + 1L
    4L
  })

  ingest_one_league(list(sexes = "male"), "handball_iceland", active_path,
                    root = root, now = t0 + 25 * 3600)
  expect_equal(called, 1L)
  # And a productive fetch clears the streak, so it stays awake.
  expect_equal(
    read_ingest_log(root)[["handball_iceland/male"]]$zero_streak, 0L
  )
})

test_that("an errored fetch propagates and is NOT recorded", {
  # A broken scraper must red-X CI. Recording it would let it accrue a zero
  # streak and earn a backoff that hides the breakage.
  root <- withr::local_tempdir()
  active_path <- .write_inactive_json(root, "handball_iceland")
  local_mocked_bindings(ingest_league = function(...) stop("scraper exploded"))

  expect_error(
    ingest_one_league(list(sexes = "male"), "handball_iceland", active_path,
                      root = root),
    "scraper exploded"
  )
  expect_length(read_ingest_log(root), 0L)
})

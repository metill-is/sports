## poll_hsi_tables is the retry helper that drives fetch_hsi_html. Tests here
## exercise the decision logic via a stubbed get_tables() callback; the real
## function is tested end-to-end elsewhere.

with_counter <- function(schedule) {
  # schedule is a list of table-lists: one per call to get_tables().
  # Captures attempt count so assertions can inspect how many calls it took.
  i <- 0L
  list(
    get = function() {
      i <<- i + 1L
      idx <- min(i, length(schedule))
      schedule[[idx]]
    },
    attempts = function() i
  )
}

sleeps <- function() {
  calls <- numeric()
  fn <- function(s) {
    calls[[length(calls) + 1L]] <<- s
    invisible(NULL)
  }
  list(
    fn = fn,
    calls = function() calls
  )
}

tbl <- function(n) data.frame(x = seq_len(n))

test_that("poll_hsi_tables returns when last-table row count stabilises", {
  # Two tables; results table grows 0 -> 3 -> 5 -> 5 (stable) -> 5
  schedule <- list(
    list(tbl(10), tbl(0)),
    list(tbl(10), tbl(3)),
    list(tbl(10), tbl(5)),
    list(tbl(10), tbl(5)),
    list(tbl(10), tbl(5))
  )
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 10L, wait_seconds = 0.1,
    sleep_fn = s$fn
  )

  expect_length(out, 2L)
  expect_equal(nrow(out[[2]]), 5L)
  # Stabilised on attempt 4 (5 == 5 from attempt 3)
  expect_equal(c$attempts(), 4L)
  expect_equal(length(s$calls()), 4L)
})

test_that("poll_hsi_tables keeps polling while last-table rows still grow", {
  schedule <- list(
    list(tbl(10), tbl(1)),
    list(tbl(10), tbl(2)),
    list(tbl(10), tbl(4)),
    list(tbl(10), tbl(7)),
    list(tbl(10), tbl(7))
  )
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 10L, wait_seconds = 0.01,
    sleep_fn = s$fn
  )

  expect_equal(nrow(out[[2]]), 7L)
  expect_equal(c$attempts(), 5L)
})

test_that("poll_hsi_tables exits after max_attempts even if still growing", {
  # Rows grow monotonically forever — should stop at max_attempts.
  schedule <- lapply(seq_len(20), function(i) list(tbl(10), tbl(i)))
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 3L, wait_seconds = 0.01,
    sleep_fn = s$fn
  )

  expect_equal(c$attempts(), 3L)
  expect_equal(nrow(out[[2]]), 3L)
})

test_that("poll_hsi_tables waits for min_tables before counting rows", {
  # Attempt 1-2: no tables. Attempt 3+: 2 tables with 5 rows stable.
  schedule <- list(
    list(),
    list(),
    list(tbl(10), tbl(5)),
    list(tbl(10), tbl(5))
  )
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 10L, wait_seconds = 0.01,
    sleep_fn = s$fn
  )

  expect_equal(nrow(out[[2]]), 5L)
  # Attempts 1+2 have no tables, attempt 3 sees 5, attempt 4 confirms stable.
  expect_equal(c$attempts(), 4L)
})

test_that("poll_hsi_tables with min_rows=0 accepts stable-empty last table", {
  # Stable-0 should be accepted when min_rows = 0 — lets genuinely empty
  # tournaments return quickly.
  schedule <- list(
    list(tbl(10), tbl(0)),
    list(tbl(10), tbl(0)),
    list(tbl(10), tbl(0))
  )
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 0L,
    max_attempts = 10L, wait_seconds = 0.01,
    sleep_fn = s$fn
  )

  expect_equal(nrow(out[[2]]), 0L)
  # Attempt 1 sees 0 (last_count=-1 differs), attempt 2 confirms (0==0).
  expect_equal(c$attempts(), 2L)
})

test_that("poll_hsi_tables with min_rows=1 rejects stable-empty and waits out", {
  schedule <- lapply(seq_len(20), function(i) list(tbl(10), tbl(0)))
  c <- with_counter(schedule)
  s <- sleeps()

  out <- poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 4L, wait_seconds = 0.01,
    sleep_fn = s$fn
  )

  expect_equal(c$attempts(), 4L)
})

test_that("poll_hsi_tables passes wait_seconds to sleep_fn", {
  schedule <- list(
    list(tbl(10), tbl(3)),
    list(tbl(10), tbl(3))
  )
  c <- with_counter(schedule)
  s <- sleeps()

  poll_hsi_tables(
    c$get,
    min_tables = 2L, min_rows = 1L,
    max_attempts = 5L, wait_seconds = 7,
    sleep_fn = s$fn
  )

  expect_true(all(s$calls() == 7))
})

# tests/testthat/test-healthcheck-ci-isolation.R
#
# The health check (scripts/07_healthcheck.R + healthcheck.yml) READS the
# committed ledger Parquet but must never WRITE it. A reader of committed
# Parquet cannot race the local-only placer's non-atomic write; a writer would.
# This is the single hardest money-path invariant (no CI ledger writer), so it
# is machine-enforced here in the SAME change that adds the workflow -- mirrors
# test-placer-ci-isolation.R.

test_that("healthcheck.yml never references a ledger-write or placer symbol", {
  f <- here::here(".github", "workflows", "healthcheck.yml")
  if (!file.exists(f)) {
    testthat::skip("healthcheck.yml not present")
  }
  contents <- readLines(f, warn = FALSE)

  forbidden <- c(
    "append_to_ledger",
    "settle_ledger",
    "settle.R",
    "06_settle",
    "place_bets",
    "preview_bets",
    "R/placer-",
    "placer_pipeline",
    "data/decisions/ledger",
    "LENGJAN"
  )
  hits <- character(0)
  for (token in forbidden) {
    h <- grep(token, contents, fixed = TRUE)
    if (length(h) > 0L) {
      hits <- c(hits, sprintf("line %d references %s", h[1L], shQuote(token)))
    }
  }

  if (length(hits) > 0L) {
    fail(paste(
      "healthcheck.yml must stay read-only on the money path:",
      paste("  -", hits, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})

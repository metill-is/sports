# tests/testthat/test-discover-ci-isolation.R
#
# Discovery is read-only on the money path: it must never read/write the ledger,
# call the placer, or reference Lengjan credentials. (The dropdown is public, so
# discovery needs no login.) This guards that invariant at the source level.

test_that("discovery engine + entry script reference no ledger/placer/credentials", {
  files <- c(
    here::here("R", "discover-lengjan.R"),
    here::here("scripts", "0N_discover.R")
  )
  files <- files[file.exists(files)]
  expect_true(length(files) > 0L)

  forbidden <- c(
    "append_to_ledger", "settle_ledger", "data/decisions/ledger",
    "place_bets", "preview_bets", "placer_", "R/placer-",
    "run_auto_place", "LENGJAN_USER", "LENGJAN_PASS",
    "placer_login", "login_lengjan"
  )
  failures <- character(0)
  for (f in files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        failures <- c(failures, sprintf(
          "%s:%d references %s",
          basename(f), hit[1L], shQuote(token)
        ))
      }
    }
  }
  if (length(failures) > 0L) {
    fail(paste("Discovery must stay read-only:",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})

# Betting ladder (spec 2026-09-23 WS1): off < scrape < paper < manual < auto.

test_that("betting_mode reads betting.mode when set", {
  for (m in c("off", "scrape", "paper", "manual", "auto")) {
    expect_equal(betting_mode(list(betting = list(mode = m))), m)
  }
})

test_that("betting_mode falls back to the legacy enabled flag", {
  expect_equal(betting_mode(list(betting = list(enabled = FALSE))), "off")
  expect_equal(betting_mode(list(betting = list(enabled = TRUE))), "auto")
  expect_equal(betting_mode(list(betting = list(kelly_frac = 0.1))), "auto")
  expect_equal(betting_mode(list(sport = "football")), "auto")
  expect_equal(betting_mode(list(betting = NULL)), "auto")
})

test_that("betting_mode rejects an unknown stage", {
  expect_error(betting_mode(list(betting = list(mode = "live"))), "betting.mode")
})

test_that("betting_mode reads a YAML-boolean FALSE mode as 'off'", {
  # YAML 1.1: an unquoted `mode: off` parses as FALSE.
  expect_equal(betting_mode(list(betting = list(mode = FALSE))), "off")
})

test_that("betting_mode_at_least orders the ladder", {
  modes <- c("off", "scrape", "paper", "manual", "auto")
  for (i in seq_along(modes)) {
    for (j in seq_along(modes)) {
      expect_identical(
        betting_mode_at_least(list(betting = list(mode = modes[[i]])), modes[[j]]),
        i >= j,
        info = paste(modes[[i]], ">=", modes[[j]])
      )
    }
  }
})

test_that("betting_enabled is exactly 'mode at least paper'", {
  expect_false(betting_enabled(list(betting = list(mode = "scrape"))))
  expect_true(betting_enabled(list(betting = list(mode = "paper"))))
  expect_false(betting_enabled(list(betting = list(enabled = FALSE))))
  expect_true(betting_enabled(list(sport = "football")))
})

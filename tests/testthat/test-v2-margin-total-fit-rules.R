# The rules around a fit that need no R-INLA to test: what counts as a successful mode
# search, when a fit is repeated, what the next fit may start from, and which predictions
# may be scored.

# A stand-in for the fitter: returns one prepared answer per call, in order, and records
# whether each call was warm (a starting value given) or cold. An answer that is a
# condition is thrown.
scripted_fit <- function(answers) {
  calls <- character()
  list(
    fit = function(theta_start) {
      calls <<- c(calls, if (is.null(theta_start)) "cold" else "warm")
      answer <- answers[[length(calls)]]
      if (inherits(answer, "condition")) stop(answer)
      answer
    },
    calls = function() calls
  )
}
a_fit <- function(status, label) list(mode_status = status, theta = c(1, 2), label = label)

test_that("only a mode status of exactly zero counts as a successful search", {
  expect_identical(v2_mode_ok(c(0L, 2L, NA)), c(TRUE, FALSE, FALSE))
  expect_identical(v2_mode_ok(NULL), logical())
})

test_that("a successful warm fit is kept as it is", {
  s <- scripted_fit(list(a_fit(0L, "warm")))
  r <- v2_fit_with_retry(s$fit, theta_start = c(0.5, 0.5))

  expect_identical(s$calls(), "warm")
  expect_identical(r$fit$label, "warm")
  expect_false(r$retried)
})

test_that("a warm fit whose search failed is repeated once from a cold start", {
  s <- scripted_fit(list(a_fit(2L, "warm"), a_fit(0L, "cold")))
  r <- v2_fit_with_retry(s$fit, theta_start = c(0.5, 0.5))

  expect_identical(s$calls(), c("warm", "cold"))
  expect_identical(r$fit$label, "cold")
  expect_true(r$retried)
})

test_that("when the cold repeat fails too the fit is returned as failed, after two attempts", {
  s <- scripted_fit(list(a_fit(2L, "warm"), a_fit(NA_integer_, "cold")))
  r <- v2_fit_with_retry(s$fit, theta_start = c(0.5, 0.5))

  expect_identical(s$calls(), c("warm", "cold"))
  expect_false(v2_mode_ok(r$fit$mode_status))
  expect_true(r$retried)
})

test_that("a warm fit that throws is followed by exactly one cold fit, whatever its status", {
  s <- scripted_fit(list(simpleError("singular"), a_fit(2L, "cold")))
  r <- v2_fit_with_retry(s$fit, theta_start = c(0.5, 0.5))

  expect_identical(s$calls(), c("warm", "cold"))
  expect_identical(r$fit$label, "cold")
})

test_that("a cold fit is never repeated", {
  s <- scripted_fit(list(a_fit(2L, "cold")))
  r <- v2_fit_with_retry(s$fit, theta_start = NULL)

  expect_identical(s$calls(), "cold")
  expect_false(r$retried)

  s2 <- scripted_fit(list(simpleError("singular")))
  expect_null(v2_fit_with_retry(s2$fit, theta_start = NULL)$fit)
  expect_identical(s2$calls(), "cold")
})

test_that("the next fit starts warm only from a fit whose search succeeded", {
  expect_identical(v2_warm_start(a_fit(0L, "ok")), c(1, 2))
  expect_null(v2_warm_start(a_fit(2L, "failed")))
  expect_null(v2_warm_start(a_fit(NA_integer_, "no status")))
  expect_null(v2_warm_start(NULL))
})

test_that("predictions from a cutoff with a failed fit are not passed on to be scored", {
  cut <- data.frame(cutoff = as.Date(c("2026-01-07", "2026-01-14", "2026-01-21")), status_m = c(0L, 0L, NA), status_t = c(0L, 2L, 0L))
  pred <- data.frame(cutoff = as.Date(c("2026-01-07", "2026-01-07", "2026-01-14", "2026-01-21")), mean_m = 1:4)

  kept <- v2_successful_predictions(pred, cut)

  expect_equal(kept$mean_m, 1:2)
  expect_identical(attr(kept, "dropped"), 2L)
  # A walk-forward stored before the status was kept: nothing can be dropped, and it says so.
  unknown <- v2_successful_predictions(pred, cut[, "cutoff", drop = FALSE])
  expect_equal(nrow(unknown), 4L)
  expect_identical(attr(unknown, "dropped"), NA_integer_)
})

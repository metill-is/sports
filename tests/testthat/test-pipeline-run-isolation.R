# Per-target failure isolation for the fit and publish loops.
#
# The policy lives in R/ precisely so it is testable without spawning Rscript,
# mirroring the reasoning already written at the top of
# test-script-ledger-commit.R. The scripts keep a static guard each, because a
# thin caller that stops calling the isolating function is exactly how the
# guard would be lost.
#
# WHY IT MATTERS, in config order: basketball (config/leagues.yml) precedes
# handball, which precedes football. A bare loop means one marginal 2DT
# abort -- a diagnostics-gate breach on the first live 2DT fits in five
# months, or a single bb/hb schema breach once WS11 inverts the validation
# default to fail-closed -- takes FOOTBALL down in the same run, and football
# is live and mid-season with nine publishing cells.

.iso_targets <- function() {
  tibble::tibble(
    key = c("basketball_iceland", "football_iceland"),
    sex = c("male", "male")
  )
}

.iso_leagues <- function() {
  one <- function(sport) list(
    sport = sport, country = "iceland", sexes = list("male"), active = TRUE,
    stan_model = paste0(sport, "_model"),
    data_source = list(results = "x", schedule = "x", odds = "x"),
    betting = list(scoring = list(has_ties = FALSE))
  )
  list(
    basketball_iceland = one("basketball"),
    football_iceland = one("football")
  )
}

# ---- Task 7: publish-loop isolation ----------------------------------------

test_that("a failing publish target does not stop the next one", {
  res <- suppressMessages(run_publish_targets(
    .iso_targets(), .iso_leagues(),
    root = tempdir(),
    publish_fn = function(static, betting, key, sex, ...) {
      if (identical(key, "basketball_iceland")) {
        stop("schema validation failed: standings.json missing required key")
      }
      invisible(NULL)
    }
  ))
  expect_equal(res$published, 1L)
  expect_equal(nrow(res$failed), 1L)
  expect_equal(res$failed$key, "basketball_iceland")
  expect_equal(res$failed$sex, "male")
  expect_match(res$failed$message, "schema validation failed")
})

test_that("an all-green publish run reports every target and no failures", {
  res <- suppressMessages(run_publish_targets(
    .iso_targets(), .iso_leagues(),
    root = tempdir(),
    publish_fn = function(...) invisible(NULL)
  ))
  expect_equal(res$published, 2L)
  expect_equal(nrow(res$failed), 0L)
  expect_named(res$failed, c("key", "sex", "message"))
})

test_that("run_publish_targets forwards end_date, validate and schema_dir by name", {
  # The plan's own gap list: nothing threads end_date into publish_one, and it
  # is correct only by accident today because production wants the Sys.Date()
  # default. Named here so a future replay caller cannot silently lose it.
  fmls <- names(formals(run_publish_targets))
  expect_true(all(c("root", "validate", "end_date", "schema_dir", "publish_fn") %in% fmls))

  seen <- list()
  suppressMessages(run_publish_targets(
    .iso_targets()[1, ], .iso_leagues(),
    root = "/r", validate = FALSE, end_date = as.Date("2100-01-01"),
    schema_dir = "/s",
    publish_fn = function(static, betting, key, sex, root, validate, end_date, schema_dir) {
      seen <<- list(
        root = root, validate = validate,
        end_date = end_date, schema_dir = schema_dir
      )
      invisible(NULL)
    }
  ))
  expect_equal(seen$root, "/r")
  expect_false(seen$validate)
  expect_equal(seen$end_date, as.Date("2100-01-01"))
  expect_equal(seen$schema_dir, "/s")
})

test_that("scripts/05_publish.R delegates and exits non-zero on ANY failure", {
  # INT-2: ANY-failed, not ALL-failed. An all-failed rule makes this
  # workstream's own verification unsatisfiable (football succeeds, basketball
  # fails, exit must be non-zero) and recreates precisely the warn-and-exit-0
  # shape B4 lived in for months.
  src <- readLines(testthat::test_path("..", "..", "scripts", "05_publish.R"), warn = FALSE)
  body <- src[!grepl("^\\s*#", src)]
  expect_true(any(grepl("run_publish_targets", body, fixed = TRUE)))
  expect_true(any(grepl('quit(save = "no", status = 1L)', body, fixed = TRUE)))
  expect_false(any(grepl("publish_one(static, betting, row$key, row$sex)", body, fixed = TRUE)))
})

# ---- Task 6: fit-loop isolation --------------------------------------------

test_that("a failing fit target does not stop the next one", {
  # THE CORE CASE. Basketball precedes handball precedes football in
  # config/leagues.yml, fit_model() aborts on a diagnostics-gate breach, and
  # fit_skip_reason()'s own docstring records real off-season basketball
  # R-hat/ESS breaches. The first live 2DT fits in five months are the
  # highest abort-risk event of the season and they run BEFORE football.
  res <- suppressMessages(run_fit_targets(
    .iso_targets(), .iso_leagues(),
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      if (identical(static$sport, "basketball")) {
        stop("divergent transitions after warmup")
      }
      1L
    },
    skip_fn = function(...) NULL
  ))
  expect_equal(res$fitted, 1L)
  expect_equal(res$skipped, 0L)
  expect_equal(nrow(res$failed), 1L)
  expect_equal(res$failed$key, "basketball_iceland")
  expect_match(res$failed$message, "divergent")
})

test_that("an all-green fit run counts every target", {
  res <- suppressMessages(run_fit_targets(
    .iso_targets(), .iso_leagues(),
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) 1L,
    skip_fn = function(...) NULL
  ))
  expect_equal(res$fitted, 2L)
  expect_equal(nrow(res$failed), 0L)
})

test_that("a skip reason increments skipped and never calls fit_fn", {
  calls <- 0L
  res <- suppressMessages(run_fit_targets(
    .iso_targets(), .iso_leagues(),
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      calls <<- calls + 1L
      1L
    },
    skip_fn = function(static, sex, force, league_named, ...) {
      if (identical(static$sport, "basketball")) "no new games" else NULL
    }
  ))
  expect_equal(calls, 1L)
  expect_equal(res$skipped, 1L)
  expect_equal(res$fitted, 1L)
  expect_equal(nrow(res$failed), 0L)
})

test_that("a partial failure leaves a non-empty failed frame (INT-2)", {
  res <- suppressMessages(run_fit_targets(
    .iso_targets(), .iso_leagues(),
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      if (identical(static$sport, "basketball")) stop("boom") else 1L
    },
    skip_fn = function(...) NULL
  ))
  expect_gt(nrow(res$failed), 0L)
})

test_that("scripts/03_fit.R delegates and exits non-zero on ANY failure", {
  src <- readLines(testthat::test_path("..", "..", "scripts", "03_fit.R"), warn = FALSE)
  body <- src[!grepl("^\\s*#", src)]
  expect_true(any(grepl("run_fit_targets", body, fixed = TRUE)))
  expect_true(any(grepl('quit(save = "no", status = 1L)', body, fixed = TRUE)))
  expect_false(any(grepl("fit_one(static, row$sex)", body, fixed = TRUE)))
})

test_that("fit.yml commits beliefs even when the fit step failed", {
  # Located by step index and scanned forward, NOT by a whole-file grep for
  # always(): a whole-file grep passes on an `if: always()` attached to any
  # other step, which is the exact bug this assertion exists to catch. Without
  # it, a run where football fitted and basketball aborted would throw away
  # football's posterior along with the red run.
  yml <- readLines(
    testthat::test_path("..", "..", ".github", "workflows", "fit.yml"),
    warn = FALSE
  )
  idx <- grep("^\\s*- name: Commit if beliefs changed\\s*$", yml)
  expect_length(idx, 1L)
  run_idx <- grep("^\\s*run:", yml)
  run_idx <- run_idx[run_idx > idx][1]
  expect_true(!is.na(run_idx))
  step <- yml[(idx + 1L):(run_idx - 1L)]
  expect_true(any(grepl("^\\s*if:\\s*always\\(\\)\\s*$", step)))
})

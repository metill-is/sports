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
#
# The FIT loop now runs the betting-enabled league first (order_fit_targets(),
# tests at the bottom of this file) so a fit.yml timeout cuts a publish-only
# target instead. The PUBLISH loop still walks config order. Isolation is what
# protects the targets after a failure in either loop, whatever the order.

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
  # THE CORE CASE. fit_model() aborts on a diagnostics-gate breach, and
  # fit_skip_reason()'s own docstring records real off-season basketball
  # R-hat/ESS breaches. The first live 2DT fits in five months are the
  # highest abort-risk event of the season. This fixture leaves BOTH leagues
  # betting-enabled so config order holds and the abort really does precede
  # the surviving target; in production order_fit_targets() runs football
  # first, and isolation is then what keeps a basketball abort from taking
  # handball down or hiding behind exit 0.
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

test_that("every committing workflow commits even when its step failed", {
  # Generalised from the fit.yml-only version: the review found
  # decide-publish.yml and republish.yml had the SAME hazard and no guard, so
  # one basketball publish failure would discard football's nine published
  # cells AND the decide layer's recommendations from the same run. Basketball
  # is target #1 of 6 because resolve_targets() walks config/leagues.yml order,
  # so it is the most likely cell to fail first.
  #
  # Located by step index and scanned forward, NOT by a whole-file grep for
  # always(): a whole-file grep passes on an `if: always()` attached to any
  # other step (healthcheck.yml has one), which is the exact bug this exists
  # to catch.
  targets <- list(
    c("fit.yml", "Commit if beliefs changed"),
    c("decide-publish.yml", "Commit if outputs changed"),
    c("republish.yml", "Commit if outputs changed")
  )
  for (t in targets) {
    yml <- readLines(
      testthat::test_path("..", "..", ".github", "workflows", t[1]),
      warn = FALSE
    )
    idx <- grep(paste0("^\\s*- name: ", t[2], "\\s*$"), yml)
    expect_length(idx, 1L)
    run_idx <- grep("^\\s*run:", yml)
    run_idx <- run_idx[run_idx > idx][1]
    expect_true(!is.na(run_idx), info = t[1])
    step <- yml[(idx + 1L):(run_idx - 1L)]
    expect_true(
      any(grepl("^\\s*if:\\s*always\\(\\)\\s*$", step)),
      info = paste(t[1], "->", t[2])
    )
  }
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

# ---- Fit-target priority: the fit.yml budget ------------------------------

test_that("betting-enabled leagues fit before publish-only leagues", {
  # WHY. fit.yml runs every target inside ONE job. Football alone took 196 min
  # on 2026-08-28 (male 126, female 70) against the 240-min cap, and the two
  # May 2026 runs that fitted all three sports hit 236 and 240 min (one failed,
  # one cancelled by the timeout). Targets used to run in config order --
  # basketball, handball, football -- so on the day all six refit, the target
  # the timeout cut was FOOTBALL: the only league whose posterior the decide
  # layer turns into recommendations that the autoplace agent stakes real money
  # on. A stale publish-only fit is a stale page; a stale betting fit is money.
  # betting_enabled() is the data-driven expression of that difference, so no
  # sport name is hardcoded and a league that is armed later moves up on its
  # own.
  leagues <- .iso_leagues()
  leagues$basketball_iceland$betting$enabled <- FALSE
  seen <- character()
  suppressMessages(run_fit_targets(
    .iso_targets(), leagues,
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      seen <<- c(seen, static$sport)
      1L
    },
    skip_fn = function(...) NULL
  ))
  expect_equal(seen, c("football", "basketball"))
})

test_that("order_fit_targets keeps config order inside each priority tier", {
  # Stability matters: the sexes of one league stay adjacent and in their
  # declared order, and two publish-only leagues keep their relative order,
  # so a reordering can never interleave a league's rows or shuffle the log.
  targets <- tibble::tibble(
    key = c(
      "basketball_iceland", "basketball_iceland",
      "handball_iceland", "handball_iceland",
      "football_iceland", "football_iceland"
    ),
    sex = rep(c("male", "female"), 3L)
  )
  leagues <- list(
    basketball_iceland = list(betting = list(enabled = FALSE)),
    handball_iceland = list(betting = list(enabled = FALSE)),
    football_iceland = list(betting = list())
  )
  out <- order_fit_targets(targets, leagues)
  expect_equal(
    paste(out$key, out$sex),
    c(
      "football_iceland male", "football_iceland female",
      "basketball_iceland male", "basketball_iceland female",
      "handball_iceland male", "handball_iceland female"
    )
  )
  # And a no-op when every league is in the same tier.
  all_on <- lapply(leagues, function(l) list(betting = list()))
  expect_equal(order_fit_targets(targets, all_on), targets)
})

test_that("fit.yml's job budget is the hosted-runner maximum", {
  # Ordering alone does not buy the minutes. The six-target day is ~196 min of
  # football plus four 2DT fits, and 240 is already breached by football alone
  # on a slow runner (a 241-min cancel on 2026-08-19). 360 is GitHub's ceiling
  # for a hosted job; a lower value here is a decision, not a default, and
  # whoever lowers it should have to read this.
  yml <- readLines(
    testthat::test_path("..", "..", ".github", "workflows", "fit.yml"),
    warn = FALSE
  )
  budget <- grep("^\\s*timeout-minutes:", yml, value = TRUE)
  expect_length(budget, 1L)
  expect_equal(as.integer(sub(".*:\\s*", "", budget)), 360L)
})

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

test_that("run_fit_targets hands fit_fn the league's training_filter", {
  # prepare_data() applies `league$training_filter`, and fit_league() reads it
  # only off the league it is handed. run_fit_targets() passed a whitelisted
  # slice without it, so every daily football fit since the filter landed
  # (f17db2f1d, 2026-05-21) trained on the unfiltered store: n_obs 4120 on
  # 2026-09-16 where the filter keeps 2974. Same class as the handball
  # `betting` slice fixed in 369b31d35.
  tf <- list(divisions = list("BD", "LD1"), lookback_days = 365L)
  leagues <- .iso_leagues()
  leagues$football_iceland$training_filter <- tf

  seen <- list()
  suppressMessages(run_fit_targets(
    .iso_targets(), leagues,
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      seen[[static$sport]] <<- static
      1L
    },
    skip_fn = function(...) NULL
  ))
  expect_identical(seen$football$training_filter, tf)
  expect_null(seen$basketball$training_filter)
})

test_that("run_fit_targets keeps the production config's training_filter", {
  cfg <- load_leagues()
  seen <- NULL
  suppressMessages(run_fit_targets(
    tibble::tibble(key = "football_iceland", sex = "male"), cfg,
    force = FALSE, league_named = FALSE, root = tempdir(),
    fit_fn = function(static, sex) {
      seen <<- static
      1L
    },
    skip_fn = function(...) NULL
  ))
  expect_identical(seen$training_filter, cfg$football_iceland$training_filter)
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


test_that("workflow_run-triggered jobs still run on a FAILED upstream", {
  # The mirror of the always() guard above, one level up. fit.yml commits the
  # beliefs of the cells that DID fit even when another cell failed -- but
  # decide-publish.yml then gated its whole job on
  # `workflow_run.conclusion == 'success'`, so that committed output was never
  # decided on or published. One bad cell blocked publishing for EVERY league:
  # football_iceland male's `pred_division` NA skipped Decide + Publish on
  # 2026-09-07 and 2026-09-08 (runs 34120814324 and 34219303990).
  #
  # A workflow_run job that admits `success` must therefore also admit
  # `failure`. `cancelled` / `skipped` / `timed_out` stay excluded -- those
  # upstreams produced nothing to act on.
  ymls <- list.files(
    testthat::test_path("..", "..", ".github", "workflows"),
    pattern = "\\.ya?ml$", full.names = TRUE
  )
  triggered <- Filter(
    function(f) any(grepl("^\\s*workflow_run:", readLines(f, warn = FALSE))),
    ymls
  )
  # Guards the guard: if the trigger style is ever renamed, fail loudly rather
  # than vacuously passing over an empty set.
  expect_gt(length(triggered), 0L)

  for (f in triggered) {
    txt <- paste(readLines(f, warn = FALSE), collapse = " ")
    if (!grepl("workflow_run\\.conclusion", txt)) next
    expect_true(
      grepl("conclusion\\s*==\\s*'failure'", txt),
      info = paste(
        basename(f),
        "gates on workflow_run.conclusion but never admits 'failure' --",
        "a partially failed upstream that still committed output would be",
        "silently discarded."
      )
    )
  }
})

# ---- Odds-scrape + decide loop isolation (final review F1) ------------------
#
# scripts/02_scrape_odds.R and scripts/04_decide.R walked config/leagues.yml
# order (basketball, handball, football) in bare loops, so one plain error on
# the handball API path (an unexpected current-program shape, an HTML 200 body)
# or in handball's paper decide aborted the loop BEFORE football -- the live
# money path -- and scrape-odds.yml then committed nothing from the run.

test_that("run_per_league continues past a failing key and reports it", {
  seen <- character()
  res <- suppressMessages(run_per_league(
    c("basketball_iceland", "handball_iceland", "football_iceland"),
    function(key) {
      seen <<- c(seen, key)
      if (identical(key, "handball_iceland")) {
        stop("parse_lengjan_program: unexpected current-program shape")
      }
      nchar(key)
    },
    what = "odds scrape"
  ))
  expect_equal(seen, c("basketball_iceland", "handball_iceland", "football_iceland"))
  expect_equal(nrow(res$failed), 1L)
  expect_named(res$failed, c("key", "message"))
  expect_equal(res$failed$key, "handball_iceland")
  expect_match(res$failed$message, "unexpected current-program shape")
  expect_named(res$results, c("basketball_iceland", "handball_iceland", "football_iceland"))
  expect_equal(res$results$football_iceland, nchar("football_iceland"))
  expect_null(res$results$handball_iceland)
})

test_that("a failing first key does not prevent the second key's fn from running", {
  ran_second <- FALSE
  res <- suppressMessages(run_per_league(
    c("handball_iceland", "football_iceland"),
    function(key) {
      if (identical(key, "handball_iceland")) stop("boom")
      ran_second <<- TRUE
      7L
    }
  ))
  expect_true(ran_second)
  expect_equal(res$failed$key, "handball_iceland")
  expect_equal(res$results$football_iceland, 7L)
})

test_that("run_per_league alerts loudly, naming the league and the error", {
  msgs <- character()
  withCallingHandlers(
    run_per_league("handball_iceland", function(key) stop("HTML body, not JSON"),
      what = "odds scrape"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  hit <- grepl("handball_iceland", msgs) & grepl("HTML body, not JSON", msgs)
  expect_true(any(hit))
  expect_true(any(grepl("odds scrape", msgs[hit])))
})

test_that("run_per_league labels work items by names() and passes the value", {
  # scripts/04_decide.R iterates (league, sex) rows: it passes row indices
  # named "key (sex)" so the alert and the failed frame name the cell.
  got <- integer()
  res <- suppressMessages(run_per_league(
    c("handball_iceland (male)" = 1L, "football_iceland (male)" = 2L),
    function(i) {
      got <<- c(got, i)
      if (i == 1L) stop("decide boom")
      invisible(NULL)
    },
    what = "decide"
  ))
  expect_equal(got, c(1L, 2L))
  expect_equal(res$failed$key, "handball_iceland (male)")
})

test_that("an all-green run_per_league reports no failures and zero-row frames", {
  res <- run_per_league(character(), function(key) stop("never called"))
  expect_equal(nrow(res$failed), 0L)
  expect_named(res$failed, c("key", "message"))
  expect_length(res$results, 0L)
})

test_that("a Lengjan fetch error stays a 0-row soft-fail under run_per_league", {
  # ingest_one_lengjan()'s lengjan_fetch_error contract is unchanged: a
  # transport blip is 0 rows, not a failed league. A plain (parse) error is a
  # failure, contained to its own league.
  testthat::local_mocked_bindings(
    .is_league_active = function(active_path, key) TRUE,
    ingest_lengjan_api = function(leagues, ...) {
      if (identical(names(leagues), "basketball_iceland")) {
        stop(structure(
          class = c("lengjan_fetch_error", "error", "condition"),
          list(message = "Lengjan API /current-program: HTTP 503", call = NULL)
        ))
      }
      if (identical(names(leagues), "handball_iceland")) {
        stop("parse_lengjan_program: unexpected current-program shape")
      }
      5L
    }
  )
  lj <- list(source = "api", competitions = list(list(id = "1", name = "x", sex = "male")))
  res <- suppressMessages(run_per_league(
    c("basketball_iceland", "handball_iceland", "football_iceland"),
    function(key) {
      ingest_one_lengjan(
        list(sport = sub("_iceland$", "", key), country = "iceland"), lj,
        key, "active.json"
      )
    },
    what = "odds scrape"
  ))
  expect_equal(res$failed$key, "handball_iceland")
  expect_identical(res$results$basketball_iceland, 0L)
  expect_identical(res$results$football_iceland, 5L)
})

test_that("scripts/02_scrape_odds.R and 04_decide.R isolate leagues and exit non-zero on ANY failure", {
  for (s in c("02_scrape_odds.R", "04_decide.R")) {
    src <- readLines(testthat::test_path("..", "..", "scripts", s), warn = FALSE)
    body <- src[!grepl("^\\s*#", src)]
    expect_true(any(grepl("run_per_league(", body, fixed = TRUE)), info = s)
    expect_true(any(grepl('quit(save = "no", status = 1L)', body, fixed = TRUE)), info = s)
    # No bare per-league loop left calling the step directly.
    expect_false(any(grepl("^for \\(", body)), info = s)
  }
})

test_that("scrape-odds commits and decide-publish publishes after a failed step", {
  # A red scrape (one league failed) must still commit the leagues that did
  # scrape; a red decide must still publish. `success() || failure()` rather
  # than always(): a cancelled run commits and publishes nothing. Located by
  # step name and scanned up to the step's run:, as in the always() guard.
  targets <- list(
    c("scrape-odds.yml", "Commit if data changed"),
    c("decide-publish.yml", "Publish JSONs")
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
      any(grepl("^\\s*if:\\s*success\\(\\)\\s*\\|\\|\\s*failure\\(\\)\\s*$", step)),
      info = paste(t[1], "->", t[2])
    )
  }
})

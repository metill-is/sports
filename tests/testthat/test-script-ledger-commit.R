# tests/testthat/test-script-ledger-commit.R
#
# Guards that scripts/place_bets.R, scripts/06_settle.R, and
# scripts/auto_place.R all
#   (a) invoke commit_ledger_changes() after a successful run, and
#   (b) propagate a commit failure via quit(status = 1L).
# (auto_place.R was missed when it shipped 2026-06-03; the 2026-06-10
# orphaned-ledger incident is the regression this list-entry prevents.)
#
# Spawning the actual Rscript in a subprocess would be a slow/flaky
# integration test (heavy devtools::load_all, chromote dep). Static
# content checks catch every regression that matters in practice:
# accidentally deleting the commit call, or accidentally swallowing
# a failure into a continue-anyway branch.

scripts_to_check <- list(
  list(
    file = "scripts/place_bets.R",
    label = "placer wrapper",
    needs_dry_run_guard = TRUE
  ),
  list(
    file = "scripts/06_settle.R",
    label = "settle wrapper",
    needs_dry_run_guard = FALSE
  ),
  list(
    file = "scripts/auto_place.R",
    label = "unattended placer wrapper",
    needs_dry_run_guard = FALSE
  )
)

test_that("placer and settle wrapper scripts call commit_ledger_changes after success", {
  for (s in scripts_to_check) {
    path <- here::here(s$file)
    if (!file.exists(path)) {
      fail(sprintf("expected script does not exist: %s", s$file))
      next
    }
    src <- readLines(path, warn = FALSE)
    expect_true(
      any(grepl("commit_ledger_changes", src, fixed = TRUE)),
      info = sprintf(
        "%s (%s) must call commit_ledger_changes() after the layer succeeds",
        s$file, s$label
      )
    )
  }
})

test_that("the commit-failure branch itself quits with status 1 in every wrapper", {
  for (s in scripts_to_check) {
    path <- here::here(s$file)
    if (!file.exists(path)) next
    src <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # The quit must live INSIDE the `failed = {...}` switch branch. A
    # whole-file grep was satisfiable by an unrelated quit elsewhere in the
    # script (auto_place.R has one for sync_failed), letting a
    # log-and-continue failed branch pass -- the swallowed-commit-failure
    # regression this file exists to catch. Brace-free branch bodies are a
    # convention here, so the non-greedy [^}]* capture is exact.
    branch <- regmatches(src, regexpr("failed\\s*=\\s*\\{[^}]*\\}", src))
    expect_length(branch, 1L)
    expect_match(
      branch, "quit\\([^)]*status\\s*=\\s*1L?\\)",
      info = sprintf(
        "%s (%s): the failed = { } branch must itself quit(status = 1L)",
        s$file, s$label
      )
    )
  }
})

test_that("placer wrapper gates the commit on !dry_run", {
  path <- here::here("scripts/place_bets.R")
  if (!file.exists(path)) skip("placer wrapper missing")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # We want to be defensive about ever committing in --dry-run mode,
  # even though place_bets() never reports status == "placed" in dry-run.
  expect_true(
    grepl("!dry_run", src, fixed = TRUE),
    info = "placer wrapper must gate commit on !dry_run"
  )
})

test_that("unattended wrapper skips the commit on disabled/locked/sync_failed runs", {
  path <- here::here("scripts/auto_place.R")
  if (!file.exists(path)) skip("unattended wrapper missing")
  src <- paste(readLines(path, warn = FALSE), collapse = "\n")
  # sync_failed in the skip set is what keeps money commits off feature
  # branches: a branch-guard refusal reports sync_failed, and committing
  # then would land ledger rows on whatever branch is checked out.
  expect_match(src, 'c\\("disabled", "locked", "sync_failed"\\)')
})

test_that("unattended wrapper settles inside the commit gate, before the commit", {
  # 2026-09-25 review: only a manual scripts/06_settle.R ever settled, and
  # 37 bets sat unsettled for 10+ days. Settlement belongs in the local cycle
  # (never CI: this machine is the ledger's canonical writer), behind the
  # same disabled/locked/sync_failed gate as the ledger commit, and a settle
  # error must not stop the placement commit.
  path <- testthat::test_path("..", "..", "scripts", "auto_place.R")
  if (!file.exists(path)) skip("unattended wrapper missing")
  src <- readLines(path, warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]
  gate <- grep('c\\("disabled", "locked", "sync_failed"\\)', code)
  settle <- grep("settle_ledger\\(", code)
  commit <- grep("commit_ledger_changes\\(", code)
  expect_length(gate, 1L)
  expect_length(settle, 1L)
  expect_length(commit, 1L)
  expect_gt(settle, gate)
  expect_lt(settle, commit)
  # tryCatch wraps the call: it opens on the settle line or just above it.
  window <- code[max(1L, settle - 2L):settle]
  expect_true(any(grepl("tryCatch(", window, fixed = TRUE)))
  # The settled count reaches the single commit message.
  expect_true(any(grepl("bet(s) settled", code, fixed = TRUE)))
})

test_that("unattended wrapper's settle step never throws and always releases the lock", {
  # Behavioural, not a grep: evaluate the script's own `n_settled <- ...`
  # expression against mocked lock/settle functions. Whatever fails in the
  # settle step (settle_ledger, or the lock file I/O around it), the value is
  # 0 and control reaches the placement commit.
  path <- testthat::test_path("..", "..", "scripts", "auto_place.R")
  if (!file.exists(path)) skip("unattended wrapper missing")
  exprs <- parse(path, keep.source = FALSE)
  find_assign <- function(e) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(e[[2]], as.name("n_settled")) && is.call(e[[3]])) {
      return(e)
    }
    if (is.call(e)) {
      args <- as.list(e)[-1]
      for (i in seq_along(args)) {
        # Only calls can hold the assignment; this also steps over R's empty
        # argument symbol (e.g. x[, 1]), which cannot be bound to a variable.
        if (!is.call(args[[i]])) next
        hit <- find_assign(args[[i]])
        if (!is.null(hit)) return(hit)
      }
    }
    NULL
  }
  settle_expr <- NULL
  for (e in exprs) {
    settle_expr <- find_assign(e)
    if (!is.null(settle_expr)) break
  }
  expect_false(is.null(settle_expr))

  run <- function(acquire, settle) {
    released <- 0L
    env <- new.env(parent = globalenv())
    env$root <- tempdir()
    env$acquire_auto_place_lock <- function(root) acquire()
    env$release_auto_place_lock <- function(root) released <<- released + 1L
    env$settle_ledger <- function(root) settle()
    env$cli <- NULL
    suppressMessages(eval(settle_expr, env))
    list(n = env$n_settled, released = released)
  }

  ok <- run(function() TRUE, function() invisible(3L))
  expect_identical(ok$n, 3L)
  expect_identical(ok$released, 1L)

  settle_err <- run(function() TRUE, function() stop("parquet write failed"))
  expect_identical(settle_err$n, 0L)
  expect_identical(settle_err$released, 1L)

  lock_err <- run(function() stop("lock file unreadable"), function() 5L)
  expect_identical(lock_err$n, 0L)

  held <- run(function() FALSE, function() stop("must not settle without the lock"))
  expect_identical(held$n, 0L)
  expect_identical(held$released, 0L)
})

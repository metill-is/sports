# Audit 2026-11-03 finding B: scripts/place_bets.R (--live) and
# scripts/06_settle.R read, modify and rewrite whole ledger partitions but took
# no lock, while the unattended placer holds the auto-place lock around its own
# placement and settle. A manual --live run during an autoplace settle could
# lose the just-placed row. Both scripts now go through with_ledger_lock().

lock_file <- function(root) fs::path(root, ".auto_place.lock")

hold_lock_live <- function(root) {
  # Our own PID is alive, so acquire_auto_place_lock() treats it as held.
  writeLines(as.character(Sys.getpid()), lock_file(root))
}

# ---- with_ledger_lock() ------------------------------------------------------

test_that("with_ledger_lock holds the lock while code runs, then releases it", {
  root <- withr::local_tempdir()
  holder <- NULL
  out <- with_ledger_lock(
    {
      holder <- readLines(lock_file(root))
      "done"
    },
    root = root,
    what = "test writer"
  )
  expect_equal(out, "done")
  expect_equal(holder, as.character(Sys.getpid()))
  expect_false(fs::file_exists(lock_file(root)))
})

test_that("with_ledger_lock stops without running code when another run holds the lock", {
  root <- withr::local_tempdir()
  hold_lock_live(root)
  ran <- FALSE
  expect_error(
    with_ledger_lock(ran <- TRUE, root = root, what = "test writer"),
    "test writer: .*holds the ledger lock"
  )
  expect_false(ran)
  # The holder's lock is left alone.
  expect_equal(readLines(lock_file(root)), as.character(Sys.getpid()))
})

test_that("with_ledger_lock releases the lock when code errors", {
  root <- withr::local_tempdir()
  expect_error(
    with_ledger_lock(stop("parquet write failed"), root = root, what = "test writer"),
    "parquet write failed"
  )
  expect_false(fs::file_exists(lock_file(root)))
})

test_that("with_ledger_lock reclaims a lock left by a dead process", {
  root <- withr::local_tempdir()
  writeLines("999999", lock_file(root))
  expect_equal(with_ledger_lock(1L, root = root, what = "test writer"), 1L)
  expect_false(fs::file_exists(lock_file(root)))
})

# ---- The manual writer scripts use it ----------------------------------------
#
# Behavioural, not a grep: evaluate each script's own top-level writer
# assignment against a mocked ledger writer in a temp root, as
# test-script-ledger-commit.R does for the unattended settle step. The
# script's `root <- here::here("data")` is asserted, never evaluated, so no
# test touches the checkout's real data/ lock.

script_exprs <- function(file) {
  path <- testthat::test_path("..", "..", "scripts", file)
  as.list(parse(path, keep.source = FALSE))
}

assigns_to <- function(e, targets) {
  is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
    as.character(e[[2]]) %in% targets
}

# Evaluates `targets` from `file` with `writer_name` mocked; the mock records
# into `probe` whether it ran and whether this process held the lock then.
run_script_writer <- function(file, targets, writer_name, root, probe,
                              vars = list()) {
  exprs <- script_exprs(file)
  expect_true(any(vapply(exprs, assigns_to, logical(1), targets = "root")))
  picked <- Filter(function(e) assigns_to(e, targets), exprs)
  expect_length(picked, length(targets))

  env <- new.env(parent = environment())
  for (nm in names(vars)) assign(nm, vars[[nm]], envir = env)
  env$root <- root
  probe$called <- FALSE
  assign(writer_name, function(...) {
    probe$called <- TRUE
    probe$held <- fs::file_exists(lock_file(root)) &&
      identical(readLines(lock_file(root)), as.character(Sys.getpid()))
    tibble::tibble()
  }, envir = env)
  for (e in picked) eval(e, env)
  invisible(probe)
}

placer_vars <- function(dry_run) {
  list(
    dry_run = dry_run, interactive = FALSE, today_only = FALSE,
    headless = TRUE, leagues = NULL, target_date = NULL
  )
}
placer_targets <- c("place", "results")

test_that("scripts/place_bets.R --live places under the ledger lock", {
  root <- withr::local_tempdir()
  probe <- new.env()
  run_script_writer("place_bets.R", placer_targets, "place_bets", root, probe,
    vars = placer_vars(dry_run = FALSE)
  )
  expect_true(probe$called)
  expect_true(probe$held)
  expect_false(fs::file_exists(lock_file(root)))
})

test_that("scripts/place_bets.R --live refuses to place while another run holds the lock", {
  root <- withr::local_tempdir()
  hold_lock_live(root)
  probe <- new.env()
  expect_error(
    run_script_writer("place_bets.R", placer_targets, "place_bets", root, probe,
      vars = placer_vars(dry_run = FALSE)
    ),
    "holds the ledger lock"
  )
  expect_false(probe$called)
})

test_that("scripts/place_bets.R dry run neither waits for nor takes the lock", {
  root <- withr::local_tempdir()
  hold_lock_live(root)
  probe <- new.env()
  run_script_writer("place_bets.R", placer_targets, "place_bets", root, probe,
    vars = placer_vars(dry_run = TRUE)
  )
  expect_true(probe$called)
  # The other run's lock is still there, untouched.
  expect_equal(readLines(lock_file(root)), as.character(Sys.getpid()))
})

test_that("scripts/06_settle.R settles under the ledger lock", {
  root <- withr::local_tempdir()
  probe <- new.env()
  run_script_writer("06_settle.R", "n", "settle_ledger", root, probe)
  expect_true(probe$called)
  expect_true(probe$held)
  expect_false(fs::file_exists(lock_file(root)))
})

test_that("scripts/06_settle.R refuses to settle while another run holds the lock", {
  root <- withr::local_tempdir()
  hold_lock_live(root)
  probe <- new.env()
  expect_error(
    run_script_writer("06_settle.R", "n", "settle_ledger", root, probe),
    "holds the ledger lock"
  )
  expect_false(probe$called)
})

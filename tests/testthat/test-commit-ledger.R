# tests/testthat/test-commit-ledger.R
#
# Unit tests for commit_ledger_changes() — the script-layer helper that
# auto-commits data/decisions/ledger/ after a successful placer or settle
# run. See commit 121710d for the 2026-05-08 incident that motivated it.

git_run_silent <- function(args, cwd) {
  out <- suppressWarnings(system2(
    "git", c("-C", cwd, args),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("git ", paste(args, collapse = " "), " failed:\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }
  out
}

init_temp_repo <- function(dir) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  git_run_silent(c("init", "--initial-branch=main"), dir)
  git_run_silent(c("config", "user.email", "test@example.com"), dir)
  git_run_silent(c("config", "user.name", "Test User"), dir)
  git_run_silent(c("config", "commit.gpgsign", "false"), dir)
  writeLines("# tmp", file.path(dir, "README.md"))
  git_run_silent(c("add", "README.md"), dir)
  git_run_silent(c("commit", "-m", "init"), dir)
  invisible(dir)
}

write_fake_ledger <- function(dir, sport = "football", country = "iceland") {
  led_dir <- file.path(
    dir, "data", "decisions", "ledger",
    sprintf("sport=%s", sport), sprintf("country=%s", country)
  )
  dir.create(led_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("fake-parquet-bytes", file.path(led_dir, "part-0.parquet"))
  invisible(led_dir)
}

test_that("commit_ledger_changes commits ledger changes and returns 'committed'", {
  skip_if(Sys.which("git") == "", "git not available")
  tmp <- withr::local_tempdir()
  init_temp_repo(tmp)
  write_fake_ledger(tmp)

  res <- commit_ledger_changes(tmp, "data(ledger): unit test")

  expect_equal(res$status, "committed")
  expect_equal(res$message, "data(ledger): unit test")

  log_lines <- git_run_silent(c("log", "--oneline"), tmp)
  expect_match(log_lines[[1]], "data\\(ledger\\): unit test", fixed = FALSE)
})

test_that("commit_ledger_changes only stages data/decisions/ledger/ — unrelated WIP is left alone", {
  skip_if(Sys.which("git") == "", "git not available")
  tmp <- withr::local_tempdir()
  init_temp_repo(tmp)
  write_fake_ledger(tmp)

  # Add an unrelated dirty file outside the ledger path
  writeLines("source code WIP", file.path(tmp, "scratch.R"))

  res <- commit_ledger_changes(tmp, "data(ledger): isolation test")
  expect_equal(res$status, "committed")

  # scratch.R must still be untracked, not swept into the commit
  status_lines <- suppressWarnings(system2(
    "git", c("-C", tmp, "status", "--porcelain"),
    stdout = TRUE, stderr = TRUE
  ))
  expect_true(any(grepl("\\?\\? scratch\\.R", status_lines)))
})

test_that("commit_ledger_changes returns 'nothing_to_commit' when ledger is clean", {
  skip_if(Sys.which("git") == "", "git not available")
  tmp <- withr::local_tempdir()
  init_temp_repo(tmp)
  # no ledger changes

  res <- commit_ledger_changes(tmp, "data(ledger): no-op")
  expect_equal(res$status, "nothing_to_commit")
})

test_that("commit_ledger_changes returns 'failed' outside a git repo", {
  skip_if(Sys.which("git") == "", "git not available")
  tmp <- withr::local_tempdir()
  # No `git init`
  write_fake_ledger(tmp)

  res <- commit_ledger_changes(tmp, "data(ledger): no-repo")
  expect_equal(res$status, "failed")
  expect_true(nzchar(res$output))
})

test_that("commit_ledger_changes validates its arguments", {
  expect_error(commit_ledger_changes(NULL, "msg"))
  expect_error(commit_ledger_changes("/tmp", ""))
  expect_error(commit_ledger_changes("/tmp", NULL))
  expect_error(commit_ledger_changes(c("/a", "/b"), "msg"))
})

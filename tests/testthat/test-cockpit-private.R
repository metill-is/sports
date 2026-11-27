# tests/testthat/test-cockpit-private.R
#
# The Sports cockpit's state (decision log, owner cards, comments, status) lives in the
# separate private repo, cloned at cockpit/. This repo is public, so
# nothing under cockpit/ may ever be tracked here. Fails the build if it is.

test_that("cockpit/ is git-ignored", {
  ignore <- readLines(here::here(".gitignore"), warn = FALSE)
  expect_true("cockpit/" %in% trimws(ignore))
})

test_that("nothing under cockpit/ is tracked by the public repo", {
  if (Sys.which("git") == "") testthat::skip("git not available")
  tracked <- suppressWarnings(system2(
    "git", c("-C", here::here(), "ls-files", "--", "cockpit"),
    stdout = TRUE, stderr = FALSE
  ))
  if (!is.null(attr(tracked, "status"))) testthat::skip("not a git checkout")
  expect_identical(tracked, character(0))
})

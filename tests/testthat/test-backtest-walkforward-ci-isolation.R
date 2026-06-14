# tests/testthat/test-backtest-walkforward-ci-isolation.R
#
# G10: the walk-forward harness is a multi-hour Stan sweep, read-only, never
# on CI. Fails the build if any workflow references its symbols.

test_that("no GitHub Actions workflow references the walk-forward harness", {
  workflow_dir <- here::here(".github", "workflows")
  if (!dir.exists(workflow_dir)) {
    testthat::skip("no .github/workflows yet")
  }
  yml_files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(yml_files) == 0L) {
    testthat::skip("no workflow files yet")
  }
  forbidden <- c(
    "R/backtest-walkforward",
    "bt_walkforward",
    "0Nb_walkforward",
    "bt_walkforward_cutoff",
    "bt_wf_default_decide"
  )
  failures <- character(0)
  for (f in yml_files) {
    contents <- readLines(f, warn = FALSE)
    for (token in forbidden) {
      hit <- grep(token, contents, fixed = TRUE)
      if (length(hit) > 0L) {
        failures <- c(failures, sprintf(
          "%s: line %d references forbidden token %s",
          basename(f), hit[1L], shQuote(token)
        ))
      }
    }
  }
  if (length(failures) > 0L) {
    fail(paste(
      "CI workflow(s) reference the walk-forward harness.",
      "It is a multi-hour Stan sweep and must remain local-only.",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})

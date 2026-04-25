# tests/testthat/test-placer-ci-isolation.R
#
# Spec §6 mitigation for "Placer isolation slip" risk.
# Fails the build if any GitHub Actions workflow file references the
# placer or its credentials. Placer is local-only — see
# `.claude/rules/sports-betting.md` and CLAUDE.md "Local-only subsystem".

test_that("no GitHub Actions workflow references the placer", {
  workflow_dir <- here::here(".github", "workflows")
  if (!dir.exists(workflow_dir)) {
    testthat::skip("no .github/workflows yet")
  }
  yml_files <- list.files(workflow_dir, pattern = "\\.ya?ml$", full.names = TRUE)
  if (length(yml_files) == 0L) {
    testthat::skip("no workflow files yet")
  }

  forbidden <- c(
    "R/placer-",
    "placer_pipeline",
    "place_bets",
    "preview_bets",
    "LENGJAN_USER",
    "LENGJAN_PASS"
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
      "CI workflow(s) reference the placer.",
      "Placer must remain local-only.",
      paste("  -", failures, collapse = "\n"),
      sep = "\n"
    ))
  }
  expect_true(TRUE)
})

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

test_that("placer and settle wrapper scripts propagate commit failure via quit(status = 1)", {
  for (s in scripts_to_check) {
    path <- here::here(s$file)
    if (!file.exists(path)) next
    src <- paste(readLines(path, warn = FALSE), collapse = "\n")
    # Permissive match: quit(..., status = 1[L]) anywhere near the
    # commit failure handling. The combination of "failed =" branch
    # and quit(...status = 1...) is the contract.
    has_failed_branch <- grepl("failed\\s*=\\s*\\{", src)
    has_quit_one <- grepl("quit\\([^)]*status\\s*=\\s*1L?\\)", src)
    expect_true(
      has_failed_branch && has_quit_one,
      info = sprintf(
        "%s (%s) must handle res$status == \"failed\" with quit(status = 1L)",
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

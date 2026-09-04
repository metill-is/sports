# The bb/hb coverage spent its whole life skipping: 8 publish blocks gated on a
# machine-local backup path, 6 extract blocks on a gitignored 300MB fit.rds.
# This test exists so it can never quietly stop running again.

test_that("the bb/hb publish + extract tests carry no skip gates", {
  guarded <- c(
    "test-publish-basketball.R",
    "test-publish-handball.R",
    "test-extract-basketball-iceland.R",
    "test-extract-handball-iceland.R",
    # WS3's 2DT home-advantage units test (B5). Named here so it lands ungated
    # from day one; this file asserts hygiene only, never that test's content.
    "test-extract-2dt-home-advantage-units.R",
    # WS7's sport-neutral division accessors. Its expected_meetings block
    # asserts against live git-tracked data/facts/results, so a skip gate there
    # would silently retire the only check that a federation format change is
    # caught.
    "test-iceland-division-helpers.R",
    # WS9's per-sport publish registry. It is the sole author of every
    # "does this sport do X" fact, so a skip here would silently retire the
    # bb/hb half of the contract.
    "test-publish-profile.R",
    # WS9's one reader. Its bb/hb blocks are the only proof that the
    # generalised reader splits a 2DT partition by division at all.
    "test-extract-iceland-read.R",
    # WS9's one next_games contract. Its match-summary block is the only proof
    # that bb/hb emit football's field names and goal_diff_distribution.
    "test-publish-next-games.R",
    # WS10's regular-season boundary. Its Block A runs on committed REAL
    # basketball data (playoff-overhang.parquet); a skip there would retire the
    # only proof that the embedded urslitakeppni is cut before the league table
    # is simulated.
    "test-publish-format.R",
    # WS10's single-source-of-truth check on the goal-diff bin width. It is the
    # only thing stopping meta.units.diff_bin_width from drifting away from the
    # width the 2DT extractors actually binned with.
    "test-publish-profile-units.R",
    # WS8's sport-neutral per-round strength trajectory. It is the helper the
    # bb/hb round_strengths_quantiles surface is built from, so a skip here
    # would retire the only exact check of the (matchweek -> fit round index)
    # mapping all three sports share.
    "test-strength-trajectory.R"
  )
  banned <- c("skip(", "skip_if(", "skip_if_not(", "skip_if_not_installed(", "Sys.getenv")

  present <- guarded[file.exists(testthat::test_path(guarded))]
  # Every file except WS3's must exist by the end of WS2.
  expect_setequal(
    setdiff(guarded, "test-extract-2dt-home-advantage-units.R"),
    setdiff(present, "test-extract-2dt-home-advantage-units.R")
  )

  for (f in present) {
    src <- readLines(testthat::test_path(f), warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]
    for (pat in banned) {
      hits <- grep(pat, src, fixed = TRUE, value = TRUE)
      expect_length(hits, 0L)
      if (length(hits) > 0L) {
        cat("\n", f, " -> ", pat, ":\n", paste(hits, collapse = "\n"), "\n", sep = "")
      }
    }
  }
})

# The machine-local backup root (/Users/brynjolfurjonsson/sports-backup-*, or
# $SPORTS_BACKUP_ROOT) resolves on exactly one laptop, so every test gated on it
# is a permanent skip everywhere else. WS2 cleared it out of the four bb/hb
# files; these five football/replay files still carry it and are a later plan's
# to repoint. The allowlist is deliberately explicit so the debt is visible and
# cannot GROW: a new file reaching for that path fails the build here.
LEGACY_BACKUP_GATED <- c(
  "test-extract-football-iceland.R",
  "test-model-golden.R",
  "test-publish-football-round-predictions.R",
  "test-publish-football.R",
  "test-replay.R"
)

test_that("no new test file reaches for the machine-local backup root", {
  files <- list.files(testthat::test_path("."), pattern = "^(test|helper)-.*\\.R$")
  files <- setdiff(files, c(LEGACY_BACKUP_GATED, "test-fixture-skip-hygiene.R"))
  offenders <- character()
  for (f in files) {
    src <- readLines(testthat::test_path(f), warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]
    if (any(grepl("sports-backup-|SPORTS_BACKUP_ROOT", src))) {
      offenders <- c(offenders, f)
    }
  }
  expect_equal(offenders, character())
})

test_that("the legacy backup-root allowlist has no stale entries", {
  # A file that has been repointed must be struck from the list, so the
  # allowlist shrinks to nothing rather than quietly outliving the problem.
  for (f in LEGACY_BACKUP_GATED) {
    path <- testthat::test_path(f)
    expect_true(file.exists(path), info = f)
    if (file.exists(path)) {
      expect_true(
        any(grepl("sports-backup-|SPORTS_BACKUP_ROOT", readLines(path, warn = FALSE))),
        info = paste(f, "no longer uses the backup root -- drop it from LEGACY_BACKUP_GATED")
      )
    }
  }
})

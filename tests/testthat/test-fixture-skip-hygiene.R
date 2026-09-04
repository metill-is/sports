# The bb/hb coverage spent its whole life skipping: 8 publish blocks gated on a
# machine-local backup path, 6 extract blocks on a gitignored 300MB fit.rds.
# This test exists so it can never quietly stop running again.

test_that("the bb/hb publish + extract tests carry no skip gates", {
  guarded <- c(
    # SUPERSEDES test-publish-basketball.R and test-publish-handball.R, which
    # this list used to name. WS9 unified the two per-sport 2DT publishers into
    # publish_iceland_league(), so there is no per-sport publisher left to have
    # a per-sport test file. The B4 acceptance test is now the only proof the
    # two sports publish at all -- a skip gate there would restore exactly the
    # silence B4 hid in.
    "test-publish-b4-acceptance.R",
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
    "test-strength-trajectory.R",
    # WS8's hoisted 2DT posterior pulls. The B5 units guarantee is now the
    # composition of the pull and the band, so this is the only place the pull
    # half is checked in isolation.
    "test-extract-2dt-draw-pulls.R",
    # WS8's multi-division 2DT extractor. It is the only proof that the second
    # tier gets its own table rather than the top tier's base points, which is
    # a wrong table rather than a visible error.
    "test-extract-2dt-divisions.R",
    # WS8's 2DT round-strength trajectory. It carries the only assertion that
    # the trajectory's global round index IS prepare_data's round1/round2, so a
    # skip would retire the check that the published trajectory reads the round
    # it claims to.
    "test-extract-2dt-round-strengths.R",
    # WS8's partition-level fit_meta. It is the only assertion that the file
    # carries NO division column, which is what stops the reader's per-division
    # split from filtering it to zero rows on every cell.
    "test-extract-fit-meta.R",
    # WS10's meta v2 contract. It publishes all 17 cells and is the only place
    # the D3 relabel is checked in the PAYLOAD -- season_scope, postseason,
    # final_positions.basis, and the assertion that no bb/hb JSON carries
    # p_winner, p_top_six or the word Islandsmeistari.
    "test-publish-meta-contract.R",
    # WS10's cross-cell next_games lock. It is the only check that the bin
    # widths in next_games.json match the width meta.units declares, which is
    # what makes the units claim falsifiable rather than decorative.
    "test-publish-next-games-contract.R",
    # WS12's publish-freshness check. Its "in-season, active, no publish output
    # and no extract partition -> FAIL" block is the single assertion that
    # would have made B4 visible; a skip gate there restores the silence.
    "test-health-publish-freshness.R",
    # WS12's season-resolution check. It is what distinguishes "the season is
    # genuinely over" from "the scraper went blind in October" -- two states
    # that are identical in the results table -- so a skip retires the only
    # alarm for a silent federation-id regression.
    "test-health-season-resolution.R",
    # WS11's schema generator. Its byte-equality block is the only thing
    # stopping a hand edit to a generated per-sport schema from being silently
    # reverted by the next render, and its ASCII block guards the verified
    # jsonlite <U+2014> corruption.
    "test-publish-schema-generation.R",
    # WS12's format-agreement check. Its triple-round-robin case is the only
    # coverage of the fact that 2 * (n_teams - 1) is wrong for four of the
    # seven measured 2DT cells, which is why n_rounds is published at all.
    "test-health-format-agreement.R",
    # WS12's per-target failure isolation. Its basketball-fails-football-still-
    # publishes block is the only proof that one 2DT abort cannot take
    # football's nine live cells down in the same run, which is precisely what
    # WS11's fail-closed validation default makes possible.
    "test-pipeline-run-isolation.R",
    # WS11's bb/hb publish schemas. It is the only proof that the eight
    # fixture-published cells satisfy the contract that is about to be armed on
    # both sides of the metill-platform rsync, and the only place the D3
    # p_top_six / p_winner refusal is checked in the CONTRACT rather than the
    # payload.
    "test-publish-schema-draft.R",
    # WS11's arming precondition. It is the only assertion that the stale
    # pre-division bb/hb cells stay deleted and that the publish tree stays
    # clear of metill-platform's rsync floor.
    "test-publish-legacy-cells.R"
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

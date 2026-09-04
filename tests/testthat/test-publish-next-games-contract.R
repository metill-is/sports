# ONE next_games contract, asserted on every published cell of all three sports.
#
# This is a CONTRACT LOCK, not a TDD cycle. The implementation
# (.next_games_rows_pfi(), R/publish-next-games.R) landed with the unified
# publisher; this file pins the result across every configured cell so a later
# per-sport shim cannot creep back in. It was green on arrival -- see the
# commit body for the evidence line.
#
# The fixture's upcoming matches are dated 2100-01-16..2100-01-20 against
# FIXTURE_END_DATE 2100-01-15, so the horizon filter admits them with no
# Sys.Date() arithmetic anywhere near a date literal.

test_that("every next_games match ships exactly the contract key set", {
  out <- .publish_all_cells()
  # The retired bespoke 2DT names. p_tie is a deliberate contract break --
  # nothing on metill-platform reads the bb/hb paths yet.
  retired <- c("mean_home", "mean_away", "mean_diff", "p_home", "p_away", "p_tie")

  non_empty <- character()
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    ng <- .read_cell_json(cell, "next_games.json")
    if (length(ng$matches) == 0L) next
    non_empty <- c(non_empty, id)
    for (m in ng$matches) {
      expect_setequal(names(m), .NEXT_GAMES_COLUMNS)
      expect_length(intersect(names(m), retired), 0L)
    }
  }
  # Every sport must contribute at least one non-empty cell, or the loop above
  # asserts nothing about it.
  expect_true(any(startsWith(non_empty, "football")))
  expect_true(any(startsWith(non_empty, "basketball")))
  expect_true(any(startsWith(non_empty, "handball")))
})

test_that("goal_diff_distribution is binned at the width meta declares", {
  # The cross-FILE consistency check that makes the units claim falsifiable:
  # meta.units.diff_bin_width is 1 stig for football, 5 for basketball and
  # 2 for handball, and the bins in next_games.json must actually be on it.
  out <- .publish_all_cells()
  checked <- 0L
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    width <- .read_cell_json(cell, "meta.json")$units$diff_bin_width
    for (m in .read_cell_json(cell, "next_games.json")$matches) {
      gdd <- m$goal_diff_distribution
      expect_gt(length(gdd), 0L)
      diffs <- vapply(gdd, function(b) b$diff, numeric(1))
      ps <- vapply(gdd, function(b) b$p, numeric(1))
      expect_setequal(names(gdd[[1L]]), c("diff", "p"))
      expect_true(all(diffs %% width == 0), info = id)
      expect_equal(sum(ps), 1, tolerance = 1e-6, info = id)
      checked <- checked + 1L
    }
  }
  expect_gt(checked, 0L)
})

test_that("division_code is the configured badge and venue is football-only", {
  out <- .publish_all_cells()
  for (cell in .published_cells(out)) {
    id <- paste(cell$sport, cell$sex, cell$division)
    badges <- .iceland_division_badges(cell$key, cell$sex)
    for (m in .read_cell_json(cell, "next_games.json")$matches) {
      expect_true("venue" %in% names(m), info = id)
      if (!identical(cell$sport, "football")) {
        # An indoor league has no static ground table, and it must not inherit
        # football's: the club names overlap across sports.
        expect_null(m$venue, info = id)
      }
      expect_true(m$division %in% names(badges), info = id)
      expect_equal(m$division_code, unname(badges[[m$division]]), info = id)
    }
  }
})

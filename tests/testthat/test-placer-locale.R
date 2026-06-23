# tests/testthat/test-placer-locale.R
#
# The placer entry scripts run unattended under launchd (auto_place.R) and
# interactively (place_bets.R / preview_bets.R). launchd fires them under the
# C locale, where R cannot translate the Icelandic literals in the package
# source ("unable to translate 'maí' to native encoding" warnings) and any
# non-defensive Icelandic string matching (team / competition names) risks
# mojibake -> no_match_id -> unplaced bets. Every other scripts/0N_*.R entry
# point already pins a UTF-8 locale before devtools::load_all(); this guards
# that the three placer scripts do too.

test_that("placer entry scripts pin a UTF-8 locale before load_all()", {
  scripts <- c("auto_place.R", "place_bets.R", "preview_bets.R")
  failures <- character(0)
  for (s in scripts) {
    # test_path() anchors to this test file's tree (tests/testthat/), so it
    # reads the scripts being committed -- here::here() follows a worktree's
    # .git linkage back to the main checkout and would read the wrong tree.
    path <- testthat::test_path("..", "..", "scripts", s)
    if (!file.exists(path)) {
      failures <- c(failures, sprintf("%s: missing", s))
      next
    }
    lines <- readLines(path, warn = FALSE)
    loc_idx <- grep("Sys\\.setlocale\\(.*UTF-8", lines)
    # Match the qualified call only, not a comment that mentions load_all().
    load_idx <- grep("devtools::load_all", lines)
    if (length(loc_idx) == 0L) {
      failures <- c(failures, sprintf("%s: no Sys.setlocale(..., UTF-8) call", s))
      next
    }
    if (length(load_idx) > 0L && min(loc_idx) > min(load_idx)) {
      failures <- c(failures, sprintf(
        "%s: Sys.setlocale (line %d) must come before load_all (line %d)",
        s, min(loc_idx), min(load_idx)
      ))
    }
  }
  if (length(failures) > 0L) {
    fail(paste("placer locale guard:", paste(failures, collapse = "; ")))
  }
  expect_equal(length(failures), 0L)
})

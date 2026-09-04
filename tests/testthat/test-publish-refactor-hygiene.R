# A machine-checkable guard against resurrecting the shape B4 and B5 lived in.
#
# B4: publish_one() read a gitignored fit RDS for basketball and handball, so
# CI skipped them silently forever. B5: two parallel per-sport publishers drifted
# apart from football's, which is how the 2DT home-advantage units bug survived.
# Both are fixed by DELETION -- of the fit-RDS input, of the two publishers, and
# of the football-only reader and division helpers they hung off. A guard that
# only checked behaviour would let a future session reintroduce the parallel
# shape and simply not notice.
#
# The grep is over raw text, comments and roxygen included, and it walks man/
# too so a stale generated .Rd is caught. Prose that merely NAMES a dead symbol
# is a reference: the rename history lives in git and in
# .claude/rules/publish-layer.md, not scattered through source files that a
# later reader will mistake for live API.

.hygiene_files <- function() {
  pkg <- testthat::test_path("..", "..")
  dirs <- file.path(pkg, c("R", "scripts", "tools", "man"))
  files <- unlist(lapply(dirs[dir.exists(dirs)], list.files,
    pattern = "[.](R|Rd)$", full.names = TRUE, recursive = TRUE
  ), use.names = FALSE)
  tests <- list.files(
    testthat::test_path("."),
    pattern = "[.]R$", full.names = TRUE
  )
  tests <- tests[basename(tests) != "test-publish-refactor-hygiene.R"]
  c(files, tests)
}

.hygiene_hits <- function(pattern) {
  hits <- character()
  for (f in .hygiene_files()) {
    src <- readLines(f, warn = FALSE)
    idx <- grep(pattern, src)
    if (length(idx) > 0L) {
      hits <- c(hits, sprintf("%s:%d", basename(f), idx))
    }
  }
  hits
}

test_that("the retired 2DT publishers and football-only helpers are unreferenced", {
  retired <- c(
    # The two parallel publishers WS9 replaced with one loop (737 lines).
    "publish_basketball_iceland",
    "publish_handball_iceland",
    # The football-only reader, generalised into read_extracted_iceland().
    "read_extracted_football",
    "football_extract_partition_exists",
    # WS7's deletion: the football-only division helpers, no aliases.
    "[.]football_iceland_division_",
    # The duplicate standings tabulation, replaced by the unified publisher's
    # inline one driven by profile$points.
    "compute_standings_rows_2dt"
  )
  for (pattern in retired) {
    expect_equal(
      .hygiene_hits(pattern), character(),
      info = pattern
    )
  }
})

test_that("publish_one has no fit-RDS input left", {
  src <- readLines(
    testthat::test_path("..", "..", "R", "publish-pipeline.R"),
    warn = FALSE
  )
  expect_equal(grep("fit\\.rds", src, value = TRUE), character())
  expect_equal(grep('beliefs", "fits"', src, value = TRUE), character())
  # And the argument list carries nothing fit-shaped. This is the final SC-7
  # signature: end_date seventh (WS9), schema_dir EIGHTH (WS11). Both are
  # defaulted and passed by name everywhere.
  expect_equal(
    names(formals(publish_one)),
    c(
      "static", "betting", "key", "sex", "root", "validate", "end_date",
      "schema_dir"
    )
  )
})

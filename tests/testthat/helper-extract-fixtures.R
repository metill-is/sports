# Materialise the committed extracts fixture into a temp tree so tests that
# publish from it can write alongside without dirtying the repo.

# Copy the committed 2DT extracts partitions into a temp extracts root.
fixture_extracts_root <- function(sports = c("basketball", "handball"),
                                  env = parent.frame()) {
  tmp <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  src <- testthat::test_path("fixtures", "extracts")
  for (sport in sports) {
    from <- file.path(src, paste0("sport=", sport))
    if (dir.exists(from)) {
      file.copy(from, tmp, recursive = TRUE)
    }
  }
  tmp
}

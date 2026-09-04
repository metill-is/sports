# Build an `extracted` list from a CmdStanMCMC fit by chaining
# extract_football_iceland() + read_extracted_iceland() through a tempdir.
# Used by publish-football tests that exercise the per-cell publisher.
#
# The tempdir's lifetime is tied to the caller's frame via withr — one
# extracted set per test_that() block.
.build_extracted_football_for_test <- function(fit, league, sex, end_date) {
  extract_dir <- withr::local_tempdir(.local_envir = parent.frame())
  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = sex,
      fit_date = end_date,
      end_date = end_date,
      extracts_root = extract_dir
    )
  ))
  read_extracted_iceland(
    league,
    sex = sex,
    fit_date = end_date,
    extracts_root = extract_dir
  )
}

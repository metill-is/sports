# Build a Phase 2 `extracted` list from a CmdStanMCMC fit by chaining
# extract_football_iceland() + read_extracted_football() through a
# tempdir. Used by publish-football tests that previously called the
# legacy fit-based publisher; with Phase 2c that path is internal
# (.publish_football_iceland_from_fit_pfi) and the public publisher
# takes `extracted` instead.
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
      archive_root = extract_dir
    )
  ))
  read_extracted_football(
    league,
    sex = sex,
    fit_date = end_date,
    archive_root = extract_dir
  )
}

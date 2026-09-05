# Committed synthetic facts tree, materialised into a temp hive-partitioned data
# root. Mirrors setup_mini_root() in test-model-prepare.R but covers all three
# sports and both sexes, at far-future dates so it can never rot (the repo's
# time-bomb rule: no near-date literals in fixtures).
#
# The three constants and FIXTURE_DIVISIONS MUST stay identical to
# tools/make-extract-fixtures.R, which generates the committed parquets.

FIXTURE_END_DATE <- as.Date("2100-01-15")
FIXTURE_FIT_DATE <- as.Date("2100-01-01")
FIXTURE_N_DRAWS <- 50L

FIXTURE_DIVISIONS <- list(
  # BD / OD are 4 teams, not 6: team_strengths_quantiles is 9 cells x 99
  # quantiles per team, and at 6 teams the committed extracts tree came to
  # 318 KB -- past the then-250 KB budget. Every non-top division stays at 6,
  # which is also what makes the two divisions differ in size and so exercises
  # the extractor's per-division scoping. The budget is now 2048 KB (see
  # test-fixture-harness.R) because round_strengths_quantiles adds a matchweek
  # dimension to the same 9 x 99 grid.
  basketball = list(male = c(BD = 4L, `1D` = 6L), female = c(BD = 4L, `1D` = 6L)),
  handball   = list(male = c(OD = 4L, G66 = 6L), female = c(OD = 4L, G66 = 6L)),
  football   = list(
    male   = c(BD = 12L, LD1 = 6L, LD2 = 6L, LD3 = 6L, CUP = 4L),
    female = c(BD = 10L, LD1 = 6L, LD2 = 6L, CUP = 4L)
  )
)

fixture_division_teams <- function(sport, sex, division) {
  n <- FIXTURE_DIVISIONS[[sport]][[sex]][[division]]
  stopifnot(!is.null(n))
  sprintf(
    "%s%s %s %02d",
    toupper(substr(sport, 1L, 2L)), toupper(substr(sex, 1L, 1L)),
    division, seq_len(n)
  )
}

# Materialise the committed facts fixture into a temp data root.
# Lifetime is tied to `env` (the calling test_that frame by default).
fixture_facts_root <- function(env = parent.frame()) {
  tmp <- withr::local_tempdir(.local_envir = env)
  results <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "results.parquet")
  )
  schedules <- arrow::read_parquet(
    testthat::test_path("fixtures", "facts", "schedules.parquet")
  )
  write_table(results, "results", root = tmp)
  write_table(schedules, "schedules", root = tmp)
  tmp
}

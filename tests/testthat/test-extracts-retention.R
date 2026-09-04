# WS13 (Plan B follow-on): data/beliefs/extracts/ is the SOLE publish input
# and is git-tracked and committed by fit.yml on every run. Nothing pruned it,
# so it reached 1.3 GB across 99 partitions (~22 MB per football fit, ~24 fits
# a month) inside a 1.7 GB .git that CI full-clones on every workflow run.
#
# The invariant that makes pruning safe: publish reads the NEWEST partition
# per cell (read_extracted_iceland with fit_date = NULL), so the newest must
# survive unconditionally -- INCLUDING for a cell whose newest is months old.
# That is not hypothetical: basketball and handball are dormant all summer, so
# a pure date cutoff would delete every partition they have and silently break
# their publish the moment the season restarts.

.mk_part <- function(root, sport, sex, date) {
  d <- file.path(root, paste0("sport=", sport), "country=iceland",
                 paste0("sex=", sex), paste0("fit_date=", date))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines("x", file.path(d, "predicted_matches.parquet"))
  d
}

test_that("prune_extracts is a dry run by default and reports what it would drop", {
  root <- withr::local_tempdir()
  for (d in c("2026-01-01", "2026-08-01", "2026-09-01")) {
    .mk_part(root, "football", "male", d)
  }
  res <- prune_extracts(root = root, keep_days = 14L, keep_min = 1L,
                        now = as.Date("2026-09-04"))
  expect_true(all(dir.exists(file.path(
    root, "sport=football", "country=iceland", "sex=male",
    paste0("fit_date=", c("2026-01-01", "2026-08-01", "2026-09-01"))
  ))))
  expect_equal(nrow(res), 2L)
  expect_setequal(as.character(res$fit_date), c("2026-01-01", "2026-08-01"))
})

test_that("a DORMANT cell keeps its newest partition however old it is", {
  # THE invariant. Basketball in July: every partition months old. A date
  # cutoff alone deletes all of them and publish then has nothing to read.
  root <- withr::local_tempdir()
  for (d in c("2026-03-01", "2026-04-20", "2026-05-18")) {
    .mk_part(root, "basketball", "male", d)
  }
  prune_extracts(root = root, keep_days = 14L, keep_min = 1L,
                 now = as.Date("2026-09-04"), dry_run = FALSE)
  left <- basename(list.dirs(
    file.path(root, "sport=basketball", "country=iceland", "sex=male"),
    recursive = FALSE
  ))
  expect_equal(left, "fit_date=2026-05-18")
})

test_that("keep_min protects more than one partition per cell", {
  root <- withr::local_tempdir()
  for (d in c("2026-03-01", "2026-04-20", "2026-05-18")) {
    .mk_part(root, "handball", "female", d)
  }
  prune_extracts(root = root, keep_days = 14L, keep_min = 2L,
                 now = as.Date("2026-09-04"), dry_run = FALSE)
  left <- sort(basename(list.dirs(
    file.path(root, "sport=handball", "country=iceland", "sex=female"),
    recursive = FALSE
  )))
  expect_equal(left, c("fit_date=2026-04-20", "fit_date=2026-05-18"))
})

test_that("recent partitions inside the window are kept regardless of keep_min", {
  root <- withr::local_tempdir()
  for (d in c("2026-08-30", "2026-09-01", "2026-09-03")) {
    .mk_part(root, "football", "female", d)
  }
  res <- prune_extracts(root = root, keep_days = 14L, keep_min = 1L,
                        now = as.Date("2026-09-04"), dry_run = FALSE)
  expect_equal(nrow(res), 0L)
  expect_length(list.dirs(
    file.path(root, "sport=football", "country=iceland", "sex=female"),
    recursive = FALSE
  ), 3L)
})

test_that("cells are pruned independently of one another", {
  root <- withr::local_tempdir()
  .mk_part(root, "football", "male", "2026-01-01")
  .mk_part(root, "football", "male", "2026-09-03")
  .mk_part(root, "football", "female", "2026-02-01")
  prune_extracts(root = root, keep_days = 14L, keep_min = 1L,
                 now = as.Date("2026-09-04"), dry_run = FALSE)
  # female's only partition is ancient but it is her newest, so it survives.
  expect_true(dir.exists(file.path(root, "sport=football", "country=iceland",
                                   "sex=female", "fit_date=2026-02-01")))
  expect_false(dir.exists(file.path(root, "sport=football", "country=iceland",
                                    "sex=male", "fit_date=2026-01-01")))
})

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
  # 2026-01-01 is the calendar-year anchor and survives; only the mid-season
  # straggler is prunable.
  expect_equal(nrow(res), 1L)
  expect_setequal(as.character(res$fit_date), "2026-08-01")
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
  # Newest survives (publish reads it) and so does the year's anchor.
  expect_setequal(left, c("fit_date=2026-03-01", "fit_date=2026-05-18"))
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
  expect_setequal(left, c("fit_date=2026-03-01", "fit_date=2026-04-20",
                          "fit_date=2026-05-18"))
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
  # male's 2026-01-01 is that year's anchor, so it stays; the assertion is
  # that the two cells are evaluated independently, which they are.
  expect_true(dir.exists(file.path(root, "sport=football", "country=iceland",
                                   "sex=male", "fit_date=2026-01-01")))
})

test_that("03_fit.R prunes only when something was actually fitted", {
  # A skip-only fit run writes no new partition, so there is nothing to age
  # out. Pruning anyway would delete history on a run that produced nothing --
  # and every off-season day is a skip-only run.
  src <- readLines(testthat::test_path("..", "..", "scripts", "03_fit.R"),
                   warn = FALSE)
  i <- grep("prune_extracts", src)
  expect_length(i, 1L)
  guard <- grep("if \\(fitted > 0L\\)", src)
  expect_length(guard, 1L)
  expect_lt(guard, i)   # the guard opens before the call
})

test_that("retention keeps the earliest partition per cell per season", {
  # Season anchors. .read_preseason_team_strengths_pfi() needs a fit STRICTLY
  # EARLIER than a division's season start to publish team_strengths.json's
  # `preseason` block, and it has no archive fallback. The first version of
  # this retention pass deleted football's April partitions and the block
  # silently vanished from all 9 live cells -- undetectable, because
  # `preseason` is optional in the schema and the golden fixture writes a
  # single partition that IS the fit being published, so its hashes already
  # encode a no-preseason payload.
  root <- withr::local_tempdir()
  for (d in c("2026-04-09", "2026-04-16", "2026-08-30", "2026-09-01")) {
    .mk_part(root, "football", "male", d)
  }
  res <- prune_extracts(root = root, keep_days = 14L, keep_min = 2L,
                        now = as.Date("2026-09-04"), dry_run = FALSE)
  left <- sort(basename(list.dirs(
    file.path(root, "sport=football", "country=iceland", "sex=male"),
    recursive = FALSE
  )))
  expect_true("fit_date=2026-04-09" %in% left)   # the anchor
  expect_false("fit_date=2026-04-16" %in% left)  # a mid-season straggler goes
  expect_equal(as.character(res$fit_date), "2026-04-16")
})

test_that("a season anchor is kept per SEASON, not just once overall", {
  root <- withr::local_tempdir()
  for (d in c("2025-04-02", "2025-06-01", "2026-04-09", "2026-09-01")) {
    .mk_part(root, "football", "male", d)
  }
  prune_extracts(root = root, keep_days = 14L, keep_min = 1L,
                 now = as.Date("2026-09-04"), dry_run = FALSE)
  left <- sort(basename(list.dirs(
    file.path(root, "sport=football", "country=iceland", "sex=male"),
    recursive = FALSE
  )))
  expect_true(all(c("fit_date=2025-04-02", "fit_date=2026-04-09") %in% left))
  expect_false("fit_date=2025-06-01" %in% left)
})

test_that("keep_season_anchor = FALSE is available but not the default", {
  expect_true(formals(prune_extracts)$keep_season_anchor)
})

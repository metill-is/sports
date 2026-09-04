# check_publish_format_agreement asks one question: does the format the
# publisher DERIVED for a cell match the format the config DECLARES for it?
#
# The concrete motivating number: Icelandic women's handball plays a TRIPLE
# round robin -- 8 teams, 84 matches, 21 rounds, meetings = 3 -- against
# 2 * (n_teams - 1) = 14. Four of the seven measured 2DT cells disagree with
# that formula, which is why n_rounds is derived and published upstream at all
# rather than being recomputed by every consumer. When a federation changes a
# competition's format mid-season the two numbers part company, and this is the
# row that says so.
#
# It never escalates past WARN. A format change is a thing to look at, not an
# outage, and the alert channel is a twice-daily email.

.fmt_league <- function(is_cup = FALSE, expected_meetings = 2L, qualify = NULL) {
  div <- list(
    code = "BD", slug = "bd", label_is = "Bonusdeild", is_cup = is_cup
  )
  if (!is.null(expected_meetings)) div$expected_meetings <- expected_meetings
  if (!is.null(qualify)) div$qualify <- qualify
  list(basketball_iceland = list(
    sport = "basketball", country = "iceland", sexes = list("male"),
    active = TRUE, publish_divisions = list(male = list(div))
  ))
}

.fmt_cell <- function(root, n_rounds, source, n_teams = 12L, qualify = NULL) {
  d <- .publish_cell_dir(root, "basketball", "male", "bd")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  meta <- list(
    sport = "basketball", sex = "male", division = "BD", is_cup = FALSE,
    generated_at = "2026-09-04T12:00:00+0000",
    n_rounds = n_rounds, n_rounds_source = source
  )
  if (!is.null(qualify)) meta$qualify <- qualify
  jsonlite::write_json(meta, file.path(d, "meta.json"),
    auto_unbox = TRUE, null = "null"
  )
  jsonlite::write_json(
    list(
      season = 2027L,
      rows = lapply(seq_len(n_teams), function(i) list(team = paste0("T", i)))
    ),
    file.path(d, "standings.json"),
    auto_unbox = TRUE
  )
  d
}

test_that("a derived round count matching the configured format is OK", {
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 22L, source = "schedule", n_teams = 12L)
  res <- check_publish_format_agreement(.fmt_league(), root)
  expect_equal(nrow(res), 1L)
  expect_equal(res$check, "publish_format")
  expect_equal(res$status, "OK")
  expect_equal(res$scope, "basketball_iceland male BD")
})

test_that("a schedule-derived disagreement WARNs and names BOTH numbers", {
  # A WARN saying only "n_rounds disagrees" costs a diagnostic round trip every
  # time it fires. The whole value of this check is the two numbers side by
  # side in the healthcheck output.
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 44L, source = "schedule", n_teams = 12L)
  res <- check_publish_format_agreement(.fmt_league(expected_meetings = 2L), root)
  expect_equal(res$status, "WARN")
  expect_match(res$value, "44")
  expect_match(res$value, "22")
})

test_that("a config-sourced round count cannot disagree with itself", {
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 44L, source = "config", n_teams = 12L)
  res <- check_publish_format_agreement(.fmt_league(expected_meetings = 2L), root)
  expect_equal(res$status, "OK")
})

test_that("a qualification cut that takes every team WARNs", {
  root <- withr::local_tempdir()
  .fmt_cell(root,
    n_rounds = 22L, source = "schedule", n_teams = 12L,
    qualify = list(slots = 12L, label_is = "Urslitakeppni")
  )
  res <- check_publish_format_agreement(
    .fmt_league(qualify = list(slots = 12L, label_is = "Urslitakeppni")), root
  )
  expect_equal(res$status, "WARN")
  expect_match(res$value, "qualif")
})

test_that("a cup cell produces no row at all", {
  # INT-6: a cup has no league table, so expected_meetings * (n_teams - 1) is
  # meaningless there.
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 22L, source = "schedule")
  res <- check_publish_format_agreement(.fmt_league(is_cup = TRUE), root)
  expect_equal(nrow(res), 0L)
})

test_that("a division with no configured expected_meetings produces no row", {
  # expected_meetings is optional in the config schema and female basketball
  # 1D genuinely has none today. Nothing to compare against is not a fault.
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 22L, source = "schedule")
  res <- check_publish_format_agreement(.fmt_league(expected_meetings = NULL), root)
  expect_equal(nrow(res), 0L)
})

test_that("a cell with no publish output produces no row", {
  # check_publish_freshness owns that failure. Two checks reporting one fault
  # is noise on a low-bandwidth channel.
  root <- withr::local_tempdir()
  res <- check_publish_format_agreement(.fmt_league(), root)
  expect_equal(nrow(res), 0L)
})

test_that("a pre-v2 meta.json with no n_rounds produces no row", {
  # The live football tree was published before WS10 landed meta v2, so every
  # cell there is v1. Skipping is right: there is nothing to disagree with.
  root <- withr::local_tempdir()
  d <- .publish_cell_dir(root, "basketball", "male", "bd")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(sport = "basketball", sex = "male", division = "BD"),
    file.path(d, "meta.json"),
    auto_unbox = TRUE
  )
  res <- check_publish_format_agreement(.fmt_league(), root)
  expect_equal(nrow(res), 0L)
})

test_that("the check never escalates past WARN", {
  root <- withr::local_tempdir()
  .fmt_cell(root, n_rounds = 999L, source = "schedule", n_teams = 12L)
  res <- check_publish_format_agreement(.fmt_league(), root)
  expect_true(all(res$status %in% c("OK", "WARN")))
})

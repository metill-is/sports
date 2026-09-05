# check_publish_freshness is the alarm for B4: from the Plan-7 cutover to
# 2026-09 basketball and handball published nothing at all from CI, and
# pipeline_health() composed nine checks not one of which read data/publish/.
# The pipeline was green throughout. This file's Block (3) is the assertion
# that makes that state loud.

# ---- Task 1: threshold, stamp parser, expected-artefact set ----------------

test_that("health_thresholds carries a publish age threshold", {
  expect_true("publish_max_age_hours" %in% names(health_thresholds()))
  expect_equal(health_thresholds()$publish_max_age_hours, 36)
})

test_that(".parse_publish_stamp reads both stamp shapes actually on disk", {
  # The publishers write %z (R/publish-iceland-league.R); write_health_status
  # writes a literal Z (R/health.R). A parser handling only one would silently
  # NA every meta.json it met and turn the whole check into a false FAIL.
  expect_equal(
    format(.parse_publish_stamp("2026-09-02T22:27:08+0000"),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2026-09-02 22:27:08"
  )
  expect_equal(
    format(.parse_publish_stamp("2026-09-02T22:27:08Z"),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2026-09-02 22:27:08"
  )
  expect_true(is.na(.parse_publish_stamp("not a stamp")))
  expect_true(is.na(.parse_publish_stamp(NA_character_)))
  expect_true(is.na(.parse_publish_stamp(NULL)))
})

test_that(".publish_cell_dir mirrors the publisher's own path shape", {
  expect_equal(
    basename(.publish_cell_dir("/r", "basketball", "male", "bd")),
    "karla-bd"
  )
  expect_equal(
    basename(.publish_cell_dir("/r", "handball", "female", "g66")),
    "kvenna-g66"
  )
  expect_equal(
    .publish_cell_dir("/r", "basketball", "male", "bd"),
    file.path("/r", "publish", "basketball", "iceland", "karla-bd")
  )
})

test_that(".expected_publish_artefacts keeps cup-only surfaces for cups only", {
  surf <- function(sport) {
    c("meta", "standings", "cup_bracket", "tournament_placements")
  }
  expect_setequal(
    .expected_publish_artefacts("football", is_cup = FALSE, surfaces_for = surf),
    c("meta", "standings")
  )
  expect_setequal(
    .expected_publish_artefacts("football", is_cup = TRUE, surfaces_for = surf),
    c("meta", "standings", "bracket", "tournament_placements")
  )
})

test_that(".expected_publish_artefacts drops the non-file payload surfaces", {
  # profile$surfaces is NOT a list of JSON basenames. Football's 16 entries
  # include five payload FEATURES that live inside other files -- xg, split,
  # preseason_strengths, round_predictions_history -- plus cup_bracket, whose
  # file is bracket.json. Treating the surface list as a file list would make
  # every football league cell report four missing artefacts forever.
  expect_setequal(
    .expected_publish_artefacts("football", is_cup = FALSE),
    c(
      "meta", "next_games", "standings", "standings_history",
      "team_strengths", "team_strengths_history", "final_positions",
      "final_positions_history", "points_distribution", "home_advantage"
    )
  )
  expect_length(.expected_publish_artefacts("football", is_cup = TRUE), 12L)
  expect_setequal(
    .expected_publish_artefacts("basketball", is_cup = FALSE),
    sport_publish_profile("basketball")$surfaces
  )
})

test_that("the expected artefact set matches what football actually publishes", {
  # Measured against the live tree rather than asserted: karla-bd holds 10
  # files, karla-bikar 12 (bracket.json + tournament_placements.json).
  league <- file.path("data", "publish", "football", "iceland", "karla-bd")
  cup <- file.path("data", "publish", "football", "iceland", "karla-bikar")
  expect_setequal(
    tools::file_path_sans_ext(list.files(here::here(league), pattern = "[.]json$")),
    .expected_publish_artefacts("football", is_cup = FALSE)
  )
  expect_setequal(
    tools::file_path_sans_ext(list.files(here::here(cup), pattern = "[.]json$")),
    .expected_publish_artefacts("football", is_cup = TRUE)
  )
})

# ---- Task 2: check_publish_freshness ---------------------------------------
#
# has_upcoming_games() reads Sys.Date() internally (R/pipeline-freshness.R) and
# ignores the `now` threaded through every health check, so PAUSED-vs-evaluated
# depends on the real clock. Every fixture below therefore seeds Sys.Date() + N
# rather than back-dating `now`, and the PAUSED block seeds no schedule at all.

.bb_league <- function(active = TRUE, is_cup = FALSE) {
  list(basketball_iceland = list(
    sport = "basketball", country = "iceland", sexes = list("male"),
    active = active,
    publish_divisions = list(male = list(
      list(
        code = "BD", slug = "bd", label_is = "Bonusdeild",
        is_cup = is_cup
      )
    ))
  ))
}

.seed_upcoming <- function(root, sport = "basketball") {
  write_table(
    tibble::tibble(
      sport = sport, country = "iceland", sex = "male", season = 2027L,
      match_date = Sys.Date() + 3L, home_team = "A", away_team = "B",
      division = "BD", round = 1L, kickoff_time = NA_character_
    ),
    "schedules",
    root = root
  )
}

.seed_cell <- function(root, stamp, files = c("meta", "standings")) {
  d <- .publish_cell_dir(root, "basketball", "male", "bd")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  for (f in files) {
    jsonlite::write_json(
      list(generated_at = stamp), file.path(d, paste0(f, ".json")),
      auto_unbox = TRUE
    )
  }
  d
}

.surf <- function(sport) c("meta", "standings")

.freshness <- function(root, now = Sys.time(), leagues = .bb_league(),
                       extract_exists = TRUE) {
  check_publish_freshness(
    leagues, root, now, health_thresholds(),
    surfaces_for = .surf,
    extract_exists_fn = function(...) extract_exists
  )
}

test_that("a fresh cell with the expected artefacts is OK", {
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  .seed_cell(root, format(now - 3600, "%Y-%m-%dT%H:%M:%S%z"))
  res <- .freshness(root, now)
  expect_equal(nrow(res), 1L)
  expect_equal(res$status, "OK")
  expect_equal(res$check, "publish_freshness")
  expect_equal(res$scope, "basketball_iceland male BD")
})

test_that("a cell directory with no meta.json FAILs and names the cell", {
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  .seed_cell(root, format(now, "%Y-%m-%dT%H:%M:%S%z"), files = "standings")
  res <- .freshness(root, now)
  expect_equal(res$status, "FAIL")
  expect_match(res$scope, "basketball_iceland male BD")
})

test_that("an in-season active cell with no publish output and no extract FAILs", {
  # THE CORE CASE, and the reason this workstream exists. This is exactly the
  # state basketball and handball were in from the Plan-7 cutover to 2026-09:
  # active, in-season, and publishing nothing, with a green pipeline. A check
  # that calls this PAUSED re-hides B4, so the assertion is written twice --
  # once for what the status IS and once for what it must NOT be.
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  res <- .freshness(root, now, extract_exists = FALSE)
  expect_equal(res$status, "FAIL")
  expect_false(res$status == "PAUSED")
  expect_match(res$value, "no extract partition")
})

test_that("a stale generated_at FAILs with the age in hours", {
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  .seed_cell(root, format(now - 50 * 3600, "%Y-%m-%dT%H:%M:%S%z"))
  res <- .freshness(root, now)
  expect_equal(res$status, "FAIL")
  expect_match(res$value, "h old")
  expect_equal(res$threshold, "36h")
})

test_that("a cell with no upcoming games is PAUSED, not FAIL", {
  # The off-season. Seeding no schedule is the only honest way to reach this
  # branch: has_upcoming_games() ignores the injected clock.
  root <- withr::local_tempdir()
  res <- .freshness(root, Sys.time(), extract_exists = FALSE)
  expect_equal(res$status, "PAUSED")
  expect_match(res$value, "no upcoming games")
})

test_that("a missing expected artefact FAILs and names it", {
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  .seed_cell(root, format(now, "%Y-%m-%dT%H:%M:%S%z"), files = "meta")
  res <- .freshness(root, now)
  expect_equal(res$status, "FAIL")
  expect_match(res$value, "standings")
})

test_that("an unexpected extra artefact WARNs rather than FAILs", {
  # INT-3: the comparison is asymmetric. A MISSING artefact is a 404 on the
  # platform; an EXTRA one is leftover output from an older shape, which is
  # worth looking at but is not an outage.
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  .seed_cell(root, format(now, "%Y-%m-%dT%H:%M:%S%z"),
    files = c("meta", "standings", "final_positions_history")
  )
  res <- .freshness(root, now)
  expect_equal(res$status, "WARN")
  expect_match(res$value, "final_positions_history")
})

test_that("an inactive league contributes no rows", {
  root <- withr::local_tempdir()
  .seed_upcoming(root)
  expect_equal(nrow(.freshness(root, Sys.time(), leagues = .bb_league(active = FALSE))), 0L)
})

test_that("a corrupt meta.json becomes a FAIL row, not an abort", {
  # pipeline_health() wraps each check in safe(); an abort inside would
  # collapse the whole check into one check_error row and lose every cell's
  # scope, which is the opposite of what this check is for.
  root <- withr::local_tempdir()
  now <- Sys.time()
  .seed_upcoming(root)
  d <- .seed_cell(root, format(now, "%Y-%m-%dT%H:%M:%S%z"))
  writeLines("{not json", file.path(d, "meta.json"))
  res <- expect_no_error(.freshness(root, now))
  expect_equal(res$status, "FAIL")
})

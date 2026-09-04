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

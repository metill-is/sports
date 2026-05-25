# Test for cumulative_xpts_long() -- the analytical helper that
# concatenates all per-cell standings_history.json files into one long
# tibble for xPts-vs-actual residual analyses, calibration backtests,
# and team-level xPts trajectories across the season.
#
# See F8 in docs/audits/2026-05-25-pipeline-cross-project-review.html.

test_that("cumulative_xpts_long: returns a long tibble across cells with parsed path columns", {
  if (!dir.exists(here::here("data", "publish", "football", "iceland"))) {
    testthat::skip("data/publish absent")
  }

  out <- cumulative_xpts_long(
    output_root = here::here("data", "publish")
  )

  expect_s3_class(out, "tbl_df")
  if (nrow(out) == 0L) {
    testthat::skip("no standings_history records in data/publish yet")
  }

  required_cols <- c(
    "sport", "country", "sex", "division",
    "as_of", "generated_at", "round", "season",
    "team", "played", "points",
    "xg_for", "xg_against", "xpts"
  )
  expect_true(all(required_cols %in% names(out)))

  expect_setequal(unique(out$sport), "football")
  expect_setequal(unique(out$country), "iceland")
  expect_true(all(unique(out$sex) %in% c("male", "female")))
  expect_true(length(unique(out$division)) >= 1L)
})

test_that("cumulative_xpts_long: handles empty output_root gracefully", {
  empty <- withr::local_tempdir()
  out <- cumulative_xpts_long(output_root = empty)

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 0L)
  expect_true("sport" %in% names(out))
  expect_true("xpts" %in% names(out))
})

test_that("cumulative_xpts_long: cell path -> sex + division parses correctly", {
  # Build a synthetic two-cell tree (no real fits needed).
  out_root <- withr::local_tempdir()
  for (cell in c("karla-bd", "kvenna-ld")) {
    dir.create(
      file.path(out_root, "football", "iceland", cell),
      recursive = TRUE
    )
    jsonlite::write_json(
      list(
        schema_version = 1L,
        records = list(
          list(
            as_of = "2026-05-01", generated_at = "2026-05-02T00:00:00+0000",
            round = 1L, season = 2026L,
            team = "TestTeam", short = "TT", played = 1L,
            wins = 1L, draws = 0L, losses = 0L,
            goals_for = 2L, goals_against = 1L, goal_diff = 1L, points = 3L,
            xg_for = 1.4, xg_against = 0.8, xpts = 2.1,
            n_predicted_matches = 1L, n_played_matches = 1L, rank = 1L
          )
        )
      ),
      file.path(
        out_root, "football", "iceland", cell, "standings_history.json"
      ),
      auto_unbox = TRUE
    )
  }

  out <- cumulative_xpts_long(output_root = out_root)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$sex, c("male", "female"))
  expect_setequal(out$division, c("bd", "ld"))
  expect_true(all(out$team == "TestTeam"))
})

skip_if_no_recommendations <- function() {
  if (!dir.exists(here::here("data", "decisions", "recommendations"))) {
    testthat::skip("data/decisions/recommendations absent; decide_all.R hasn't run")
  }
}

skip_if_no_publish <- function() {
  if (!dir.exists(here::here("data", "publish"))) {
    testthat::skip("data/publish absent; publish_all.R hasn't run")
  }
}

test_that("decide_all populates candidates + recommendations for active leagues", {
  skip_if_no_recommendations()
  recs <- read_table("recommendations", filter = list(country = "iceland"))
  cands <- read_table("candidates", filter = list(country = "iceland"))

  # Some active leagues have no upcoming games today (off-season basketball,
  # handball cup-ends), so empty is OK. Just check the columns + types.
  if (nrow(recs) > 0L) {
    expect_true(all(c("market", "outcome", "p", "odds", "ev", "kelly", "bet_amount")
    %in% names(recs)))
    expect_type(recs$bet_amount, "double")
  }
  if (nrow(cands) > 0L) {
    expect_true("stage" %in% names(cands))
  }
})

test_that("publish_all writes the football 7-JSON contract", {
  skip_if_no_publish()
  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (sex_dir in c("karla-bd", "karla-ld", "kvenna-bd", "kvenna-ld")) {
    out <- here::here("data", "publish", "football", "iceland", sex_dir)
    if (!dir.exists(out)) next # OK if publish didn't have a fit for that sex/div
    for (f in expected) {
      expect_true(
        file.exists(file.path(out, f)),
        info = paste("missing:", sex_dir, f)
      )
    }
  }
})

test_that("any live basketball or handball cell ships the full profile contract", {
  # This block used to assert a two-JSON "scaffold" under the un-suffixed
  # data/publish/<sport>/iceland/{karla,kvenna}/ path. Both premises are gone:
  # those cells were deleted as the schema-arming precondition, and the
  # retired per-sport 2DT publishers that wrote a scaffold were unified into
  # publish_iceland_league(), which emits the same ten artefacts football does.
  #
  # It now asserts the CONTRACT rather than a snapshot, because the live tree
  # holds no bb/hb cell until the first real 2DT fit lands. Deliberately no
  # early `next`: a loop over an empty set with no expectation registers as a
  # SKIP, and a contract test that can skip itself into silence is exactly the
  # shape of the breakage this branch exists to remove. The fixture-side proof
  # that all eight cells publish is test-publish-b4-acceptance.R.
  skip_if_no_publish()
  cells <- character()
  for (sport in c("basketball", "handball")) {
    dir <- here::here("data", "publish", sport, "iceland")
    if (!dir.exists(dir)) next
    for (cell in list.dirs(dir, recursive = FALSE, full.names = FALSE)) {
      cells <- c(cells, file.path(sport, cell))
      expect_match(cell, "^(karla|kvenna)-[a-z0-9]+$")
      expect_setequal(
        list.files(file.path(dir, cell), pattern = "[.]json$"),
        paste0(sport_publish_profile(sport)$surfaces, ".json")
      )
    }
  }
  # The unconditional assertion: whatever is on disk, the old shape is gone.
  expect_false(any(basename(cells) %in% c("karla", "kvenna")))
})

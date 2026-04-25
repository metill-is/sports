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
  for (sex_dir in c("karla", "kvenna")) {
    out <- here::here("data", "publish", "football", "iceland", sex_dir)
    if (!dir.exists(out)) next # OK if publish didn't have a fit for that sex
    for (f in expected) {
      expect_true(
        file.exists(file.path(out, f)),
        info = paste("missing:", sex_dir, f)
      )
    }
  }
})

test_that("publish_all writes the basketball + handball 2-JSON scaffold contract", {
  skip_if_no_publish()
  for (sport in c("basketball", "handball")) {
    for (sex_dir in c("karla", "kvenna")) {
      out <- here::here("data", "publish", sport, "iceland", sex_dir)
      if (!dir.exists(out)) next
      for (f in c("meta.json", "next_games.json")) {
        expect_true(
          file.exists(file.path(out, f)),
          info = paste("missing:", sport, sex_dir, f)
        )
      }
    }
  }
})

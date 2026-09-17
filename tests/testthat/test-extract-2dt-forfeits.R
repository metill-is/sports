# A forfeit is a real result for the TABLE and not an observation for the MODEL.
#
# model_training_results() drops 2DT walkover scores (20-0 basketball, 10-0
# handball) from what the fit trains on. The 2DT extractor used that same set
# for its tables too, so without its own split a forfeit win would vanish from
# the standings and the pairing would be simulated a second time.
#
# The fixture's men's BD (4 teams) has played its first leg of 2100; its last
# game, BAM BD 03 82-80 BAM BD 04 on 2100-01-08, becomes a 0-20 forfeit that
# BAM BD 04 -- the weakest side, which lost its other two games -- is awarded.

forfeit_cell <- function(env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  teams <- fixture_division_teams("basketball", "male", "BD")
  results <- read_table("results", root = root)
  hit <- results$sport == "basketball" & results$sex == "male" &
    results$season == 2100L & results$division == "BD" &
    results$home_team == teams[3L] & results$away_team == teams[4L]
  stopifnot(sum(hit) == 1L)
  results$home_score[hit] <- 0L
  results$away_score[hit] <- 20L
  write_table(results, "results", root = root)

  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root, n_draws = 200L))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  suppressMessages(extract_basketball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  part <- file.path(
    extracts_root, "sport=basketball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  list(
    teams = teams, st = st,
    read = function(ft) {
      d <- arrow::read_parquet(file.path(part, paste0(ft, ".parquet")))
      d[d$division == "BD", ]
    }
  )
}

test_that("the fit does not train on the forfeit", {
  cell <- forfeit_cell()
  # 42 played basketball men's games in the fixture, less the forfeit.
  expect_equal(cell$st$prep$stan_data$N, 41L)
})

test_that("the forfeit win stays banked in the simulated table", {
  cell <- forfeit_cell()
  pd <- cell$read("points_distribution")
  bd04 <- pd[pd$team == cell$teams[4L] & pd$probability > 0, ]
  # Banked: 2 points from the forfeit. Dropped from the table, the pairing is
  # re-simulated and the weakest side can finish on 0.
  expect_gte(min(bd04$points), 2L)
  # Still a 12-game double round robin: 6 banked, 6 simulated, no pairing twice.
  expect_equal(sum(pd$points * pd$probability), 24, tolerance = 1e-9)
})

test_that("the trajectory holds only the forfeiting team's real matchweeks", {
  cell <- forfeit_cell()
  rs <- cell$read("round_strengths_quantiles")
  # BAM BD 04 played two real 2100 games; its forfeit is not a fit round, and
  # counting it would shift every later matchweek onto the wrong offense[r, k].
  expect_equal(max(rs$round[rs$team == cell$teams[4L]]), 2L)
  expect_equal(max(rs$round[rs$team == cell$teams[1L]]), 3L)
})

# Driven by the committed facts fixture + stub_fit(). These assertions never ran
# before: they gated on data/beliefs/fits/sport=basketball/.../fit.rds, a
# gitignored 300-600 MB artefact that CI never has.

extract_bb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  list(
    root = root, league = league, st = st, extracts_root = extracts_root,
    partition = file.path(
      extracts_root, "sport=basketball", "country=iceland",
      paste0("sex=", sex), paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

test_that("extract_basketball_iceland writes the 5 expected parquets", {
  f <- extract_bb_fixture("male")

  extract_basketball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  # %in%, not setequal: a later workstream adding fit_meta.parquet or
  # round_strengths_quantiles.parquet must not break this contract.
  expected_files <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  written <- list.files(f$partition)
  for (fl in expected_files) {
    expect_true(fl %in% written, info = paste("missing", fl))
  }

  pm <- arrow::read_parquet(file.path(f$partition, "predicted_matches.parquet"))
  expect_gt(nrow(pm), 0L)
  expect_true(all(c(
    "game_nr", "match_date", "division", "home_team", "away_team",
    "mean_home_goals", "mean_away_goals", "mean_goal_diff",
    "p_home_win", "p_draw", "p_away_win", "goal_diff_distribution"
  ) %in% names(pm)))
  # Basketball has no ties.
  expect_true(all(pm$p_draw == 0))
})

test_that("extracted team_strengths_quantiles covers the 9-cell grid", {
  f <- extract_bb_fixture("male")

  extract_basketball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

  ts <- arrow::read_parquet(
    file.path(f$partition, "team_strengths_quantiles.parquet")
  )
  expect_true(all(c("team", "component", "location", "quantile", "value") %in% names(ts)))
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), PUBLISH_QUANTILE_GRID)
  # Since the extractor loops over the configured publish divisions, the file
  # spans BD + 1D and each division's slice carries its OWN teams.
  expect_setequal(unique(ts$division), .iceland_division_codes("basketball_iceland", "male"))
  expect_setequal(
    unique(ts$team[ts$division == "BD"]),
    fixture_division_teams("basketball", "male", "BD")
  )
  expect_setequal(
    unique(ts$team[ts$division == "1D"]),
    fixture_division_teams("basketball", "male", "1D")
  )
})

test_that("extract_basketball_iceland is idempotent (rerun overwrites cleanly)", {
  f <- extract_bb_fixture("female")

  args <- list(
    fit = f$st$fit, league = f$league, sex = "female",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )
  do.call(extract_basketball_iceland, args)
  target <- file.path(f$partition, "team_strengths_quantiles.parquet")
  size_before <- file.info(target)$size
  digest_before <- digest::digest(file = target)
  do.call(extract_basketball_iceland, args)
  expect_equal(file.info(target)$size, size_before)
  expect_equal(digest::digest(file = target), digest_before)
})

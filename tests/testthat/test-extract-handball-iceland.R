# Driven by the committed facts fixture + stub_fit(). Previously gated on
# data/beliefs/fits/sport=handball/.../fit.rds and therefore never executed.

extract_hb_fixture <- function(sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  list(
    root = root, league = league, st = st, extracts_root = extracts_root,
    partition = file.path(
      extracts_root, "sport=handball", "country=iceland",
      paste0("sex=", sex), paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

test_that("extract_handball_iceland writes the 5 expected parquets", {
  f <- extract_hb_fixture("male")

  extract_handball_iceland(
    fit = f$st$fit,
    league = f$league,
    sex = "male",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )

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
  # Handball has ties, so the three outcome probabilities must sum to 1.
  expect_equal(
    pm$p_home_win + pm$p_draw + pm$p_away_win,
    rep(1, nrow(pm)),
    tolerance = 1e-9
  )
})

test_that("handball extracted team_strengths_quantiles covers the 9-cell grid", {
  f <- extract_hb_fixture("male")

  extract_handball_iceland(
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
  expect_setequal(unique(ts$quantile), seq_len(99L))
  expect_setequal(
    unique(ts$team),
    fixture_division_teams("handball", "male", "OD")
  )
})

test_that("extract_handball_iceland is idempotent (rerun overwrites cleanly)", {
  f <- extract_hb_fixture("female")

  args <- list(
    fit = f$st$fit, league = f$league, sex = "female",
    fit_date = FIXTURE_FIT_DATE,
    end_date = FIXTURE_END_DATE,
    root = f$root,
    extracts_root = f$extracts_root,
    prep = f$st$prep
  )
  do.call(extract_handball_iceland, args)
  target <- file.path(f$partition, "team_strengths_quantiles.parquet")
  size_before <- file.info(target)$size
  digest_before <- digest::digest(file = target)
  do.call(extract_handball_iceland, args)
  expect_equal(file.info(target)$size, size_before)
  expect_equal(digest::digest(file = target), digest_before)
})

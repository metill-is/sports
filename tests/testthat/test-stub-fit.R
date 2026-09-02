# Contract tests for the cmdstanr-fit substitute the 2DT extract/publish tests
# run on. stub_fit() is deliberately NOT class CmdStanMCMC: the only `fit$`
# usage anywhere on the 2DT extract/publish path is `fit$draws`, so a list
# carrying that one closure is the whole dependency, and a fake class would
# hide a real dispatch dependency if one were ever added.

test_that("stub_fit takes a named list of draws and serves them by name", {
  d <- stub_2dt_draws(teams = c("A", "B", "C"), n_pred = 4L, n_draws = 20L)
  expect_type(d, "list")
  expect_true(all(c("lp__", "goals1_pred", "cur_offense_home") %in% names(d)))

  fit <- stub_fit(d)
  # A list, never a fake CmdStanMCMC.
  expect_false(inherits(fit, "CmdStanMCMC"))
  expect_true(is.function(fit$draws))

  expect_error(fit$draws("not_a_variable"), "not_a_variable")
})

test_that("stub_2dt_draws covers the whole 2DT variable surface", {
  d <- stub_2dt_draws(teams = c("A", "B", "C"), n_pred = 4L, n_draws = 20L)
  fit <- stub_fit(d)

  expect_equal(posterior::ndraws(fit$draws("lp__")), 20L)

  team_vars <- c(
    "cur_offense_home", "cur_defense_home", "cur_strength_home",
    "cur_offense_away", "cur_defense_away", "cur_strength_away",
    "home_advantage_off", "home_advantage_def", "home_advantage_tot"
  )
  for (v in team_vars) {
    sub <- fit$draws(v)
    expect_equal(
      posterior::variables(sub), paste0(v, "[", 1:3, "]"),
      info = v
    )
  }

  joint <- fit$draws(c("goals1_pred", "goals2_pred"))
  expect_length(posterior::variables(joint), 8L)
  expect_s3_class(posterior::as_draws_df(joint), "draws_df")
})

test_that("stub_2dt_draws honours pinned constants", {
  d <- stub_2dt_draws(
    teams = c("A", "B"), n_pred = 2L, n_draws = 10L,
    constants = list(home_advantage_tot = 4)
  )
  expect_true(all(d$home_advantage_tot == 4))
})

test_that("stub_2dt_draws is deterministic for a fixed seed", {
  a <- stub_2dt_draws(c("A", "B"), n_pred = 2L, n_draws = 10L, seed = 7L)
  b <- stub_2dt_draws(c("A", "B"), n_pred = 2L, n_draws = 10L, seed = 7L)
  expect_equal(a, b)
})

test_that("local_stub_2dt sizes goals*_pred from prepare_data's own pred_d", {
  root <- fixture_facts_root()
  league <- load_leagues()[["basketball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root))

  # The publishers call prepare_data() internally at the DEFAULT
  # schedule_horizon_days = 14L and take no prep= argument, so a stub sized at
  # any other horizon would make .compute_posterior_goals_2dt warn and return
  # zero rows. Assert the match rather than trusting it.
  expect_equal(
    max(as.integer(sub(
      ".*\\[(\\d+)\\]$", "\\1",
      grep("^goals1_pred", posterior::variables(st$fit$draws("goals1_pred")), value = TRUE)
    ))),
    nrow(st$prep$pred_d)
  )
  expect_silent(pg <- .compute_posterior_goals_2dt(st$fit, st$prep$pred_d))
  expect_gt(nrow(pg), 0L)
})

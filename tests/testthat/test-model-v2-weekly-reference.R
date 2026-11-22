# The weekly Stan reference fit (decision 5 of the football cutover plan,
# docs/reports/2026-11-15-football-v2-proposal.qmd): once a week the football
# v2.0 specification is fitted twice on the latest data of each cell, by
# R-INLA as production fits it and by NUTS with the Stan reference program,
# and their match probabilities are compared. A mean absolute difference above
# 0.01 is a health warning.
#
# The INLA and NUTS fitters are arguments, so these tests drive the
# orchestration with fake fitters and need neither engine.

# Draws in the engines' shared layout, with the same value in every draw so
# that the match probabilities are plain Poisson ones.
.flat_draws <- function(spec, att = 0, def = 0, c_home = 0.3, c_away = 0.1, n = 4L) {
  m <- function(cols, v = 0) matrix(v, n, cols)
  list(
    att = m(spec$S, att), def = m(spec$S, def),
    ha_att = m(spec$K), ha_def = m(spec$K),
    c_home = rep(c_home, n), c_away = rep(c_away, n),
    lvl_goal = m(length(spec$weeks)), lvl_ha = m(length(spec$weeks)),
    hyper = matrix(0.1, n, 1L, dimnames = list(NULL, "sigma_str"))
  )
}

.fake_fit <- function(shift = 0, divergent = 0L, rhat = 1.001, mode_status = 0) {
  function(spec) {
    d <- .flat_draws(spec, c_home = 0.3 + shift)
    list(
      draws = d, mode_status = mode_status,
      diagnostics = list(n_divergent = divergent, state_max_rhat = rhat),
      summary = data.frame(variable = "sigma_str", rhat = rhat)
    )
  }
}

test_that("the production specification inputs are what the INLA fit hands fv2_build", {
  skip_if_no_inla()
  root <- withr::local_tempdir()
  s <- v2_store(root)
  prep <- prepare_data(s$league, "male", end_date = s$end_date, root = root)
  captured <- NULL
  testthat::local_mocked_bindings(fv2_build = function(...) {
    captured <<- list(...)
    stop("captured")
  })
  expect_error(
    suppressMessages(fit_model_v2_inla(prep, s$league, "male", end_date = s$end_date, root = root)),
    "captured"
  )
  inputs <- fv2_production_inputs(prep, s$league, "male", end_date = s$end_date, root = root)
  # The store does not promise a row order between two reads (Arrow scans the
  # partition files in parallel), so the frames are compared sorted.
  sorted <- function(d) {
    d <- as.data.frame(d)
    d <- d[do.call(order, d[intersect(c("match_date", "home_team", "away_team", "team"), names(d))]), , drop = FALSE]
    rownames(d) <- NULL
    d
  }
  expect_identical(sorted(captured[[1]]), sorted(inputs$train))
  expect_identical(sorted(captured[[2]]), sorted(inputs$pred))
  expect_identical(sorted(captured$carry), sorted(inputs$carry))
  expect_identical(sorted(captured$divisions), sorted(inputs$divisions))
  expect_identical(captured[names(football_v2_options())], football_v2_options())
  expect_equal(nrow(inputs$pred), s$n_pred)
})

test_that("off-season fixtures pair every team of each league division of the season both ways, one date", {
  divisions <- data.frame(
    team = c("A", "B", "C", "D", "E", "A", "F"),
    season = c(2026L, 2026L, 2026L, 2026L, 2026L, 2025L, 2025L),
    division = c("BD", "BD", "BD", "LD1", "LD1", "BD", "BD"),
    rank = c(1L, 1L, 1L, 2L, 2L, 1L, 1L)
  )
  fx <- fv2_offseason_fixtures(divisions, season = 2026L, match_date = as.Date("2026-11-30"))
  expect_equal(nrow(fx), 3L * 2L + 2L * 1L)
  expect_setequal(paste(fx$home_team, fx$away_team), c(
    "A B", "B A", "A C", "C A", "B C", "C B", "D E", "E D"
  ))
  expect_true(all(fx$match_date == as.Date("2026-11-30")))
  expect_true(all(fx$season == 2026L))
  expect_true(all(is.na(fx$home_score) & is.na(fx$away_score)))
  expect_false(any(fx$home_team == "F" | fx$away_team == "F"))
  expect_identical(
    names(fx),
    names(fv2_pred_from_prep(data.frame(match_date = as.Date("2026-11-30"), home_team = "A", away_team = "B", division = "BD")))
  )
})

test_that("outcome probabilities from draws are the Poisson ones and sum to one", {
  train <- toy_matches()
  pred <- toy_fixture()
  spec <- do.call(fv2_build, c(list(train, pred, divisions = toy_divisions()), football_v2_options()))
  p <- fv2_outcome_probs(.flat_draws(spec), spec, fixtures = pred, max_goals = 15L)
  expect_identical(dim(p), c(nrow(pred), 3L))
  expect_identical(colnames(p), c("home", "draw", "away"))
  expect_equal(unname(rowSums(p)), rep(1, nrow(pred)), tolerance = 1e-12)
  expect_identical(
    rownames(p),
    paste(pred$home_team, pred$away_team, format(pred$match_date, "%Y-%m-%d"), sep = " | ")
  )
  # All states zero: home goals ~ Poisson(exp(0.3)), away ~ Poisson(exp(0.1)).
  g <- 0:15
  grid <- outer(stats::dpois(g, exp(0.3)), stats::dpois(g, exp(0.1)))
  grid <- grid / sum(grid)
  expect_equal(p[1, "home"], sum(grid[lower.tri(grid)]), tolerance = 1e-12)
  expect_equal(p[1, "draw"], sum(diag(grid)), tolerance = 1e-12)
})

test_that("the weekly reference compares the two engines' probabilities fixture by fixture", {
  root <- withr::local_tempdir()
  s <- v2_store(root)
  res <- suppressMessages(fv2_weekly_reference(
    s$league, "male",
    end_date = s$end_date, root = root,
    fit_inla = .fake_fit(0), fit_nuts = .fake_fit(0)
  ))
  expect_equal(nrow(res$fixtures), s$n_pred)
  expect_equal(res$row$n_fixtures, s$n_pred)
  expect_identical(res$row$fixtures, "upcoming")
  expect_equal(res$row$mean_abs_diff, 0)
  expect_true(res$row$passed)
  expect_identical(res$row$sex, "male")
  expect_identical(res$row$run_date, s$end_date)
  expect_equal(res$row$n_train, s$n_obs)
  expect_no_error(validate_against_schema(res$row, "engine_reference"))

  # NUTS puts the home side 0.25 higher on the log scale: far above 0.01.
  apart <- suppressMessages(fv2_weekly_reference(
    s$league, "male",
    end_date = s$end_date, root = root,
    fit_inla = .fake_fit(0), fit_nuts = .fake_fit(0.25)
  ))
  expect_gt(apart$row$mean_abs_diff, FV2_REFERENCE_MAX_MEAN_ABS_DIFF)
  expect_false(apart$row$passed)
  expect_equal(
    apart$row$mean_abs_diff,
    mean(abs(as.matrix(apart$fixtures[c("inla_home", "inla_draw", "inla_away")]) -
      as.matrix(apart$fixtures[c("nuts_home", "nuts_draw", "nuts_away")])))
  )
})

test_that("with no fixture ahead the weekly reference compares every pairing of the season's divisions", {
  root <- withr::local_tempdir()
  s <- v2_store(root)
  # The off-season: no fixture inside the horizon.
  unlink(file.path(root, "facts", "schedules"), recursive = TRUE)
  res <- suppressMessages(fv2_weekly_reference(
    s$league, "male",
    end_date = s$end_date, root = root,
    fit_inla = .fake_fit(0), fit_nuts = .fake_fit(0)
  ))
  expect_identical(res$row$fixtures, "off-season pairings")
  expect_equal(res$row$n_fixtures, length(s$teams) * (length(s$teams) - 1L))
  expect_true(all(res$fixtures$match_date == s$end_date + 7L))
})

test_that("a NUTS reference that is not converged, or an INLA fit without a mode, does not pass", {
  root <- withr::local_tempdir()
  s <- v2_store(root)
  run <- function(...) {
    suppressMessages(fv2_weekly_reference(
      s$league, "male",
      end_date = s$end_date, root = root, ...
    ))$row
  }
  expect_false(run(fit_inla = .fake_fit(0), fit_nuts = .fake_fit(0, divergent = 3L))$passed)
  expect_false(run(fit_inla = .fake_fit(0), fit_nuts = .fake_fit(0, rhat = 1.05))$passed)
  expect_false(run(fit_inla = .fake_fit(0, mode_status = 1), fit_nuts = .fake_fit(0))$passed)
})

test_that("the reference result is a registered store under data/health/engine_reference", {
  expect_true("engine_reference" %in% names(schemas()))
  expect_identical(table_subdir("engine_reference"), c("health", "engine_reference"))
  expect_identical(table_partitions()[["engine_reference"]], c("sport", "country", "sex", "run_date"))
})

.ref_row <- function(sex = "male", run_date = as.Date("2026-11-23"), mad = 0.003, passed = TRUE,
                     divergences = 0L, rhat = 1.003) {
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex, run_date = run_date,
    run_at = as.POSIXct(paste(run_date, "06:00:00"), tz = "UTC"),
    fixtures = "upcoming", n_fixtures = 12L, n_train = 2900L,
    mean_abs_diff = mad, max_abs_diff = mad * 4, threshold = 0.01,
    nuts_divergences = divergences, nuts_max_rhat = rhat, inla_mode_status = 0L,
    passed = passed, inla_version = "25.6.7", cmdstan_version = "2.38.0",
    stan_md5 = "abc", model_version = "2.0", code_commit = "sha"
  )
}

test_that("the health check warns on a breach, an unconverged reference or a reference gone stale", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-11-25 12:00:00", tz = "UTC")
  th <- health_thresholds()
  status_of <- function(sex) {
    h <- check_engine_reference(root, now, th)
    h$status[h$scope == paste0("football/iceland/", sex)]
  }

  # Nothing has run yet: nothing to judge, and not an alarm.
  h <- check_engine_reference(root, now, th)
  expect_true(all(h$status == "PAUSED"))
  expect_setequal(h$scope, c("football/iceland/male", "football/iceland/female"))

  write_table(.ref_row("male"), "engine_reference", root = root)
  write_table(.ref_row("female", mad = 0.014, passed = FALSE), "engine_reference", root = root)
  expect_identical(status_of("male"), "OK")
  expect_identical(status_of("female"), "WARN")

  # The newest row decides: last week's breach is not this week's.
  write_table(.ref_row("female", run_date = as.Date("2026-11-24"), mad = 0.004), "engine_reference", root = root)
  expect_identical(status_of("female"), "OK")

  write_table(.ref_row("male", run_date = as.Date("2026-11-24"), divergences = 2L, passed = FALSE), "engine_reference", root = root)
  expect_identical(status_of("male"), "WARN")

  later <- now + as.difftime(th$engine_reference_max_age_days + 1, units = "days")
  h <- check_engine_reference(root, later, th)
  expect_true(all(h$status == "WARN"))
  expect_true(all(grepl("days old", h$value)))
})

test_that("pipeline_health runs the engine reference check", {
  root <- withr::local_tempdir()
  h <- suppressWarnings(suppressMessages(pipeline_health(root = root, leagues = list())))
  expect_true("engine_reference" %in% h$check)
})

test_that("the weekly reference script fits as production does and stores only its summary row", {
  src <- readLines(testthat::test_path("..", "..", "scripts", "engine_reference.R"), warn = FALSE)
  body <- paste(src[!grepl("^\\s*#", src)], collapse = "\n")
  expect_match(body, "fv2_weekly_reference(", fixed = TRUE)
  expect_match(body, 'int_strategy = "auto"', fixed = TRUE)
  expect_match(body, 'threads = "1:1"', fixed = TRUE)
  expect_match(body, "research/football-v2/engines.R", fixed = TRUE)
  expect_match(body, 'write_table(res$row, "engine_reference"', fixed = TRUE)
  expect_false(grepl("write_table\\([^)]*\"(results|schedules|odds|ledger|candidates|recommendations|beliefs_[a-z]+|fit_diagnostics|forecast_archive)\"", body))
})

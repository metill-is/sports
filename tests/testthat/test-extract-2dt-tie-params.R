# Where the 2DT extractors get has_ties / tie_threshold.
#
# They used to read `league$betting$scoring$...`. That is unreachable on the
# production path: run_fit_targets() hands the extractor a WHITELISTED league
# slice (R/model-league.R:453) built from
#   c("sport", "country", "sexes", "active", "stan_model", "data_source")
# which omits `betting` entirely. So `isTRUE(NULL)` gave has_ties = FALSE and the
# is.null() ternary gave tie_threshold = 0 on every real fit, and handball
# published p_draw = 0 for every match despite config saying has_ties: true.
#
# Because the 2DT posterior scores are CONTINUOUS (multi_student_t_rng, never
# rounded), P(diff == 0) is exactly 0 -- so has_ties = TRUE with a threshold of 0
# is numerically identical to has_ties = FALSE. Restoring has_ties alone would
# have kept p_draw at 0; the threshold has to arrive too.
#
# The publish profile already carries both values, keyed by sport rather than
# passed through a lossy slice, so it is the single source of truth.

test_that("handball tie params resolve from the profile", {
  tp <- sports:::.tie_params_pfi("handball")
  expect_true(tp$has_ties)
  expect_equal(tp$tie_threshold, 0.5)
})

test_that("basketball resolves to no ties", {
  # Correct today only by accident -- NULL -> FALSE happened to match. Now it is
  # correct by construction.
  tp <- sports:::.tie_params_pfi("basketball")
  expect_false(tp$has_ties)
  expect_equal(tp$tie_threshold, 0)
})

test_that("tie params survive the whitelisted slice the fit path actually passes", {
  # The regression guard: reconstruct run_fit_targets()'s slice verbatim and
  # confirm the params still resolve. Under the old code this yielded FALSE/0.
  league <- load_leagues()[["handball_iceland"]]
  static <- league[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]
  tp <- sports:::.tie_params_pfi(static$sport)
  expect_true(tp$has_ties)
  expect_equal(tp$tie_threshold, 0.5)
})

test_that("a continuous 2DT posterior yields a non-zero draw probability", {
  # The user-visible half: on continuous scores only a non-zero threshold can
  # produce a draw at all. Two evenly matched teams, diff ~ N(0, 6) -- close to
  # a real handball goal-difference spread.
  set.seed(3L)
  n <- 20000L
  posterior <- tibble::tibble(
    game_nr = 1L,
    match_date = as.Date("2026-09-10"),
    division = "OD",
    home_team = "A", away_team = "B",
    home_score = 30 + stats::rnorm(n, 0, 3),
    away_score = 30 + stats::rnorm(n, 0, 3)
  )
  pred_d <- tibble::tibble(
    game_nr = 1L, match_date = as.Date("2026-09-10"),
    division = "OD", home_team = "A", away_team = "B"
  )

  tp <- sports:::.tie_params_pfi("handball")
  out <- sports:::.compute_predicted_matches_2dt(
    fit = NULL, pred_d = pred_d,
    bucket_width = 2L, bucket_low = -20L, bucket_high = 20L,
    has_ties = tp$has_ties, tie_threshold = tp$tie_threshold,
    posterior_goals = posterior
  )

  expect_gt(out$p_draw, 0)
  expect_equal(out$p_home_win + out$p_draw + out$p_away_win, 1, tolerance = 1e-9)

  # And the no-ties setting is what produced the published zero.
  zero <- sports:::.compute_predicted_matches_2dt(
    fit = NULL, pred_d = pred_d,
    bucket_width = 2L, bucket_low = -20L, bucket_high = 20L,
    has_ties = FALSE, tie_threshold = 0,
    posterior_goals = posterior
  )
  expect_equal(zero$p_draw, 0)
})

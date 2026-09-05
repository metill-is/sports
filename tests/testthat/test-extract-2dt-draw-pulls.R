# The 2DT quantile helpers used to pull their own posterior draws. That is fine
# for one division and catastrophic for a loop: nine `fit$draws()` calls PER
# DIVISION against a 300-600 MB fit. Football has always hoisted the pulls above
# its division loop (R/extract-football-iceland.R:1514-1547); these two functions
# are the 2DT equivalent, and the quantile helpers now take draws.

pulls_case <- function(k = 3L, n_draws = 12L, constants = list()) {
  teams <- tibble::tibble(
    team = sprintf("T%02d", seq_len(k)), team_nr = seq_len(k)
  )
  list(
    fit = stub_fit(stub_2dt_draws(
      teams$team,
      n_pred = 2L, n_draws = n_draws, n_rounds = 4L,
      constants = constants
    )),
    teams = teams,
    n_draws = n_draws,
    k = k
  )
}

test_that(".extract_team_strength_draws_2dt covers the six raw blocks", {
  cs <- pulls_case()
  out <- .extract_team_strength_draws_2dt(cs$fit, cs$teams)

  expect_true(all(c(".draw", "team", "component", "location", "value") %in% names(out)))
  expect_setequal(unique(out$component), c("offence", "defence", "total"))
  # No `avg` here: it is a per-draw mean the quantile helper computes downstream,
  # so that the interval reflects the joint posterior rather than a post-hoc
  # average of two separate bands.
  expect_setequal(unique(out$location), c("home", "away"))
  expect_equal(nrow(out), cs$n_draws * cs$k * 3L * 2L)
  expect_equal(
    nrow(dplyr::distinct(out, .data$.draw, .data$team, .data$component, .data$location)),
    nrow(out)
  )
})

test_that(".extract_home_advantage_draws_2dt covers the three components", {
  cs <- pulls_case()
  out <- .extract_home_advantage_draws_2dt(cs$fit, cs$teams)

  expect_setequal(names(out), c("team", "component", ".draw", "value"))
  expect_setequal(unique(out$component), c("offence", "defence", "total"))
  expect_equal(nrow(out), cs$n_draws * cs$k * 3L)
  expect_setequal(unique(out$team), cs$teams$team)
})

test_that("the pulls report the parameter itself, untransformed", {
  # Pinning makes this an exact identity rather than a tolerance game, and it is
  # the B5 guard at the new seam: an exp() reintroduced in either pull would
  # publish 4.4817 instead of 1.5.
  cs <- pulls_case(constants = list(
    home_advantage_off = 1.5, cur_offense_away = -2.25
  ))

  ha <- .extract_home_advantage_draws_2dt(cs$fit, cs$teams)
  expect_equal(unique(ha$value[ha$component == "offence"]), 1.5, tolerance = 0)

  ts <- .extract_team_strength_draws_2dt(cs$fit, cs$teams)
  expect_equal(
    unique(ts$value[ts$component == "offence" & ts$location == "away"]),
    -2.25,
    tolerance = 0
  )
})

test_that("the quantile helpers now take draws, not a fit", {
  cs <- pulls_case(constants = list(cur_offense_away = 7))

  ts <- .compute_team_strengths_quantiles_2dt(
    .extract_team_strength_draws_2dt(cs$fit, cs$teams),
    cs$teams["team"]
  )
  expect_setequal(names(ts), c("team", "component", "location", "quantile", "value"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_equal(
    unique(ts$value[ts$component == "offence" & ts$location == "away"]),
    7,
    tolerance = 0
  )

  ha <- .compute_home_advantage_quantiles_2dt(
    .extract_home_advantage_draws_2dt(cs$fit, cs$teams),
    cs$teams["team"]
  )
  expect_setequal(names(ha), c("team", "component", "quantile", "value"))
})

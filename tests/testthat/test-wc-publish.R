# ---- Strength records (league team_strengths contract) ----------------------

test_that(".wc_strength_records emits the league contract shape", {
  teams <- c("X", "Y")
  set.seed(1)
  sit <- tidyr::expand_grid(.draw = 1:500, team = teams)
  sit$cur_offense <- stats::rnorm(nrow(sit), ifelse(sit$team == "X", 0.4, -0.1), 0.1)
  sit$cur_defense <- stats::rnorm(nrow(sit), 0.2, 0.1)
  out <- .wc_strength_records(sit, teams)
  expect_equal(nrow(out), 2L * 3L * 3L) # teams x components x coverages
  expect_setequal(unique(out$component), c("total", "offence", "defence"))
  expect_setequal(unique(out$coverage), c(0.5, 0.8, 0.95))
  expect_true(all(out$lower <= out$median & out$median <= out$upper))
  # Wider coverage nests narrower coverage per (team, component).
  for (g in split(out, list(out$team, out$component))) {
    g <- g[order(g$coverage), ]
    expect_true(all(diff(g$lower) <= 1e-12))
    expect_true(all(diff(g$upper) >= -1e-12))
  }
  # X's total median ~ 0.6 (0.4 offence + 0.2 defence).
  tot <- out[out$component == "total" & out$coverage == 0.5 & out$team == "X", ]
  expect_gt(tot$median, 0.5)
})

test_that("publish_world_cup writes the new contract fields", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)
  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)
  dir <- file.path(root, "publish", "world_cup", "karla")

  pred <- jsonlite::read_json(file.path(dir, "predictions.json"))
  m1 <- pred$matches[[1]]
  expect_true("goal_diff_distribution" %in% names(m1))
  gdd <- m1$goal_diff_distribution
  expect_true(all(vapply(gdd, function(e) all(c("diff", "p") %in% names(e)), logical(1))))
  expect_equal(sum(vapply(gdd, function(e) e$p, numeric(1))), 1, tolerance = 1e-2)

  ts <- jsonlite::read_json(file.path(dir, "team_strengths.json"))
  expect_true("records" %in% names(ts))
  expect_length(ts$records, length(teams) * 3L * 3L)
  r1 <- ts$records[[1]]
  expect_setequal(
    names(r1),
    c("team", "team_is", "component", "location", "coverage", "median", "lower", "upper")
  )
  expect_equal(r1$location, "avg")
  # The legacy point-estimate array is still there (endpoint stability).
  expect_true("teams" %in% names(ts))
  expect_length(ts$teams, length(teams))
})

test_that("publish_world_cup emits match_no + kickoff, kickoff-ordered", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)
  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)
  pred <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "predictions.json")
  )
  ms <- pred$matches
  expect_gt(length(ms), 0L)
  # Every prediction here is a group-stage match, so each resolves against
  # the schedule's validated 72 rows and carries both fields. The
  # omit-when-NA emit path is for future knockout rows (deferred phase).
  expect_true(all(vapply(ms, function(m) !is.null(m$match_no), logical(1))))
  expect_true(all(vapply(ms, function(m) !is.null(m$kickoff), logical(1))))
  # kickoff is ISO-8601 UTC.
  expect_match(ms[[1]]$kickoff, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  # Emitted in (match_date, match_no) order: match_no non-decreasing within a date.
  prev_date <- ""
  prev_no <- -1L
  for (m in ms) {
    if (m$match_date != prev_date) {
      prev_date <- m$match_date
      prev_no <- -1L
    }
    if (!is.null(m$match_no)) {
      expect_gte(m$match_no, prev_no)
      prev_no <- m$match_no
    }
  }
})

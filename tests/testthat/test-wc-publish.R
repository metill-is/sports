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
  # Emitted in (match_date, kickoff) order: kickoff non-decreasing within a date.
  prev_date <- ""
  prev_kick <- ""
  for (m in ms) {
    if (m$match_date != prev_date) {
      prev_date <- m$match_date
      prev_kick <- ""
    }
    if (!is.null(m$kickoff)) {
      expect_gte(m$kickoff, prev_kick)
      prev_kick <- m$kickoff
    }
  }
})

test_that("publish_world_cup serialises knockout rows with round + p_advance", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 60L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 2L)

  # Append synthetic R32 knockout predictions to the group cards.
  kfx <- .wc_knockout_fixtures_from(make_r32_schedule(s), make_group_results(s), s)
  kpred <- wc_knockout_predictions(kfx, si$team, si$scalar, s, out$bracket_model$W)
  out$predictions <- dplyr::bind_rows(out$predictions, kpred)

  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)
  pred <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "predictions.json")
  )

  ko <- Filter(function(m) !is.null(m$round), pred$matches)
  expect_equal(length(ko), nrow(kfx)) # all 16 knockout rows present
  m <- ko[[1]]
  expect_true(m$round %in% c("R32", "R16", "QF", "SF", "Final"))
  expect_true(is.numeric(m$p_advance) && m$p_advance >= 0 && m$p_advance <= 1)
  expect_null(m$group) # knockout rows carry no group
  expect_true("goal_diff_distribution" %in% names(m))
  expect_true(c("p_home", "p_draw", "p_away", "eg_home") %in% names(m) |> all())

  # Group rows still carry no round/p_advance (contract isolation).
  grp <- Filter(function(m) is.null(m$round), pred$matches)
  expect_gt(length(grp), 0L)
  expect_null(grp[[1]]$p_advance)
})

test_that("publish_world_cup handles knockout-only predictions (group stage over)", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 4L)
  kfx <- .wc_knockout_fixtures_from(make_r32_schedule(s), make_group_results(s), s)
  # Group stage finished: predictions hold ONLY knockout rows (group preds NULL).
  out$predictions <- wc_knockout_predictions(kfx, si$team, si$scalar, s, out$bracket_model$W)

  root <- withr::local_tempdir()
  expect_no_error(publish_world_cup(out, si$team, s, fx, root = root))
  pred <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "predictions.json")
  )
  expect_equal(length(pred$matches), nrow(kfx))
  expect_true(all(vapply(pred$matches, function(m) !is.null(m$round), logical(1))))
  expect_true(all(vapply(pred$matches, function(m) is.null(m$group), logical(1))))
})

test_that("publish_world_cup writes played[] (0-based) in bracket.json", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  si <- make_sim_inputs(unlist(s$groups, use.names = FALSE), n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)
  out$bracket_model$played <- list(list(
    match_no = 73L, winner = 5L, loser = 12L,
    winner_score = 2L, loser_score = 1L, shootout = FALSE
  ))

  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)
  br <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "bracket.json")
  )

  expect_length(br$played, 1L)
  p <- br$played[[1L]]
  expect_equal(p$match_no, 73L)
  expect_equal(p$winner, 4L) # 1-based 5 -> 0-based 4
  expect_equal(p$loser, 11L)
  expect_equal(p$winner_score, 2L)
  expect_equal(p$loser_score, 1L)
  expect_false(p$shootout)
})

test_that("publish_world_cup emits an empty played[] when nothing is decided", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  si <- make_sim_inputs(unlist(s$groups, use.names = FALSE), n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)

  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)
  br <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "bracket.json")
  )
  expect_length(br$played, 0L)
})

test_that("publish writes Third/Fourth placements and a bronze bracket node", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 60L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 3L)
  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)

  tp <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "tournament_placements.json"),
    simplifyVector = FALSE
  )
  rounds <- vapply(tp$records, function(r) r$round_name, character(1))
  expect_true("Third" %in% rounds)
  expect_true("Fourth" %in% rounds)
  expect_true(!is.null(tp$summary[[1]]$p_bronze))

  br <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "bracket.json"),
    simplifyVector = FALSE
  )
  thirds <- Filter(function(m) m$round == "Third", br$matches)
  expect_length(thirds, 1L)
  expect_equal(thirds[[1]]$match_no, 103L)
  expect_equal(thirds[[1]]$feeder_a, "L101")
})

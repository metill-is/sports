wwc_tie_fit <- function() {
  teams <- c("P", "Q", "X", "Y")
  theta <- rbind(c(0.4, 0.1, -0.2, 0), c(-0.1, 0.3, 0.2, -0.3), c(0, 0, 0, 0.5))
  colnames(theta) <- teams
  list(
    draws = list(
      theta = theta, delta = theta * 0, mu = c(0.1, 0.2, 0),
      home_for = c(0.1, 0.2, 0.05), home_against = c(-0.2, -0.1, -0.15)
    ),
    teams = teams, kappa = 1
  )
}

wwc_tie_table <- function(...) {
  t <- tibble::tibble(
    team_a = c("P", "Q"), team_b = c("X", "Y"),
    rank_a = c(4L, 1L), rank_b = c(2L, 3L)
  )
  extra <- list(...)
  for (n in names(extra)) t[[n]] <- extra[[n]]
  t
}

test_that("weighted play-off places equal the places from repeated draws", {
  ties <- wwc_tie_table()
  p <- matrix(c(0.6, 0.2, 0.7, 0.5), 2, 2)
  weighted <- nt_playoff_places(p, ties, n_direct = 1L, weights = c(3, 1))
  repeated <- nt_playoff_places(p[c(1, 1, 1, 2), ], ties, n_direct = 1L)
  expect_equal(weighted, repeated)
})

test_that("before any leg the forecast is the closed form on the fit's rates", {
  fit <- wwc_tie_fit()
  fc <- nt_two_leg_forecast(fit, wwc_tie_table())
  leg1 <- nt_match_rates(fit, c("P", "Q"), c("X", "Y"), venue = 1L)
  leg2 <- nt_match_rates(fit, c("X", "Y"), c("P", "Q"), venue = 1L)
  closed <- nt_two_leg_win_prob(leg1$home, leg1$away, leg2$away, leg2$home)
  expect_equal(fc$draws$a_wins, closed$a_wins, ignore_attr = TRUE)
  expect_equal(fc$draws$b_shootout, closed$b_shootout, ignore_attr = TRUE)
  expect_equal(fc$weights, rep(1 / 3, 3))
  expect_equal(fc$ties$status, c("scheduled", "scheduled"))
})

test_that("a played first leg reweights the draws and carries its margin", {
  fit <- wwc_tie_fit()
  fc <- nt_two_leg_forecast(fit, wwc_tie_table(leg1_goals_a = c(2L, NA), leg1_goals_b = c(0L, NA)))
  leg1 <- nt_match_rates(fit, c("P", "Q"), c("X", "Y"), venue = 1L)
  leg2 <- nt_match_rates(fit, c("X", "Y"), c("P", "Q"), venue = 1L)
  lik <- stats::dpois(2, leg1$home[, 1]) * stats::dpois(0, leg1$away[, 1])
  expect_equal(fc$weights, lik / sum(lik))
  second <- nt_two_leg_win_prob(0, 0, leg2$away[, 1], leg2$home[, 1], lead = 2L)
  expect_equal(fc$draws$a_wins[, 1], second$a_wins)
  untouched <- nt_two_leg_win_prob(leg1$home[, 2], leg1$away[, 2], leg2$away[, 2], leg2$home[, 2])
  expect_equal(fc$draws$a_wins[, 2], untouched$a_wins)
  expect_equal(fc$ties$status, c("first_leg_played", "scheduled"))
})

test_that("a decided tie is certain and its winner is checked", {
  fit <- wwc_tie_fit()
  fc <- nt_two_leg_forecast(fit, wwc_tie_table(winner = c("X", NA)))
  expect_equal(fc$draws$a_wins[, 1], rep(0, 3))
  expect_true(all(is.na(fc$draws$a_regulation[, 1])))
  expect_equal(fc$ties$status[1], "decided")
  expect_error(nt_two_leg_forecast(fit, wwc_tie_table(winner = c("Z", NA))), "winner")
  expect_error(nt_two_leg_forecast(fit, wwc_tie_table(leg1_goals_a = c(1L, NA))), "both")
})

wwc_names_csv <- function(dir, teams = c("P", "Q", "X", "Y")) {
  path <- file.path(dir, "names.csv")
  utils::write.csv(
    data.frame(team = teams, name_is = paste0(teams, "\u00f0")),
    path,
    row.names = FALSE, fileEncoding = "UTF-8"
  )
  path
}

test_that("the play-off JSON holds the forecast's numbers", {
  dir <- withr::local_tempdir()
  fit <- wwc_tie_fit()
  ties <- wwc_tie_table(
    leg1_goals_a = c(1L, NA), leg1_goals_b = c(1L, NA),
    leg1_date = c("2026-12-01", NA), leg2_date = c("2026-12-05", NA)
  )
  fc <- nt_two_leg_forecast(fit, ties)
  out <- publish_wwc2027_playoff(fc,
    n_direct = 1L, fit_date = as.Date("2026-12-01"), data_through = as.Date("2026-10-13"),
    root = dir, country_csv = wwc_names_csv(dir), featured_team = "X"
  )
  expect_equal(out, file.path(dir, "publish", "world_cup", "kvenna"))
  meta <- jsonlite::read_json(file.path(out, "meta.json"))
  expect_equal(meta$stage, "playoff")
  expect_equal(meta$sex, "female")
  expect_equal(meta$featured_team_is, "X\u00f0")
  pl <- jsonlite::read_json(file.path(out, "playoff.json"))
  expect_length(pl$ties, 2L)
  w <- fc$weights

  t1 <- pl$ties[[1]]
  expect_equal(t1$status, "first_leg_played")
  expect_equal(t1$team_a_is, "P\u00f0")
  expect_equal(t1$p_a, round(sum(fc$draws$a_wins[, 1] * w), 4))
  expect_equal(t1$p_a + t1$p_b, 1, tolerance = 1e-4)
  expect_equal(sum(unlist(t1$decided_by)), 1, tolerance = 1e-3)
  expect_true(t1$legs[[1]]$played)
  expect_equal(c(t1$legs[[1]]$goals_home, t1$legs[[1]]$goals_away), c(1L, 1L))
  expect_null(t1$legs[[1]]$p_home)
  expect_null(t1$after_first_leg)
  expect_equal(t1$legs[[2]]$date, "2026-12-05")
  expect_equal(sum(vapply(t1$aggregate_margin, `[[`, 1, "p")), 1, tolerance = 1e-3)

  t2 <- pl$ties[[2]]
  expect_equal(t2$status, "scheduled")
  expect_null(t2$legs[[1]]$date)
  l1 <- t2$legs[[1]]
  expect_equal(l1$p_home + l1$p_draw + l1$p_away, 1, tolerance = 1e-3)
  rates <- nt_match_rates(fit, "Q", "Y", venue = 1L)
  expect_equal(l1$xg_home, round(sum(rates$home[, 1] * w), 3))
  expect_length(l1$scorelines, 10L)
  expect_length(t2$after_first_leg, 25L)
  # P(score) times P(A wins | score) summed over the table cannot exceed P(A wins).
  joint <- sum(vapply(t2$after_first_leg, function(r) r$p_score * r$p_a, 1))
  expect_lte(joint, t2$p_a + 1e-3)

  places <- pl$places
  p_win <- vapply(places, `[[`, 1, "p_win")
  p_direct <- vapply(places, `[[`, 1, "p_direct")
  p_ic <- vapply(places, `[[`, 1, "p_interconf_playoff")
  expect_equal(sum(p_win), 2, tolerance = 1e-3)
  expect_equal(sum(p_direct), 1, tolerance = 1e-3)
  expect_equal(p_direct + p_ic, p_win, tolerance = 1e-3)
})

test_that("the publisher refuses a team without an Icelandic name", {
  dir <- withr::local_tempdir()
  fc <- nt_two_leg_forecast(wwc_tie_fit(), wwc_tie_table())
  expect_error(
    publish_wwc2027_playoff(fc,
      n_direct = 1L, fit_date = as.Date("2026-12-01"), data_through = as.Date("2026-10-13"),
      root = dir, country_csv = wwc_names_csv(dir, c("P", "Q", "X"))
    ),
    "Y"
  )
})

wwc_finals_draw <- function() {
  teams <- c("Brazil", paste0("T", 2:32))
  draw <- data.frame(group = rep(LETTERS[1:8], each = 4), team = teams, candidates = "")
  draw$team[32] <- "IC1"
  draw$candidates[32] <- "T32:0.75; Z1:0.25"
  draw
}

wwc_finals_fit <- function() {
  teams <- c("Brazil", paste0("T", 2:32), "Z1")
  withr::with_seed(8, nt_test_fit(stats::setNames(stats::rnorm(33, 0, 0.4), teams), mu = 0.1,
    home_for = 0.1, home_against = -0.1))
}

test_that("the finals forecast pools over the play-off candidates by their weights", {
  fit <- wwc_finals_fit()
  fc <- nt_wwc2027_finals(fit, wwc_finals_draw(), n_sims = 400L, seed = 3L)
  t <- fc$teams
  get <- function(team, col) t[[col]][t$team == team]
  expect_equal(fc$group_rule, "wwc_2023")
  expect_equal(fc$knockout_hosts, "Brazil")
  expect_equal(get("T32", "p_in_finals"), 0.75)
  expect_equal(get("Z1", "p_in_finals"), 0.25)
  expect_equal(get("Z1", "placeholder"), "IC1")
  expect_equal(get("Z1", "group"), "H")
  expect_true(is.na(get("Brazil", "placeholder")))
  expect_equal(sum(t$p_champion), 1)
  expect_equal(sum(t$p_first[t$group == "H"]), 1)
  expect_equal(sum(t$p_advance[t$group == "H"]), 2)
  # Each combination is its own seeded run; the pool weighs them by their runs.
  run <- function(team_h4, n, c) {
    g <- split(wwc_finals_draw()$team, wwc_finals_draw()$group)
    g$H[4] <- team_h4
    sim <- nt_simulate_world_cup(fit, nt_wwc2027_structure(g), n_sims = n, seed = 3000L + c)$teams
    sim$p_champion[sim$team == "Brazil"]
  }
  expect_equal(get("Brazil", "p_champion"), (300 * run("T32", 300L, 1L) + 100 * run("Z1", 100L, 2L)) / 400)
})

test_that("the finals JSON holds the pooled forecast in Icelandic names", {
  dir <- withr::local_tempdir()
  fit <- wwc_finals_fit()
  fc <- nt_wwc2027_finals(fit, wwc_finals_draw(), n_sims = 400L, seed = 3L)
  csv <- wwc_names_csv(dir, fit$teams)
  out <- publish_wwc2027_finals(fc,
    fit_date = as.Date("2026-12-11"), data_through = as.Date("2026-12-05"),
    root = dir, country_csv = csv
  )
  meta <- jsonlite::read_json(file.path(out, "meta.json"))
  expect_equal(meta$stage, "finals")
  expect_equal(meta$group_rule, "wwc_2023")
  expect_equal(meta$n_sims, 400L)
  top <- fc$teams$team[which.max(fc$teams$p_champion)]
  expect_equal(meta$champion_favourite$team, top)
  groups <- jsonlite::read_json(file.path(out, "groups.json"))$groups
  expect_equal(vapply(groups, `[[`, "", "group"), LETTERS[1:8])
  h <- groups[[8]]$teams
  expect_length(h, 5L)
  z1 <- h[[which(vapply(h, `[[`, "", "team") == "Z1")]]
  expect_equal(z1$team_is, "Z1\u00f0")
  expect_equal(z1$placeholder, "IC1")
  expect_equal(z1$p_in_finals, 0.25)
  expect_equal(sum(vapply(h, `[[`, 1, "p_first")), 1, tolerance = 1e-3)
  tp <- jsonlite::read_json(file.path(out, "tournament_placements.json"))
  expect_equal(tp$n_teams, 32L)
  rec <- do.call(rbind, lapply(tp$records, as.data.frame))
  expect_setequal(unique(rec$round_name), c("R16", "QF", "SF", "Final", "Champion", "Third", "Fourth"))
  expect_equal(sum(rec$probability[rec$round_name == "Champion"]), 1, tolerance = 1e-3)
  expect_equal(sum(rec$probability[rec$round_name == "R16"]), 16, tolerance = 1e-2)
  expect_equal(sum(rec$probability[rec$round_name == "Fourth"]), 1, tolerance = 1e-3)
})

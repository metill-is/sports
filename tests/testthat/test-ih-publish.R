# International handball: the files written for metill-platform.

test_that("before the start: six groups, every preliminary match priced, contract fields present", {
  x <- publish_fixture()
  sim <- ih_simulate_tournament(x$fit, x$spec, n_sims = 400L, seed = 1)
  out <- withr::local_tempdir()
  paths <- ih_publish_tournament(x$fit, sim, x$spec, NULL, out, names = x$names, n_sims_matches = 400L)
  expect_setequal(basename(paths), c("groups.json", "predictions.json", "tournament_placements.json", "team_strengths.json", "meta.json"))
  g <- jsonlite::read_json(file.path(out, "groups.json"))
  expect_length(g$groups, 6L)
  expect_true(all(vapply(g$groups, function(x) x$stage, "") == "preliminary"))
  t1 <- g$groups[[1]]$teams[[1]]
  expect_named(t1, c(
    "team", "team_is", "iso2", "played", "won", "drawn", "lost", "points", "gf", "ga", "gd",
    "proj_points", "proj_gd", "p_pos", "p_advance"
  ), ignore.order = TRUE)
  expect_equal(sum(unlist(t1$p_pos)), 1, tolerance = 1e-3)
  expect_equal(t1$team, "T01")
  p <- jsonlite::read_json(file.path(out, "predictions.json"))$matches
  expect_length(p, 36L)
  expect_equal(p[[1]]$p_home + p[[1]]$p_draw + p[[1]]$p_away, 1, tolerance = 1e-3)
  expect_equal(sum(vapply(p[[1]]$goal_diff_distribution, function(b) b$p, 0)), 1, tolerance = 1e-3)
  expect_true(all(vapply(p[[1]]$goal_diff_distribution, function(b) b$diff %% 2 == 0, TRUE)))
  pl <- jsonlite::read_json(file.path(out, "tournament_placements.json"))
  expect_equal(vapply(pl$rounds, function(r) r$code, ""), c("MR", "SF", "Final", "Champion"))
  expect_equal(sum(vapply(pl$summary, function(s) s$p_champion, 0)), 1, tolerance = 1e-3)
  meta <- jsonlite::read_json(file.path(out, "meta.json"))
  expect_false(meta$placeholder)
  expect_equal(meta$fit_date, "2026-12-01")
  expect_equal(meta$matches_group_total, 36L)
  expect_equal(meta$focus_round, "SF")
  st <- jsonlite::read_json(file.path(out, "team_strengths.json"))
  expect_length(st$teams, 24L)
  expect_length(st$records, 24L * 3L * 3L)
})

test_that("a finished preliminary round adds the main-round groups and prices their matches", {
  x <- publish_fixture()
  pf <- x$spec$prelim
  res <- data.frame(
    stage = "preliminary", match_date = pf$match_date, home_team = pf$home_team,
    away_team = pf$away_team, home_60 = 30L, away_60 = 25L, home_score = 30L, away_score = 25L,
    pen_home = NA_integer_, pen_away = NA_integer_, home_wins = TRUE
  )
  sim <- ih_simulate_tournament(x$fit, x$spec, res, n_sims = 400L, seed = 1)
  out <- withr::local_tempdir()
  ih_publish_tournament(x$fit, sim, x$spec, res, out, names = x$names, n_sims_matches = 400L)
  g <- jsonlite::read_json(file.path(out, "groups.json"))$groups
  expect_length(g, 8L)
  main <- g[vapply(g, function(x) x$stage, "") == "main"]
  expect_equal(vapply(main, function(x) x$label_is, ""), c("Milliri\u00f0ill I", "Milliri\u00f0ill II"))
  # The listed home side won every match: T01 and T02 top their groups and carry
  # their result against each other (T01 won) into the main round.
  mi <- main[[1]]$teams
  expect_setequal(vapply(mi, function(t) t$team, ""), c("T01", "T02", "T05", "T06", "T09", "T10"))
  t01 <- mi[[which(vapply(mi, function(t) t$team, "") == "T01")]]
  expect_equal(t01$played, 1L)
  expect_equal(t01$points, 2L)
  p <- jsonlite::read_json(file.path(out, "predictions.json"))$matches
  expect_length(p, 24L)
  expect_true(all(vapply(p, function(m) m$stage, "") == "main"))
})

test_that("knockout cards carry the chance of going through", {
  x <- publish_fixture()
  spec <- x$spec
  pf <- spec$prelim
  res <- data.frame(
    stage = "preliminary", match_date = pf$match_date, home_team = pf$home_team,
    away_team = pf$away_team, home_60 = 30L, away_60 = 25L, home_score = 30L, away_score = 25L,
    pen_home = NA_integer_, pen_away = NA_integer_, home_wins = TRUE
  )
  st <- ih_actual_standings(spec, res)
  mfx <- spec$main_fixtures
  mr <- do.call(rbind, lapply(names(spec$main), function(G) {
    s <- st$main[[G]]$slots
    f <- mfx[mfx$group == G, ]
    data.frame(
      stage = "main", match_date = f$match_date, home_team = unname(s[f$home_slot]),
      away_team = unname(s[f$away_slot]), home_60 = 28L, away_60 = 27L, home_score = 28L,
      away_score = 27L, pen_home = NA_integer_, pen_away = NA_integer_, home_wins = TRUE
    )
  }))
  res2 <- rbind(res, mr)
  sim <- ih_simulate_tournament(x$fit, spec, res2, n_sims = 400L, seed = 1)
  out <- withr::local_tempdir()
  ih_publish_tournament(x$fit, sim, spec, res2, out, names = x$names, n_sims_matches = 400L)
  p <- jsonlite::read_json(file.path(out, "predictions.json"))$matches
  expect_equal(vapply(p, function(m) m$round, ""), c("SF", "SF", "Fifth"))
  expect_true(all(vapply(p, function(m) !is.null(m$p_advance), TRUE)))
  expect_true(all(vapply(p, function(m) m$p_advance >= 0 && m$p_advance <= 1, TRUE)))
  tm <- sim$teams
  expect_equal(sum(tm$p_SF), 4)
  expect_true(all(tm$p_SF %in% c(0, 1)))
})
test_that("a finished group is listed in its tie-break order, head to head before goal difference", {
  x <- publish_fixture()
  spec <- x$spec
  pf <- spec$prelim
  # Group A: T02 beats T01 25-24 and both end on 4 points, T01 with a goal
  # difference of +39 against T02's +4; T04 beats T03 and both end on 2 points,
  # T03 with the better goal difference. Every other match: the listed home
  # side wins 30-25.
  a_scores <- list(
    "T01 T02" = c(24L, 25L), "T01 T03" = c(40L, 20L), "T01 T04" = c(40L, 20L),
    "T02 T03" = c(20L, 22L), "T02 T04" = c(25L, 20L), "T03 T04" = c(20L, 21L)
  )
  key <- paste(pf$home_team, pf$away_team)
  pick <- function(k, i, default) if (is.null(a_scores[[k]])) default else a_scores[[k]][i]
  hs <- vapply(key, pick, 1L, i = 1L, default = 30L, USE.NAMES = FALSE)
  as <- vapply(key, pick, 1L, i = 2L, default = 25L, USE.NAMES = FALSE)
  res <- data.frame(
    stage = "preliminary", match_date = pf$match_date, home_team = pf$home_team,
    away_team = pf$away_team, home_60 = hs, away_60 = as, home_score = hs, away_score = as,
    pen_home = NA_integer_, pen_away = NA_integer_, home_wins = hs > as, row.names = NULL
  )
  # Main round I (1A T02, 2A T01, 1B T05, 2B T06, 1C T09, 2C T10): T01 wins its
  # four new matches by ten, T02 loses 24-25 to T05 and wins its other three
  # 25-24, the rest are drawn 25-25. T01 and T02 end on 8 points, T01 with the
  # better goal difference (+39 against +3), but T02 won their carried match.
  st <- ih_actual_standings(spec, res)
  expect_equal(unname(st$main$I$slots[c("1A", "2A")]), c("T02", "T01"))
  s <- st$main$I$slots
  f <- spec$main_fixtures[spec$main_fixtures$group == "I", ]
  h <- unname(s[f$home_slot])
  a <- unname(s[f$away_slot])
  score <- function(h, a) {
    if (h == "T01") {
      return(c(30L, 20L))
    }
    if (a == "T01") {
      return(c(20L, 30L))
    }
    if (h == "T02") {
      return(if (a == "T05") c(24L, 25L) else c(25L, 24L))
    }
    if (a == "T02") {
      return(if (h == "T05") c(25L, 24L) else c(24L, 25L))
    }
    c(25L, 25L)
  }
  sc <- t(mapply(score, h, a))
  mr <- data.frame(
    stage = "main", match_date = f$match_date, home_team = h, away_team = a,
    home_60 = sc[, 1], away_60 = sc[, 2], home_score = sc[, 1], away_score = sc[, 2],
    pen_home = NA_integer_, pen_away = NA_integer_, home_wins = sc[, 1] > sc[, 2], row.names = NULL
  )
  res2 <- rbind(res, mr)
  sim <- ih_simulate_tournament(x$fit, spec, res2, n_sims = 400L, seed = 1)
  out <- withr::local_tempdir()
  ih_publish_tournament(x$fit, sim, spec, res2, out, names = x$names, n_sims_matches = 400L)
  g <- jsonlite::read_json(file.path(out, "groups.json"))$groups
  team_order <- function(grp) vapply(grp$teams, function(t) t$team, "")
  ga <- g[[which(vapply(g, function(x) x$group, "") == "A")]]
  expect_equal(team_order(ga), c("T02", "T01", "T04", "T03"))
  expect_equal(unlist(ga$teams[[1]]$p_pos), c(1, 0, 0, 0))
  gi <- g[[which(vapply(g, function(x) x$group, "") == "I")]]
  expect_equal(team_order(gi)[1:4], c("T02", "T01", "T05", "T09"))
  expect_equal(vapply(gi$teams[1:2], function(t) t$points, 1L), c(8L, 8L))
  expect_equal(unlist(gi$teams[[1]]$p_pos)[1], 1)
})
test_that("a group match without a 60-minute score counts with its final score; a knockout one does not", {
  spec <- publish_fixture()$spec
  pf <- spec$prelim
  # A group match never goes to extra time, so its final score is its score
  # after 60 minutes; the knockout match went to extra time, so its final
  # score is not.
  m <- data.frame(
    competition = spec$competition, edition = spec$edition, block = "",
    match_date = c(pf$match_date[1], spec$knockout$match_date[1]),
    home_team = c(pf$home_team[1], "T01"), away_team = c(pf$away_team[1], "T05"),
    home_score = c(30L, 33L), away_score = c(25L, 31L),
    home_60 = NA_integer_, away_60 = NA_integer_, pen_home = NA_integer_, pen_away = NA_integer_
  )
  r <- ih_tournament_results(m, spec)
  expect_equal(r$stage, c("preliminary", "knockout"))
  expect_equal(r$home_60, c(30L, NA))
  expect_equal(r$away_60, c(25L, NA))
  expect_equal(r$home_wins, c(TRUE, TRUE))
  tab <- ih_actual_standings(spec, r)$prelim$A$table
  expect_equal(tab$points[tab$team == pf$home_team[1]], 2)
  expect_equal(tab$gd[tab$team == pf$away_team[1]], -5)
})

test_that("a forecast as of day D counts D's matches and is dated D", {
  # WHY: the refresh runs in the evening after the day's matches; a forecast
  # as of D that left out D's results would carry a finished match as still to
  # be played, under D's date.
  skip_if_no_inla()
  x <- publish_fixture()
  spec <- x$spec
  hist <- simulate_nations(n_teams = 24)$matches
  hist$home_team <- sub("^N", "T", hist$home_team)
  hist$away_team <- sub("^N", "T", hist$away_team)
  hist$competition <- "FRI"
  day <- min(spec$prelim$match_date)
  first <- spec$prelim[spec$prelim$match_date == day, ][1, ]
  champ <- data.frame(
    match_date = day, season = ih_season(day), division = "intl", home_team = first$home_team,
    away_team = first$away_team, home_score = 30L, away_score = 25L, home_60 = 30L, away_60 = 25L,
    venue = 0L, competition = spec$competition
  )
  m <- rbind(hist, champ)
  m$edition <- spec$edition
  m$block <- ""
  m$pen_home <- NA_integer_
  m$pen_away <- NA_integer_

  on_day <- ih_forecast_fit(m, spec, day, n_draws = 50L, int_strategy = "eb")
  before <- ih_forecast_fit(m, spec, day - 1, n_draws = 50L, int_strategy = "eb")
  expect_equal(nrow(on_day$results), 1L)
  expect_equal(nrow(before$results), 0L)
  expect_equal(on_day$fit$n_train, nrow(hist) + 1L)
  expect_equal(before$fit$n_train, nrow(hist))
  expect_equal(on_day$fit$as_of, day)
  expect_equal(before$fit$as_of, day - 1)

  sim <- ih_simulate_tournament(on_day$fit, spec, on_day$results, n_sims = 200L, seed = 1)
  out <- withr::local_tempdir()
  ih_publish_tournament(on_day$fit, sim, spec, on_day$results, out, names = x$names, n_sims_matches = 200L)
  meta <- jsonlite::read_json(file.path(out, "meta.json"))
  expect_equal(meta$fit_date, format(day))
  expect_equal(meta$matches_played, 1L)
})

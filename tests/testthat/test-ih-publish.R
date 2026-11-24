# International handball: the files written for metill-platform.

publish_fixture <- function() {
  teams <- sprintf("T%02d", 1:24)
  groups <- split(teams, rep(LETTERS[1:6], each = 4))
  prelim <- do.call(rbind, lapply(names(groups), function(g) {
    p <- t(utils::combn(groups[[g]], 2))
    data.frame(
      match_no = NA, group = g, match_date = as.Date("2026-12-03") + 0:5,
      kickoff = as.POSIXct("2026-12-03 17:00", tz = "UTC") + 86400 * 0:5,
      venue_city = paste("City", g), host = NA_character_, home_team = p[, 1], away_team = p[, 2]
    )
  }))
  slots <- function(gs) as.vector(t(outer(gs, 1:2, function(g, p) paste0(p, g))))
  mf <- do.call(rbind, lapply(list(I = LETTERS[1:3], II = LETTERS[4:6]), function(gs) {
    s <- slots(gs)
    pr <- t(utils::combn(s, 2))
    pr <- pr[substr(pr[, 1], 2, 2) != substr(pr[, 2], 2, 2), ]
    data.frame(home_slot = pr[, 1], away_slot = pr[, 2])
  }))
  mf$group <- rep(c("I", "II"), each = 12)
  mf$match_date <- as.Date("2026-12-10")
  mf$kickoff <- as.POSIXct(NA)
  spec <- IH_TOURNAMENTS$euro2026w
  spec$groups <- groups
  spec$prelim <- prelim
  spec$main_fixtures <- mf
  spec$main_city <- c(I = "City I", II = "City II")
  spec$main_host <- c(I = NA_character_, II = NA_character_)
  spec$knockout$match_date <- as.Date(c("2026-12-18", "2026-12-18", "2026-12-18", "2026-12-20", "2026-12-20"))
  spec$knockout$kickoff <- as.POSIXct(NA)
  spec$knockout$venue_city <- "City K"
  spec$knockout$host <- NA_character_
  strength <- stats::setNames(rep(c(6, 2, -2, -6), 6), teams)
  n <- 200L
  m <- matrix(rep(strength, each = n), n, 24, dimnames = list(NULL, teams))
  fit <- list(
    teams = teams, m_state = m, t_state = m * 0, level = rep(55, n), hfa = rep(0, n), ha = m * 0,
    sigma_m = rep(5, n), sigma_t = rep(6, n), as_of = as.Date("2026-12-01")
  )
  names <- data.frame(team = teams, official = teams, team_is = paste0("Li\u00f0 ", 1:24), iso2 = NA_character_)
  list(spec = spec, fit = fit, names = names)
}

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

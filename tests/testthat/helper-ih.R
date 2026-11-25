# International handball test fixtures: simulated nations for the model,
# and a small championship with a stub fit for the simulator and publisher.

# Matches between simulated nations over several seasons: a strength per
# nation and season, a known home advantage `hfa` on the venue term.
simulate_nations <- function(seed = 5, n_teams = 12, seasons = 2021:2024, hfa = 3) {
  set.seed(seed)
  teams <- sprintf("N%02d", seq_len(n_teams))
  level <- rnorm(n_teams, 0, 6)
  pace <- rnorm(n_teams, 0, 2)
  rows <- list()
  for (s in seasons) {
    state <- level + rnorm(n_teams, 0, 2)
    pairs <- expand.grid(h = seq_len(n_teams), a = seq_len(n_teams))
    pairs <- pairs[pairs$h < pairs$a, ]
    venue <- sample(c(-1L, 0L, 1L), nrow(pairs), replace = TRUE, prob = c(0.2, 0.5, 0.3))
    margin <- state[pairs$h] - state[pairs$a] + hfa * venue + rnorm(nrow(pairs), 0, 5)
    total <- 55 + pace[pairs$h] + pace[pairs$a] + rnorm(nrow(pairs), 0, 6)
    home <- round((total + margin) / 2)
    away <- round((total - margin) / 2)
    rows[[length(rows) + 1L]] <- data.frame(
      match_date = as.Date(sprintf("%d-09-01", s)) + sort(sample(0:250, nrow(pairs), replace = TRUE)),
      season = s, division = "intl", home_team = teams[pairs$h], away_team = teams[pairs$a],
      home_score = home, away_score = away, home_60 = home, away_60 = away, venue = venue
    )
  }
  list(matches = do.call(rbind, rows), teams = teams, level = level)
}

# A 24-team championship in the EURO 2026 format (six groups of four, two
# main-round groups, semi-finals) with a stub fit whose strengths fall by
# group position: 6, 2, -2, -6.
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

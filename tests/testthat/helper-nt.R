# Shared builders for the national-team (nt_*) tests.

nt_test_results <- function(...) {
  rows <- tibble::tribble(
    ~match_date, ~home_team, ~away_team, ~home_score, ~away_score, ~tournament, ~country, ~neutral,
    ...
  )
  rows$match_date <- as.Date(rows$match_date)
  rows$home_score <- as.integer(rows$home_score)
  rows$away_score <- as.integer(rows$away_score)
  rows
}

# A hand-built "fit" in the layout nt_fit_inla() returns: `n_draws` identical
# draws unless `theta`/`delta` are given as matrices.
nt_test_fit <- function(theta, delta = NULL, mu = 0, home_for = 0,
                        home_against = 0, kappa = 1, n_draws = 2L) {
  teams <- names(theta)
  if (is.null(delta)) delta <- stats::setNames(rep(0, length(teams)), teams)
  as_draws <- function(x) {
    matrix(x, n_draws, length(x), byrow = TRUE, dimnames = list(NULL, names(x)))
  }
  list(
    draws = list(
      theta = as_draws(theta), delta = as_draws(delta),
      mu = rep(mu, n_draws), home_for = rep(home_for, n_draws),
      home_against = rep(home_against, n_draws)
    ),
    teams = teams, kappa = kappa
  )
}

# Euro 2024 qualifying fixture (see fixtures/euro2024_qualifying/SOURCES.md).
nt_e24_fixture <- function() {
  read <- function(f) {
    utils::read.csv(
      testthat::test_path("fixtures", "euro2024_qualifying", f),
      stringsAsFactors = FALSE, encoding = "UTF-8"
    )
  }
  tables <- read("tables.csv")
  nl <- read("nations_league_rank.csv")
  disc <- read("disciplinary.csv")
  list(
    matches = read("matches.csv"),
    tables = tables,
    overall = read("overall.csv"),
    groups = split(tables$team, tables$group),
    nl_rank = stats::setNames(nl$rank, nl$team),
    disciplinary = stats::setNames(disc$points, disc$team)
  )
}

nt_q_matches <- function(...) {
  rows <- tibble::tribble(~home_team, ~away_team, ~home_score, ~away_score, ...)
  rows$home_score <- as.integer(rows$home_score)
  rows$away_score <- as.integer(rows$away_score)
  rows
}

nt_four_way_tie <- function() {
  # A, B, C and D level on 13 points. Head-to-head among the four puts A
  # first; re-applied among B, C and D it puts B last; C and D are still
  # level. Their own matches put C first (4 points to 1); overall goal
  # difference puts D first (+5 against +1).
  nt_q_matches(
    "C", "D", 1, 0, "D", "C", 0, 0,
    "C", "B", 1, 0, "B", "C", 1, 0,
    "D", "B", 1, 0, "B", "D", 0, 1,
    "C", "A", 0, 1, "A", "C", 1, 0,
    "D", "A", 0, 1, "A", "D", 1, 0,
    "B", "A", 1, 0, "A", "B", 0, 0,
    "A", "E", 0, 1, "E", "A", 1, 0,
    "B", "E", 1, 0, "E", "B", 0, 1,
    "C", "E", 1, 0, "E", "C", 0, 1,
    "D", "E", 5, 0, "E", "D", 0, 1
  )
}

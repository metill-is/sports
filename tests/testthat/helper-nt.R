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

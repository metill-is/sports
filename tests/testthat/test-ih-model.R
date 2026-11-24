# International handball: seasons, venues and the nations model.

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

test_that("a season runs from July to June", {
  expect_equal(ih_season(as.Date(c("2026-06-30", "2026-07-01", "2026-12-03", "2027-01-13"))), c(2025L, 2026L, 2026L, 2026L))
})

test_that("venues: qualifiers and friendlies at home, championships neutral except for hosts", {
  hosts <- data.frame(competition = "EURO", sex = "female", edition = 2026L, host = c("Poland", "Romania"))
  m <- data.frame(
    sex = "female",
    competition = c("EURO", "EURO", "EURO", "EURO", "FRI", "GL_NOR", "GL_NOR", "GL_NOR", "EUROCUP", "WC"),
    kind = c("tournament", "tournament", "tournament", "tournament", "friendly", "friendly", "friendly", "friendly", "tournament", "tournament"),
    edition = c(2026L, 2026L, 2026L, 2026L, 2025L, 2025L, 2025L, 2025L, 2026L, 2026L),
    block = c("EHF Euro Women - Qualification - Second stage", "EHF Euro Women", "EHF Euro Women", "EHF Euro Women", NA, NA, NA, NA, "EHF Euro Cup Women", "World Championship Women"),
    home_team = c("Iceland", "Poland", "Hungary", "Hungary", "Iceland", "Norway", "France", "Denmark", "Iceland", "Poland"),
    away_team = c("Portugal", "Hungary", "Romania", "Slovenia", "Germany", "France", "Norway", "Sweden", "Norway", "Hungary")
  )
  expect_equal(ih_venue(m, hosts), c(1L, 1L, -1L, 0L, 1L, 1L, -1L, 0L, 1L, 0L))
})

test_that("the hosts file names each championship's hosts once", {
  h <- ih_hosts()
  expect_named(h, c("competition", "sex", "edition", "host"))
  expect_false(anyDuplicated(h) > 0)
  expect_true(all(h$sex %in% c("female", "male")))
})

test_that("the nations fit's composed locations are R-INLA's own predictor, draw by draw", {
  # WHY: the simulator prices every pairing from the composed states; a state
  # read under the wrong node or a total draw paired out of order still looks
  # like a forecast. Only R-INLA's own predictor for the same rows shows it.
  skip_if_no_inla()
  sim <- simulate_nations()
  m <- sim$matches
  as_of <- as.Date("2025-03-01")
  fx <- m[m$match_date >= as_of, c("match_date", "home_team", "away_team", "venue")]
  fit <- ih_fit(m, fx, as_of, n_draws = 300L, int_strategy = "eb")

  expect_identical(unname(fit$mode_status), c(0L, 0L))
  expect_equal(fit$season, 2024L)
  loc <- ih_locations(fit, fx$home_team, fx$away_team, fx$venue)
  expect_lt(max(abs(loc$loc_m - fit$loc_m)), 0.01)
  expect_lt(max(abs(loc$loc_t - fit$loc_t)), 0.01)
  expect_lt(abs(mean(fit$hfa) - 3), 1)

  p <- ih_match_probs(fit, fx$home_team[1:5], fx$away_team[1:5], fx$venue[1:5])
  expect_equal(p$p_home + p$p_draw + p$p_away, rep(1, 5), tolerance = 1e-9)
  expect_true(all(p$eg_home > 0 & p$eg_away > 0))
})

test_that("a nation without a training match cannot be forecast", {
  skip_if_no_inla()
  sim <- simulate_nations()
  fx <- data.frame(match_date = as.Date("2025-03-02"), home_team = "N01", away_team = "Nowhere", venue = 0L)
  expect_error(ih_fit(sim$matches, fx, as.Date("2025-03-01"), n_draws = 10L, int_strategy = "eb"), "Nowhere")
})

test_that("states can be drawn for a season with no fixture", {
  skip_if_no_inla()
  sim <- simulate_nations()
  fit <- ih_fit(sim$matches, sim$matches[0, c("match_date", "home_team", "away_team", "venue")],
    as.Date("2026-01-01"),
    season = 2025L, n_draws = 50L, int_strategy = "eb"
  )
  expect_equal(fit$season, 2025L)
  expect_identical(dim(fit$m_state), c(50L, 12L))
  expect_null(fit$loc_m)
})

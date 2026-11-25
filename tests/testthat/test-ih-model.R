# International handball: seasons, venues and the nations model.

test_that("a season runs from July to June", {
  expect_equal(ih_season(as.Date(c("2026-06-30", "2026-07-01", "2026-12-03", "2027-01-13"))), c(2025L, 2026L, 2026L, 2026L))
})

no_qualifier_venues <- function() {
  data.frame(
    competition = character(), sex = character(), edition = integer(), first = as.Date(character()),
    last = as.Date(character()), teams = character(), venue_city = character(), venue_country = character(),
    source = character()
  )
}

test_that("venues: friendlies at the listed home side, friendly tournaments at home for their host", {
  m <- data.frame(
    sex = "female", competition = c("FRI", "GL_NOR", "GL_NOR", "GL_NOR", "EUROCUP"),
    kind = c("friendly", "friendly", "friendly", "friendly", "tournament"), edition = c(2025L, 2025L, 2025L, 2025L, 2026L),
    block = c(NA, NA, NA, NA, "EHF Euro Cup Women"), match_date = as.Date("2025-10-01"),
    home_team = c("Iceland", "Norway", "France", "Denmark", "Iceland"),
    away_team = c("Germany", "France", "Norway", "Sweden", "Norway")
  )
  hosts <- data.frame(competition = character(), sex = character(), edition = integer(), host = character())
  expect_equal(ih_venue(m, hosts, ih_championship_venues()[0, ], no_qualifier_venues()), c(1L, 1L, -1L, 0L, 1L))
})

test_that("championship venues: only a host playing in its own country is at home, as in the simulator", {
  hosts <- data.frame(
    competition = c("EURO", "EURO", "OG"), sex = "female", edition = c(2026L, 2026L, 2024L),
    host = c("Poland", "Romania", "France")
  )
  venues <- data.frame(
    competition = "EURO", sex = "female", edition = 2026L,
    match_date = as.Date(c("2026-12-04", "2026-12-05", "2026-12-12", "2026-12-13")),
    home_team = c("Poland", "Hungary", "Poland", "Romania"), away_team = c("Hungary", "Romania", "Romania", "Hungary"),
    venue_city = c("Katowice", "Oradea", "Cluj-Napoca", "Katowice"),
    venue_country = c("Poland", "Romania", "Romania", "Poland"), source = "test"
  )
  m <- data.frame(
    sex = "female", competition = c(rep("EURO", 5), "OG", "OG", "WC"), kind = "tournament",
    edition = c(rep(2026L, 5), 2024L, 2024L, 2025L),
    block = c(rep("EHF Euro Women", 5), "Olympic Games Women", "Olympic Games Women", "World Championship Women"),
    match_date = as.Date(c("2026-12-04", "2026-12-05", "2026-12-12", "2026-12-13", "2026-12-06", "2024-07-25", "2024-07-27", "2025-12-01")),
    home_team = c("Poland", "Hungary", "Poland", "Romania", "Hungary", "France", "Spain", "Poland"),
    away_team = c("Hungary", "Romania", "Romania", "Hungary", "Slovenia", "Spain", "France", "Hungary")
  )
  # Two hosts meeting in Romania favour Romania, the listed away side; a host
  # playing in a co-host's country is on neutral ground; a single host is at
  # home in every match; a championship without hosts is neutral.
  expect_equal(ih_venue(m, hosts, venues, no_qualifier_venues()), c(1L, -1L, -1L, 0L, 0L, 1L, -1L, 0L))

  unplaced <- m[1, ]
  unplaced$match_date <- as.Date("2026-12-07")
  expect_warning(v <- ih_venue(unplaced, hosts, venues, no_qualifier_venues()), "no venue")
  expect_equal(v, 0L)
})

test_that("qualifiers: home and away at home; a single-venue tournament at home only for its host", {
  hosts <- data.frame(competition = character(), sex = character(), edition = integer(), host = character())
  rr <- function(teams, first, block, competition, edition) {
    p <- t(utils::combn(teams, 2))
    data.frame(
      sex = "female", competition = competition, kind = "tournament", edition = edition, block = block,
      match_date = as.Date(first) + rep(0:2, length.out = nrow(p)), home_team = p[, 1], away_team = p[, 2]
    )
  }
  # A home-and-away group: two matchdays in October, the return legs in March.
  ha <- data.frame(
    sex = "female", competition = "EURO", kind = "tournament", edition = 2026L,
    block = "EHF Euro Women - Qualification - Second stage",
    match_date = as.Date(c("2024-10-16", "2024-10-16", "2024-10-20", "2024-10-20", "2025-03-05", "2025-03-05", "2025-03-09", "2025-03-09")),
    home_team = c("Iceland", "Sweden", "Portugal", "Iceland", "Sweden", "Iceland", "Iceland", "Portugal"),
    away_team = c("Sweden", "Portugal", "Iceland", "Bosnia", "Iceland", "Portugal", "Sweden", "Bosnia")
  )
  oqt <- rr(c("Great Britain", "Hungary", "Japan", "Sweden"), "2024-04-11", "Olympic Games Women - Qualification", "OG", 2024L)
  first <- rr(c("Estonia", "Finland", "Israel"), "2024-10-24", "World Championship Women - Qualification - First stage", "WC", 2025L)
  amer <- rbind(
    rr(c("Greenland", "Mexico", "Puerto Rico", "USA"), "2021-08-22", "World Championship Women - Qualification", "WC", 2021L),
    data.frame(
      sex = "female", competition = "WC", kind = "tournament", edition = 2021L,
      block = "World Championship Women - Qualification - Play Offs",
      match_date = as.Date(c("2021-08-25", "2021-08-26")), home_team = c("Mexico", "Greenland"), away_team = c("USA", "Puerto Rico")
    )
  )
  legs <- data.frame(
    sex = "female", competition = "WC", kind = "tournament", edition = 2025L,
    block = "World Championship Women - Qualification - Play Offs",
    match_date = as.Date(c("2025-04-09", "2025-04-13")), home_team = c("Iceland", "Israel"), away_team = c("Israel", "Iceland")
  )
  m <- rbind(ha, oqt, first, amer, legs)
  qv <- data.frame(
    competition = c("OG", "WC"), sex = "female", edition = c(2024L, 2021L),
    first = as.Date(c("2024-04-11", "2021-08-22")), last = as.Date(c("2024-04-14", "2021-08-26")),
    teams = c("Great Britain; Hungary; Japan; Sweden", "Greenland; Mexico; Puerto Rico; USA"),
    venue_city = c("Debrecen", "Mexico City"), venue_country = c("Hungary", "Mexico"), source = "test"
  )
  side <- function(x, country) as.integer((x$home_team == country) - (x$away_team == country))
  expect_equal(
    ih_venue(m, hosts, ih_championship_venues()[0, ], qv),
    c(rep(1L, 8), side(oqt, "Hungary"), rep(0L, 3), side(amer, "Mexico"), 1L, 1L)
  )
})

test_that("a single-venue tournament is a short single round robin; a group short of its return legs is not", {
  base <- data.frame(sex = "female", competition = "EURO", kind = "tournament", edition = 2020L)
  # The first two matchdays of a home-and-away group whose return legs were
  # never played: four teams, four matches, no pair twice; not a round robin.
  fragment <- data.frame(base,
    block = "European Championship Women - Qualification - Second round",
    match_date = as.Date(c("2019-09-25", "2019-09-25", "2019-09-29", "2019-09-29")),
    home_team = c("A", "C", "B", "D"), away_team = c("B", "D", "C", "A")
  )
  p <- t(utils::combn(c("E", "F", "G", "H"), 2))
  tour <- data.frame(base,
    block = "European Championship Women - Qualification - First round",
    match_date = as.Date("2019-05-31") + c(0, 0, 1, 1, 2, 2), home_team = p[, 1], away_team = p[, 2]
  )
  late <- data.frame(base,
    block = "European Championship Women - Qualification - First round - 5th-8th places",
    match_date = as.Date(c("2019-06-03", "2019-07-20")), home_team = c("E", "F"), away_team = c("H", "G")
  )
  # A three-team home-and-away group whose first legs fell in one week.
  trio <- data.frame(base,
    block = "European Championship Women - Qualification - Third round",
    match_date = as.Date(c("2019-10-02", "2019-10-04", "2019-10-06", "2020-03-04", "2020-03-06", "2020-03-08")),
    home_team = c("J", "K", "L", "K", "L", "J"), away_team = c("K", "L", "J", "J", "K", "L")
  )
  expect_equal(
    ih_single_venue(rbind(fragment, tour, late, trio)),
    c(rep(FALSE, 4), rep(TRUE, 6), TRUE, FALSE, rep(FALSE, 6))
  )
})

test_that("the championship being forecast trains on its own schedule's venues, as the simulator prices them", {
  spec <- publish_fixture()$spec
  spec$prelim$host[spec$prelim$group == "A"] <- "T01"
  spec$main_host[["I"]] <- "T01"
  spec$knockout$host <- "T05"
  m <- data.frame(
    sex = "female", competition = spec$competition, kind = "tournament", edition = spec$edition, block = "",
    match_date = as.Date(c("2026-12-03", "2026-12-04", "2026-12-10", "2026-12-10", "2026-12-18", "2026-10-01")),
    home_team = c("T02", "T05", "T01", "T06", "T05", "T01"), away_team = c("T01", "T06", "T05", "T09", "T01", "T02"),
    home_score = 30L, away_score = 25L, venue = 9L
  )
  m$competition[6] <- "FRI"
  # T01 hosts group A and main-round group I, T05 the final weekend.
  expect_equal(ih_with_spec_venues(m, spec)$venue, c(-1L, 0L, 1L, 0L, 1L, 9L))
})

test_that("the venue files: each match or tournament once, venues in host countries, every row sourced", {
  cv <- ih_championship_venues()
  expect_named(cv, c("competition", "sex", "edition", "match_date", "home_team", "away_team", "venue_city", "venue_country", "source"))
  expect_false(anyNA(cv$match_date))
  expect_false(anyDuplicated(cv[, c("competition", "sex", "edition", "match_date", "home_team", "away_team")]) > 0)
  h <- ih_hosts()
  host_of <- function(team) paste(cv$competition, cv$sex, cv$edition, team) %in% paste(h$competition, h$sex, h$edition, h$host)
  expect_true(all(host_of(cv$venue_country)))
  expect_true(all(host_of(cv$home_team) | host_of(cv$away_team)))
  expect_true(all(nzchar(cv$venue_city) & nzchar(cv$source)))

  qv <- ih_qualifier_venues()
  expect_named(qv, c("competition", "sex", "edition", "first", "last", "teams", "venue_city", "venue_country", "source"))
  # The longest, the Asian Olympic qualification of November 2015 in Doha,
  # ran 13 days with its placement rounds.
  expect_true(all(qv$first <= qv$last & qv$last - qv$first <= 21))
  expect_true(all(lengths(strsplit(qv$teams, "; ", fixed = TRUE)) >= 3))
  expect_true(all(nzchar(qv$venue_city) & nzchar(qv$venue_country) & nzchar(qv$source)))
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
  expect_identical(fit$unusable, character())
  expect_identical(ih_fit_check(fit), character())
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
test_that("a fit that has gone wrong is refused before anything is published", {
  ok <- list(
    mode_status = c(margin = 0L, total = 0L),
    sigma_m = rep(5.3, 20), sigma_t = rep(6, 20), hfa = rep(1, 20), level = rep(54, 20),
    m_state = matrix(0, 20, 3), t_state = matrix(0, 20, 3)
  )
  expect_identical(ih_fit_check(ok), character())

  no_mode <- ok
  no_mode$mode_status[["total"]] <- 1L
  expect_match(ih_fit_check(no_mode), "mode", all = FALSE)

  nan <- ok
  nan$sigma_t[3] <- NaN
  nan$m_state[2, 2] <- Inf
  expect_match(ih_fit_check(nan), "non-finite draws in sigma_total, margin_state")

  wide <- ok
  wide$sigma_m <- rep(40, 20)
  wide$level <- rep(120, 20)
  p <- ih_fit_check(wide)
  expect_length(p, 2L)
  expect_match(p[1], "observation sd of the margin 40.00 outside \\[2, 15\\]")
  expect_match(p[2], "mean total 120.00 outside \\[30, 80\\]")

  stuck <- ok
  stuck$unusable <- "total sd: b = 9.08e+09"
  expect_match(ih_fit_check(stuck), "unusable after a restart at the mode: total sd: b = 9.08e\\+09")
})

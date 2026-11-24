# EURO 2028 qualifying: draw inputs, Nations League quarter-final
# placeholders, reserved host places and play-offs, and the draw itself.

e28_facts_dir <- function() testthat::test_path("..", "..", "data", "nt", "euro2028")
e28_groups_csv <- function() testthat::test_path("fixtures", "euro2028", "groups.csv")

# A fit over every team the draw can contain, all equal unless given.
e28_fit <- function(theta = NULL, n_draws = 4L, ...) {
  teams <- nt_euro2028_facts(e28_facts_dir())$seeding$team
  base <- stats::setNames(rep(0, length(teams)), teams)
  if (!is.null(theta)) base[names(theta)] <- theta
  nt_test_fit(base, n_draws = n_draws, ...)
}

test_that("UEFA spellings map to the results file's names", {
  expect_identical(
    nt_uefa_to_martj42(c("Türkiye", "Czechia", "Bosnia-Herzegovina", "Iceland", " Czechia ")),
    c("Turkey", "Czech Republic", "Bosnia and Herzegovina", "Iceland", "Czech Republic")
  )
})

test_that("the seeding holds the 54 entrants in five pots, ranked 1 to 54", {
  s <- nt_euro2028_facts(e28_facts_dir())$seeding
  expect_equal(nrow(s), 54L)
  expect_equal(unname(as.vector(table(s$pot))), c(12L, 12L, 12L, 12L, 6L))
  expect_equal(sort(s$nl_rank), 1:54)
  expect_true(all(diff(s$nl_rank[order(s$pot, s$nl_rank)]) == 1L))
  # Every Euro 2024 qualifier (martj42 names) plus Germany, the 2024 host.
  e24 <- unlist(nt_e24_fixture()$groups, use.names = FALSE)
  expect_setequal(s$team, c(e24, "Germany"))
  expect_setequal(s$team[s$host], c("England", "Republic of Ireland", "Scotland", "Wales"))
  winners <- s$team[s$nl_group_position == 1]
  expect_length(winners, 14L)
  expect_true(all(c("Iceland", "Northern Ireland", "Malta", "Azerbaijan") %in% winners))
  expect_equal(sum(s$nl_knockout == "QF"), 8L)
  expect_equal(sum(s$nl_knockout %in% c("AB_PO", "BC_PO")), 16L)
})

test_that("the quarter-finals pair the eight League A qualifiers", {
  q <- nt_euro2028_facts(e28_facts_dir())$quarter_finals
  expect_equal(q$winner_slot, paste0("QF", 1:4, " winner"))
  expect_equal(q$defeated_slot, paste0("QF", 1:4, " defeated"))
  s <- nt_euro2028_facts(e28_facts_dir())$seeding
  expect_setequal(c(q$first_leg_home, q$second_leg_home), s$team[s$nl_knockout == "QF"])
})

test_that("the draw loader returns slot-labelled groups and generated fixtures", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  expect_named(d$groups, LETTERS[1:12])
  expect_equal(unname(lengths(d$groups)), c(rep(4L, 6), rep(5L, 6)))
  expect_identical(d$groups$A[1], "QF1 winner")
  expect_identical(d$groups$G[1:2], c("QF1 defeated", "Turkey"))
  expect_true("Czech Republic" %in% d$groups$H)
  expect_length(d$teams, 54L)
  expect_false(any(grepl("^QF", d$teams)))
  # Double round robin per group; Belarus, Ukraine and Israel host on neutral ground.
  expect_equal(nrow(d$fixtures), 6 * 12 + 6 * 20)
  expect_true(all(d$fixtures$venue[d$fixtures$home_team %in% c("Belarus", "Ukraine", "Israel")] == 0L))
  expect_true(all(d$fixtures$venue[!d$fixtures$home_team %in% c("Belarus", "Ukraine", "Israel")] == 1L))
  expect_true(all(is.na(d$fixtures$home_score)))
})

test_that("the draw loader asserts every drawn team is in the fit", {
  fit <- e28_fit()
  expect_silent(nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir(), fit = fit))
  fit$teams <- setdiff(fit$teams, c("Turkey", "Iceland"))
  expect_error(
    nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir(), fit = fit),
    "Turkey.*Iceland|Iceland.*Turkey"
  )
})

test_that("placeholders may name the quarter-final pairing instead of its number", {
  g <- utils::read.csv(e28_groups_csv(), stringsAsFactors = FALSE, encoding = "UTF-8")
  g$team[g$team == "QF1 winner"] <- "Winner Spain/France"
  g$team[g$team == "QF1 defeated"] <- "France / Spain defeated team"
  g$team[g$team == "QF2 winner"] <- "QF2 winners"
  g$team[g$team == "QF2 defeated"] <- "Norway/England loser"
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(g, path, row.names = FALSE, fileEncoding = "UTF-8")
  d <- nt_read_euro2028_draw(path, facts_dir = e28_facts_dir())
  expect_identical(d$groups$A[1], "QF1 winner")
  expect_identical(d$groups$G[1], "QF1 defeated")
  expect_identical(d$groups$B[1], "QF2 winner")
  expect_identical(d$groups$H[1], "QF2 defeated")
})

test_that("the draw loader refuses a missing, repeated or unknown team", {
  g <- utils::read.csv(e28_groups_csv(), stringsAsFactors = FALSE, encoding = "UTF-8")
  write_g <- function(x) {
    path <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
    utils::write.csv(x, path, row.names = FALSE, fileEncoding = "UTF-8")
    path
  }
  dup <- g
  dup$team[dup$team == "Malta"] <- "Iceland"
  expect_error(nt_read_euro2028_draw(write_g(dup), facts_dir = e28_facts_dir()), "Iceland")
  unknown <- g
  unknown$team[unknown$team == "Malta"] <- "Atlantis"
  expect_error(nt_read_euro2028_draw(write_g(unknown), facts_dir = e28_facts_dir()), "Atlantis")
  direct <- g
  direct$team[direct$team == "QF1 winner"] <- "Spain"
  expect_error(nt_read_euro2028_draw(write_g(direct), facts_dir = e28_facts_dir()), "QF1")
  wrong_pot <- g
  wrong_pot$pot[wrong_pot$team == "Iceland"] <- 2L
  expect_error(nt_read_euro2028_draw(write_g(wrong_pot), facts_dir = e28_facts_dir()), "Iceland")
})

test_that("a fixture list's venue may be written as home/neutral or 1/0", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  fx <- d$fixtures
  fx$match_date <- "2027-09-04"
  fx$venue <- ifelse(fx$venue == 1L, "home", "neutral")
  fx$venue[fx$home_team == "Iceland"] <- "neutral"
  fx$home_team[fx$home_team == "Turkey"] <- "Türkiye"
  fx$away_team[fx$away_team == "Turkey"] <- "Türkiye"
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(fx, path, row.names = FALSE, fileEncoding = "UTF-8")
  d2 <- nt_read_euro2028_draw(e28_groups_csv(), fixtures = path, facts_dir = e28_facts_dir())
  expect_true(all(d2$fixtures$venue[d2$fixtures$home_team == "Iceland"] == 0L))
  expect_true(all(d2$fixtures$venue[d2$fixtures$home_team == "Turkey"] == 1L))
  fx$venue <- ifelse(fx$venue == "home", 1L, 0L)
  utils::write.csv(fx, path, row.names = FALSE, fileEncoding = "UTF-8")
  d3 <- nt_read_euro2028_draw(e28_groups_csv(), fixtures = path, facts_dir = e28_facts_dir())
  expect_identical(d3$fixtures$venue, d2$fixtures$venue)
  fx$venue[1] <- "away"
  utils::write.csv(fx, path, row.names = FALSE, fileEncoding = "UTF-8")
  expect_error(nt_read_euro2028_draw(e28_groups_csv(), fixtures = path, facts_dir = e28_facts_dir()), "venue")
})

test_that("quarter-final placeholders take each simulation's winner and defeated team", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  fit <- e28_fit(c(France = 5), n_draws = 3L, mu = 0.1)
  s <- nt_simulate_qualifying(
    fit, d$groups, d$fixtures, n_sims = 300L, quarter_finals = d$quarter_finals, seed = 1L
  )
  expect_true(all(s$qf_winner[, "QF1"] == "France"))
  expect_identical(s$positions[, "France"], s$slot_positions[, "QF1 winner"])
  expect_identical(s$positions[, "Spain"], s$slot_positions[, "QF1 defeated"])
  expect_gt(s$teams$p_winner[s$teams$team == "France"], 0.99)
  expect_identical(s$teams$group[s$teams$team == "France"], "A/G")
  # The three equal ties go either way.
  share <- colMeans(sweep(s$qf_winner[, 2:4], 2, d$quarter_finals$first_leg_home[2:4], `==`))
  expect_true(all(share > 0.35 & share < 0.65))
  for (q in 2:4) {
    a <- d$quarter_finals$first_leg_home[q]
    b <- d$quarter_finals$second_leg_home[q]
    ws <- d$quarter_finals$winner_slot[q]
    a_won <- s$qf_winner[, q] == a
    expect_identical(s$positions[a_won, a], s$slot_positions[a_won, ws])
    expect_identical(s$positions[!a_won, b], s$slot_positions[!a_won, ws])
  }
})

test_that("a simulation with placeholders reports every entrant once and fills the places", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  withr::local_seed(3)
  th <- stats::setNames(stats::rnorm(54, 0, 0.4), d$teams)
  fit <- e28_fit(th, n_draws = 5L, mu = 0.2, home_for = 0.15, home_against = -0.2)
  s <- nt_simulate_qualifying(
    fit, d$groups, d$fixtures, n_sims = 400L, quarter_finals = d$quarter_finals, seed = 4L
  )
  expect_setequal(colnames(s$positions), d$teams)
  expect_setequal(s$teams$team, d$teams)
  expect_equal(nrow(s$slots), 54L)
  expect_equal(sum(s$teams$p_winner), 12)
  expect_equal(sum(s$teams$p_runner_up_direct), 8)
  expect_equal(sum(s$teams$p_runner_up_other), 4)
  expect_equal(sum(s$slots$p_qualify_direct), 20)
  expect_true(all(s$runner_up %in% d$teams))
  expect_equal(range(s$draw), c(1L, 5L))
  for (nm in c("points", "gd", "gf", "away_gf", "won", "away_won", "tiebreak")) {
    expect_identical(dim(s$overall[[nm]]), c(400L, 54L), info = nm)
  }
  # Runner-up per group is the team placed second.
  for (g in names(d$groups)) {
    expect_identical(s$positions[cbind(seq_len(400), match(s$runner_up[, g], colnames(s$positions)))], rep(2L, 400))
  }
})

test_that("placeholders without their quarter-final, or a quarter-finalist drawn directly, are refused", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  fit <- e28_fit()
  half <- d$quarter_finals
  half$defeated_slot[1] <- "QF9 defeated"
  expect_error(
    nt_simulate_qualifying(fit, d$groups, d$fixtures, n_sims = 2L, quarter_finals = half),
    "QF1"
  )
  expect_error(
    nt_simulate_qualifying(fit, d$groups, d$fixtures, n_sims = 2L),
    "QF1 winner"
  )
})

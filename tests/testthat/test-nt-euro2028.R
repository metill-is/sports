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

# ---- Reserved host places and play-offs (Articles 14.04, 16, 17) ----------

e28_seeding <- function() nt_euro2028_facts(e28_facts_dir())$seeding

test_that("with every host through directly, the four lowest runners-up meet four Nations League winners", {
  s <- e28_seeding()
  direct <- c(
    "England", "Portugal", "France", "Germany", "Spain", "Netherlands", "Belgium", "Italy",
    "Croatia", "Denmark", "Republic of Ireland", "Scotland", "Wales", "Switzerland", "Austria",
    "Turkey", "Serbia", "Norway", "Sweden", "Poland"
  )
  ru_out <- c("Ukraine", "Czech Republic", "Hungary", "Greece")
  f <- .nt_euro_playoff_field(direct, ru_out, character(), s)
  expect_identical(f$reserved, character())
  expect_identical(f$format, "ties")
  expect_identical(f$runners_up, ru_out)
  # A/B/C group winners by rank, skipping those already through: NI (20),
  # Montenegro (33), Albania (34), Iceland (35).
  expect_identical(f$nations_league, c("Northern Ireland", "Montenegro", "Albania", "Iceland"))
})

test_that("one host using a reserved place makes it 12 teams in three paths", {
  s <- e28_seeding()
  direct <- c(
    "England", "Portugal", "France", "Germany", "Spain", "Netherlands", "Belgium", "Italy",
    "Croatia", "Denmark", "Republic of Ireland", "Wales", "Switzerland", "Austria",
    "Turkey", "Serbia", "Norway", "Sweden", "Poland", "Montenegro"
  )
  ru_out <- c("Ukraine", "Czech Republic", "Hungary", "Greece")
  f <- .nt_euro_playoff_field(direct, ru_out, "Scotland", s)
  expect_identical(f$reserved, "Scotland")
  expect_identical(f$format, "paths")
  expect_length(c(f$runners_up, f$nations_league), 12L)
  # Eight Nations League places: the five A/B/C winners left, then the best
  # League D winner, then the best-ranked teams not yet through.
  expect_identical(
    f$nations_league,
    c("Northern Ireland", "Albania", "Iceland", "Slovakia", "Malta",
      "Bosnia and Herzegovina", "Slovenia", "Kosovo")[order(
      s$nl_rank[match(c("Northern Ireland", "Albania", "Iceland", "Slovakia", "Malta",
        "Bosnia and Herzegovina", "Slovenia", "Kosovo"), s$team)])]
  )
})

test_that("a host among the lowest runners-up takes its reserved place instead of a play-off place", {
  s <- e28_seeding()
  direct <- c(
    "England", "Portugal", "France", "Germany", "Spain", "Netherlands", "Belgium", "Italy",
    "Croatia", "Denmark", "Republic of Ireland", "Switzerland", "Austria", "Iceland",
    "Turkey", "Serbia", "Norway", "Sweden", "Poland", "Albania"
  )
  ru_out <- c("Ukraine", "Wales", "Hungary", "Greece")
  f <- .nt_euro_playoff_field(direct, ru_out, c("Wales", "Scotland"), s)
  expect_identical(f$reserved, c("Wales", "Scotland"))
  expect_identical(f$format, "paths")
  expect_identical(f$runners_up, c("Ukraine", "Hungary", "Greece"))
  # Three A/B/C winners left (NI, Montenegro, Slovakia), Malta, then the
  # best-ranked team not yet through: Czechia (16).
  expect_identical(f$nations_league, c("Czech Republic", "Northern Ireland", "Montenegro", "Slovakia", "Malta"))
})

test_that("with three hosts out, the two best-ranked take the places and the third may play off", {
  s <- e28_seeding()
  direct <- c(
    "England", "Portugal", "France", "Germany", "Spain", "Netherlands", "Belgium", "Italy",
    "Croatia", "Denmark", "Iceland", "Switzerland", "Austria", "Albania",
    "Turkey", "Serbia", "Norway", "Sweden", "Poland", "Montenegro"
  )
  ru_out <- c("Ukraine", "Scotland", "Hungary", "Greece")
  f <- .nt_euro_playoff_field(direct, ru_out, c("Republic of Ireland", "Wales", "Scotland"), s)
  expect_identical(f$reserved, c("Republic of Ireland", "Wales"))
  expect_identical(f$runners_up, ru_out)
  expect_length(f$nations_league, 4L)
})

test_that("the play-off draw keeps hosts apart and puts each pot in every path", {
  withr::local_seed(2)
  part <- c("Ukraine", "Scotland", "Hungary", "Wales", "Northern Ireland", "Albania",
    "Iceland", "Slovakia", "Malta", "Kosovo", "Slovenia", "Greece")
  seen_ni_with_host <- 0L
  for (i in 1:200) {
    b <- .nt_euro_playoff_draw(part, "paths", hosts = c("Scotland", "Wales"), northern_ireland = "Northern Ireland")
    expect_identical(dim(b), c(3L, 4L))
    for (q in 1:4) expect_setequal(b[, q], part[(3 * q - 2):(3 * q)])
    host_path <- which(apply(b, 1, function(p) any(p %in% c("Scotland", "Wales"))))
    expect_length(host_path, 2L)
    ni_path <- which(apply(b, 1, function(p) "Northern Ireland" %in% p))
    seen_ni_with_host <- seen_ni_with_host + (ni_path %in% host_path)
  }
  expect_equal(seen_ni_with_host, 0L)
  # Ties: seeded against unseeded.
  t8 <- .nt_euro_playoff_draw(part[1:8], "ties", hosts = character(), northern_ireland = "Northern Ireland")
  expect_identical(dim(t8), c(4L, 2L))
  expect_setequal(t8[, 1], part[1:4])
  expect_setequal(t8[, 2], part[5:8])
})

test_that("Northern Ireland may share a path with a host only when nothing else is possible", {
  withr::local_seed(3)
  part <- c("Scotland", "Ukraine", "Wales", "Hungary", "Northern Ireland", "Albania", "Iceland", "Slovakia")
  # Two paths, two hosts in different paths: Northern Ireland must join one of them.
  b <- .nt_euro_playoff_draw(part, "paths", hosts = c("Scotland", "Wales"), northern_ireland = "Northern Ireland")
  expect_identical(dim(b), c(2L, 4L))
  expect_false(any(apply(b, 1, function(p) all(c("Scotland", "Wales") %in% p))))
})

test_that("simulated play-offs fill exactly 24 places, seeded sides hosting", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  withr::local_seed(6)
  th <- stats::setNames(stats::rnorm(54, 0, 0.3), d$teams)
  fit <- e28_fit(th, n_draws = 5L, mu = 0.1, home_for = 2.5, home_against = -2.5)
  out <- nt_simulate_euro2028(fit, d, n_sims = 300L, seed = 7L)
  expect_true(all(rowSums(out$qualified) == 24L))
  expect_equal(sum(out$teams$p_qualify), 24)
  expect_true(all(out$teams$p_qualify >= out$teams$p_qualify_direct))
  expect_equal(sum(out$teams$p_host_place), mean(out$n_host_places))
  expect_true(all(out$teams$p_host_place[!out$teams$team %in% d$hosts] == 0))
  expect_equal(sum(out$teams$p_playoff), mean(ifelse(out$n_host_places == 1L, 12, 8)))
  # A home side all but always wins, so each path goes to its pot 1 side.
  paths <- out$n_host_places > 0L
  expect_gt(sum(paths), 0L)
  won <- out$playoff_winner & !is.na(out$playoff_seed)
  per <- ifelse(out$n_host_places == 1L, 3L, 2L)
  for (i in which(paths)) expect_true(all(out$playoff_seed[i, won[i, ]] <= per[i]), info = paste("simulation", i))
})

# ---- The draw ---------------------------------------------------------------

e28_facts <- function() nt_euro2028_facts(e28_facts_dir())

test_that("the fixture draw keeps every published condition", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  expect_identical(nt_check_uefa_draw(d$groups, e28_facts()), character())
})

test_that("draws that break a condition are reported", {
  f <- e28_facts()
  g <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())$groups
  swap <- function(g, a, b) {
    ga <- names(g)[vapply(g, function(x) a %in% x, logical(1))]
    gb <- names(g)[vapply(g, function(x) b %in% x, logical(1))]
    g[[ga]][g[[ga]] == a] <- b
    g[[gb]][g[[gb]] == b] <- a
    g
  }
  expect_match(nt_check_uefa_draw(swap(g, "Romania", "Ukraine"), f), "Ukraine|Belarus", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "Kosovo", "Poland"), f), "Kosovo", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "Kosovo", "Scotland"), f), "host", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "QF4 winner", "Croatia"), f), "QF4 winner", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "Estonia", "Finland"), f), "group of five|play-off", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "Luxembourg", "Latvia"), f), "winter", all = FALSE)
  expect_match(nt_check_uefa_draw(swap(g, "Liechtenstein", "Malta"), f), "travel", all = FALSE)
})

test_that("simulated draws keep every condition", {
  f <- e28_facts()
  draws <- nt_simulate_uefa_draw(f, n_draws = 25L, seed = 1L)
  expect_length(draws, 25L)
  for (i in seq_along(draws)) {
    expect_identical(nt_check_uefa_draw(draws[[i]], f), character(), info = paste("draw", i))
  }
  iceland <- vapply(draws, function(g) names(g)[vapply(g, function(x) "Iceland" %in% x, logical(1))], "")
  expect_gt(length(unique(iceland)), 3L)
})

test_that("each drawn team goes to the first group that keeps the rest of the draw possible", {
  f <- e28_facts()
  pot1 <- c("Greece", "Italy", "Denmark", "Croatia", paste0("QF", 1:4, " defeated"), paste0("QF", 1:4, " winner"))
  g <- nt_simulate_uefa_draw(f, draw_order = list(pot1), seed = 2L)[[1]]
  got <- vapply(g, `[`, "", 1)
  # A and B take Greece and Italy; C-F must be left for the four winners, so
  # Denmark, Croatia and the defeated teams skip to G-L.
  expect_identical(unname(got), c(
    "Greece", "Italy", paste0("QF", 1:4, " winner"), "Denmark", "Croatia",
    paste0("QF", 1:4, " defeated")
  ))
})

test_that("a prohibited clash sends the drawn team to the next available group", {
  f <- e28_facts()
  pot1 <- c(paste0("QF", 1:4, " winner"), "Greece", "Italy", paste0("QF", 1:4, " defeated"), "Denmark", "Croatia")
  s <- f$seeding
  pot2 <- c("Serbia", "Bosnia and Herzegovina", setdiff(s$team[s$pot == 2], c("Serbia", "Bosnia and Herzegovina")))
  pot3 <- c("Kosovo", setdiff(s$team[s$pot == 3], "Kosovo"))
  g <- nt_simulate_uefa_draw(f, draw_order = list(pot1, pot2, pot3), seed = 3L)[[1]]
  where <- function(t) names(g)[vapply(g, function(x) t %in% x, logical(1))]
  expect_identical(where("Serbia"), "A")
  expect_identical(where("Bosnia and Herzegovina"), "B")
  expect_identical(where("Kosovo"), "C")
})

test_that("the loader warns when the typed draw breaks a condition", {
  g <- utils::read.csv(e28_groups_csv(), stringsAsFactors = FALSE, encoding = "UTF-8")
  a <- which(g$team == "Kosovo")
  b <- which(g$team == "Poland")
  g$group[c(a, b)] <- g$group[c(b, a)]
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(g, path, row.names = FALSE, fileEncoding = "UTF-8")
  expect_warning(nt_read_euro2028_draw(path, facts_dir = e28_facts_dir()), "Kosovo")
})

test_that("a simulated draw becomes a draw object like the loader's", {
  f <- e28_facts()
  csv <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  d <- nt_euro2028_draw(csv$groups, facts = f)
  expect_identical(d$groups, csv$groups)
  expect_identical(d$fixtures, csv$fixtures)
  expect_identical(d$teams, csv$teams)
  sim <- nt_simulate_uefa_draw(f, n_draws = 1L, seed = 4L)[[1]]
  expect_silent(d2 <- nt_euro2028_draw(sim, facts = f, fit = e28_fit()))
  expect_equal(nrow(d2$fixtures), 192L)
})

test_that("the two readings of Article 15 d) are compared simulation by simulation", {
  f <- e28_facts()
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  # Group K plays out the four-way tie on which the readings disagree.
  k <- d$groups$K
  tie <- nt_four_way_tie()
  map <- stats::setNames(k, c("A", "B", "C", "D", "E"))
  fx <- d$fixtures
  key <- paste(fx$home_team, fx$away_team)
  played <- paste(map[tie$home_team], map[tie$away_team])
  fx$home_score[match(played, key)] <- tie$home_score
  fx$away_score[match(played, key)] <- tie$away_score
  d$fixtures <- fx
  fit <- e28_fit(n_draws = 3L, mu = 0.1)
  a <- nt_simulate_euro2028(fit, d, n_sims = 40L, seed = 9L)
  b <- nt_simulate_euro2028(fit, d, n_sims = 40L, h2h_reapply = "once", seed = 9L)
  cmp <- nt_compare_h2h_readings(a, b)
  expect_equal(cmp$n_sims, 40L)
  expect_equal(cmp$any_position, 1)
  expect_equal(cmp$by_group[["K"]], 1)
  expect_true(all(cmp$by_group[setdiff(names(cmp$by_group), "K")] == 0))
  # C (Albania) and D (Latvia) swap second and third place.
  expect_true(all(a$qualifying$positions[, "Albania"] == 2L))
  expect_true(all(b$qualifying$positions[, "Albania"] == 3L))
  same <- nt_compare_h2h_readings(a, a)
  expect_equal(same$any_position, 0)
  expect_equal(same$direct, 0)
  expect_equal(same$field, 0)
})

test_that("slots carry the total qualification odds of whoever fills them", {
  d <- nt_read_euro2028_draw(e28_groups_csv(), facts_dir = e28_facts_dir())
  withr::local_seed(12)
  fit <- e28_fit(stats::setNames(stats::rnorm(54, 0, 0.4), d$teams), n_draws = 4L, mu = 0.1)
  out <- nt_simulate_euro2028(fit, d, n_sims = 200L, seed = 13L)
  sl <- out$slots
  tm <- out$teams
  fixed <- intersect(sl$slot, tm$team)
  for (col in c("p_host_place", "p_playoff", "p_qualify")) {
    expect_equal(sl[[col]][match(fixed, sl$slot)], tm[[col]][match(fixed, tm$team)], info = col)
    for (q in seq_len(nrow(d$quarter_finals))) {
      qf <- d$quarter_finals[q, ]
      expect_equal(
        sum(sl[[col]][sl$slot %in% c(qf$winner_slot, qf$defeated_slot)]),
        sum(tm[[col]][tm$team %in% c(qf$first_leg_home, qf$second_leg_home)]),
        info = paste(col, qf$winner_slot)
      )
    }
  }
  expect_equal(sum(sl$p_qualify), 24)
})

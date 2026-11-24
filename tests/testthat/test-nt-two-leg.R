test_that("the six ways a two-legged tie ends sum to one", {
  p <- nt_two_leg_win_prob(1.1, 0.8, 0.6, 1.5, shootout_p = 0.4)
  parts <- c("a_regulation", "a_extra_time", "a_shootout", "b_regulation", "b_extra_time", "b_shootout")
  expect_equal(Reduce(`+`, p[parts]), 1)
  expect_equal(p$a_wins, p$a_regulation + p$a_extra_time + p$a_shootout)
})

test_that("a one-goal first leg and nothing after is known by hand", {
  # A scores in leg 1 with probability 1 - e^-1; otherwise 0-0 throughout and
  # a coin-toss shoot-out.
  p <- nt_two_leg_win_prob(1, 0, 0, 0)
  expect_equal(p$a_wins, 1 - 0.5 * exp(-1))
  expect_equal(p$a_shootout, 0.5 * exp(-1))
  expect_equal(p$b_regulation, 0)
})

test_that("with an empty first leg the tie is the single knockout match of the second", {
  for (sp in c(0.5, 0.3)) {
    expect_equal(
      nt_two_leg_win_prob(0, 0, 1.7, 0.9, shootout_p = sp)$a_wins,
      nt_knockout_win_prob(1.7, 0.9, shootout_p = sp)
    )
  }
})

test_that("away goals do not count: only aggregate rates decide the 180 minutes", {
  a <- nt_two_leg_win_prob(0.7, 1.2, 1.6, 0.5)
  b <- nt_two_leg_win_prob(1.6, 0.5, 0.7, 1.2)
  expect_equal(a$a_regulation, b$a_regulation)
  expect_equal(a$b_regulation, b$b_regulation)
  # Extra time is played at the second leg's venue, so it differs.
  expect_false(isTRUE(all.equal(a$a_extra_time, b$a_extra_time)))
})

test_that("the two sides' chances sum to one when the roles are swapped", {
  a <- nt_two_leg_win_prob(1.3, 0.9, 0.8, 1.6, shootout_p = 0.45)
  b <- nt_two_leg_win_prob(0.9, 1.3, 1.6, 0.8, shootout_p = 0.55)
  expect_equal(a$a_wins + b$a_wins, 1)
  expect_equal(a$a_extra_time, b$b_extra_time)
})

test_that("a lead carried into the second leg shifts the aggregate", {
  # Two goals up with the second leg to come: B must win it by two to force
  # extra time.
  p <- nt_two_leg_win_prob(0, 0, 1.2, 1.4, lead = 2L)
  d <- .nt_poisson_compare(1.2, 1.4, lead = 2L)
  expect_equal(p$a_regulation, d$win)
  et <- .nt_poisson_compare(1.2 / 3, 1.4 / 3)
  expect_equal(p$a_wins, d$win + d$draw * (et$win + et$draw * 0.5))
  by_hand <- sum(stats::dpois(0:40, 1.4) * stats::ppois(0:40 - 2L, 1.2, lower.tail = FALSE))
  expect_equal(d$win, by_hand)
  expect_equal(nt_two_leg_win_prob(0, 0, 1.2, 1.4, lead = -1L)$a_wins +
    nt_two_leg_win_prob(0, 0, 1.4, 1.2, lead = 1L, shootout_p = 0.5)$a_wins, 1)
})

test_that("rates given as draws x ties matrices keep their shape", {
  m <- matrix(c(1, 1.2, 0.8, 1.5), 2, 2)
  p <- nt_two_leg_win_prob(m, t(m), m * 0.9, t(m) * 1.1)
  expect_equal(dim(p$a_wins), c(2L, 2L))
})

test_that("simulated two-legged ties match the closed form", {
  withr::local_seed(17)
  n <- 200000L
  sim <- nt_simulate_two_leg(rep(0.9, n), rep(1.3, n), rep(1.5, n), rep(0.7, n), shootout_p = 0.4)
  cf <- nt_two_leg_win_prob(0.9, 1.3, 1.5, 0.7, shootout_p = 0.4)
  se <- sqrt(0.25 / n)
  expect_lt(abs(mean(sim$a_wins) - cf$a_wins), 4 * se)
  expect_lt(abs(mean(sim$decided == 1L) - (cf$a_regulation + cf$b_regulation)), 4 * se)
  expect_lt(abs(mean(sim$decided == 2L) - (cf$a_extra_time + cf$b_extra_time)), 4 * se)
  expect_lt(abs(mean(sim$decided == 3L) - (cf$a_shootout + cf$b_shootout)), 4 * se)
  expect_lt(abs(mean(sim$leg1_a) - 0.9), 0.02)
  expect_lt(abs(mean(sim$leg2_b) - 0.7), 0.02)
  tie_a <- sim$leg1_a + sim$leg2_a
  tie_b <- sim$leg1_b + sim$leg2_b
  expect_true(all(sim$a_wins[tie_a > tie_b]))
  expect_false(any(sim$a_wins[tie_a < tie_b]))
  expect_true(all(sim$decided[tie_a != tie_b] == 1L))
  expect_true(all(sim$decided[tie_a == tie_b] > 1L))
})

test_that("play-off places go to the best-ranked winners", {
  ties <- tibble::tibble(
    team_a = c("P", "Q", "R"), team_b = c("X", "Y", "Z"),
    rank_a = c(5L, 2L, 9L), rank_b = c(1L, 7L, 3L)
  )
  # Certain results: P, Y and Z win; ranks 5, 7, 3; two direct places.
  sure <- matrix(c(1, 0, 0), 1, 3)
  out <- nt_playoff_places(sure, ties, n_direct = 2L)
  expect_equal(out$p_win[match(c("P", "Y", "Z", "X"), out$team)], c(1, 1, 1, 0))
  expect_equal(out$p_direct[match(c("P", "Y", "Z"), out$team)], c(1, 0, 1))
  expect_equal(out$p_other[match(c("P", "Y", "Z"), out$team)], c(0, 1, 0))
})

test_that("play-off places average over draws and weigh every combination", {
  ties <- tibble::tibble(
    team_a = c("P", "Q"), team_b = c("X", "Y"),
    rank_a = c(4L, 1L), rank_b = c(2L, 3L)
  )
  # One direct place. P (4) goes direct only if Y (3) does not win, i.e. Q wins
  # (rank 1 beats P) -- never. X (2) goes direct unless Q (1) wins.
  p <- matrix(c(0.6, 0.2, 0.7, 0.5), 2, 2)
  out <- nt_playoff_places(p, ties, n_direct = 1L)
  get <- function(team, col) out[[col]][out$team == team]
  expect_equal(get("P", "p_direct"), 0)
  expect_equal(get("X", "p_direct"), mean((1 - p[, 1]) * (1 - p[, 2])))
  expect_equal(get("Q", "p_direct"), mean(p[, 2]))
  expect_equal(sum(out$p_direct), mean(rep(1, 2)))
  expect_equal(sum(out$p_win), 2)
})

wwc_test_groups <- function() {
  teams <- c("Brazil", paste0("T", 2:32))
  stats::setNames(split(teams, rep(1:8, each = 4)), LETTERS[1:8])
}

test_that("the 2027 Women's World Cup structure has eight groups and FIFA's bracket", {
  s <- nt_wwc2027_structure(wwc_test_groups())
  expect_equal(names(s$groups), LETTERS[1:8])
  expect_equal(s$hosts, "Brazil")
  expect_null(s$third_slots)
  r16 <- s$bracket[s$bracket$round == "R16", ]
  expect_equal(r16$match_no, 49:56)
  expect_equal(paste(r16$feeder_a, r16$feeder_b),
    c("1A 2B", "1C 2D", "1B 2A", "1D 2C", "1E 2F", "1G 2H", "1F 2E", "1H 2G"))
  qf <- s$bracket[s$bracket$round == "QF", ]
  expect_equal(paste(qf$match_no, qf$feeder_a, qf$feeder_b),
    c("57 W49 W50", "58 W53 W54", "59 W51 W52", "60 W55 W56"))
  sf <- s$bracket[s$bracket$round == "SF", ]
  expect_equal(paste(sf$match_no, sf$feeder_a, sf$feeder_b), c("61 W57 W58", "62 W59 W60"))
  expect_equal(paste(s$third_place$feeder_a, s$third_place$feeder_b), "L61 L62")
})

test_that("the 2027 structure refuses a draw without the host in group A", {
  g <- wwc_test_groups()
  g$A[1] <- "T99"
  g$B[1] <- "Brazil"
  expect_error(nt_wwc2027_structure(g), "Brazil")
  expect_error(nt_wwc2027_structure(g[1:7]), "eight groups")
})

test_that("a 32-team World Cup simulates with no third-placed qualifiers", {
  s <- nt_wwc2027_structure(wwc_test_groups())
  teams <- unlist(s$groups, use.names = FALSE)
  fit <- nt_test_fit(stats::setNames(seq(0.6, -0.6, length.out = 32), teams), mu = 0.1,
    home_for = 0.15, home_against = -0.2, n_draws = 3L)
  sim <- nt_simulate_world_cup(fit, s, n_sims = 600L, seed = 3L)
  t <- sim$teams
  expect_equal(sum(t$p_advance), 16)
  expect_equal(t$p_advance, t$p_first + t$p_second, ignore_attr = TRUE)
  expect_equal(t$p_r16, t$p_advance)
  expect_equal(sum(t$p_qf), 8)
  expect_equal(sum(t$p_sf), 4)
  expect_equal(sum(t$p_final), 2)
  expect_equal(sum(t$p_champion), 1)
  expect_equal(sum(t$p_bronze), 1)
  expect_null(sim$r32)
  expect_equal(sim$first_round$round, "R16")
  expect_equal(dim(sim$first_round$a), c(600L, 8L))
})

test_that("the 2027 bracket keeps group winners A and B apart until the final", {
  s <- nt_wwc2027_structure(wwc_test_groups())
  teams <- unlist(s$groups, use.names = FALSE)
  base <- stats::setNames(rep(0, 32), teams)
  ab <- base
  ab[c("Brazil", "T5")] <- 6 # A1 and B1
  ac <- base
  ac[c("Brazil", "T9")] <- 6 # A1 and C1: same quarter-final (57)
  sim_ab <- nt_simulate_world_cup(nt_test_fit(ab), s, n_sims = 300L, seed = 1L)$teams
  sim_ac <- nt_simulate_world_cup(nt_test_fit(ac), s, n_sims = 300L, seed = 1L)$teams
  get <- function(sim, team, col) unname(sim[[col]][sim$team == team])
  expect_gt(get(sim_ab, "Brazil", "p_final") + get(sim_ab, "T5", "p_final"), 1.95)
  expect_equal(get(sim_ac, "Brazil", "p_sf") + get(sim_ac, "T9", "p_sf"), 1, tolerance = 0.02)
})

test_that("the 2027 structure defaults to the 2023 group order and Brazil at home in knockouts", {
  s <- nt_wwc2027_structure(wwc_test_groups())
  expect_equal(s$group_rule, "wwc_2023")
  expect_equal(s$knockout_hosts, "Brazil")
  s0 <- nt_wwc2027_structure(wwc_test_groups(), group_rule = "head_to_head", host_knockout_home = FALSE)
  expect_equal(s0$group_rule, "head_to_head")
  expect_length(s0$knockout_hosts, 0L)
})

test_that("the simulator ranks groups by the structure's rule unless given one", {
  s <- nt_wwc2027_structure(wwc_test_groups())
  teams <- unlist(s$groups, use.names = FALSE)
  withr::local_seed(5)
  fit <- nt_test_fit(stats::setNames(stats::rnorm(32, 0, 0.4), teams), mu = 0.1)
  by_default <- nt_simulate_world_cup(fit, s, n_sims = 800L, seed = 2L)$teams
  given <- nt_simulate_world_cup(fit, s, n_sims = 800L, group_rule = "wwc_2023", seed = 2L)$teams
  h2h <- nt_simulate_world_cup(fit, s, n_sims = 800L, group_rule = "head_to_head", seed = 2L)$teams
  expect_identical(by_default, given)
  expect_false(isTRUE(all.equal(by_default$p_first, h2h$p_first)))
})

test_that("Brazil plays its knockout matches at home unless the structure says not", {
  teams <- unlist(wwc_test_groups(), use.names = FALSE)
  fit <- nt_test_fit(stats::setNames(rep(0, 32), teams), home_for = 3, home_against = -3)
  champion <- function(home) {
    s <- nt_wwc2027_structure(wwc_test_groups(), host_knockout_home = home)
    t <- nt_simulate_world_cup(fit, s, n_sims = 400L, seed = 6L)$teams
    unname(t$p_champion[t$team == "Brazil"])
  }
  expect_gt(champion(TRUE), 0.95)
  expect_lt(champion(FALSE), 0.2)
})

test_that("a scalar rate is recycled across simulated ties", {
  withr::local_seed(4)
  sim <- nt_simulate_two_leg(rep(1, 50), 1, 1, 1)
  expect_false(anyNA(sim$a_wins))
  expect_length(sim$leg2_b, 50L)
})

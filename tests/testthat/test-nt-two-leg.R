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

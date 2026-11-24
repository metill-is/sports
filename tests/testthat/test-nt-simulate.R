test_that("evenly matched sides advance half the time under either rule", {
  expect_equal(nt_knockout_win_prob(1.3, 1.3, "extra_time"), 0.5)
  expect_equal(nt_knockout_win_prob(1.3, 1.3, "replay"), 0.5)
})

test_that("advancement probabilities of the two sides sum to one", {
  for (method in c("extra_time", "replay")) {
    expect_equal(
      nt_knockout_win_prob(1.8, 0.7, method) + nt_knockout_win_prob(0.7, 1.8, method), 1
    )
  }
})

test_that("extra time and a coin-toss shootout favour the stronger side less than a replay", {
  et <- nt_knockout_win_prob(1.8, 0.7, "extra_time")
  replay <- nt_knockout_win_prob(1.8, 0.7, "replay")
  reg <- .nt_poisson_compare(1.8, 0.7)
  expect_gt(et, 0.5)
  expect_lt(et, replay)
  expect_gt(et, reg$win + 0.5 * reg$draw)
  expect_equal(
    nt_knockout_win_prob(1.8, 0.7, "extra_time", et_factor = 1e-9),
    reg$win + 0.5 * reg$draw,
    tolerance = 1e-6
  )
})

test_that("the closed form is the extra-time recipe written out", {
  reg <- .nt_poisson_compare(1.6, 0.9)
  et <- .nt_poisson_compare(1.6 / 3, 0.9 / 3)
  expect_equal(
    nt_knockout_win_prob(1.6, 0.9, shootout_p = 0.4),
    reg$win + reg$draw * (et$win + et$draw * 0.4)
  )
})

test_that("simulated knockout ties match the closed form", {
  withr::local_seed(7)
  n <- 200000L
  sim <- nt_simulate_knockout(rep(1.6, n), rep(0.9, n))
  se <- sqrt(0.25 / n)
  expect_lt(abs(mean(sim$a_wins) - nt_knockout_win_prob(1.6, 0.9)), 4 * se)
  reg <- .nt_poisson_compare(1.6, 0.9)
  et <- .nt_poisson_compare(1.6 / 3, 0.9 / 3)
  expect_lt(abs(mean(sim$decided == 1L) - (1 - reg$draw)), 4 * se)
  expect_lt(abs(mean(sim$decided == 3L) - reg$draw * et$draw), 4 * se)
  rep <- nt_simulate_knockout(rep(1.6, n), rep(0.9, n), "replay")
  expect_lt(abs(mean(rep$a_wins) - nt_knockout_win_prob(1.6, 0.9, "replay")), 4 * se)
  expect_true(all(rep$decided == 1L))
})

test_that("head-to-head matrix is antisymmetric around one half", {
  fit <- nt_test_fit(c(A = 0.5, B = 0, C = -0.4), mu = 0.2)
  h <- nt_head_to_head(fit, c("A", "B", "C"))
  expect_equal(h["A", "B"] + h["B", "A"], 1)
  expect_gt(h["A", "C"], h["A", "B"])
  expect_true(all(is.na(diag(h))))
  expect_lt(h["A", "C"], nt_head_to_head(fit, c("A", "B", "C"), "replay")["A", "C"])
})

test_that("a simulated World Cup fills every round exactly", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  withr::local_seed(3)
  fit <- nt_test_fit(stats::setNames(stats::rnorm(48, 0, 0.4), teams), mu = 0.15, n_draws = 5L)
  sim <- nt_simulate_world_cup(fit, s, n_sims = 2000L, seed = 5L)
  tt <- sim$teams
  expect_equal(sum(tt$p_advance), 32)
  expect_equal(sum(tt$p_r16), 16)
  expect_equal(sum(tt$p_qf), 8)
  expect_equal(sum(tt$p_sf), 4)
  expect_equal(sum(tt$p_final), 2)
  expect_equal(sum(tt$p_champion), 1)
  expect_equal(sum(tt$p_bronze), 1)
  expect_equal(tt$p_first + tt$p_second + tt$p_third + tt$p_fourth, rep(1, 48))
  expect_equal(unname(tapply(tt$p_first, tt$group, sum)), rep(1, 12), ignore_attr = TRUE)
  expect_true(all(tt$p_advance >= tt$p_r16 & tt$p_r16 >= tt$p_qf & tt$p_qf >= tt$p_sf))
  expect_true(all(tt$p_first + tt$p_second <= tt$p_advance + 1e-12))
  expect_equal(sum(sim$decided), 1)
  expect_equal(dim(sim$r32$a), c(2000L, 16L))
  both <- cbind(sim$r32$a, sim$r32$b)
  expect_true(all(apply(both, 1L, function(x) length(unique(x))) == 32L))
})

test_that("a far stronger team wins the simulated World Cup", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  theta <- stats::setNames(rep(0, 48), teams)
  theta["Iran"] <- 3
  sim <- nt_simulate_world_cup(nt_test_fit(theta), s, n_sims = 500L, seed = 1L)
  expect_gt(sim$teams$p_champion[sim$teams$team == "Iran"], 0.95)
  expect_equal(sim$teams$p_first[sim$teams$team == "Iran"], 1)
})

test_that("the replay rule never produces extra time or shootouts", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  fit <- nt_test_fit(stats::setNames(rep(0, 48), teams))
  sim <- nt_simulate_world_cup(fit, s, n_sims = 200L, knockout = "replay", seed = 1L)
  expect_equal(unname(sim$decided), c(1, 0, 0))
})

test_that("a single simulated tournament runs", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  fit <- nt_test_fit(stats::setNames(rep(0, 48), teams))
  sim <- nt_simulate_world_cup(fit, s, n_sims = 1L, seed = 1L)
  expect_equal(sum(sim$teams$p_champion), 1)
  expect_equal(sum(sim$teams$p_advance), 32)
})

test_that("simulation uses each draw's own strengths", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  fit <- nt_test_fit(stats::setNames(rep(0, 48), teams), n_draws = 2L)
  fit$draws$theta[1, "Iran"] <- 4
  fit$draws$theta[2, "Iran"] <- -4
  sim <- nt_simulate_world_cup(fit, s, n_sims = 400L, seed = 2L)
  iran <- sim$teams[sim$teams$team == "Iran", ]
  expect_equal(iran$p_first, 0.5, tolerance = 0.02)
  expect_equal(iran$p_fourth, 0.5, tolerance = 0.05)
  expect_gt(iran$p_champion, 0.45)
  expect_lt(iran$p_champion, 0.52)
})

wc_group_games <- function(...) {
  m <- matrix(c(...), ncol = 4, byrow = TRUE)
  list(
    home = m[, 1], away = m[, 2],
    goals_h = matrix(as.integer(m[, 3]), 1), goals_a = matrix(as.integer(m[, 4]), 1)
  )
}

test_that("World Cup group ties go to head-to-head before goal difference", {
  # A beat B and both finish on 6 points; B's goal difference is +7, A's +1.
  # FIFA World Cup 2026 Regulations, art. 13: head-to-head comes first.
  g <- wc_group_games(
    "A", "B", 1, 0, "A", "C", 0, 1, "A", "D", 1, 0,
    "B", "C", 5, 0, "B", "D", 3, 0, "C", "D", 0, 1
  )
  rk <- .nt_wc_rank_group(
    g$home, g$away, c("A", "B", "C", "D"), g$goals_h, g$goals_a,
    matrix(c(0.1, 0.2, 0.3, 0.4), 1)
  )
  expect_equal(rk$rank[1, ], c(1, 2, 4, 3))
  expect_equal(rk$gd[1, ], c(1, 7, -5, -3))
})

test_that("World Cup group ties level on head-to-head go to overall goal difference", {
  # A and B draw each other and finish on 7 points; B's goal difference is better.
  g <- wc_group_games(
    "A", "B", 1, 1, "A", "C", 1, 0, "A", "D", 1, 0,
    "B", "C", 4, 0, "B", "D", 2, 0, "C", "D", 0, 0
  )
  rk <- .nt_wc_rank_group(
    g$home, g$away, c("A", "B", "C", "D"), g$goals_h, g$goals_a,
    matrix(c(0.4, 0.3, 0.2, 0.1), 1)
  )
  expect_equal(rk$rank[1, c(1, 2)], c(2, 1))
})

test_that("the World Cup simulator ranks groups under the rule it is given", {
  s <- wc_structure()
  teams <- unlist(s$groups, use.names = FALSE)
  withr::local_seed(4)
  fit <- nt_test_fit(stats::setNames(stats::rnorm(48, 0, 0.4), teams), mu = 0.15)
  h2h <- nt_simulate_world_cup(fit, s, n_sims = 1000L, seed = 9L)
  gd <- nt_simulate_world_cup(fit, s, n_sims = 1000L, group_rule = "goal_difference", seed = 9L)
  expect_false(isTRUE(all.equal(h2h$teams$p_first, gd$teams$p_first)))
  expect_equal(h2h$teams$p_first + h2h$teams$p_second + h2h$teams$p_third + h2h$teams$p_fourth, rep(1, 48))
  expect_equal(sum(h2h$teams$p_advance), 32)
})

test_that("a two-legged tie matches its closed form, extra time at the second leg's rates", {
  withr::local_seed(8)
  n <- 200000L
  leg1 <- list(a = rep(1.6, n), b = rep(0.9, n))
  leg2 <- list(a = rep(1.1, n), b = rep(1.4, n))
  res <- nt_simulate_two_legged(leg1, leg2, et_factor = 1 / 3, shootout_p = 0.5)
  agg <- sports:::.nt_poisson_compare(1.6 + 1.1, 0.9 + 1.4)
  et <- sports:::.nt_poisson_compare(1.1 / 3, 1.4 / 3)
  want <- agg$win + agg$draw * (et$win + et$draw * 0.5)
  near <- function(x, p) expect_lt(abs(mean(x) - p), 4 * sqrt(p * (1 - p) / n))
  near(res$a_wins, want)
  near(res$decided == 1L, 1 - agg$draw)
  near(res$decided == 3L, agg$draw * et$draw)
  # A shootout won by A with probability 1 shifts every penalty decision to A.
  sure <- nt_simulate_two_legged(leg1, leg2, shootout_p = 1)
  expect_true(all(sure$a_wins[sure$decided == 3L]))
})

test_that("two-legged ties use each draw's own rates", {
  withr::local_seed(9)
  leg1 <- list(a = c(8, 0.01), b = c(0.01, 8))
  leg2 <- list(a = c(8, 0.01), b = c(0.01, 8))
  res <- nt_simulate_two_legged(leg1, leg2)
  expect_identical(res$a_wins, c(TRUE, FALSE))
})

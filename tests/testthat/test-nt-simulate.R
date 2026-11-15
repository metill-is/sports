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

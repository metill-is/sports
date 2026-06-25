# Tests for the GISKO optimal-prediction engine (R/gisko.R).

test_that("gisko_match_points matches the rules examples", {
  expect_equal(gisko_match_points(2, 1, 2, 1), 5L)
  expect_equal(gisko_match_points(3, 1, 2, 1), 3L)
  expect_equal(gisko_match_points(1, 1, 2, 2), 3L)
  expect_equal(gisko_match_points(2, 0, 0, 2), 0L)
  expect_equal(gisko_match_points(2, 0, 3, 1), 3L)
})

test_that("gisko_match_points is vectorised", {
  expect_equal(gisko_match_points(c(2, 3), c(1, 1), c(2, 2), c(1, 1)), c(5L, 3L))
})

test_that("a per-match score of 4 is impossible", {
  g <- expand.grid(ph = 0:6, pa = 0:6, ah = 0:6, aa = 0:6)
  s <- gisko_match_points(g$ph, g$pa, g$ah, g$aa)
  expect_true(all(s %in% c(0L, 1L, 2L, 3L, 5L)))
  expect_false(any(s == 4L))
})

test_that("gisko_score_matrix orientation is rows=home, cols=away", {
  sm <- gisko_score_matrix(2, 1, max_goals = 4L)
  expect_equal(dim(sm), c(5L, 5L))
  expect_equal(sm[3, 2], 5L)
  expect_equal(sm[4, 2], 3L)
})

test_that(".gisko_bvpois_pmf sums to one and has the right means", {
  l <- c(1.4, 0.9, 0.3)
  P <- sports:::.gisko_bvpois_pmf(l, max_goals = 15L)
  expect_equal(sum(P), 1)
  g <- 0:15
  expect_equal(sum(rowSums(P) * g), l[1] + l[3], tolerance = 1e-4)
  expect_equal(sum(colSums(P) * g), l[2] + l[3], tolerance = 1e-4)
})

test_that(".gisko_bvpois_pmf factorises when lambda3 = 0", {
  l <- c(1.2, 0.8, 0)
  P <- sports:::.gisko_bvpois_pmf(l, max_goals = 20L)
  g <- 0:20
  indep <- outer(stats::dpois(g, l[1]), stats::dpois(g, l[2]))
  indep <- indep / sum(indep)
  expect_equal(P, indep, tolerance = 1e-9)
})

test_that("the goal-difference distribution is invariant to lambda3 (Skellam cancellation)", {
  corr <- sports:::.gisko_bvpois_pmf(c(1.3, 0.8, 0.6), max_goals = 25L)
  indep <- sports:::.gisko_bvpois_pmf(c(1.3, 0.8, 0), max_goals = 25L)
  gd_corr <- gisko_marginals_from_pbar(corr)$gd_pmf
  gd_indep <- gisko_marginals_from_pbar(indep)$gd_pmf
  common <- intersect(names(gd_corr), names(gd_indep))
  expect_equal(unname(gd_corr[common]), unname(gd_indep[common]), tolerance = 1e-6)
  g <- 0:25
  expect_gt(sum(g * rowSums(corr)), sum(g * rowSums(indep)))
})

test_that("the outcome cannot be computed from independent (lambda3-inflated) marginals", {
  P <- sports:::.gisko_bvpois_pmf(c(1.3, 0.8, 0.6), max_goals = 25L)
  marg <- gisko_marginals_from_pbar(P)
  p_draw_true <- unname(marg$p_outcome[["draw"]])
  g <- as.character(0:25)
  p_draw_indep <- sum(marg$home_pmf[g] * marg$away_pmf[g])
  expect_false(isTRUE(all.equal(p_draw_true, p_draw_indep)))
})

test_that("expected points: grid sum equals the marginal (Lemma 1) formula", {
  set.seed(1)
  pbar <- matrix(stats::runif(36), 6, 6)
  pbar <- pbar / sum(pbar)
  marg <- gisko_marginals_from_pbar(pbar)
  for (h in 0:3) {
    for (a in 0:3) {
      expect_equal(
        gisko_expected_points(h, a, pbar),
        gisko_expected_points_marginal(h, a, marg),
        tolerance = 1e-9
      )
    }
  }
})

test_that("optimal scoreline can differ from the modal scoreline and never scores worse", {
  pbar <- matrix(0, 4, 4)
  pbar[2, 2] <- 0.30
  pbar[3, 2] <- 0.22
  pbar[2, 1] <- 0.20
  pbar[3, 1] <- 0.13
  pbar[1, 2] <- 0.08
  pbar[2, 3] <- 0.07
  opt <- gisko_optimal_scoreline(pbar)
  expect_true(opt$optimal_differs_from_modal)
  expect_gte(opt$exp_points, gisko_expected_points(1, 1, pbar))
})

test_that("a degenerate posterior is optimised by its point mass", {
  pbar <- matrix(0, 5, 5)
  pbar[3, 2] <- 1
  opt <- gisko_optimal_scoreline(pbar)
  expect_equal(c(opt$home, opt$away), c(2, 1))
  expect_equal(opt$exp_points, 5)
  expect_false(opt$optimal_differs_from_modal)
})

test_that("gisko_predictive_matrix integrates per-draw PMFs over the posterior", {
  team <- tibble::tibble(
    team = rep(c("A", "B"), times = 3),
    .draw = rep(1:3, each = 2),
    cur_offense = c(0.4, -0.1, 0.5, 0.0, 0.3, -0.2),
    cur_defense = c(0.2, -0.3, 0.1, -0.2, 0.25, -0.35),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:3,
    mean_log_goals = c(0.1, 0.05, 0.12),
    alpha_mu3 = c(-1, -1.1, -0.9),
    beta_mu3_strength_diff = c(0.2, 0.25, 0.15)
  )
  si <- list(team = team, scalar = scalar)
  res <- gisko_predictive_matrix("A", "B", si, venue = "neutral", max_goals = 10L)
  expect_equal(res$n_draws, 3L)
  expect_equal(dim(res$lambdas), c(3L, 3L))
  expect_equal(sum(res$pbar), 1, tolerance = 1e-9)
  manual <- (sports:::.gisko_bvpois_pmf(res$lambdas[1, ], 10L) +
    sports:::.gisko_bvpois_pmf(res$lambdas[2, ], 10L) +
    sports:::.gisko_bvpois_pmf(res$lambdas[3, ], 10L)) / 3
  expect_equal(res$pbar, manual, tolerance = 1e-9)
})

test_that("gisko_predictive_matrix errors on an unknown team", {
  si <- list(
    team = tibble::tibble(
      team = "A", .draw = 1L, cur_offense = 0, cur_defense = 0,
      home_advantage_off = 0, home_advantage_def = 0
    ),
    scalar = tibble::tibble(
      .draw = 1L, mean_log_goals = 0.1, alpha_mu3 = -1, beta_mu3_strength_diff = 0.2
    )
  )
  expect_error(gisko_predictive_matrix("A", "Z", si), "not in sim_inputs")
})

test_that("round total mean equals the sum of per-match expected points", {
  team <- tibble::tibble(
    team = rep(c("A", "B", "C", "D"), times = 2),
    .draw = rep(1:2, each = 4),
    cur_offense = c(0.5, 0.1, -0.2, -0.4, 0.45, 0.05, -0.25, -0.35),
    cur_defense = c(0.3, 0.0, -0.1, -0.3, 0.28, 0.02, -0.12, -0.28),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:2, mean_log_goals = c(0.1, 0.12),
    alpha_mu3 = c(-1, -0.9), beta_mu3_strength_diff = c(0.2, 0.18)
  )
  si <- list(team = team, scalar = scalar)
  matches <- tibble::tibble(
    home = c("A", "C"), away = c("B", "D"),
    venue = "neutral", label = c("A v B", "C v D")
  )
  res <- gisko_optimise_round(matches, si, max_goals = 10L)
  support <- seq_along(res$total_pmf) - 1L
  expect_equal(sum(support * res$total_pmf), sum(res$picks$exp_points),
    tolerance = 1e-6
  )
  jl <- res$joker
  supp_j <- seq_along(res$total_pmf_joker) - 1L
  expect_equal(sum(supp_j * res$total_pmf_joker),
    sum(res$picks$exp_points) + res$picks$exp_points[jl],
    tolerance = 1e-6
  )
})

test_that("the joker is the highest-expected-points match", {
  team <- tibble::tibble(
    team = rep(c("A", "B", "C", "D"), times = 2),
    .draw = rep(1:2, each = 4),
    cur_offense = c(1.2, -0.8, 0.1, 0.0, 1.1, -0.7, 0.05, 0.02),
    cur_defense = c(0.6, -0.6, 0.0, 0.0, 0.55, -0.55, 0.0, 0.0),
    home_advantage_off = 0, home_advantage_def = 0
  )
  scalar <- tibble::tibble(
    .draw = 1:2, mean_log_goals = c(0.1, 0.1),
    alpha_mu3 = c(-1, -1), beta_mu3_strength_diff = c(0.2, 0.2)
  )
  si <- list(team = team, scalar = scalar)
  matches <- tibble::tibble(
    home = c("A", "C"), away = c("B", "D"),
    venue = "neutral", label = c("mismatch", "even")
  )
  res <- gisko_optimise_round(matches, si, max_goals = 10L)
  expect_equal(res$joker, which.max(res$picks$exp_points))
})

# Tests for the GISKO retrospective backtest (R/gisko-backtest.R).

test_that("gisko_marginals_from_log adapts the accountability-log shape", {
  m <- list(
    p_home = 0.5, p_draw = 0.3, p_away = 0.2,
    dist_home = list(list(goals = 0, p = 0.4), list(goals = 1, p = 0.6)),
    dist_away = list(list(goals = 0, p = 0.7), list(goals = 1, p = 0.3)),
    dist_diff = list(
      list(diff = -1, p = 0.2), list(diff = 0, p = 0.3), list(diff = 1, p = 0.5)
    )
  )
  marg <- gisko_marginals_from_log(m)
  expect_equal(unname(marg$p_outcome), c(0.5, 0.3, 0.2))
  expect_equal(unname(marg$home_pmf[c("0", "1")]), c(0.4, 0.6))
  expect_equal(unname(marg$away_pmf[c("0", "1")]), c(0.7, 0.3))
  expect_equal(unname(marg$gd_pmf["1"]), 0.5)
})

test_that("gisko_optimal_group_order solves the placement assignment", {
  # Two teams both most-likely 1st, but the assignment can't place both 1st;
  # it must give one the 2nd slot to maximise expected correct placements.
  p <- rbind(
    A = c(0.6, 0.3, 0.1, 0.0),
    B = c(0.5, 0.4, 0.1, 0.0),
    C = c(0.0, 0.2, 0.5, 0.3),
    D = c(0.0, 0.1, 0.3, 0.6)
  )
  colnames(p) <- 1:4
  ord <- gisko_optimal_group_order(p)
  expect_equal(ord, c("A", "B", "C", "D"))
  expect_length(ord, 4L)

  # degenerate: each team certain of a distinct slot -> that exact order
  pd <- diag(4)
  rownames(pd) <- c("W", "X", "Y", "Z")
  expect_equal(gisko_optimal_group_order(pd), c("W", "X", "Y", "Z"))
})

test_that("gisko_backtest_score totals base + per-round joker correctly", {
  pm_marg <- function(h, a, n = 6L) {
    pbar <- matrix(0, n, n)
    pbar[h + 1L, a + 1L] <- 1
    gisko_marginals_from_pbar(pbar)
  }
  # m2: half mass on 1-0, half on 2-0 -> optimal 1-0 with exp_points 4
  m2 <- matrix(0, 4, 4)
  m2[2, 1] <- 0.5
  m2[3, 1] <- 0.5
  m2_marg <- gisko_marginals_from_pbar(m2)

  played <- tibble::tibble(
    round = c("1", "1", "2"),
    label = c("m1", "m2", "m3"),
    marg = list(pm_marg(2, 1), m2_marg, pm_marg(0, 0)),
    act_home = c(2L, 2L, 0L),
    act_away = c(1L, 0L, 0L)
  )
  res <- gisko_backtest_score(played)

  expect_equal(res$picks$points, c(5L, 3L, 5L)) # 2-1 exact; 1-0 vs 2-0; 0-0 exact
  expect_equal(res$base_total, 13)
  # round 1 joker -> m1 (exp 5 > m2 exp 4); round 2 joker -> m3
  r1 <- res$by_round[res$by_round$round == "1", ]
  expect_equal(r1$joker_match, "m1")
  expect_equal(r1$joker_bonus, 5)
  expect_equal(res$joker_total, 10) # 5 (m1) + 5 (m3)
  expect_equal(res$grand_total, 23)
})

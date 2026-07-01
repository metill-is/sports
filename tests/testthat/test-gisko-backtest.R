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

test_that("gisko_render_report writes a self-contained HTML file", {
  data <- list(
    picks = tibble::tibble(
      round = c("G1", "G1"), label = c("A v B", "C v D"),
      pick = c("1-0", "0-0"), actual = c("1-0", "1-1"),
      exp_points = c(2.5, 1.8), points = c(5L, 1L)
    ),
    by_round = tibble::tibble(
      round = "G1", n = 2L, base = 6L, joker_match = "A v B",
      joker_bonus = 5L, complete = FALSE
    ),
    structural = tibble::tibble(
      pool = "A", placement = 4L, qualification = 4L,
      model_order = "A > B > C > D", actual_order = "A > B > C > D"
    ),
    base_total = 6L, match_total = 6L, pool_pts = 4L, qual_pts = 4L,
    model_total = 14L, n_played = 2L, ceiling = 10L,
    pending_joker = NULL, complete_pools = "A"
  )
  tmp <- withr::local_tempfile(fileext = ".html")
  gisko_render_report(data, tmp,
    leader = list(match = 10L, pool = 4L, qual = 4L),
    field_size = 2624L, generated_at = "2026-06-25"
  )
  html <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  expect_match(html, "<!doctype html>", fixed = TRUE)
  expect_match(html, "2,624", fixed = TRUE)
  expect_match(html, "chip p5", fixed = TRUE)
  expect_false(grepl("https?://|cdn|googleapis", html))
})

test_that(".gisko_group_stage drops knockout rows so completed pools survive", {
  # Two groups, each with its 6 round-robin fixtures, plus one played knockout
  # match per group that inherits the group's label from its home team.
  res <- data.frame(
    grp = c(rep("A", 6L), rep("B", 6L), "A", "B"),
    stringsAsFactors = FALSE
  )
  res_round <- c(rep("1", 4L), rep("2", 4L), rep("3", 4L), NA, NA)

  # Before the fix, table(res$grp) == 6L saw A = 7, B = 7 and dropped both.
  expect_false(all(table(res$grp) == 6L))

  gs <- .gisko_group_stage(res, res_round)
  expect_equal(nrow(gs), 12L)
  expect_equal(sort(names(which(table(gs$grp) == 6L))), c("A", "B"))
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

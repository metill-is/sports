# Review finding: .compute_final_positions_2dt() ranked each draw's table on
# points ALONE and took placement from row_number(), so teams level on points
# were ordered by whichever had the earlier upcoming fixture in pred_d. At two
# points a win over 22 rounds exact ties are common, so a genuine 0.37/0.36
# title race published as 0.62/0.11 -- a confident call that was an artefact of
# the fixture calendar. Football never had this: it ranks points -> gd -> gf.

.tb_goals <- function(alpha_first = TRUE, n_draws = 400L) {
  tidyr::expand_grid(.draw = seq_len(n_draws), game_nr = 1:2) |>
    dplyr::mutate(
      division = "BD",
      home_team = dplyr::if_else(
        .data$game_nr == 1L,
        if (alpha_first) "Alpha" else "Beta",
        if (alpha_first) "Beta" else "Alpha"
      ),
      away_team = dplyr::if_else(.data$game_nr == 1L, "Gamma", "Delta"),
      home_score = 100, away_score = 90
    )
}

.tb_base <- tibble::tibble(
  team = c("Alpha", "Beta", "Gamma", "Delta"),
  base_points = c(10L, 10L, 0L, 0L), base_diff = c(0, 0, 0, 0)
)

.tb_p1 <- function(alpha_first) {
  fp <- .compute_final_positions_2dt(
    .tb_goals(alpha_first), "BD", .tb_base,
    has_ties = FALSE, tie_threshold = 0,
    current_top_teams = tibble::tibble(team = .tb_base$team)
  )
  p <- fp[fp$placement == 1L, ]
  stats::setNames(p$probability, p$team)[c("Alpha", "Beta")]
}

test_that("teams tied on points split placement 1, they do not take all of it", {
  p <- .tb_p1(TRUE)
  # Before the fix this was Alpha 1.00 / Beta 0.00.
  expect_gt(p[["Alpha"]], 0.3)
  expect_gt(p[["Beta"]], 0.3)
  expect_equal(unname(p[["Alpha"]] + p[["Beta"]]), 1, tolerance = 1e-9)
})

test_that("placement probability does not depend on fixture order", {
  # THE regression. Swapping which tied team owns the earlier fixture used to
  # move p(placement 1) from 1.00 to 0.00.
  a <- .tb_p1(TRUE)
  b <- .tb_p1(FALSE)
  expect_lt(max(abs(a - b)), 0.15)   # Monte Carlo noise only, not a flip
})

test_that("a better point difference still outranks, so the jitter is a last resort", {
  # The jitter must only settle EXACT (points, point_diff) ties -- if it
  # overrode point difference the ranking would be noise.
  goals <- .tb_goals(TRUE)
  goals$home_score[goals$home_team == "Alpha"] <- 130  # Alpha's diff is larger
  fp <- .compute_final_positions_2dt(
    goals, "BD", .tb_base, has_ties = FALSE, tie_threshold = 0,
    current_top_teams = tibble::tibble(team = .tb_base$team)
  )
  p <- fp[fp$placement == 1L, ]
  p <- stats::setNames(p$probability, p$team)
  expect_equal(unname(p[["Alpha"]]), 1, tolerance = 1e-9)
})

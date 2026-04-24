make_fake_fit <- function(N_pred = 2L, n_draws = 10L) {
  vars <- c(
    paste0("goals1_pred[", seq_len(N_pred), "]"),
    paste0("goals2_pred[", seq_len(N_pred), "]")
  )
  if (N_pred == 0L) {
    # Return a fit with no predictive variables
    mat <- matrix(numeric(0), nrow = n_draws, ncol = 0L)
    draws_arr <- posterior::as_draws_array(mat)
  } else {
    mat <- matrix(
      runif(n_draws * length(vars), min = 0, max = 5),
      nrow = n_draws, ncol = length(vars),
      dimnames = list(NULL, vars)
    )
    draws_arr <- posterior::as_draws_array(mat)
  }
  structure(
    list(draws = function(variables = NULL) {
      if (is.null(variables)) {
        return(draws_arr)
      }
      posterior::subset_draws(draws_arr, variable = variables)
    }),
    class = c("fake_fit", "CmdStanMCMC")
  )
}

test_that("extract_posteriors returns a tibble with the beliefs_latest schema", {
  pred_d <- tibble::tibble(
    game_nr = 1:2,
    match_date = as.Date(c("2026-05-01", "2026-05-03")),
    home_team = c("Alpha", "Bravo"),
    away_team = c("Charlie", "Delta"),
    division = c("D1", "D1"),
    home_nr = 1:2, away_nr = 3:4,
    home_timediff = c(7, 7), away_timediff = c(7, 7)
  )
  fit <- make_fake_fit(N_pred = 2L, n_draws = 10L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, pred_d,
    league = league, sex = "male",
    fit_date = as.Date("2026-04-24")
  )

  expect_s3_class(out, "tbl_df")
  expected_cols <- c(
    "sport", "country", "sex", "fit_date",
    "match_date", "home_team", "away_team",
    "draw_id", "home_goals", "away_goals"
  )
  expect_named(out, expected_cols, ignore.order = TRUE)
  expect_equal(nrow(out), 2L * 10L) # N_pred * n_draws
  expect_type(out$draw_id, "integer")
  expect_type(out$home_goals, "double")
  expect_type(out$away_goals, "double")
  expect_true(all(out$fit_date == as.Date("2026-04-24")))
})

test_that("extract_posteriors handles empty pred_d gracefully", {
  empty_pred <- tibble::tibble(
    game_nr = integer(0), match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    division = character(),
    home_nr = integer(), away_nr = integer(),
    home_timediff = numeric(), away_timediff = numeric()
  )
  fit <- make_fake_fit(N_pred = 0L, n_draws = 10L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, empty_pred, league = league, sex = "male")
  expect_equal(nrow(out), 0L)
  expect_named(out, c(
    "sport", "country", "sex", "fit_date",
    "match_date", "home_team", "away_team",
    "draw_id", "home_goals", "away_goals"
  ),
  ignore.order = TRUE
  )
})

test_that("extract_posteriors output round-trips through the beliefs_latest schema", {
  pred_d <- tibble::tibble(
    game_nr = 1L, match_date = as.Date("2026-05-01"),
    home_team = "Alpha", away_team = "Bravo",
    division = "D1", home_nr = 1L, away_nr = 2L,
    home_timediff = 7, away_timediff = 7
  )
  fit <- make_fake_fit(N_pred = 1L, n_draws = 5L)
  league <- list(sport = "basketball", country = "iceland")

  out <- extract_posteriors(fit, pred_d, league,
    sex = "male",
    fit_date = Sys.Date()
  )

  tmp <- withr::local_tempdir()
  expect_no_error(write_table(out, "beliefs_latest", root = tmp))
  back <- read_table("beliefs_latest", root = tmp)
  expect_equal(nrow(back), nrow(out))
})

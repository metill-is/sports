Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Generate model results for handball/other leagues
#'
#' Extracts posterior goal predictions and writes posterior_goals.csv.
#'
#' @param country Country name (e.g., "denmark", "france")
#' @param sex "male" or "female"
#' @param end_date Date for filtering
#' @param make_plots Ignored (kept for API compatibility)
#' @param sports_dir Absolute path to Sports/ root
#' @export
generate_model_results <- function(
  country,
  sex = "male",
  end_date = Sys.Date(),
  make_plots = TRUE,
  sports_dir = NULL
) {
  if (!is.null(sports_dir)) {
    source(file.path(sports_dir, "R", "shared", "extract_posterior.R"), local = TRUE)
  }

  fit <- readr::read_rds(here::here("results", country, sex, "fit.rds"))
  pred_d <- readr::read_csv(
    here::here("results", country, sex, "pred_d.csv"),
    show_col_types = FALSE
  )

  posterior_raw <- extract_posterior_goals(fit, pred_d)

  posterior_goals <- posterior_raw |>
    dplyr::filter(
      date <= end_date + 7,
      date >= end_date
    ) |>
    dplyr::mutate(game_nr = game_nr - min(game_nr) + 1) |>
    dplyr::select(
      iteration = .draw, game_nr, division, date, home, away,
      home_goals, away_goals
    )

  readr::write_csv(
    posterior_goals,
    here::here("results", country, sex, "posterior_goals.csv")
  )

  invisible(posterior_goals)
}

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Generate model results for football leagues
#'
#' Extracts posterior goal predictions and writes posterior_goals.csv.
#'
#' @param sex "male" or "female"
#' @param from_season Integer, starting season (unused, kept for compatibility)
#' @param make_plots Ignored (kept for API compatibility)
#' @param league_labels List with division_names and league_name (unused without plots)
#' @param sports_dir Absolute path to Sports/ root
#' @export
generate_model_results <- function(
  sex = "male",
  from_season = 2021,
  make_plots = TRUE,
  league_labels = list(
    division_names = "PL",
    league_name = "Premier League"
  ),
  sports_dir = NULL
) {
  if (!is.null(sports_dir)) {
    source(file.path(sports_dir, "R", "shared", "extract_posterior.R"), local = TRUE)
  }

  fit <- readr::read_rds(here::here("results", sex, "fit.rds"))
  pred_d <- readr::read_csv(
    here::here("results", sex, "pred_d.csv"),
    show_col_types = FALSE
  )

  posterior_raw <- extract_posterior_goals(fit, pred_d)

  posterior_goals <- posterior_raw |>
    dplyr::filter(
      date < lubridate::today() + 15,
      date >= lubridate::today()
    ) |>
    dplyr::select(
      iteration = .draw, game_nr, division, date, home, away,
      home_goals, away_goals
    )

  readr::write_csv(
    posterior_goals,
    here::here("results", sex, "posterior_goals.csv")
  )

  invisible(posterior_goals)
}

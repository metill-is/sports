Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#' Generate model results for shared pipeline (basketball/handball Iceland)
#'
#' Extracts posterior goal predictions and writes posterior_goals.csv.
#'
#' @param config Config list from sport-specific config file
#' @param sex "male" or "female"
#' @param end_date Date for filtering
#' @param make_plots Ignored (kept for API compatibility)
#' @param sports_dir Absolute path to Sports/ root
#' @export
generate_model_results <- function(
  config,
  sex = "male",
  end_date = Sys.Date(),
  make_plots = TRUE,
  sports_dir = NULL
) {
  if (!is.null(sports_dir)) {
    source(file.path(sports_dir, "R", "shared", "extract_posterior.R"), local = TRUE)
  }

  sport_dir <- config$sport_dir

  fit <- readr::read_rds(here::here(sport_dir, "results", sex, "fit.rds"))
  pred_d <- readr::read_csv(
    here::here(sport_dir, "results", sex, "pred_d.csv"),
    show_col_types = FALSE
  )
  next_games <- readr::read_csv(
    here::here(sport_dir, "results", sex, "next_games.csv"),
    show_col_types = FALSE
  )

  posterior_raw <- extract_posterior_goals(fit, pred_d)

  # Sport-specific filtering
  if (!is.null(config$divisions$schedule_filter)) {
    # Handball: filter by date range
    next_game_date <- min(next_games$date)
    filter_date <- max(c(end_date + 14, next_game_date + 7))
    posterior_goals <- posterior_raw |>
      dplyr::filter(date <= filter_date, date >= end_date)
  } else {
    # Basketball: filter top 20 per division
    posterior_goals <- posterior_raw |>
      dplyr::arrange(date) |>
      dplyr::filter(game_nr <= 20, date >= end_date, .by = division)
  }

  posterior_goals <- posterior_goals |>
    dplyr::mutate(game_nr = game_nr - min(game_nr) + 1) |>
    dplyr::select(
      iteration = .draw, game_nr, division, date, home, away,
      home_goals, away_goals
    )

  readr::write_csv(
    posterior_goals,
    here::here(sport_dir, "results", sex, "posterior_goals.csv")
  )

  invisible(posterior_goals)
}

#' Extract posterior goal predictions from a fitted Stan model
#'
#' Reads the fit.rds file, extracts goals1_pred/goals2_pred draws,
#' joins with prediction data (pred_d), and returns the full posterior.
#'
#' @param fit CmdStanMCMC object (from readRDS on fit.rds)
#' @param pred_d Data frame of predicted games (from prep_data step)
#' @return Data frame with columns: iteration, game_nr, division, date, home, away, home_goals, away_goals
extract_posterior_goals <- function(fit, pred_d) {
  posterior::as_draws_df(
    fit$draws(c("goals1_pred", "goals2_pred"))
  ) |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(parameter, "goals1"),
        "home_goals",
        "away_goals"
      ),
      game_nr = stringr::str_match(parameter, "d\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    dplyr::select(.draw, type, game_nr, value) |>
    tidyr::pivot_wider(names_from = type, values_from = value) |>
    dplyr::inner_join(pred_d, by = "game_nr")
}

#' Filter posterior to next-round predictions and write CSV
#'
#' @param posterior_raw Raw posterior (from extract_posterior_goals)
#' @param output_path Path to write posterior_goals.csv
#' @param date_filter_start Start date for filtering (default: today)
#' @param date_filter_end End date for filtering (default: today + 15)
#' @return Filtered posterior data frame
filter_and_save_posterior <- function(
  posterior_raw,
  output_path,
  date_filter_start = Sys.Date(),
  date_filter_end = Sys.Date() + 15
) {
  posterior_goals <- posterior_raw |>
    dplyr::filter(
      date < date_filter_end,
      date >= date_filter_start
    ) |>
    dplyr::select(
      iteration = .draw,
      game_nr,
      division,
      date,
      home,
      away,
      home_goals,
      away_goals
    )

  readr::write_csv(posterior_goals, output_path)
  posterior_goals
}

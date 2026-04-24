#' Extract posterior predictive draws into the canonical beliefs tibble.
#'
#' Pulls `goals1_pred` + `goals2_pred` draws from a fit, long-pivots them
#' by match (game_nr) and draw, then joins pred_d for match_date +
#' team-name metadata. Returns a tibble matching `schemas()$beliefs_latest`.
#'
#' @param fit CmdStanMCMC / CmdStanGQ with `goals1_pred`, `goals2_pred`
#'   generated quantities.
#' @param pred_d Tibble from `prepare_data()$pred_d` with `game_nr`,
#'   `match_date`, `home_team`, `away_team`.
#' @param league `list(sport = ..., country = ...)` — used to attach
#'   schema columns.
#' @param sex "male" or "female".
#' @param fit_date Date to stamp on every row. Default today.
#' @return Tibble with one row per (match, draw).
#' @importFrom rlang .data
#' @export
extract_posteriors <- function(fit, pred_d, league, sex,
                               fit_date = Sys.Date()) {
  stopifnot(!is.null(league$sport), !is.null(league$country))
  stopifnot(sex %in% c("male", "female"))

  empty_out <- function() {
    tibble::tibble(
      sport      = character(),
      country    = character(),
      sex        = character(),
      fit_date   = as.Date(character()),
      match_date = as.Date(character()),
      home_team  = character(),
      away_team  = character(),
      draw_id    = integer(),
      home_goals = numeric(),
      away_goals = numeric()
    )
  }

  if (nrow(pred_d) == 0L) {
    return(empty_out())
  }

  draws_df <- posterior::as_draws_df(
    fit$draws(c("goals1_pred", "goals2_pred"))
  ) |> tibble::as_tibble()

  if (nrow(draws_df) == 0L) {
    return(empty_out())
  }

  long <- draws_df |>
    tidyr::pivot_longer(
      cols = -c(".chain", ".iteration", ".draw"),
      names_to = "parameter", values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$parameter, "^goals1_pred"),
        "home_goals", "away_goals"
      ),
      game_nr = as.integer(
        stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
      )
    ) |>
    dplyr::select(
      draw_id = ".draw", "type", "game_nr", "value"
    ) |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  joined <- long |>
    dplyr::inner_join(
      pred_d[, c("game_nr", "match_date", "home_team", "away_team")],
      by = "game_nr"
    )

  tibble::tibble(
    sport      = league$sport,
    country    = league$country,
    sex        = sex,
    fit_date   = as.Date(fit_date),
    match_date = joined$match_date,
    home_team  = joined$home_team,
    away_team  = joined$away_team,
    draw_id    = as.integer(joined$draw_id),
    home_goals = as.numeric(joined$home_goals),
    away_goals = as.numeric(joined$away_goals)
  )
}

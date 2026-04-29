#' @include storage.R
NULL

#' Compute the cutoff date for a per-round Stan fit.
#'
#' Round N of season S is defined as the moment when every team in the top
#' division has played at least N matches in that season. The cutoff date is
#' the latest match date among each top-division team's Nth season match;
#' upstream callers pass it to `prepare_data(end_date = cutoff_date)` so
#' all training matches (any division, any earlier season) on or before that
#' date are included, while the predictive set covers everything after it.
#'
#' Returns `NULL` (with a warning) if the round is not yet complete -- i.e.
#' some top-division team has not yet played its Nth match. Callers should
#' treat that as a skip signal rather than an error.
#'
#' @param results Tibble from `read_table("results")`. Must contain
#'   `season`, `division`, `home_team`, `away_team`, `match_date`.
#' @param season Integer year of the season being analysed.
#' @param round_cutoff Integer N >= 1: the per-team match count.
#' @param top_division Division code identifying the top flight. Default `"BD"`
#'   (Besta deild for Icelandic football).
#' @return Date or NULL.
#' @importFrom rlang .data
#' @export
compute_round_cutoff_date <- function(results,
                                      season,
                                      round_cutoff,
                                      top_division = "BD") {
  stopifnot(is.numeric(round_cutoff), length(round_cutoff) == 1L, round_cutoff >= 1L)
  stopifnot(is.numeric(season), length(season) == 1L)

  td <- results[
    results$season == as.integer(season) & results$division == top_division, ,
    drop = FALSE
  ]

  if (nrow(td) == 0L) {
    cli::cli_alert_warning(
      "No top-division ({top_division}) matches found for season {season}."
    )
    return(NULL)
  }

  long <- dplyr::bind_rows(
    dplyr::transmute(td, team = .data$home_team, match_date = .data$match_date),
    dplyr::transmute(td, team = .data$away_team, match_date = .data$match_date)
  )
  long <- long |>
    dplyr::group_by(.data$team) |>
    dplyr::arrange(.data$match_date) |>
    dplyr::mutate(round = dplyr::row_number()) |>
    dplyr::ungroup()

  all_teams <- unique(long$team)
  nth <- long[long$round == as.integer(round_cutoff), , drop = FALSE]

  missing_teams <- setdiff(all_teams, nth$team)
  if (length(missing_teams) > 0L) {
    cli::cli_alert_warning(
      paste0(
        "Round {round_cutoff} not complete for season {season}: ",
        "{length(missing_teams)} team(s) have not yet played their ",
        "{round_cutoff}th match ({paste(missing_teams, collapse = ', ')})."
      )
    )
    return(NULL)
  }

  max(nth$match_date)
}

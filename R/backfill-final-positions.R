#' @include extract-football-iceland.R simulate-league-season.R round-cutoff.R
NULL

#' Build one round's final-position records from a paired (fit, prep).
#'
#' For each league `target_div`, simulates the remaining season from the fit's
#' latest-round strengths (which, for a fit trained through this round, are the
#' as-of-round strengths) and ranks to season-end placements. Stamps each row
#' with the round's `as_of` cutoff date and the division's own games-played
#' round label.
#'
#' @param fit CmdStan fit trained on games <= `cutoff_date`.
#' @param prep `prepare_data()` output that built `fit`'s `stan_data` (pairing
#'   is required for correct team-index -> name mapping).
#' @param results Played results (will be filtered to `<= cutoff_date`).
#' @param season_schedule Full-season schedule (this season).
#' @param round_idx Integer BD round.
#' @param cutoff_date Date of the round-`round_idx` completion.
#' @param season Integer season.
#' @param target_divs League division codes (no CUP).
#' @param generated_at ISO timestamp string stamped on every row.
#' @return tibble(as_of, generated_at, round, season, division, team,
#'   placement, probability). Empty when no league team is covered.
#' @export
build_round_final_positions <- function(fit, prep, results, season_schedule,
                                        round_idx, cutoff_date, season,
                                        target_divs, generated_at) {
  results <- results[
    !is.na(results$match_date) & results$match_date <= cutoff_date, ,
    drop = FALSE
  ]
  sim_inputs <- .extract_sim_inputs_pfi(fit, prep$teams)

  rows <- lapply(target_divs, function(div) {
    top <- results[results$season == season & results$division == div, , drop = FALSE]
    if (nrow(top) == 0L) {
      return(NULL)
    }
    ctt <- top |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
    if (!all(ctt$team %in% sim_inputs$team$team)) {
      warning(sprintf(
        "round %d %s: %d team(s) lack strength draws; skipping.",
        round_idx, div, sum(!(ctt$team %in% sim_inputs$team$team))
      ), call. = FALSE)
      return(NULL)
    }
    br <- .league_base_and_remaining_pfi(top, ctt, season_schedule, div)
    sim <- simulate_league_season(
      sim_inputs$team, sim_inputs$scalar,
      br$remaining_fixtures, br$base_standings
    )
    # division's own games-played round = min matches any of its teams has played
    div_round <- top |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::count(.data$team) |>
      dplyr::summarise(r = min(.data$n)) |>
      dplyr::pull(.data$r)
    sim$final_positions |>
      dplyr::transmute(
        as_of = format(cutoff_date, "%Y-%m-%d"),
        generated_at = generated_at,
        round = as.integer(div_round),
        season = as.integer(season),
        division = div,
        team = .data$team,
        placement = as.integer(.data$placement),
        probability = .data$probability
      )
  })
  dplyr::bind_rows(rows)
}

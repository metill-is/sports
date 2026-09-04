#' @include publish-profile.R
NULL

# ---- The per-round strength trajectory, shared by all three sports ----------
#
# This helper used to live in R/publish-iceland-league.R under a `_pfi`
# suffix. The suffix was a lie: nothing in it is football-specific. All three
# production models declare the SAME latent surface with the SAME names and
# shapes --
#
#   Stan/football_iceland/bivariate_poisson_no_inflation.stan:188,195
#     array[N_rounds] vector[K] offense / defense
#   Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164
#     array[N_rounds] vector[K] offense / defense
#
# and `home_advantage_off` / `home_advantage_def` are a `vector[K]` in both --
# so one implementation serves football, basketball and handball with no
# variable-name parameterisation at all. The earlier analysis that called a
# per-round trajectory "football-specific" was simply wrong about the 2DT
# models.
#
# The two round indices are DIFFERENT and must not be conflated:
#
#   global_round   each team's cumulative appearance index over the whole
#                  `results` set the model was fit on. This is what indexes
#                  `offense[r, k]` -- R/model-prepare.R builds `round1`/`round2`
#                  the same way (a `row_number()` over the team's matches,
#                  ordered by date).
#   round (output) the team's appearance index WITHIN (current_season,
#                  top_div). This is the published matchweek.
#
# The caller is therefore responsible for handing over the same `results` set
# `prepare_data()` modelled -- a `training_filter`, or a post-season cut applied
# before this call, desynchronises `global_round` from the fit and the
# trajectory silently reads a neighbouring round.

# Build per-(BD-matchweek, team) trajectory of latent strength from a
# single fit. Reads the full `offense[1..N_rounds, K]` and `defense[..]`
# matrices and slices per-team round indices that correspond to each
# team's chronological matches within `top_div` (a vector of division
# codes -- the cell's family, so a split cell's trajectory continues
# through BD_UPPER_PO / BD_LOWER_PO rounds) in the current season.
# Returns a long tibble with (round, .draw, team, component, location,
# value) where `round` is the cell matchweek (1, 2, 3, ...), not the
# model's global per-team round index.
.compute_team_strength_trajectory <- function(fit,
                                              results,
                                              teams,
                                              current_top_teams,
                                              current_season,
                                              top_div) {
  results_played <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
  if (nrow(results_played) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  long_all <- dplyr::bind_rows(
    dplyr::transmute(
      results_played,
      team = .data$home_team,
      match_date = .data$match_date,
      division = .data$division,
      season = .data$season
    ),
    dplyr::transmute(
      results_played,
      team = .data$away_team,
      match_date = .data$match_date,
      division = .data$division,
      season = .data$season
    )
  ) |>
    dplyr::arrange(.data$team, .data$match_date) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(global_round = dplyr::row_number()) |>
    dplyr::ungroup()

  bd_chrono <- long_all |>
    dplyr::filter(
      .data$season == current_season,
      .data$division %in% top_div
    ) |>
    dplyr::arrange(.data$team, .data$match_date) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(bd_matchweek = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::semi_join(current_top_teams, by = "team") |>
    dplyr::inner_join(teams[, c("team", "team_nr")], by = "team") |>
    dplyr::select("team", "team_nr", "bd_matchweek", "global_round")

  if (nrow(bd_chrono) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  rv <- fit$draws(c(
    "offense", "defense", "home_advantage_off", "home_advantage_def"
  )) |>
    posterior::as_draws_rvars()
  off_arr <- posterior::draws_of(rv$offense)
  def_arr <- posterior::draws_of(rv$defense)
  ha_off <- posterior::draws_of(rv$home_advantage_off)
  ha_def <- posterior::draws_of(rv$home_advantage_def)
  n_draws <- dim(off_arr)[1]
  fit_n_rounds <- dim(off_arr)[2]
  fit_n_teams <- dim(off_arr)[3]

  usable <- bd_chrono |>
    dplyr::filter(
      .data$global_round <= fit_n_rounds,
      .data$team_nr <= fit_n_teams
    )
  if (nrow(usable) < nrow(bd_chrono)) {
    warning(sprintf(
      paste0(
        ".compute_team_strength_trajectory: fit covers %d rounds x %d teams; ",
        "%d (matchweek, team) entries fall outside (likely stale fit) -- skipping."
      ),
      fit_n_rounds, fit_n_teams, nrow(bd_chrono) - nrow(usable)
    ))
  }
  if (nrow(usable) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(),
      .draw = integer(),
      component = character(), location = character(),
      value = numeric()
    ))
  }

  rows_long <- vector("list", nrow(usable))
  for (i in seq_len(nrow(usable))) {
    k <- usable$team_nr[i]
    r <- usable$global_round[i]
    mw <- usable$bd_matchweek[i]
    team_name <- usable$team[i]

    off_d <- off_arr[, r, k]
    def_d <- def_arr[, r, k]
    ha_o <- ha_off[, k]
    ha_d <- ha_def[, k]

    offence_away <- off_d
    offence_home <- off_d + ha_o
    defence_away <- def_d
    defence_home <- def_d + ha_d
    total_away <- offence_away + defence_away
    total_home <- offence_home + defence_home
    offence_avg <- (offence_home + offence_away) / 2
    defence_avg <- (defence_home + defence_away) / 2
    total_avg <- (total_home + total_away) / 2

    rows_long[[i]] <- tibble::tibble(
      .draw = rep(seq_len(n_draws), 9L),
      round = mw,
      team = team_name,
      component = rep(
        c(
          "offence", "offence", "offence",
          "defence", "defence", "defence",
          "total", "total", "total"
        ),
        each = n_draws
      ),
      location = rep(
        c(
          "home", "away", "avg",
          "home", "away", "avg",
          "home", "away", "avg"
        ),
        each = n_draws
      ),
      value = c(
        offence_home, offence_away, offence_avg,
        defence_home, defence_away, defence_avg,
        total_home, total_away, total_avg
      )
    )
  }

  dplyr::bind_rows(rows_long)
}

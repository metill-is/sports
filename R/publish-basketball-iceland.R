#' @include model-prepare.R storage.R config.R
NULL

#' Publish basketball Iceland JSONs (Plan 4 scaffold).
#'
#' Produces a minimal pair of JSONs (`meta.json` + `next_games.json`) for
#' basketball Iceland into `output_root/basketball/iceland/{karla,kvenna}/`.
#' League-table / season-projection / team-strengths JSONs are deferred to
#' Plan 6 -- they need the metill-platform basketball page to be designed
#' first.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with sport == "basketball" and country == "iceland".
#' @param sex `"male"` or `"female"`.
#' @param end_date Date for filtering. Default `Sys.Date()`.
#' @param root Data root. Default `here::here("data")`.
#' @param output_root Publish root. Default `here::here("data", "publish")`.
#' @return `invisible(NULL)`
#' @importFrom rlang .data
#' @export
publish_basketball_iceland <- function(fit, league, sex,
                                       end_date = Sys.Date(),
                                       root = here::here("data"),
                                       output_root = here::here("data", "publish")) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(league$sport == "basketball", league$country == "iceland")
  stopifnot(inherits(end_date, "Date"))

  sex_folder <- if (sex == "male") "karla" else "kvenna"
  out_dir <- file.path(output_root, "basketball", "iceland", sex_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  prep <- prepare_data(league, sex, end_date = end_date, root = root)

  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  # ---- meta.json -----------------------------------------------------------
  meta <- list(
    sport        = "basketball",
    sex          = sex,
    league       = "B\u00f3nusdeild",
    season       = max(prep$stan_data$season, na.rm = TRUE),
    generated_at = generated_at,
    fit_date     = format(end_date, "%Y-%m-%d"),
    n_draws      = as.integer(posterior::ndraws(fit$draws("lp__")))
  )
  jsonlite::write_json(
    meta, file.path(out_dir, "meta.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- next_games.json -----------------------------------------------------
  if (nrow(prep$pred_d) > 0L) {
    pred_long <- posterior::as_draws_df(
      fit$draws(c("goals1_pred", "goals2_pred"))
    ) |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(
        -c(".chain", ".iteration", ".draw"),
        names_to  = "parameter",
        values_to = "value"
      ) |>
      dplyr::mutate(
        type = dplyr::if_else(
          stringr::str_detect(.data$parameter, "^goals1_pred"),
          "home_score", "away_score"
        ),
        game_nr = as.integer(
          stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2]
        )
      ) |>
      dplyr::select(".draw", "type", "game_nr", "value") |>
      tidyr::pivot_wider(names_from = "type", values_from = "value") |>
      dplyr::inner_join(
        prep$pred_d[, c("game_nr", "match_date", "home_team", "away_team")],
        by = "game_nr"
      )

    matches <- pred_long |>
      dplyr::group_by(.data$match_date, .data$home_team, .data$away_team) |>
      dplyr::summarise(
        mean_home = mean(.data$home_score),
        mean_away = mean(.data$away_score),
        mean_diff = mean(.data$home_score - .data$away_score),
        p_home    = mean(.data$home_score > .data$away_score),
        p_away    = mean(.data$home_score < .data$away_score),
        p_tie     = mean(.data$home_score == .data$away_score),
        .groups   = "drop"
      )

    next_games <- list(
      generated_at = generated_at,
      matches = lapply(seq_len(nrow(matches)), function(i) {
        list(
          date      = format(matches$match_date[i], "%Y-%m-%d"),
          home      = matches$home_team[i],
          away      = matches$away_team[i],
          mean_home = round(matches$mean_home[i], 2),
          mean_away = round(matches$mean_away[i], 2),
          mean_diff = round(matches$mean_diff[i], 2),
          p_home    = round(matches$p_home[i], 4),
          p_away    = round(matches$p_away[i], 4),
          p_tie     = round(matches$p_tie[i], 4)
        )
      })
    )
  } else {
    next_games <- list(
      generated_at = generated_at,
      matches      = list()
    )
  }

  jsonlite::write_json(
    next_games, file.path(out_dir, "next_games.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # TODO Plan 6: standings.json, team_strengths.json, final_positions.json,
  # points_distribution.json -- deferred until metill-platform basketball
  # page is designed.

  message(sprintf(
    "publish_basketball_iceland: wrote 2 JSONs to %s", out_dir
  ))
  invisible(NULL)
}

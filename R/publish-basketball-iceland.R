#' @include model-prepare.R storage.R config.R publish-iceland-2dt-helpers.R
NULL

#' Publish basketball Iceland JSONs.
#'
#' Writes seven snapshot JSON files into the karla and kvenna subfolders of
#' `output_root/basketball/iceland/`:
#'   - `meta.json`
#'   - `next_games.json`
#'   - `standings.json`
#'   - `team_strengths.json`
#'   - `final_positions.json`
#'   - `points_distribution.json`
#'   - `home_advantage.json`
#'
#' Plus one accretive history file written when there's relevant data:
#'   - `final_positions_history.json`    (per-round trajectory of final-standings projection)
#'
#' Mirrors the football Iceland publish surface modulo two structural
#' differences imposed by the 2D Student-t model:
#'   - No draws -- probability schemas use only p_home and p_away (basketball
#'     is W/L only since OT resolves; the rare exact-tie probability from
#'     continuous Student-t draws is rolled into `p_tie` for back-compat).
#'   - No goals-process xG -- standings rows ship null `xg_for`/`xg_against`
#'     /`xpts`. The platform JS gates the expected-columns block on these.
#'
#' Top division: Bonusdeild (`BD`).
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
  teams <- prep$teams
  pred_d <- prep$pred_d

  # Top-division code in storage schema is `BD` (Bonusdeild).
  top_div <- "BD"

  # Played results (current + historical seasons), trimmed to end_date.
  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[
    !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]

  current_season <- if (nrow(results) > 0L) {
    max(results$season, na.rm = TRUE)
  } else {
    as.integer(format(end_date, "%Y"))
  }

  top_results <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ]

  # Current top-division teams (for strengths / home-advantage filter).
  current_top_teams <- if (nrow(top_results) > 0L) {
    top_results |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
  } else {
    tibble::tibble(team = character())
  }

  top_teams_upcoming <- if (nrow(pred_d) > 0L && "division" %in% names(pred_d)) {
    pred_d[pred_d$division == top_div, , drop = FALSE] |>
      dplyr::select("home_team", "away_team") |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::distinct(.data$team)
  } else {
    tibble::tibble(team = character())
  }
  if (nrow(top_teams_upcoming) == 0L) {
    top_teams_upcoming <- current_top_teams
  }

  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  round_num <- .compute_round_num_2dt(top_results)
  has_ties <- isTRUE(league$betting$scoring$has_ties)
  tie_threshold <- if (is.null(league$betting$scoring$tie_threshold)) {
    0
  } else {
    league$betting$scoring$tie_threshold
  }

  posterior_goals <- .compute_posterior_goals_2dt(fit, pred_d)

  # ---- meta.json -----------------------------------------------------------
  n_draws <- as.integer(posterior::ndraws(fit$draws("lp__")))
  meta <- list(
    sport        = "basketball",
    sex          = sex,
    league       = "Bónusdeild",
    season       = current_season,
    generated_at = generated_at,
    fit_date     = format(end_date, "%Y-%m-%d"),
    round        = as.integer(round_num),
    n_draws      = n_draws
  )
  write_json_consistent(
    meta, file.path(out_dir, "meta.json"),
    auto_unbox = TRUE, pretty = TRUE
  )

  # ---- next_games.json -----------------------------------------------------
  if (nrow(posterior_goals) > 0L) {
    next_games_out <- posterior_goals |>
      dplyr::filter(
        .data$match_date >= end_date,
        .data$match_date <= end_date + 14L
      ) |>
      dplyr::mutate(score_diff = .data$home_score - .data$away_score) |>
      dplyr::summarise(
        mean_home = mean(.data$home_score),
        mean_away = mean(.data$away_score),
        mean_diff = mean(.data$score_diff),
        p_home = mean(.data$score_diff > 0),
        p_away = mean(.data$score_diff < 0),
        p_tie = mean(.data$score_diff == 0),
        .by = c("game_nr", "division", "match_date", "home_team", "away_team")
      ) |>
      dplyr::arrange(.data$match_date, .data$game_nr) |>
      dplyr::mutate(date = format(.data$match_date, "%Y-%m-%d")) |>
      dplyr::select(
        "date", "division",
        home = "home_team", away = "away_team",
        "mean_home", "mean_away", "mean_diff",
        "p_home", "p_away", "p_tie"
      )
  } else {
    next_games_out <- tibble::tibble(
      date = character(), division = character(),
      home = character(), away = character(),
      mean_home = numeric(), mean_away = numeric(), mean_diff = numeric(),
      p_home = numeric(), p_away = numeric(), p_tie = numeric()
    )
  }
  write_json_consistent(
    list(generated_at = generated_at, matches = next_games_out),
    file.path(out_dir, "next_games.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )

  # ---- standings.json ------------------------------------------------------
  standings_rows <- .compute_standings_rows_2dt(
    top_results,
    has_ties = has_ties, tie_threshold = tie_threshold
  )
  write_json_consistent(
    list(
      generated_at = generated_at,
      season = current_season,
      as_of = if (nrow(top_results) > 0L) {
        format(max(top_results$match_date), "%Y-%m-%d")
      } else {
        format(end_date, "%Y-%m-%d")
      },
      rows = standings_rows
    ),
    file.path(out_dir, "standings.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )

  # ---- team_strengths.json -------------------------------------------------
  team_strengths_draws <- dplyr::bind_rows(
    .extract_team_draws_2dt(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_2dt(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_2dt(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_2dt(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_2dt(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_2dt(fit, "cur_strength_away", teams, "total", "away")
  )

  # WHY: per-draw home/away average for the location selector. Mirrors the
  # football publisher; averaging happens before quantile summarisation so
  # uncertainty intervals on `avg` reflect the joint posterior, not a
  # post-hoc point average.
  team_strengths_avg <- team_strengths_draws |>
    dplyr::summarise(
      value = mean(.data$value),
      .by = c(".draw", "team", "component")
    ) |>
    dplyr::mutate(location = "avg")

  team_strengths <- dplyr::bind_rows(
    team_strengths_draws,
    team_strengths_avg
  ) |>
    .summarise_team_intervals_2dt() |>
    dplyr::semi_join(current_top_teams, by = "team")

  write_json_consistent(
    list(generated_at = generated_at, records = team_strengths),
    file.path(out_dir, "team_strengths.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  # ---- final_positions.json + points_distribution.json --------------------
  base_points <- .compute_base_points_2dt(
    top_results,
    has_ties = has_ties, tie_threshold = tie_threshold
  )

  iter_team_points <- .compute_iter_team_points_2dt(
    posterior_goals, top_div, base_points,
    has_ties = has_ties, tie_threshold = tie_threshold
  )

  if (nrow(iter_team_points) > 0L) {
    iter_positions <- iter_team_points |>
      dplyr::arrange(.data$.draw, dplyr::desc(.data$points)) |>
      dplyr::mutate(placement = dplyr::row_number(), .by = ".draw")

    n_teams_top <- iter_positions |>
      dplyr::distinct(.data$team) |>
      nrow()

    final_positions <- iter_positions |>
      dplyr::count(.data$team, .data$placement) |>
      tidyr::complete(
        team,
        placement = seq_len(n_teams_top),
        fill = list(n = 0)
      ) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "placement", "probability") |>
      dplyr::arrange(.data$team, .data$placement)

    placement_summary <- iter_positions |>
      dplyr::summarise(
        p_top_six = mean(.data$placement <= 6L),
        p_winner = mean(.data$placement == 1L),
        p_relegation = mean(.data$placement >= n_teams_top - 1L),
        .by = "team"
      )

    write_json_consistent(
      list(
        generated_at = generated_at,
        season       = current_season,
        n_teams      = n_teams_top,
        records      = final_positions,
        summary      = placement_summary
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )

    final_positions_history_row <- final_positions |>
      dplyr::mutate(
        as_of = if (nrow(top_results) > 0L) {
          format(max(top_results$match_date), "%Y-%m-%d")
        } else {
          format(end_date, "%Y-%m-%d")
        },
        generated_at = generated_at,
        round = as.integer(round_num),
        season = current_season
      ) |>
      dplyr::select(
        "as_of", "generated_at", "round", "season",
        "team", "placement", "probability"
      )
    .append_to_history_pfi(
      file.path(out_dir, "final_positions_history.json"),
      final_positions_history_row,
      key_cols = c("as_of", "team", "placement")
    )

    points_distribution <- iter_team_points |>
      dplyr::count(.data$team, .data$points) |>
      dplyr::mutate(
        probability = .data$n / sum(.data$n),
        .by = "team"
      ) |>
      dplyr::select("team", "points", "probability") |>
      dplyr::arrange(.data$team, .data$points)

    points_summary <- iter_team_points |>
      dplyr::summarise(
        mean_points = mean(.data$points),
        median_points = stats::median(.data$points),
        lower_80 = stats::quantile(.data$points, 0.1),
        upper_80 = stats::quantile(.data$points, 0.9),
        .by = "team"
      ) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(base_points = dplyr::coalesce(.data$base_points, 0L)) |>
      dplyr::left_join(placement_summary, by = "team")

    write_json_consistent(
      list(
        generated_at = generated_at,
        season       = current_season,
        records      = points_distribution,
        summary      = points_summary
      ),
      file.path(out_dir, "points_distribution.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
  } else {
    write_json_consistent(
      list(
        generated_at = generated_at, season = current_season,
        n_teams = 0L, records = list(), summary = list()
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
    write_json_consistent(
      list(
        generated_at = generated_at, season = current_season,
        records = list(), summary = list()
      ),
      file.path(out_dir, "points_distribution.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
    final_positions_history_path <- file.path(
      out_dir, "final_positions_history.json"
    )
    if (!file.exists(final_positions_history_path)) {
      write_json_consistent(
        list(schema_version = 1L, records = list()),
        final_positions_history_path,
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    }
  }

  # ---- home_advantage.json -------------------------------------------------
  home_advantage <- .compute_home_advantage_2dt(fit, teams, top_teams_upcoming)
  write_json_consistent(
    list(generated_at = generated_at, records = home_advantage),
    file.path(out_dir, "home_advantage.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  n_files <- length(list.files(out_dir, pattern = "\\.json$"))
  message(sprintf(
    "publish_basketball_iceland: wrote %d JSONs to %s", n_files, out_dir
  ))
  invisible(NULL)
}

#' @include model-prepare.R storage.R config.R
NULL

# ---- Internal helpers --------------------------------------------------------


# Extract per-team draws for a single Stan parameter vector indexed by team.
.extract_team_draws_pfi <- function(fit, var, teams, component, location) {
  fit$draws(var) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      c(-".chain", -".draw", -".iteration"),
      names_to = "name", values_to = "value"
    ) |>
    dplyr::mutate(
      team_idx  = as.integer(readr::parse_number(.data$name)),
      team      = teams$team[.data$team_idx],
      component = component,
      location  = location
    )
}

.summarise_team_intervals_pfi <- function(draws, coverages = c(0.5, 0.8, 0.95)) {
  draws |>
    dplyr::reframe(
      median = stats::median(.data$value),
      coverage = coverages,
      lower = stats::quantile(.data$value, 0.5 - coverages / 2),
      upper = stats::quantile(.data$value, 0.5 + coverages / 2),
      .by = c("team", "component", "location")
    )
}

# Append `new_rows` to a history JSON file, dedup on `key_cols` keeping the
# row with the most recent `generated_at`. Creates the file on first call.
# Stored shape: { "schema_version": 1, "records": [ {...}, ... ] }.
# Tolerates missing or malformed existing files (treated as empty history).
.append_to_history_pfi <- function(path, new_rows, key_cols) {
  existing <- if (file.exists(path)) {
    tryCatch(
      {
        parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
        records <- parsed$records
        if (is.data.frame(records)) tibble::as_tibble(records) else tibble::tibble()
      },
      error = function(e) tibble::tibble()
    )
  } else {
    tibble::tibble()
  }

  all_rows <- dplyr::bind_rows(existing, new_rows) |>
    dplyr::arrange(dplyr::desc(.data$generated_at)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(key_cols)), .keep_all = TRUE) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(key_cols)))

  jsonlite::write_json(
    list(schema_version = 1L, records = all_rows),
    path,
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = 5,
    na = "null"
  )
}

# Assign each match a "matchweek" derived from team-chronological match counts.
# matchweek(m) = max(home_team_chrono_idx_after_m, away_team_chrono_idx_after_m).
# A team's chrono_idx is its 1-based position when its played matches are
# sorted by match_date. For perfectly synchronised round-robins this gives
# the league matchweek; for postponed matches, the rescheduled match is
# keyed to its team-chronological round (not its calendar position).
# Output: input tibble + integer column `matchweek`. Input row order is preserved.
.assign_matchweeks_pfi <- function(matches) {
  if (nrow(matches) == 0L) {
    out <- tibble::as_tibble(matches)
    out$matchweek <- integer(0)
    return(out)
  }

  ordered <- matches |>
    dplyr::mutate(.input_idx = dplyr::row_number()) |>
    dplyr::arrange(.data$match_date, .data$.input_idx) |>
    dplyr::mutate(.chrono_idx = dplyr::row_number())

  long <- dplyr::bind_rows(
    ordered |> dplyr::transmute(
      .data$.input_idx, .data$.chrono_idx,
      team = .data$home_team
    ),
    ordered |> dplyr::transmute(
      .data$.input_idx, .data$.chrono_idx,
      team = .data$away_team
    )
  ) |>
    dplyr::arrange(.data$.chrono_idx, .data$.input_idx) |>
    dplyr::group_by(.data$team) |>
    dplyr::mutate(team_idx = dplyr::row_number()) |>
    dplyr::ungroup()

  per_match <- long |>
    dplyr::summarise(
      matchweek = as.integer(max(.data$team_idx)),
      .by = ".input_idx"
    )

  matches |>
    dplyr::mutate(.input_idx = dplyr::row_number()) |>
    dplyr::left_join(per_match, by = ".input_idx") |>
    dplyr::select(-".input_idx") |>
    tibble::as_tibble()
}

# Find the latest fit_date partition strictly less than `target_date` under
# data/beliefs/archive/sport=X/country=Y/sex=Z/. Returns the parquet path
# (or NULL if no such partition exists).
.find_pre_round_fit_path_pfi <- function(archive_root, sport, country, sex, target_date) {
  base <- file.path(
    archive_root,
    paste0("sport=", sport),
    paste0("country=", country),
    paste0("sex=", sex)
  )
  if (!dir.exists(base)) {
    return(NULL)
  }

  parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
  fit_dirs <- parts[grepl("/fit_date=", parts)]
  if (length(fit_dirs) == 0L) {
    return(NULL)
  }

  fit_dates <- as.Date(sub(".*fit_date=", "", fit_dirs))
  candidates <- fit_dirs[fit_dates < target_date]
  if (length(candidates) == 0L) {
    return(NULL)
  }

  latest <- candidates[which.max(as.Date(sub(".*fit_date=", "", candidates)))]
  files <- list.files(latest, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0L) {
    return(NULL)
  }
  files[1L]
}

# Aggregate per-(round, team) frozen pre-round predictions for played matches.
# For each played match, find the latest archived fit strictly before its
# matchweek's first kickoff, read that fit's posterior draws for the match,
# and compute team-side xG_for / xG_against / xPts. Aggregates per
# (round, team) by summing across the team's matches in that round.
#
# Returns a tibble with columns:
#   round (int), team (chr), fit_date (chr), n_matches (int),
#   xg_for (dbl), xg_against (dbl), xpts (dbl),
#   p_win (dbl), p_draw (dbl), p_loss (dbl)
#
# Matches whose matchweek has no pre-round archive partition are silently
# skipped -- pre-archive early-season rounds simply do not appear.
.aggregate_round_predictions_pfi <- function(played_matches, archive_root,
                                             sport, country, sex) {
  if (nrow(played_matches) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(), fit_date = character(),
      n_matches = integer(),
      xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
      p_win = numeric(), p_draw = numeric(), p_loss = numeric()
    ))
  }

  with_mw <- .assign_matchweeks_pfi(played_matches)

  per_round <- with_mw |>
    dplyr::summarise(
      first_kickoff = min(.data$match_date),
      .by = "matchweek"
    )

  rounds <- vector("list", nrow(per_round))

  for (i in seq_len(nrow(per_round))) {
    mw <- per_round$matchweek[i]
    target <- per_round$first_kickoff[i]

    fit_path <- .find_pre_round_fit_path_pfi(
      archive_root = archive_root,
      sport = sport, country = country, sex = sex,
      target_date = target
    )
    if (is.null(fit_path)) next

    round_matches <- with_mw[with_mw$matchweek == mw, ]
    fit_date_chr <- sub(".*fit_date=([^/]+)/.*", "\\1", fit_path)

    beliefs <- arrow::read_parquet(fit_path) |>
      dplyr::semi_join(
        round_matches |> dplyr::select(
          "home_team", "away_team", "match_date"
        ),
        by = c("home_team", "away_team", "match_date")
      )

    if (nrow(beliefs) == 0L) next

    per_match <- beliefs |>
      dplyr::summarise(
        xg_home = mean(.data$home_goals),
        xg_away = mean(.data$away_goals),
        p_home_win = mean(.data$home_goals > .data$away_goals),
        p_draw_match = mean(.data$home_goals == .data$away_goals),
        p_away_win = mean(.data$home_goals < .data$away_goals),
        .by = c("home_team", "away_team", "match_date")
      ) |>
      dplyr::mutate(
        xpts_home = 3 * .data$p_home_win + .data$p_draw_match,
        xpts_away = 3 * .data$p_away_win + .data$p_draw_match
      )

    home_side <- per_match |>
      dplyr::transmute(
        team = .data$home_team,
        xg_for = .data$xg_home, xg_against = .data$xg_away,
        xpts = .data$xpts_home,
        p_win = .data$p_home_win, p_draw = .data$p_draw_match,
        p_loss = .data$p_away_win
      )
    away_side <- per_match |>
      dplyr::transmute(
        team = .data$away_team,
        xg_for = .data$xg_away, xg_against = .data$xg_home,
        xpts = .data$xpts_away,
        p_win = .data$p_away_win, p_draw = .data$p_draw_match,
        p_loss = .data$p_home_win
      )

    rounds[[i]] <- dplyr::bind_rows(home_side, away_side) |>
      dplyr::summarise(
        n_matches = dplyr::n(),
        xg_for = sum(.data$xg_for),
        xg_against = sum(.data$xg_against),
        xpts = sum(.data$xpts),
        p_win = mean(.data$p_win),
        p_draw = mean(.data$p_draw),
        p_loss = mean(.data$p_loss),
        .by = "team"
      ) |>
      dplyr::mutate(
        round = as.integer(mw),
        fit_date = fit_date_chr
      )
  }

  result <- dplyr::bind_rows(rounds)
  if (nrow(result) == 0L) {
    return(tibble::tibble(
      round = integer(), team = character(), fit_date = character(),
      n_matches = integer(),
      xg_for = numeric(), xg_against = numeric(), xpts = numeric(),
      p_win = numeric(), p_draw = numeric(), p_loss = numeric()
    ))
  }

  result |>
    dplyr::mutate(n_matches = as.integer(.data$n_matches)) |>
    dplyr::select(
      "round", "team", "fit_date", "n_matches",
      "xg_for", "xg_against", "xpts",
      "p_win", "p_draw", "p_loss"
    ) |>
    dplyr::arrange(.data$round, .data$team)
}

# ---- Public API --------------------------------------------------------------

#' Publish football Iceland posterior summaries as JSON
#'
#' Consumes a CmdStanMCMC fit from `fit_model()` / `fit_league()` and writes
#' seven JSON files into `output_root/football/iceland/{karla|kvenna}/`:
#'   - `meta.json`
#'   - `next_games.json`
#'   - `standings.json`
#'   - `team_strengths.json`
#'   - `final_positions.json`
#'   - `points_distribution.json`
#'   - `home_advantage.json`
#'
#' Plus three accretive history files written when there's relevant data:
#'   - `team_strengths_history.json`     (every fit)
#'   - `standings_history.json`          (every fit with played top-flight matches)
#'   - `round_predictions_history.json`  (every fit; empty `records` if archive not yet populated)
#'
#' @param fit CmdStanMCMC returned by `fit_model()`.  Must have been trained on
#'   data consistent with `(league, sex, end_date)`.
#' @param league A single entry from `load_leagues()` (must have `sport` and
#'   `country` set).
#' @param sex `"male"` or `"female"`.
#' @param end_date Training cutoff passed to `prepare_data()`. Default `Sys.Date()`.
#' @param root Data root for `read_table()`. Default `here::here("data")`.
#' @param output_root Root for JSON output. Default `here::here("data", "publish")`.
#' @param archive_root Root of the beliefs archive used to source frozen
#'   pre-round xG / xPts predictions. Default `here::here("data", "beliefs", "archive")`.
#' @return `invisible(NULL)`.
#' @importFrom rlang .data
#' @export
publish_football_iceland <- function(fit,
                                     league,
                                     sex,
                                     end_date = Sys.Date(),
                                     root = here::here("data"),
                                     output_root = here::here("data", "publish"),
                                     archive_root = here::here(
                                       "data", "beliefs", "archive"
                                     )) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))
  stopifnot(league$sport == "football", league$country == "iceland")
  stopifnot(inherits(end_date, "Date"))

  # Icelandic sex folder names
  sex_folder <- if (sex == "male") "karla" else "kvenna"
  out_dir <- file.path(output_root, "football", "iceland", sex_folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # -- Rebuild prep data -------------------------------------------------------
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  teams <- prep$teams
  pred_d <- prep$pred_d # next-game matches with home_team / away_team / match_date / division

  # -- Training results --------------------------------------------------------
  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[results$match_date <= end_date, , drop = FALSE]
  results <- results[!is.na(results$home_score) & !is.na(results$away_score), , drop = FALSE]

  # Top division label in the new schema
  top_div <- "BD"

  current_season <- max(results$season, na.rm = TRUE)

  # -- Current top-division teams (for strengths / home-advantage filter) -----
  current_top_teams <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ] |>
    dplyr::select("home_team", "away_team") |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::distinct(.data$team)

  # Top teams with upcoming BD fixtures (for home_advantage filter)
  top_teams_upcoming <- pred_d[pred_d$division == top_div, , drop = FALSE] |>
    dplyr::select("home_team", "away_team") |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::distinct(.data$team)

  # Fallback: if no upcoming BD fixtures, use current-season BD teams
  if (nrow(top_teams_upcoming) == 0L) {
    top_teams_upcoming <- current_top_teams
  }

  # -- Frozen pre-round xG / xPts -------------------------------------------
  # For each played top-flight match, look up the latest archived fit strictly
  # before that matchweek's first kickoff and use its posterior to compute xG
  # / xPts. Per-team aggregates that include the full set of played
  # matchweeks are exposed in standings.json; partial-coverage aggregates are
  # NA (the standings table prefers honest blanks to retroactively-improved
  # numbers). The per-(round, team) detail is appended to
  # round_predictions_history.json regardless.
  bd_played <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ]
  round_predictions <- .aggregate_round_predictions_pfi(
    played_matches = bd_played[, c("home_team", "away_team", "match_date")],
    archive_root = archive_root,
    sport = league$sport, country = league$country, sex = sex
  )

  team_expected <- if (nrow(bd_played) > 0L) {
    played_per_team <- bd_played |>
      tidyr::pivot_longer(
        c("home_team", "away_team"),
        values_to = "team"
      ) |>
      dplyr::count(.data$team, name = "played_count")

    if (nrow(round_predictions) == 0L) {
      played_per_team |>
        dplyr::transmute(
          .data$team,
          xg_for = NA_real_,
          xg_against = NA_real_,
          xpts = NA_real_,
          xg_trend = list(numeric(0))
        )
    } else {
      team_pred <- round_predictions |>
        dplyr::arrange(.data$round) |>
        dplyr::summarise(
          n_predicted = sum(.data$n_matches),
          xg_for_sum = sum(.data$xg_for),
          xg_against_sum = sum(.data$xg_against),
          xpts_sum = sum(.data$xpts),
          xg_trend = list(.data$xg_for),
          .by = "team"
        )

      played_per_team |>
        dplyr::left_join(team_pred, by = "team") |>
        dplyr::mutate(
          full_coverage = !is.na(.data$n_predicted) &
            .data$n_predicted == .data$played_count,
          xg_for = dplyr::if_else(
            .data$full_coverage, .data$xg_for_sum, NA_real_
          ),
          xg_against = dplyr::if_else(
            .data$full_coverage, .data$xg_against_sum, NA_real_
          ),
          xpts = dplyr::if_else(
            .data$full_coverage, .data$xpts_sum, NA_real_
          ),
          xg_trend = lapply(.data$xg_trend, function(x) {
            if (is.null(x)) numeric(0) else x
          })
        ) |>
        dplyr::select(
          "team", "xg_for", "xg_against", "xpts", "xg_trend"
        )
    }
  } else {
    NULL
  }

  # -- Posterior goals draws --------------------------------------------------
  posterior_goals_raw <- fit$draws(c("goals1_pred", "goals2_pred")) |>
    posterior::as_draws_df() |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      -c(".draw", ".chain", ".iteration"),
      names_to  = "parameter",
      values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(
        stringr::str_detect(.data$parameter, "goals1"), "home_goals", "away_goals"
      ),
      game_nr = as.integer(stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2])
    ) |>
    dplyr::select(".draw", "type", "game_nr", "value") |>
    tidyr::pivot_wider(names_from = "type", values_from = "value")

  # Validate pred dimension matches
  n_pred_fit <- max(posterior_goals_raw$game_nr, na.rm = TRUE)
  n_pred_data <- nrow(pred_d)
  if (n_pred_fit != n_pred_data) {
    warning(sprintf(
      paste0(
        "publish_football_iceland: fit was trained with N_pred=%d prediction matches ",
        "but prepare_data returned %d. JSONs that depend on posterior_goals will be empty."
      ),
      n_pred_fit, n_pred_data
    ))
    posterior_goals <- tibble::tibble(
      .draw = integer(), game_nr = integer(),
      home_goals = numeric(), away_goals = numeric(),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      division = character()
    )
  } else {
    posterior_goals <- posterior_goals_raw |>
      dplyr::inner_join(
        pred_d[, c("game_nr", "match_date", "home_team", "away_team", "division")],
        by = "game_nr"
      )
  }

  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  # ---- meta.json ------------------------------------------------------------

  round_num <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ] |>
    tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
    dplyr::count(.data$team) |>
    dplyr::pull("n") |>
    (\(x) if (length(x) == 0L) 0L else min(x))()

  n_draws <- posterior::ndraws(fit$draws("home_advantage_tot"))

  meta <- list(
    sex          = sex,
    league       = "Besta deild",
    season       = current_season,
    generated_at = generated_at,
    fit_date     = format(end_date, "%Y-%m-%d"),
    round        = as.integer(round_num),
    n_draws      = as.integer(n_draws)
  )
  jsonlite::write_json(
    meta,
    file.path(out_dir, "meta.json"),
    auto_unbox = TRUE
  )

  # ---- next_games.json ------------------------------------------------------

  male_top_division_venues <- tibble::tribble(
    ~team, ~venue,
    "Brei\u00f0ablik", "K\u00f3pavogsv\u00f6llur",
    "FH", "Kaplakrikav\u00f6llur",
    "Fram", "Laugardalsv\u00f6llur",
    "KA", "KA-v\u00f6llurinn",
    "KR", "KR-v\u00f6llur",
    "Keflav\u00edk", "Nettov\u00f6llurinn",
    "Stjarnan", "Stj\u00f6rnuv\u00f6llur",
    "Valur", "Hl\u00ed\u00f0arendi",
    "V\u00edkingur R.", "V\u00edkingsv\u00f6llur",
    "\u00cdA", "Nor\u00f0ur\u00e1lsv\u00f6llurinn",
    "\u00cdBV", "H\u00e1steinv\u00f6llur",
    "\u00de\u00f3r", "\u00de\u00f3rsv\u00f6llur"
  )

  # Division display codes (BD = Besta deild = division 1)
  division_labels <- c(
    BD = "BD", LD1 = "LD", LD2 = "\u00d6D", LD3 = "\u00deD",
    LD4 = "FjD", CUP = "MB",
    BD_UPPER_PO = "BD PO", BD_LOWER_PO = "BD N-PO",
    LD1_PO = "LD PO"
  )

  if (nrow(posterior_goals) > 0L) {
    next_games_out <- posterior_goals |>
      dplyr::filter(
        .data$match_date >= end_date,
        .data$match_date <= end_date + 14L
      ) |>
      dplyr::mutate(goal_diff = .data$home_goals - .data$away_goals) |>
      dplyr::summarise(
        mean_home_goals = mean(.data$home_goals),
        mean_away_goals = mean(.data$away_goals),
        mean_goal_diff = mean(.data$goal_diff),
        p_home_win = mean(.data$goal_diff > 0),
        p_draw = mean(.data$goal_diff == 0),
        p_away_win = mean(.data$goal_diff < 0),
        # NB: tibble::tibble() has no data-mask context, so .data$goal_diff
        # would fail with "Column `goal_diff` not found in `.data`". The bare
        # `goal_diff` resolves via summarise()'s outer mask before the call.
        goal_diff_distribution = list(
          tibble::tibble(diff = goal_diff) |>
            dplyr::count(.data$diff) |>
            dplyr::mutate(p = .data$n / sum(.data$n)) |>
            dplyr::select("diff", "p")
        ),
        .by = c("game_nr", "division", "match_date", "home_team", "away_team")
      ) |>
      dplyr::arrange(.data$match_date, .data$game_nr) |>
      dplyr::left_join(male_top_division_venues, by = c("home_team" = "team")) |>
      dplyr::mutate(
        division_code = dplyr::recode(.data$division, !!!division_labels, .default = .data$division),
        date          = format(.data$match_date, "%Y-%m-%d")
      ) |>
      dplyr::select(
        "date", "venue", "division", "division_code",
        home = "home_team", away = "away_team",
        "mean_home_goals", "mean_away_goals", "mean_goal_diff",
        "p_home_win", "p_draw", "p_away_win",
        "goal_diff_distribution"
      )
  } else {
    next_games_out <- tibble::tibble(
      date = character(), venue = character(),
      division = character(), division_code = character(),
      home = character(), away = character(),
      mean_home_goals = numeric(), mean_away_goals = numeric(),
      mean_goal_diff = numeric(), p_home_win = numeric(),
      p_draw = numeric(), p_away_win = numeric(),
      goal_diff_distribution = list()
    )
  }

  jsonlite::write_json(
    list(generated_at = generated_at, matches = next_games_out),
    file.path(out_dir, "next_games.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
  )

  # ---- standings.json + standings_history.json -----------------------------
  # Both sexes get a standings.json (empty `rows` if no top-flight matches
  # have been played yet). standings_history.json is only appended when
  # there are played matches -- empty-snapshot rows would be noise.

  bd_results <- results[
    results$season == current_season & results$division == top_div, ,
    drop = FALSE
  ]

  pad_form <- function(x, n = 5L) {
    tail(c(rep(NA_character_, n), x), n)
  }

  short_code <- function(team) {
    team |>
      stringr::str_remove_all("\\s|\\.") |>
      stringr::str_to_upper() |>
      stringr::str_sub(1L, 3L)
  }

  if (nrow(bd_results) > 0L) {
    long_bd <- dplyr::bind_rows(
      dplyr::transmute(bd_results,
        team = .data$home_team, match_date = .data$match_date,
        gf = .data$home_score, ga = .data$away_score
      ),
      dplyr::transmute(bd_results,
        team = .data$away_team, match_date = .data$match_date,
        gf = .data$away_score, ga = .data$home_score
      )
    ) |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$gf > .data$ga ~ "W",
          .data$gf < .data$ga ~ "L",
          TRUE ~ "D"
        )
      ) |>
      dplyr::arrange(.data$team, .data$match_date)

    standings_rows <- long_bd |>
      dplyr::summarise(
        played = dplyr::n(),
        wins = sum(.data$result == "W"),
        draws = sum(.data$result == "D"),
        losses = sum(.data$result == "L"),
        goals_for = sum(.data$gf),
        goals_against = sum(.data$ga),
        goal_diff = .data$goals_for - .data$goals_against,
        points = 3L * .data$wins + .data$draws,
        form = list(pad_form(tail(.data$result, 5L))),
        .by = "team"
      ) |>
      dplyr::arrange(
        dplyr::desc(.data$points), dplyr::desc(.data$goal_diff),
        dplyr::desc(.data$goals_for)
      ) |>
      dplyr::mutate(
        rank  = dplyr::row_number(),
        short = short_code(.data$team)
      )

    if (!is.null(team_expected)) {
      standings_rows <- standings_rows |>
        dplyr::left_join(team_expected, by = "team") |>
        dplyr::mutate(
          xg_trend = lapply(.data$xg_trend, function(x) {
            if (is.null(x)) numeric(0) else x
          })
        )
    } else {
      standings_rows <- standings_rows |>
        dplyr::mutate(
          xg_for = NA_real_, xg_against = NA_real_, xpts = NA_real_,
          xg_trend = list(numeric(0))
        )
    }

    standings_rows <- standings_rows |>
      dplyr::select(
        "team", "short", "played", "wins", "draws", "losses",
        "goals_for", "goals_against", "goal_diff", "points",
        "xg_for", "xg_against", "xpts",
        "rank", "form", "xg_trend"
      )

    jsonlite::write_json(
      list(
        generated_at = generated_at,
        season       = current_season,
        as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
        rows         = standings_rows
      ),
      file.path(out_dir, "standings.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )

    # Append per-round table to standings_history.json. Drops the snapshot-
    # only `form` and `xg_trend` columns. Dedup on (as_of, team) lets re-runs
    # against the same set of played fixtures replace rather than duplicate.
    standings_history_row <- standings_rows |>
      dplyr::mutate(
        as_of        = format(max(bd_results$match_date), "%Y-%m-%d"),
        generated_at = generated_at,
        round        = as.integer(round_num),
        season       = current_season
      ) |>
      dplyr::select(
        "as_of", "generated_at", "round", "season",
        "team", "short", "played", "wins", "draws", "losses",
        "goals_for", "goals_against", "goal_diff", "points",
        "xg_for", "xg_against", "xpts",
        "rank"
      )
    .append_to_history_pfi(
      file.path(out_dir, "standings_history.json"),
      standings_history_row,
      key_cols = c("as_of", "team")
    )
  } else {
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        as_of = format(end_date, "%Y-%m-%d"), rows = list()
      ),
      file.path(out_dir, "standings.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
    )
  }

  # ---- team_strengths.json -------------------------------------------------

  team_strengths <- dplyr::bind_rows(
    .extract_team_draws_pfi(fit, "cur_offense_home", teams, "offence", "home"),
    .extract_team_draws_pfi(fit, "cur_defense_home", teams, "defence", "home"),
    .extract_team_draws_pfi(fit, "cur_strength_home", teams, "total", "home"),
    .extract_team_draws_pfi(fit, "cur_offense_away", teams, "offence", "away"),
    .extract_team_draws_pfi(fit, "cur_defense_away", teams, "defence", "away"),
    .extract_team_draws_pfi(fit, "cur_strength_away", teams, "total", "away")
  ) |>
    .summarise_team_intervals_pfi() |>
    dplyr::semi_join(current_top_teams, by = "team")

  jsonlite::write_json(
    list(generated_at = generated_at, records = team_strengths),
    file.path(out_dir, "team_strengths.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  # Append per-fit summary to team_strengths_history.json. One row per
  # (team x component x location x coverage x fit_date). Dedup on the full
  # key so re-running against the same fit replaces rather than duplicates.
  # Written for both sexes -- women's season may not have started yet, but
  # the file is still produced so future fits accrete to a stable path.
  team_strengths_history_row <- team_strengths |>
    dplyr::mutate(
      fit_date     = format(end_date, "%Y-%m-%d"),
      generated_at = generated_at,
      round        = as.integer(round_num),
      season       = current_season
    ) |>
    dplyr::select(
      "fit_date", "generated_at", "round", "season",
      "team", "component", "location", "coverage",
      "median", "lower", "upper"
    )
  .append_to_history_pfi(
    file.path(out_dir, "team_strengths_history.json"),
    team_strengths_history_row,
    key_cols = c("fit_date", "team", "component", "location", "coverage")
  )

  # ---- round_predictions_history.json --------------------------------------
  # Per-(round, team) frozen pre-round xG / xPts. Sourced from the latest
  # archived fit strictly before each matchweek's first kickoff, so the row
  # for round R reflects the model as it stood the moment before the round
  # started -- subsequent fits cannot retroactively improve it. Written for
  # both sexes; the file is created with empty `records` even when the
  # archive has no relevant partition yet, so the website never 404s.
  round_predictions_path <- file.path(
    out_dir, "round_predictions_history.json"
  )
  if (nrow(round_predictions) > 0L) {
    round_predictions_history_row <- round_predictions |>
      dplyr::mutate(
        generated_at = generated_at,
        season = current_season
      ) |>
      dplyr::select(
        "fit_date", "generated_at", "round", "season",
        "team", "n_matches",
        "xg_for", "xg_against", "xpts",
        "p_win", "p_draw", "p_loss"
      )
    .append_to_history_pfi(
      round_predictions_path,
      round_predictions_history_row,
      key_cols = c("round", "team")
    )
  } else if (!file.exists(round_predictions_path)) {
    jsonlite::write_json(
      list(schema_version = 1L, records = list()),
      round_predictions_path,
      auto_unbox = TRUE,
      dataframe = "rows",
      digits = 5,
      na = "null"
    )
  }

  # ---- final_positions.json + points_distribution.json ---------------------

  if (nrow(posterior_goals) > 0L) {
    # Points already accumulated from played matches
    base_points <- results[
      results$season == current_season & results$division == top_div, ,
      drop = FALSE
    ] |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_score > .data$away_score ~ "home",
          .data$home_score < .data$away_score ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(base_points = sum(.data$points), .by = "team")

    # Per-draw points from remaining (predicted) top-division matches
    iter_team_points <- posterior_goals |>
      dplyr::filter(.data$division == top_div) |>
      dplyr::mutate(
        result = dplyr::case_when(
          .data$home_goals > .data$away_goals ~ "home",
          .data$home_goals < .data$away_goals ~ "away",
          TRUE ~ "tie"
        )
      ) |>
      tidyr::pivot_longer(c("home_team", "away_team"), values_to = "team") |>
      dplyr::mutate(
        name = dplyr::if_else(.data$name == "home_team", "home", "away"),
        points = dplyr::case_when(
          .data$result == "tie" ~ 1L,
          .data$result == .data$name ~ 3L,
          TRUE ~ 0L
        )
      ) |>
      dplyr::summarise(points = sum(.data$points), .by = c(".draw", "team")) |>
      dplyr::left_join(base_points, by = "team") |>
      dplyr::mutate(
        base_points = dplyr::coalesce(.data$base_points, 0L),
        points      = .data$points + .data$base_points
      )

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

    top_six <- iter_positions |>
      dplyr::summarise(
        p_top_six = mean(.data$placement <= 6L),
        p_winner = mean(.data$placement == 1L),
        p_relegation = mean(.data$placement >= n_teams_top - 1L),
        .by = "team"
      )

    jsonlite::write_json(
      list(
        generated_at = generated_at,
        season       = current_season,
        n_teams      = n_teams_top,
        records      = final_positions,
        summary      = top_six
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
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
      dplyr::left_join(top_six, by = "team")

    jsonlite::write_json(
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
    # Empty outputs when fit dimensions don't match
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        n_teams = 0L, records = list(), summary = list()
      ),
      file.path(out_dir, "final_positions.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
    jsonlite::write_json(
      list(
        generated_at = generated_at, season = current_season,
        records = list(), summary = list()
      ),
      file.path(out_dir, "points_distribution.json"),
      auto_unbox = TRUE, dataframe = "rows", digits = 5
    )
  }

  # ---- home_advantage.json -------------------------------------------------

  extract_home_adv_pfi <- function(var, component, transform = identity) {
    fit$draws(var) |>
      posterior::as_draws_df() |>
      tibble::as_tibble() |>
      tidyr::pivot_longer(c(-".chain", -".draw", -".iteration")) |>
      dplyr::mutate(
        team_idx  = as.integer(readr::parse_number(.data$name)),
        team      = teams$team[.data$team_idx],
        component = component,
        value     = transform(.data$value)
      )
  }

  home_advantage <- dplyr::bind_rows(
    extract_home_adv_pfi("home_advantage_off", "offence"),
    extract_home_adv_pfi("home_advantage_def", "defence"),
    extract_home_adv_pfi("home_advantage_tot", "total", transform = function(x) x / 2)
  ) |>
    dplyr::mutate(multiplier = exp(.data$value)) |>
    dplyr::reframe(
      median = stats::median(.data$multiplier),
      coverage = c(0.5, 0.8, 0.95),
      lower = stats::quantile(.data$multiplier, 0.5 - .data$coverage / 2),
      upper = stats::quantile(.data$multiplier, 0.5 + .data$coverage / 2),
      .by = c("team", "component")
    ) |>
    dplyr::semi_join(top_teams_upcoming, by = "team")

  jsonlite::write_json(
    list(generated_at = generated_at, records = home_advantage),
    file.path(out_dir, "home_advantage.json"),
    auto_unbox = TRUE, dataframe = "rows", digits = 5
  )

  n_files <- length(list.files(out_dir, pattern = "\\.json$"))
  message(sprintf(
    "publish_football_iceland: wrote %d JSONs to %s", n_files, out_dir
  ))
  invisible(NULL)
}

#' @include storage.R
NULL

#' Ingest international football results into the facts store.
#'
#' Reads the bulk international-results CSV (martj42 schema:
#' `date, home_team, away_team, home_score, away_score, tournament, city,
#' country, neutral`) and writes it to the canonical Parquet facts store under
#' `sport=football / country=world / sex=male`, so the standard
#' [prepare_data()] -> [fit_model()] flow can fit the existing bivariate-Poisson
#' football model on internationals unchanged.
#'
#' Played matches (non-NA scores) go to `results`; unplayed fixtures (NA scores,
#' notably the upcoming World Cup matches the source already lists) go to
#' `schedules`. `division` carries the competition type (`tournament`); the
#' football model ignores it, but it keeps the natural key unique when the same
#' two nations meet in different competitions on nearby dates.
#'
#' The training population is aligned with the prediction population the same way
#' the Iceland `training_filter` aligns cup minnows: a match is kept only if both
#' teams are either a 2026 World Cup participant or have played at least
#' `min_team_matches` internationals inside the window. This trims the long tail
#' of one-off micro-nation fixtures (the international analogue of the cup-blowout
#' funnel) while retaining every WC team's full recent history.
#'
#' @param csv_path Path to the martj42 `results.csv`.
#' @param window_start Earliest `match_date` to keep. The random-walk model
#'   down-weights older matches anyway; this bounds `N_rounds` (the busiest
#'   team's appearance count) and drops defunct teams. Default 2022-01-01 — a
#'   full post-Qatar cycle of qualifiers, continental tournaments and friendlies,
#'   which is ample for current-strength estimation and keeps the fit tractable.
#' @param min_team_matches Minimum in-window internationals for an opponent to
#'   be retained. Default 8.
#' @param root Data root. Default `here::here("data")`.
#' @return Invisibly, a list of row counts (`n_results`, `n_schedule`,
#'   `n_teams`, `n_wc_teams`).
#' @importFrom rlang .data
#' @export
wc_ingest_internationals <- function(csv_path = here::here("data", "wc", "raw", "results.csv"),
                                     window_start = as.Date("2022-01-01"),
                                     min_team_matches = 8L,
                                     root = here::here("data")) {
  raw <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      date = readr::col_date(),
      home_team = readr::col_character(),
      away_team = readr::col_character(),
      home_score = readr::col_integer(),
      away_score = readr::col_integer(),
      tournament = readr::col_character(),
      city = readr::col_character(),
      country = readr::col_character(),
      neutral = readr::col_logical()
    )
  )

  d <- raw |>
    dplyr::transmute(
      match_date = .data$date,
      home_team  = .data$home_team,
      away_team  = .data$away_team,
      home_score = .data$home_score,
      away_score = .data$away_score,
      division   = .data$tournament
    ) |>
    dplyr::filter(.data$match_date >= window_start)

  wc_teams <- d |>
    dplyr::filter(
      .data$division == "FIFA World Cup",
      format(.data$match_date, "%Y") == "2026"
    ) |>
    (\(x) unique(c(x$home_team, x$away_team)))()

  played <- d |>
    dplyr::filter(!is.na(.data$home_score) & !is.na(.data$away_score))

  match_counts <- table(c(played$home_team, played$away_team))
  frequent <- names(match_counts)[match_counts >= as.integer(min_team_matches)]
  eligible <- union(wc_teams, frequent)

  # Align the training population with the prediction population (the 48 WC
  # teams): keep a match only if it involves at least one WC team and both
  # teams clear the activity floor. Drops minnow-vs-minnow games irrelevant to
  # WC-team strength while keeping every WC team's full recent history.
  keep_match <- function(df) {
    df[(df$home_team %in% wc_teams | df$away_team %in% wc_teams) &
      df$home_team %in% eligible & df$away_team %in% eligible, , drop = FALSE]
  }

  results <- keep_match(played) |>
    dplyr::transmute(
      sport      = "football",
      country    = "world",
      sex        = "male",
      season     = as.integer(format(.data$match_date, "%Y")),
      match_date = .data$match_date,
      home_team  = .data$home_team,
      away_team  = .data$away_team,
      home_score = .data$home_score,
      away_score = .data$away_score,
      division   = .data$division,
      round      = NA_integer_
    )

  schedule <- d |>
    dplyr::filter(is.na(.data$home_score) | is.na(.data$away_score)) |>
    keep_match() |>
    dplyr::transmute(
      sport        = "football",
      country      = "world",
      sex          = "male",
      season       = as.integer(format(.data$match_date, "%Y")),
      match_date   = .data$match_date,
      home_team    = .data$home_team,
      away_team    = .data$away_team,
      division     = .data$division,
      round        = NA_integer_,
      kickoff_time = NA_character_
    )

  write_table(results, "results", root = root)
  if (nrow(schedule) > 0L) {
    write_table(schedule, "schedules", root = root)
  }

  cli::cli_alert_success(
    "Ingested {nrow(results)} played + {nrow(schedule)} upcoming international matches \\
    ({length(unique(c(results$home_team, results$away_team)))} teams, \\
    {length(wc_teams)} WC participants)."
  )

  invisible(list(
    n_results  = nrow(results),
    n_schedule = nrow(schedule),
    n_teams    = length(unique(c(results$home_team, results$away_team))),
    n_wc_teams = length(wc_teams)
  ))
}

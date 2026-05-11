#' @include storage.R decide-normalise.R
NULL

#' Parse a Lengjan handicap string ("0-1") into signed numeric (-1).
#'
#' Positive = home gets head start; negative = away gets head start.
#'
#' @param change_str Character vector of handicap strings.
#' @return Numeric vector of signed handicap values.
#' @export
parse_handicap <- function(change_str) {
  parts <- stringr::str_split_fixed(change_str, "-", n = 2)
  result <- suppressWarnings(as.numeric(parts[, 1]) - as.numeric(parts[, 2]))
  if (any(is.na(result))) {
    warning("Could not parse handicap values: ",
      paste(change_str[is.na(result)], collapse = ", "),
      call. = FALSE
    )
  }
  result
}

#' Read facts/odds for one (sport, country) and return latest-snapshot rows
#' for matches at or after `end_date`, no older than `max_age_hours`.
#'
#' @param league List with `sport` + `country`.
#' @param sex "male" or "female". Lengjan odds are sex-agnostic per
#'   competition; the parameter is here for symmetry but does not filter.
#'   Sex-aware handling happens at the kelly_joint stage where beliefs are
#'   sex-keyed.
#' @param end_date Drop matches before this date. Default today.
#' @param max_age_hours Drop scrapes older than this (vs `now`).
#' @param now Reference timestamp for age filtering. Default `Sys.time()`.
#'   Override in tests to avoid dependence on wall-clock time.
#' @param root Data root. Default `here::here("data")`.
#' @return Tibble with (match_date, home_team, away_team, market, outcome,
#'   line, odds, scraped_at). Empty (typed) when no rows match.
#' @export
prepare_odds <- function(league, sex,
                         end_date = Sys.Date(),
                         max_age_hours = 48,
                         now = Sys.time(),
                         root = here::here("data")) {
  raw <- tryCatch(
    read_table("odds",
      root = root,
      filter = list(sport = league$sport, country = league$country)
    ),
    error = function(e) tibble::tibble()
  )

  if (nrow(raw) == 0L || !"match_date" %in% names(raw)) {
    return(empty_odds())
  }

  cutoff_t <- now - lubridate::dhours(max_age_hours)
  raw <- raw[raw$match_date >= end_date &
    raw$scraped_at >= cutoff_t, , drop = FALSE]

  if (nrow(raw) == 0L) {
    return(empty_odds())
  }

  # Dedup to latest scrape per (match x market x outcome x line)
  raw <- raw |>
    dplyr::group_by(
      .data$match_date, .data$home_team, .data$away_team,
      .data$market, .data$outcome, .data$line
    ) |>
    dplyr::slice_max(.data$scraped_at, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup()

  # Rewrite Lengjan-side names (e.g. "Grindavík kv") to canonical
  # (e.g. "Grindavík") using the per-sex team_names map -- inverse
  # direction from what the placer uses. Done at decide time rather than
  # ingest time so facts/odds/ keeps the as-scraped strings (debugging
  # value for Lengjan UI deploys; cf. 2026-04-25 "vs"->"-" separator flip).
  raw <- normalise_lengjan_team_names(raw, league, sex)

  raw |>
    dplyr::select(
      "match_date", "home_team", "away_team",
      "market", "outcome", "line", "odds", "scraped_at"
    )
}

#' @keywords internal
#' @noRd
empty_odds <- function() {
  tibble::tibble(
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), odds = numeric(),
    scraped_at = as.POSIXct(character(), tz = "UTC")
  )
}

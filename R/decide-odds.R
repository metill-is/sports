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
#' Each match is priced from its latest scrape only (identified by its shared
#' `scraped_at`): a line or selection absent from that scrape was withdrawn or
#' suspended, so its older price is dropped. Team names are normalised to
#' canonical before matches are keyed, so two Lengjan spellings of one fixture
#' are one match; if one scrape lists both, each market line is priced from a
#' single listing, the primary rendering's (`team_names` order). A listing
#' whose names this sex's `team_names` maps outranks an unmapped one keyed to
#' the same canonical fixture (e.g. the men's "FH" v "Valur" in the female
#' cell): the unmapped rows are dropped with a warning.
#'
#' @param league List with `sport` + `country`.
#' @param sex "male" or "female". Rows carrying a non-NA `sex` (stamped by the
#'   JSON-API scraper from the Lengjan competition, spec 2026-09-23 WS2) are
#'   kept only for that sex; NA rows (DOM scraper, history) are sex-agnostic
#'   and kept for both, as before -- the per-sex team_names join then decides.
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

  if ("sex" %in% names(raw)) {
    raw <- raw[is.na(raw$sex) | raw$sex == sex, , drop = FALSE]
    if (nrow(raw) == 0L) {
      return(empty_odds())
    }
  }

  # Rewrite Lengjan-side names (e.g. "Grindavík kv") to canonical
  # (e.g. "Grindavík") using the per-sex team_names map -- inverse
  # direction from what the placer uses. Done at decide time rather than
  # ingest time so facts/odds/ keeps the as-scraped strings (debugging
  # value for Lengjan UI deploys; cf. 2026-04-25 "vs"->"-" separator flip).
  raw$.raw_home <- raw$home_team
  raw$.raw_away <- raw$away_team
  raw$.home_pos <- lengjan_rendering_pos(raw$home_team, league, sex)
  raw$.away_pos <- lengjan_rendering_pos(raw$away_team, league, sex)
  raw <- normalise_lengjan_team_names(raw, league, sex)

  # A row without `sex` (DOM scraper) reaches both cells, so the female cell
  # also sees the men's listings. Their names ("FH", "Valur") are no female
  # rendering and pass through unchanged -- onto the canonical key of "FH kv"
  # v "Valur kv". Where a listing mapped through this cell's team_names shares
  # a canonical fixture with an unmapped one, the mapped listing is this
  # cell's: drop the other before the latest-scrape filter can prefer it.
  raw <- raw |>
    dplyr::group_by(.data$match_date, .data$home_team, .data$away_team) |>
    dplyr::mutate(
      .mapped = !is.na(.data$.home_pos) & !is.na(.data$.away_pos),
      .twin = !.data$.mapped & any(.data$.mapped)
    ) |>
    dplyr::ungroup()
  if (any(raw$.twin)) {
    twins <- unique(raw[raw$.twin, c("match_date", "home_team", "away_team")])
    warning(
      "prepare_odds: ", league$sport, "/", league$country, "/", sex,
      ": dropped unmapped odds sharing a fixture with a mapped listing ",
      "(the other sex's game?): ",
      paste(twins$match_date, twins$home_team, "v", twins$away_team,
        collapse = "; "
      ),
      call. = FALSE
    )
    raw <- raw[!raw$.twin, , drop = FALSE]
  }

  # Price each match from its latest scrape only. The scrapers drop withdrawn
  # lines and suspended selections rather than writing them, so a row missing
  # from the newest scrape is no longer on offer at its old price. Each scrape
  # run stamps one scraped_at on every row it writes (ingest_lengjan_api(),
  # ingest_lengjan_odds()), so equality identifies the run.
  raw <- raw |>
    dplyr::group_by(.data$match_date, .data$home_team, .data$away_team) |>
    dplyr::filter(.data$scraped_at == max(.data$scraped_at)) |>
    dplyr::ungroup()

  # Every surviving row of a match now shares one scraped_at, so this step
  # only collapses canonical duplicates within that scrape: a team_names list
  # value maps several Lengjan spellings to one team (the 2026-07-17 "Víkingur
  # Rvk" rename), and one scrape can list a fixture under both, which
  # kelly_joint() would see as outcome probabilities summing to ~2. Each
  # (match x market x line) is priced from one listing, so its outcomes never
  # mix two: the one whose names come first in tn_renderings() order (the
  # primary rendering), then by raw name as a stable key.
  raw <- raw |>
    dplyr::arrange(
      .data$.home_pos, .data$.away_pos, .data$.raw_home, .data$.raw_away
    ) |>
    dplyr::group_by(
      .data$match_date, .data$home_team, .data$away_team,
      .data$market, .data$line
    ) |>
    dplyr::filter(
      .data$.raw_home == .data$.raw_home[1L],
      .data$.raw_away == .data$.raw_away[1L]
    ) |>
    dplyr::group_by(.data$outcome, .add = TRUE) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

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

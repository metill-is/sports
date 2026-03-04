#' Odds loading dispatcher
#'
#' Loads odds from different sources (local CSVs, lengjan-odds repo,
#' Google Sheets) and normalises to a common schema.
#'
#' All sources return tibbles with these columns:
#'   1x2:      date, home, away, o_home, o_draw, o_away, booker
#'   Handicap: date, home, away, change, o_home, o_draw, o_away, booker
#'   Totals:   date, home, away, limit, o_over, o_under, booker

box::use(
  readr[read_csv],
  dplyr[mutate, rename, select, any_of, filter, slice_tail, group_by, ungroup]
)

#' Default odds filenames for local source
default_files <- list(
  outcome = "odds.csv",
  handicap = "odds_handicap.csv",
  totals = "odds_totals.csv"
)

#' Read a CSV, returning an empty tibble if the file doesn't exist
read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  read_csv(path, show_col_types = FALSE)
}

#' Load odds from local CSV files
#'
#' Expected at {sport_dir}/{cfg$odds$path}/{filename}.
#' If the CSVs contain booker/sex columns (e.g. from GSheets downloads),
#' applies the same sex and booker filters as the GSheets loader.
#' Otherwise adds booker from config.
load_local <- function(cfg, sport_dir, sex = NULL) {
  odds_dir <- file.path(sport_dir, cfg$odds$path)
  files <- cfg$odds$files %||% default_files

  process <- function(filename) {
    d <- read_csv_safe(file.path(odds_dir, filename))
    if (is.null(d)) return(NULL)

    # Normalise column names
    if ("date_game" %in% names(d) && !"date" %in% names(d)) {
      d <- d |> rename(date = date_game)
    }
    if ("o_tie" %in% names(d) && !"o_draw" %in% names(d)) {
      d <- d |> rename(o_draw = o_tie)
    }
    if ("date" %in% names(d)) {
      d <- d |> mutate(date = as.Date(date))
    }

    # Apply sex/booker filters if the CSV has those columns (GSheets-derived)
    if ("booker" %in% names(d) || !is.null(cfg$odds$sex_column)) {
      d <- apply_gsheets_filters(d, cfg, sex)
    } else {
      # Simple local CSVs: add booker from config
      d <- d |> mutate(booker = cfg$odds$booker %||% "Unknown")
    }

    d
  }

  outcome <- process(files$outcome %||% default_files$outcome)
  if (!is.null(outcome)) {
    outcome <- outcome |>
      select(any_of(c("date", "league", "home", "away", "o_home", "o_draw", "o_away", "booker")))
  }

  handicap <- process(files$handicap %||% default_files$handicap)
  if (!is.null(handicap)) {
    handicap <- handicap |>
      select(any_of(c("date", "league", "home", "away", "change", "o_home", "o_draw", "o_away", "booker")))
  }

  totals <- process(files$totals %||% default_files$totals)
  if (!is.null(totals)) {
    totals <- totals |>
      select(any_of(c("date", "league", "home", "away", "limit", "o_over", "o_under", "booker")))
  }

  list(outcome = outcome, handicap = handicap, totals = totals)
}

#' Load odds from lengjan-odds repo
#'
#' Reads from {sport_dir}/{cfg$odds$lengjan_odds_path}/
load_lengjan_odds <- function(cfg, sport_dir, sex = NULL) {
  odds_dir <- file.path(sport_dir, cfg$odds$lengjan_odds_path)
  booker <- cfg$odds$booker %||% "Lengjan"

  # Lengjan-odds scrapes 3x daily — keep only the latest scrape per match×line.
  # CSVs have a scraped_at column; rows are in chronological order,
  # so slice_tail(n=1) per group keeps the freshest odds.

  outcome <- read_csv_safe(file.path(odds_dir, "odds_1x2.csv"))
  if (!is.null(outcome)) {
    outcome <- outcome |>
      group_by(date, home, away) |>
      slice_tail(n = 1) |>
      ungroup() |>
      mutate(booker = booker) |>
      select(any_of(c("date", "league", "home", "away", "o_home", "o_draw", "o_away", "booker")))
  }

  handicap <- read_csv_safe(file.path(odds_dir, "odds_handicap.csv"))
  if (!is.null(handicap)) {
    handicap <- handicap |>
      group_by(date, home, away, change) |>
      slice_tail(n = 1) |>
      ungroup() |>
      mutate(booker = booker) |>
      select(any_of(c("date", "league", "home", "away", "change", "o_home", "o_draw", "o_away", "booker")))
  }

  totals <- read_csv_safe(file.path(odds_dir, "odds_totals.csv"))
  if (!is.null(totals)) {
    totals <- totals |>
      group_by(date, home, away, limit) |>
      slice_tail(n = 1) |>
      ungroup() |>
      mutate(booker = booker) |>
      select(any_of(c("date", "league", "home", "away", "limit", "o_over", "o_under", "booker")))
  }

  list(outcome = outcome, handicap = handicap, totals = totals)
}

#' Apply sex and booker filters to a GSheets tibble
#'
#' Filters by sex column (if configured) and booker include/exclude lists.
#' Preserves existing booker column; only adds from config if missing.
apply_gsheets_filters <- function(d, cfg, sex = NULL) {
  if (is.null(d)) return(NULL)

  # Sex filter: map e.g. "male" → "kk" via cfg$sex_map
  sex_col <- cfg$odds$sex_column
  if (!is.null(sex) && !is.null(sex_col) && sex_col %in% names(d)) {
    sex_value <- cfg$sex_map[[sex]]
    if (!is.null(sex_value)) {
      d <- d |> filter(.data[[sex_col]] == sex_value)
    }
  }

  # Booker filter (include or exclude)
  if ("booker" %in% names(d)) {
    incl <- cfg$odds$booker_include
    excl <- cfg$odds$booker_exclude
    if (!is.null(incl)) d <- d |> filter(booker %in% incl)
    if (!is.null(excl)) d <- d |> filter(!booker %in% excl)
  }

  # Add booker from config only if column missing
  if (!"booker" %in% names(d)) {
    d <- d |> mutate(booker = cfg$odds$booker %||% "GSheets")
  }

  d
}

#' Load odds from Google Sheets
#'
#' Reads from configured sheet tabs, applies sex/booker filters,
#' and normalises column names.
load_gsheets <- function(cfg, sport_dir, sex = NULL) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("googlesheets4 must be installed to use GSheets odds source.")
  }

  googlesheets4::gs4_auth(email = Sys.getenv("GOOGLE_MAIL"))
  tabs <- cfg$odds$gsheets_tabs
  sheet_id <- cfg$odds$gsheets_id

  read_tab <- function(tab_name) {
    if (is.null(tab_name)) return(NULL)
    tryCatch(
      googlesheets4::read_sheet(sheet_id, sheet = tab_name),
      error = function(e) {
        warning("Could not read GSheets tab '", tab_name, "': ", e$message)
        NULL
      }
    )
  }

  outcome <- read_tab(tabs$outcome)
  if (!is.null(outcome)) {
    if ("date_game" %in% names(outcome)) {
      outcome <- outcome |> rename(date = date_game)
    }
    outcome <- outcome |> mutate(date = as.Date(date))
    outcome <- apply_gsheets_filters(outcome, cfg, sex)
    outcome <- outcome |>
      select(any_of(c("date", "league", "home", "away", "o_home", "o_draw", "o_away", "booker")))
  }

  handicap <- read_tab(tabs$handicap)
  if (!is.null(handicap)) {
    if ("date_game" %in% names(handicap)) {
      handicap <- handicap |> rename(date = date_game)
    }
    handicap <- handicap |> mutate(date = as.Date(date))
    handicap <- apply_gsheets_filters(handicap, cfg, sex)
    handicap <- handicap |>
      select(any_of(c("date", "league", "home", "away", "change", "o_home", "o_draw", "o_away", "booker")))
  }

  totals <- read_tab(tabs$totals)
  if (!is.null(totals)) {
    if ("date_game" %in% names(totals)) {
      totals <- totals |> rename(date = date_game)
    }
    totals <- totals |> mutate(date = as.Date(date))
    totals <- apply_gsheets_filters(totals, cfg, sex)
    totals <- totals |>
      select(any_of(c("date", "league", "home", "away", "limit", "o_over", "o_under", "booker")))
  }

  list(outcome = outcome, handicap = handicap, totals = totals)
}

#' Load odds from configured source
#'
#' Dispatches to the appropriate loader based on cfg$odds$source.
#' Returns a list with three tibbles: outcome, handicap, totals.
#' Each may be NULL if no data is available for that market.
#'
#' @param cfg Config list (from bets.yml)
#' @param sport_dir Root directory of the sport project
#' @param sex Optional sex label (used by GSheets loader for filtering)
#' @return list(outcome, handicap, totals) — each a tibble or NULL
#' @export
load_odds <- function(cfg, sport_dir, sex = NULL) {
  source <- cfg$odds$source %||% "local"

  result <- switch(source,
    "local" = load_local(cfg, sport_dir, sex),
    "lengjan-odds" = load_lengjan_odds(cfg, sport_dir, sex),
    "gsheets" = load_gsheets(cfg, sport_dir, sex),
    stop("Unknown odds source: '", source, "'. Must be 'local', 'lengjan-odds', or 'gsheets'.")
  )

  # Report what was loaded
  for (market in c("outcome", "handicap", "totals")) {
    d <- result[[market]]
    if (!is.null(d) && nrow(d) > 0) {
      cat("  Loaded", nrow(d), market, "odds from", source, "\n")
    }
  }

  result
}

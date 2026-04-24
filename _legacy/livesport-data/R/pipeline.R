# Pipeline functions for {targets} orchestration
#
# Each competition key gets one target that scrapes all its leagues/divisions.
# Two-tier scraping controlled by LIVESPORT_MODE env var:
#   "daily" (default) — only leagues/divisions
#   "full"            — leagues + historical_leagues, divisions + historical_divisions

#' Load competitions config
#'
#' @param config_file Path to competitions.yml
#' @return Parsed list
load_competitions <- function(config_file) {
  yaml::yaml.load(readr::read_file(config_file))
}


#' Check whether full mode is active
#'
#' @return TRUE if LIVESPORT_MODE=full
is_full_mode <- function() {
  tolower(Sys.getenv("LIVESPORT_MODE", "daily")) == "full"
}


#' Scrape all leagues/divisions for a competition
#'
#' Dispatches to football (flat league list) or handball (nested divisions).
#' In full mode, also scrapes historical_leagues / historical_divisions.
#'
#' @param config Full parsed config
#' @param key Competition key (e.g., "football_england")
#' @return Character vector of files written
scrape_competition <- function(config, key) {
  comp <- config[[key]]
  defaults <- config$defaults %||% list()
  full <- is_full_mode()

  mode_label <- if (full) "full" else "daily"
  message("\n=== ", key, " (", mode_label, ") ===")

  files <- character()

  if (!is.null(comp$leagues)) {
    # Football: flat list of leagues
    all_leagues <- comp$leagues
    if (full && !is.null(comp$historical_leagues)) {
      all_leagues <- c(all_leagues, comp$historical_leagues)
    }
    for (league_name in all_leagues) {
      output_dir <- here::here("data", comp$livesport_sport, comp$country, "male", league_name)
      new_files <- tryCatch(
        scrape_league(comp, league_name, output_dir, defaults),
        error = function(e) {
          message("  ERROR scraping ", league_name, ": ", conditionMessage(e))
          character()
        }
      )
      files <- c(files, new_files)
    }
  } else if (!is.null(comp$divisions)) {
    # Handball: nested by sex -> division list
    all_divisions <- comp$divisions
    if (full && !is.null(comp$historical_divisions)) {
      all_divisions <- merge_divisions(all_divisions, comp$historical_divisions)
    }
    for (sex in names(all_divisions)) {
      for (div in all_divisions[[sex]]) {
        output_dir <- here::here("data", comp$livesport_sport, comp$country, sex, div$label)
        new_files <- tryCatch(
          scrape_league(comp, div$name, output_dir, defaults),
          error = function(e) {
            message("  ERROR scraping ", div$name, ": ", conditionMessage(e))
            character()
          }
        )
        files <- c(files, new_files)
      }
    }
  }

  message("  ", length(files), " files written for ", key)
  files
}


#' Merge historical divisions into daily divisions
#'
#' Appends historical division entries per sex.
#'
#' @param daily Named list (sex -> list of divisions)
#' @param historical Named list (sex -> list of divisions)
#' @return Merged named list
merge_divisions <- function(daily, historical) {
  merged <- daily
  for (sex in names(historical)) {
    if (is.null(merged[[sex]])) {
      merged[[sex]] <- historical[[sex]]
    } else {
      merged[[sex]] <- c(merged[[sex]], historical[[sex]])
    }
  }
  merged
}

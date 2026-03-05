# Pipeline functions for {targets} orchestration
#
# Each competition key gets one target that scrapes all its leagues/divisions.

#' Load competitions config
#'
#' @param config_file Path to competitions.yml
#' @return Parsed list
load_competitions <- function(config_file) {
  yaml::yaml.load(readr::read_file(config_file))
}


#' Scrape all leagues/divisions for a competition
#'
#' Dispatches to football (flat league list) or handball (nested divisions).
#'
#' @param config Full parsed config
#' @param key Competition key (e.g., "football_england")
#' @return Character vector of files written
scrape_competition <- function(config, key) {
  comp <- config[[key]]
  defaults <- config$defaults %||% list()

  message("\n=== ", key, " ===")

  files <- character()

  if (!is.null(comp$leagues)) {
    # Football: flat list of leagues, single sex
    for (league_name in comp$leagues) {
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
    # Handball: nested by sex → division list
    for (sex in names(comp$divisions)) {
      for (div in comp$divisions[[sex]]) {
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

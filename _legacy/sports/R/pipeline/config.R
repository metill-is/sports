#' Pipeline config — load and filter leagues from leagues.yml
#'
#' @usage
#' box::use(R/pipeline/config[load_leagues, filter_leagues])
#' leagues <- load_leagues(here::here("config", "leagues.yml"))
#' selected <- filter_leagues(leagues, sport = "handball")

box::use(
  yaml[read_yaml = yaml.load],
  readr[read_file]
)

#' Load leagues.yml and merge defaults into each league entry
#'
#' Uses readr::read_file() + yaml::yaml.load() to handle UTF-8 correctly.
#' By default, leagues with `active: false` are excluded. Pass
#' `include_inactive = TRUE` to load all leagues regardless.
#'
#' @param path Path to leagues.yml
#' @param include_inactive If TRUE, include leagues with `active: false`
#' @return Named list of league configs
#' @export
load_leagues <- function(path, include_inactive = FALSE) {
  raw <- read_file(path)
  cfg <- read_yaml(raw)

  defaults <- cfg$defaults %||% list()
  cfg$defaults <- NULL

  leagues <- lapply(cfg, function(league) {
    # defaults first, then league-specific overrides
    merged <- defaults
    for (nm in names(league)) {
      merged[[nm]] <- league[[nm]]
    }
    merged
  })

  if (!include_inactive) {
    leagues <- Filter(function(l) !identical(l$active, FALSE), leagues)
  }

  leagues
}


#' Filter leagues by sport, country, key, or active status
#'
#' @param leagues Named list from load_leagues()
#' @param sport Filter by sport name (e.g., "handball")
#' @param country Filter by country name (e.g., "iceland")
#' @param league_key Filter by exact key (e.g., "football_england")
#' @param active_keys Character vector of active league keys (from schedule scanner)
#' @param has_bets_only If TRUE, only return leagues with has_bets: true
#' @return Filtered named list of league configs
#' @export
filter_leagues <- function(
  leagues,
  sport = NULL,
  country = NULL,
  league_key = NULL,
  active_keys = NULL,
  has_bets_only = FALSE
) {
  if (!is.null(league_key)) {
    keys <- strsplit(league_key, ",")[[1]]
    leagues <- leagues[names(leagues) %in% keys]
  }

  if (!is.null(sport)) {
    sports <- strsplit(sport, ",")[[1]]
    leagues <- Filter(function(l) l$sport %in% sports, leagues)
  }

  if (!is.null(country)) {
    countries <- strsplit(country, ",")[[1]]
    leagues <- Filter(function(l) l$country %in% countries, leagues)
  }

  if (!is.null(active_keys)) {
    leagues <- leagues[names(leagues) %in% active_keys]
  }

  if (has_bets_only) {
    leagues <- Filter(function(l) isTRUE(l$has_bets), leagues)
  }

  leagues
}


#' Map schedule registry keys to league keys
#'
#' The schedule scanner uses keys like "basketball_iceland_m" and
#' "handball_denmark" while leagues.yml uses "basketball_iceland".
#' This maps active schedule keys back to league keys.
#'
#' @param schedule_keys Character vector of active schedule registry keys
#' @param league_keys Character vector of league keys from leagues.yml
#' @return Character vector of matching league keys
#' @export
schedule_to_league_keys <- function(schedule_keys, league_keys) {
  matched <- character(0)
  for (sk in schedule_keys) {
    # Try exact match first
    if (sk %in% league_keys) {
      matched <- c(matched, sk)
      next
    }
    # Strip _m/_f suffix and try
    base <- sub("_[mf]$", "", sk)
    if (base %in% league_keys) {
      matched <- c(matched, base)
    }
  }
  unique(matched)
}

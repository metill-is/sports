#### List configs for all international basketball leagues ####

#' Euro Basket Configuration
#' @export
eurobasket <- list(
  sport = "basketball",
  country = "europe",
  name = "eurobasket",
  years = c(2025, 2022, 2017, 2015, 2013)
)

#' World Cup Configuration
#' @export
world_cup <- list(
  sport = "basketball",
  country = "world",
  name = "world-cup",
  years = c(2023, 2019, 2014)
)

#' Olympic Games Configuration
#' @export
olympic_games <- list(
  sport = "basketball",
  country = "world",
  name = "olympic-games",
  years = c(2024, 2020, 2016)
)

#' Friendly Internationals Configuration
#' @export
friendly_internationals <- list(
  sport = "basketball",
  country = "world",
  name = "friendly-international",
  years = seq(2010, 2025, by = 1)
)
#' List of all leagues to iterate over
#' @export
leagues <- list(
  eurobasket,
  world_cup,
  olympic_games,
  friendly_internationals
)

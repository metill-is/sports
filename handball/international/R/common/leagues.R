#### List configs for all international handball leagues ####

#' Leagues Configuration for female handball leagues
#' @export
female = list(
  #' Euro Handball Configuration
  euro = list(
    sport = "handball",
    country = "europe",
    name = "ehf-euro-women",
    years = c(2024, 2022, 2020, 2018)
  ),

  #' Asia Handball Configuration
  asia = list(
    sport = "handball",
    country = "asia",
    name = "asian-championship-women",
    years = c(2024, 2022, 2021, 2018)
  ),

  #' African Handball Configuration
  african = list(
    sport = "handball",
    country = "africa",
    name = "african-championship-women",
    years = c(2024, 2022, 2021, 2018)
  ),

  #' World Championship Configuration
  world_cup = list(
    sport = "handball",
    country = "world",
    name = "world-championship-women",
    years = c(2025, 2023, 2021, 2019)
  ),

  #' Olympics Configuration
  olympics = list(
    sport = "handball",
    country = "world",
    name = "olympic-games-women",
    years = c(2024, 2020, 2016)
  ),

  #' Friendly Internationals Configuration
  friendly_internationals = list(
    sport = "handball",
    country = "world",
    name = "friendly-international-women",
    years = seq(2015, 2025, by = 1)
  )
)


#' Leagues Configuration for male handball leagues
#' @export
male = list(
  #' Euro Handball Configuration
  euro = list(
    sport = "handball",
    country = "europe",
    name = "ehf-euro",
    years = seq(1998, 2026, by = 2)
  ),

  #' World Championship Configuration
  world_championship = list(
    sport = "handball",
    country = "world",
    name = "world-championship",
    years = c(2025, 2023, 2021, 2019, 2017)
  ),

  #' Olympic Games Configuration
  olympics = list(
    sport = "handball",
    country = "world",
    name = "olympic-games",
    years = c(2024, 2020, 2016)
  ),

  #' Friendly Internationals Configuration
  friendly_internationals = list(
    sport = "handball",
    country = "world",
    name = "friendly-international",
    years = seq(2015, 2026, by = 1)
  )
)

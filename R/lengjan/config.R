#' Lengjan competition registry
#'
#' Centralised config for all sports/competitions scraped from Lengjan.
#' Each sport entry maps to the URL parameters used by games.lotto.is.
#'
#' URL pattern: https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={id}

#' @export
#' Get Lengjan scraping config for a sport
#' @param sport_name Character: e.g., "handball_iceland"
#' @return List with sport, country, sexes, and per-sex league configs
get_config <- function(sport_name) {
  configs <- list(
    handball_iceland = list(
      sport = 6,
      country = "IS",
      sport_dir = "handball/iceland",
      sexes = c("male", "female"),
      male = list(
        leagues = list(
          list(competition = "1269", label = "Olís deild karla")
        )
      ),
      female = list(
        leagues = list(
          list(competition = "1270", label = "Olís deild kvenna")
        )
      )
    )
    # Future: basketball_iceland, football_iceland
    # basketball_iceland = list(
    #   sport = 1,
    #   country = "IS",
    #   sport_dir = "basketball_iceland",
    #   sexes = c("male", "female"),
    #   male = list(
    #     leagues = list(
    #       list(competition = "???", label = "Bónusdeild karla")
    #     )
    #   ),
    #   female = list(
    #     leagues = list(
    #       list(competition = "???", label = "Bónusdeild kvenna")
    #     )
    #   )
    # )
  )

  if (!sport_name %in% names(configs)) {
    stop(
      "Unknown sport: ", sport_name, "\n",
      "Available: ", paste(names(configs), collapse = ", ")
    )
  }

  configs[[sport_name]]
}

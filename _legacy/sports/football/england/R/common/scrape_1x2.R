#' Thin wrapper around the shared R/lengjan/scrape module
#'
#' Delegates to the centralised scraper that handles CSS selectors,
#' rate limiting, and browser lifecycle.

#' @export
get_odds <- function(sport, country, competition) {
  box::use(R/lengjan/scrape[scrape_competition])

  scrape_competition(sport, country, competition)
}

#' @export
get_match_detail <- function(match_url, home, away, match_date) {
  box::use(R/lengjan/scrape[scrape_match_detail])

  scrape_match_detail(match_url, home, away, match_date)
}

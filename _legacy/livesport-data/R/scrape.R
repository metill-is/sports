# Core scraping functions for Livesport data
#
# Two sport types with different CSS selectors:
#   football: .event__homeParticipant / .event__awayParticipant
#   handball: .event__participant--home / .event__participant--away
#
# Pagination varies by competition (configured in competitions.yml).

SELECTORS <- list(
  football = list(
    home = ".event__homeParticipant strong, .event__homeParticipant span",
    away = ".event__awayParticipant strong, .event__awayParticipant span"
  ),
  handball = list(
    home = ".event__participant--home",
    away = ".event__participant--away"
  )
)

COMMON_SELECTORS <- list(
  home_score = ".event__score--home",
  away_score = ".event__score--away",
  date = ".event__time"
)


#' Build Livesport results URL
#'
#' @param comp Competition config list
#' @param league_name League URL slug
#' @return URL string
make_results_url <- function(comp, league_name) {
  sport <- comp$livesport_sport
  country <- comp$country
  season <- current_season(comp$url_type)

  if (comp$url_type == "season_range") {
    glue::glue(
      "https://www.livesport.com/en/{sport}/{country}/{league_name}-{season$start}-{season$end}/results/"
    )
  } else {
    glue::glue(
      "https://www.livesport.com/en/{sport}/{country}/{league_name}-{season$start}/results/"
    )
  }
}


#' Build Livesport fixtures URL
#'
#' @param comp Competition config list
#' @param league_name League URL slug
#' @return URL string
make_schedule_url <- function(comp, league_name) {
  sport <- comp$livesport_sport
  country <- comp$country
  glue::glue(
    "https://www.livesport.com/en/{sport}/{country}/{league_name}/fixtures/"
  )
}


#' Compute current season year(s)
#'
#' @param url_type "season_range" or "single_year"
#' @return List with $start (and $end for season_range)
current_season <- function(url_type) {
  today <- Sys.Date()
  yr <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))

  if (url_type == "single_year") {
    list(start = yr)
  } else {
    # Aug-Jul season: if before August, season started previous year
    if (mo >= 8) {
      list(start = yr, end = yr + 1L)
    } else {
      list(start = yr - 1L, end = yr)
    }
  }
}


#' Scrape a single Livesport page
#'
#' Opens headless Chrome, waits for rendering, expands pagination,
#' extracts match data. Generous delays to avoid timeouts.
#'
#' @param url Livesport URL
#' @param team_selectors "football" or "handball"
#' @param pagination_selector CSS selector for "show more" button
#' @param include_scores Whether to extract scores (FALSE for schedule pages)
#' @param page_wait Seconds to wait after page load
#' @param click_wait Seconds between pagination clicks
#' @return tibble of match data, or NULL on error
scrape_page <- function(
  url,
  team_selectors = "football",
  pagination_selector = ".wclButtonLink",
  include_scores = TRUE,
  page_wait = 10,
  click_wait = 5
) {
  sel <- SELECTORS[[team_selectors]]

  page <- tryCatch(
    rvest::read_html_live(url),
    error = function(e) {
      message("  Failed to load: ", url, "\n  Error: ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(page)) return(NULL)
  on.exit(try(page$session$close(), silent = TRUE))

  # Generous wait for JS rendering
  Sys.sleep(page_wait + stats::runif(1, 0, 3))

  # Expand all paginated content
  n_clicks <- 0
  while (length(page$html_elements(pagination_selector)) > 0 && n_clicks < 50) {
    tryCatch(
      page$click(pagination_selector),
      error = function(e) {
        message("  Pagination click failed: ", conditionMessage(e))
      }
    )
    n_clicks <- n_clicks + 1
    Sys.sleep(click_wait + stats::runif(1, 0, 2))
  }

  # Extract data
  home <- page |>
    rvest::html_elements(sel$home) |>
    rvest::html_text()

  away <- page |>
    rvest::html_elements(sel$away) |>
    rvest::html_text()

  dates <- page |>
    rvest::html_elements(COMMON_SELECTORS$date) |>
    rvest::html_text()

  if (length(home) == 0) {
    message("  No matches found on page")
    return(NULL)
  }

  # Ensure consistent lengths (take minimum)
  n <- min(length(home), length(away), length(dates))
  if (n == 0) return(NULL)

  if (include_scores) {
    home_score <- page |>
      rvest::html_elements(COMMON_SELECTORS$home_score) |>
      rvest::html_text()
    away_score <- page |>
      rvest::html_elements(COMMON_SELECTORS$away_score) |>
      rvest::html_text()

    n <- min(n, length(home_score), length(away_score))
    if (n == 0) return(NULL)

    dplyr::tibble(
      date = dates[seq_len(n)],
      home = home[seq_len(n)],
      away = away[seq_len(n)],
      home_score = home_score[seq_len(n)],
      away_score = away_score[seq_len(n)]
    )
  } else {
    dplyr::tibble(
      date = dates[seq_len(n)],
      home = home[seq_len(n)],
      away = away[seq_len(n)]
    )
  }
}


#' Scrape results and schedule for a single league
#'
#' @param comp Competition config
#' @param league_name League URL slug
#' @param output_dir Base directory for output files
#' @param defaults Default timing parameters
#' @return Character vector of files written
scrape_league <- function(comp, league_name, output_dir, defaults) {
  files_written <- character()

  page_wait <- comp$page_wait %||% defaults$page_wait %||% 10
  click_wait <- comp$click_wait %||% defaults$click_wait %||% 5
  between <- comp$between_pages %||% defaults$between_pages %||% 5

  # Results
  results_url <- make_results_url(comp, league_name)
  message("  Scraping results: ", league_name)

  results <- scrape_page(
    url = results_url,
    team_selectors = comp$team_selectors,
    pagination_selector = comp$pagination,
    include_scores = TRUE,
    page_wait = page_wait,
    click_wait = click_wait
  )

  if (!is.null(results) && nrow(results) > 0) {
    results_path <- file.path(output_dir, "results.csv")
    if (!dir.exists(dirname(results_path))) {
      dir.create(dirname(results_path), recursive = TRUE)
    }
    readr::write_csv(results, results_path)
    files_written <- c(files_written, results_path)
    message("    ", nrow(results), " results written")
  }

  Sys.sleep(between + stats::runif(1, 0, 3))

  # Schedule
  schedule_url <- make_schedule_url(comp, league_name)
  message("  Scraping schedule: ", league_name)

  schedule <- scrape_page(
    url = schedule_url,
    team_selectors = comp$team_selectors,
    pagination_selector = comp$pagination,
    include_scores = FALSE,
    page_wait = page_wait,
    click_wait = click_wait
  )

  if (!is.null(schedule) && nrow(schedule) > 0) {
    schedule_path <- file.path(output_dir, "schedule.csv")
    readr::write_csv(schedule, schedule_path)
    files_written <- c(files_written, schedule_path)
    message("    ", nrow(schedule), " fixtures written")
  }

  Sys.sleep(between + stats::runif(1, 0, 3))

  files_written
}

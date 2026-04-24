#' Read a page of football data
#'
#' @param page A page of football data as read by rvest::read_html_live()
#' @return A tibble of the football data
#' @export
read_page <- function(page) {
  box::use(
    rvest[html_elements, html_text],
    dplyr[tibble]
  )

  home <- page |>
    html_elements(
      ".event__homeParticipant strong, .event__homeParticipant span"
    ) |>
    html_text()

  away <- page |>
    html_elements(
      ".event__awayParticipant strong, .event__awayParticipant span"
    ) |>
    html_text()

  home_score <- page |>
    html_elements(".event__score--home") |>
    html_text()

  away_score <- page |>
    html_elements(".event__score--away") |>
    html_text()

  date <- page |>
    html_elements(".event__time") |>
    html_text()

  tibble(
    date,
    home,
    away,
    home_score,
    away_score
  )
}


#' Make a results URL for Norwegian football league
#'
#' @param league_name The name of the league
#' @param year_start The start year of the league
#' @param year_end The end year of the league
#' @return A URL to the results
#' @export
make_results_url <- function(league_name, year_start, year_end) {
  box::use(
    glue[glue]
  )

  glue(
    "https://www.livesport.com/en/soccer/norway/{league_name}-{year_start}/results/"
  )
}

#' Make a schedule URL for Norwegian football league
#'
#' @param league_name The name of the league
#' @return A URL to the schedule
#' @export
make_schedule_url <- function(league_name) {
  box::use(
    glue[glue]
  )

  glue(
    "https://www.livesport.com/en/soccer/norway/{league_name}/fixtures/"
  )
}

#' Read historical results for a given league
#'
#' @param league_list The league to read
#' @return A tibble of the historical results
#' @export
read_historical_results <- function(league_list) {
  box::use(
    purrr[map],
    rvest[read_html_live],
    here[here],
    readr[write_csv]
  )

  output <- list()

  for (year in league_list$years) {
    folder_path <- here(
      "data",
      "male",
      league_list$name,
      year
    )
    file_path <- here(folder_path, "results.csv")
    if (file.exists(file_path)) {
      next
    }
    year_start <- year
    year_end <- year + 1
    url <- make_results_url(
      league_list$name,
      year_start,
      year_end
    )
    page <- read_html_live(url)
    Sys.sleep(4)

    while (length(page$html_elements(".event__more")) > 0) {
      page$click(".event__more")
      Sys.sleep(2)
    }
    out <- read_page(page)
    page$session$close()
    Sys.sleep(2)
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
    }
    write_csv(out, file_path)
  }
}

#' Update results for current season
#'
#' @param league_list The league to update
#' @return A tibble of the updated results
#' @export
update_historical_results <- function(league_list) {
  box::use(
    purrr[map],
    rvest[read_html_live],
    here[here],
    readr[write_csv]
  )

  for (year in league_list$years) {
    url <- make_results_url(
      league_list$name,
      year,
      year + 1
    )
    page <- read_html_live(url)
    Sys.sleep(1)
    out <- read_page(page)
    page$session$close()
    Sys.sleep(1)
    if (
      !dir.exists(here(
        "data",
        "male",
        league_list$name,
        year
      ))
    ) {
      dir.create(
        here("data", "male", league_list$name, year),
        recursive = TRUE
      )
    }
    write_csv(
      out,
      here(
        "data",
        "male",
        league_list$name,
        year,
        "results.csv"
      )
    )
  }
}

#' Update schedule for current season
#'
#' @param league_list The league to update
#' @return A tibble of the updated schedule
#' @export
update_current_schedule <- function(league_list) {
  box::use(
    purrr[map],
    rvest[read_html_live],
    here[here],
    readr[write_csv]
  )

  url <- make_schedule_url(
    league_list$name
  )
  page <- read_html_live(url)
  Sys.sleep(2)
  out <- read_page(page)
  page$session$close()
  Sys.sleep(1)

  write_csv(
    out,
    here(
      "data",
      "male",
      league_list$name,
      "schedule.csv"
    )
  )
  Sys.sleep(1)
}

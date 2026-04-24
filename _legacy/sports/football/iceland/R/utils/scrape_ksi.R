library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(tibble)
library(lubridate)
library(purrr)

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

# KSI competition IDs (new oll-mot format)
# The old stakt-mot/?motnumer= URLs redirect to oll-mot/mot?id=
ksi_ids <- list(
  male = list(
    div1 = c("2025" = 190366, "2026" = 7025510),
    div2 = c("2025" = 190359, "2026" = 7025540),
    div3 = c("2025" = 190365, "2026" = 7025548),
    div4 = c("2025" = 190363, "2026" = 7025551),
    div5 = c("2025" = 190358, "2026" = 7025560),
    cup = c("2025" = 190367, "2026" = 7059683),
    div1_upper_playoffs = c("2026" = 7025527),
    div1_lower_playoffs = c("2026" = 7025532),
    div2_playoffs = c("2026" = 7025545)
  ),
  female = list(
    div1 = c("2025" = 190372, "2026" = 7025645),
    div2 = c("2025" = 190375, "2026" = 7025676),
    div3 = c("2025" = 190364, "2026" = 7025681),
    cup = c("2025" = 190356, "2026" = 7058831),
    div1_upper_playoffs = c("2026" = 7025657),
    div1_lower_playoffs = c("2026" = 7025661)
  )
)


#' Parse KSI date text to Date
#'
#' @param date_raw Character vector of raw date strings from KSI (e.g. "Lau 5. apr<U+00ED>l  19:15")
#' @param year Integer year from the timabil
#' @return Date vector
parse_ksi_date <- function(date_raw, year) {
  date_raw |>
    sub("^[^ ]+ ", "", x = _) |> # Remove day name ("Lau ")
    sub("  .*$", "", x = _) |> # Remove time ("  19:15")
    trimws() |>
    paste(year) |> # Append year for dmy() parsing
    dmy()
}


#' Fetch and parse a single KSI competition page
#'
#' @param id Competition ID
#' @return Parsed HTML page or NULL on failure
fetch_ksi_page <- function(id) {
  url <- paste0(
    "https://www.ksi.is/oll-mot/mot?id=", id,
    "&banner-tab=matches-and-results"
  )
  resp <- tryCatch(
    GET(url, timeout(30)),
    error = function(e) {
      message("Failed to fetch id=", id, ": ", e$message)
      return(NULL)
    }
  )
  if (is.null(resp) || status_code(resp) != 200) {
    return(NULL)
  }
  content(resp, as = "parsed", encoding = "UTF-8")
}


#' Extract season year from a parsed KSI page
#'
#' Reads the T<U+00ED>mabil select dropdown to find the season year.
#' @param page Parsed HTML page
#' @return Integer year, or current year as fallback
extract_season_year <- function(page) {
  sel <- html_element(page, "select#timabil-select-filter-select")
  if (is.na(sel)) {
    return(as.integer(format(Sys.Date(), "%Y")))
  }

  opts <- html_elements(sel, "option")
  years <- opts |>
    html_attr("value") |>
    (\(x) x[nchar(x) == 4])() |>
    as.integer()

  if (length(years) == 0) {
    return(as.integer(format(Sys.Date(), "%Y")))
  }
  max(years)
}


#' Extract match data from a parsed KSI page
#'
#' @param page Parsed HTML page
#' @param played_only If TRUE (default), only return matches with scores
#' @return List of tibble rows
extract_matches <- function(page, played_only = TRUE) {
  score_nodes <- html_elements(page, "span.body-4.whitespace-nowrap")
  if (length(score_nodes) == 0) {
    return(list())
  }

  map(score_nodes, function(node) {
    score_text <- html_text(node) |> trimws()
    is_played <- grepl("^[0-9]+ - [0-9]+$", score_text)

    if (played_only && !is_played) {
      return(NULL)
    }

    parent <- xml2::xml_parent(node)
    links <- html_elements(parent, "a")
    if (length(links) < 2) {
      return(NULL)
    }

    home <- html_text(links[[1]]) |> trimws()
    away <- html_text(links[[2]]) |> trimws()

    # Date from match row
    row <- xml2::xml_parent(parent)
    first_col <- html_element(row, "div.flex.flex-col")
    date_el <- html_element(first_col, "div")
    date_text <- if (!is.na(date_el)) html_text(date_el) |> trimws() else ""

    if (is_played) {
      score_parts <- str_match(score_text, "^([0-9]+) - ([0-9]+)$")
      tibble(
        date_raw = date_text,
        heima = home,
        stig_heima = as.integer(score_parts[1, 2]),
        gestir = away,
        stig_gestir = as.integer(score_parts[1, 3])
      )
    } else {
      tibble(date_raw = date_text, heima = home, gestir = away)
    }
  })
}


#' Scrape match results from KSI.is for a given competition
#'
#' @param comp_ids Named vector of year = competition_id pairs
#' @param division Character string for the division column value
#' @return A tibble with columns: timabil, dags, heima, stig_heima, gestir, stig_gestir[, division]
scrape_ksi_results <- function(comp_ids, division = NA_character_) {
  results <- imap(comp_ids, function(id, year) {
    Sys.sleep(2)
    page <- fetch_ksi_page(id)
    if (is.null(page)) {
      return(NULL)
    }

    # Extract season year from the page rather than trusting the key name
    season_year <- extract_season_year(page)

    matches <- extract_matches(page, played_only = TRUE)
    d <- bind_rows(matches)
    if (nrow(d) == 0) {
      return(NULL)
    }

    d |> mutate(timabil = season_year)
  })

  d <- bind_rows(results)
  if (nrow(d) == 0) {
    return(d)
  }

  d <- d |>
    mutate(dags = parse_ksi_date(date_raw, timabil)) |>
    select(timabil, dags, heima, stig_heima, gestir, stig_gestir)

  if (!is.na(division)) {
    d <- d |> mutate(division = division)
  }

  d
}


#' Scrape upcoming match schedule from KSI.is
#'
#' @param comp_id Single competition ID
#' @return A tibble with columns: dags, heima, gestir
scrape_ksi_schedule <- function(comp_id) {
  empty <- tibble(dags = as.Date(character()), heima = character(), gestir = character())

  page <- fetch_ksi_page(comp_id)
  if (is.null(page)) {
    return(empty)
  }

  year <- extract_season_year(page)

  matches <- extract_matches(page, played_only = FALSE)
  d <- bind_rows(matches)
  if (nrow(d) == 0) {
    return(empty)
  }

  d |>
    mutate(dags = parse_ksi_date(date_raw, year)) |>
    filter(dags >= Sys.Date()) |>
    select(dags, heima, gestir)
}

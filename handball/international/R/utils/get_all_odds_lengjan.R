#' Get all odds (1x2, handicap, goals) from Lengjan
#'
#' This module scrapes outcome, handicap, and total goals odds from Lengjan.
#' It requires a live browser session since the site uses JavaScript rendering.
#'
#' The approach:
#' 1. Load the competition page to get list of matches
#' 2. For each match, navigate to its detail page
#' 3. Extract all available bet types from the detail page
#'
#' @examples
#' box::use(R/utils/get_all_odds_lengjan)
#' odds <- get_all_odds_lengjan$get_all_odds(
#'   sport = 6,
#'   country = "IS",
#'   competition = "1269"
#' )

#' Get match URLs from competition page
#' @param sport Sport ID (6 = handball)
#' @param country Country code (e.g., "IS")
#' @param competition Competition ID
#' @return Character vector of match URLs
get_match_urls <- function(sport, country, competition) {
  box::use(
    glue[glue],
    rvest[read_html_live, html_elements, html_attr],
    stringr[str_c, str_detect]
  )

  base_url <- "https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}"
  url <- glue(base_url)

  page <- read_html_live(url)
  Sys.sleep(2)

  # Click "show all" button if present
  show_all_selector <- "._14isqi70._14isqi7q._14isqi7z.pi7fsa4"
  if (length(page$html_elements(show_all_selector)) > 0) {
    page$click(show_all_selector)
    Sys.sleep(1)
  }

 # Get match links - these should be the clickable match rows
  hrefs <- page |>
    html_elements(".lj1n6v1") |>
    html_attr("href")

  # Filter to only match pages (not other links)
  match_hrefs <- hrefs[str_detect(hrefs, "leikur|match|event", negate = FALSE) |
                        str_detect(hrefs, "^/[0-9]+")]

  # Construct full URLs
  match_urls <- str_c("https://games.lotto.is", match_hrefs)

  page$session$close()

  match_urls
}

#' Extract all odds from a single match page
#' @param match_url Full URL to match detail page
#' @return List with match info and all odds types
get_match_odds <- function(match_url) {
  box::use(
    rvest[read_html_live, html_elements, html_text, html_attr],
    stringr[str_split, str_detect, str_extract, str_trim, str_replace_all],
    tibble[tibble],
    dplyr[bind_rows],
    purrr[map, map_chr, possibly]
  )

  page <- read_html_live(match_url)
  Sys.sleep(2)

  result <- list(
    url = match_url,
    outcome = NULL,
    handicap = NULL,
    goals = NULL
  )

  # Try to get match info (teams and date)
  tryCatch({
    # Match title usually contains "Team1 - Team2"
    match_title <- page |>
      html_elements(".lj1n6v9, .match-title, [class*='teams']") |>
      html_text()

    if (length(match_title) > 0) {
      teams <- str_split(match_title[1], " - ")[[1]]
      if (length(teams) >= 2) {
        result$home <- str_trim(teams[1])
        result$away <- str_trim(teams[2])
      }
    }

    # Get date
    date_elements <- page |>
      html_elements("[class*='date'], [class*='time']") |>
      html_text()

    date_text <- date_elements[str_detect(date_elements, "\\d+\\.")]
    if (length(date_text) > 0) {
      result$date_raw <- date_text[1]
    }
  }, error = function(e) {
    message("Could not extract match info: ", e$message)
  })

  # Look for betting market sections
  # Lengjan typically organizes markets in expandable sections

  # Try clicking on different market tabs/sections to reveal odds
  market_selectors <- c(
    "[data-market='handicap']",
    "[data-market='total']",
    ".market-tab",
    ".bet-type-selector",
    "button:contains('Forgjöf')",
    "button:contains('Mörk')",
    "[class*='handicap']",
    "[class*='total']"
  )

  for (selector in market_selectors) {
    tryCatch({
      elements <- page$html_elements(selector)
      if (length(elements) > 0) {
        page$click(selector)
        Sys.sleep(0.5)
      }
    }, error = function(e) NULL)
  }

  Sys.sleep(1)

  # Extract all visible odds
  # Look for odds buttons/elements
  odds_elements <- page |>
    html_elements(".uazl1c1, [class*='odds'], [class*='price'], .bet-button")

  odds_text <- odds_elements |> html_text()

  # Try to identify and categorize odds by their context
  # This will need refinement based on actual page structure

  # Look for handicap odds (usually have +/- numbers nearby)
  handicap_section <- page |>
    html_elements("[class*='handicap'], [data-type='handicap']")

  if (length(handicap_section) > 0) {
    handicap_odds <- handicap_section |>
      html_elements("[class*='odds'], .uazl1c1") |>
      html_text()

    handicap_lines <- handicap_section |>
      html_elements("[class*='line'], [class*='spread']") |>
      html_text()

    if (length(handicap_odds) > 0) {
      result$handicap <- list(
        odds = handicap_odds,
        lines = handicap_lines
      )
    }
  }

  # Look for total goals odds
  goals_section <- page |>
    html_elements("[class*='total'], [data-type='total'], [class*='over-under']")

  if (length(goals_section) > 0) {
    goals_odds <- goals_section |>
      html_elements("[class*='odds'], .uazl1c1") |>
      html_text()

    goals_lines <- goals_section |>
      html_elements("[class*='line'], [class*='total']") |>
      html_text()

    if (length(goals_odds) > 0) {
      result$goals <- list(
        odds = goals_odds,
        lines = goals_lines
      )
    }
  }

  page$session$close()

  result
}

#' Alternative approach: Scrape using page inspection
#' This function provides a more robust approach by examining the full page source
#' @param sport Sport ID
#' @param country Country code
#' @param competition Competition ID
#' @return Tibble with all available odds
get_all_odds_inspect <- function(sport, country, competition) {
  box::use(
    glue[glue],
    rvest[read_html_live, html_elements, html_text, html_attr, html_children],
    stringr[str_split, str_detect, str_extract, str_trim, str_c, str_replace,
            str_extract_all, str_match],
    tibble[tibble, add_row],
    dplyr[bind_rows, mutate, filter],
    purrr[map, map_chr, map_df, possibly],
    lubridate[dmy]
  )

  base_url <- "https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}"
  url <- glue(base_url)

  page <- read_html_live(url)
  Sys.sleep(2)

  # Click "show all" button if present
  show_all_selector <- "._14isqi70._14isqi7q._14isqi7z.pi7fsa4"
  if (length(page$html_elements(show_all_selector)) > 0) {
    page$click(show_all_selector)
    Sys.sleep(1)
  }

  # Get all match containers
  # Each match is typically in a row/card element
  match_containers <- page$html_elements(".lj1n6v1, [class*='event-row'], [class*='match-row']")

  results <- tibble(
    date = character(),
    home = character(),
    away = character(),
    bet_type = character(),
    selection = character(),
    line = numeric(),
    odds = numeric()
  )

  for (i in seq_along(match_containers)) {
    container <- match_containers[[i]]

    # Extract teams
    team_text <- container |>
      html_elements("._469dd00._469dd0o._469dd0v.lj1n6v9, [class*='teams']") |>
      html_text()

    if (length(team_text) == 0) next

    teams <- str_split(team_text[1], " - ")[[1]]
    if (length(teams) < 2) next

    home <- str_trim(teams[1])
    away <- str_trim(teams[2])

    # Extract date
    date_text <- container |>
      html_elements("._469dd00._469dd0t._469dd0v, [class*='date']") |>
      html_text()

    match_date <- NA
    if (length(date_text) > 0) {
      date_str <- date_text[str_detect(date_text, "\\.")][1]
      if (!is.na(date_str)) {
        # Parse Icelandic date format
        date_str <- date_str |>
          str_extract("\\d+\\. \\w+") |>
          str_c(" 2025") |>
          str_replace("jan", "01") |>
          str_replace("feb", "02") |>
          str_replace("mar", "03") |>
          str_replace("apr", "04") |>
          str_replace("maí", "05") |>
          str_replace("jún", "06") |>
          str_replace("júl", "07") |>
          str_replace("ágú", "08") |>
          str_replace("sep", "09") |>
          str_replace("okt", "10") |>
          str_replace("nóv", "11") |>
          str_replace("des", "12")

        tryCatch({
          match_date <- dmy(date_str)
        }, error = function(e) NULL)
      }
    }

    # Get 1x2 odds from the main view
    odds_elements <- container |>
      html_elements(".lj1n6vd .uazl1c1.uazl1c5.uazl1ca, [class*='odds-button']") |>
      html_text()

    if (length(odds_elements) >= 3) {
      # 1x2 odds
      results <- results |>
        add_row(
          date = as.character(match_date),
          home = home,
          away = away,
          bet_type = "outcome",
          selection = "home",
          line = NA_real_,
          odds = as.numeric(odds_elements[1])
        ) |>
        add_row(
          date = as.character(match_date),
          home = home,
          away = away,
          bet_type = "outcome",
          selection = "draw",
          line = NA_real_,
          odds = as.numeric(odds_elements[2])
        ) |>
        add_row(
          date = as.character(match_date),
          home = home,
          away = away,
          bet_type = "outcome",
          selection = "away",
          line = NA_real_,
          odds = as.numeric(odds_elements[3])
        )
    }

    # Try to click into match for more odds
    match_link <- container |>
      html_attr("href")

    if (!is.na(match_link) && nchar(match_link) > 0) {
      full_url <- str_c("https://games.lotto.is", match_link)

      tryCatch({
        detail_page <- read_html_live(full_url)
        Sys.sleep(1.5)

        # Look for handicap section
        # Try clicking handicap tab if exists
        handicap_tabs <- c(
          "button:contains('Forgjöf')",
          "[data-tab='handicap']",
          ".market-selector:contains('Forgjöf')"
        )

        for (tab in handicap_tabs) {
          tryCatch({
            if (length(detail_page$html_elements(tab)) > 0) {
              detail_page$click(tab)
              Sys.sleep(0.5)
              break
            }
          }, error = function(e) NULL)
        }

        # Extract handicap odds
        handicap_rows <- detail_page$html_elements("[class*='handicap-row'], [class*='spread-row']")

        for (row in handicap_rows) {
          line_text <- row |> html_elements("[class*='line']") |> html_text()
          odds_vals <- row |> html_elements("[class*='odds']") |> html_text()

          if (length(line_text) > 0 && length(odds_vals) >= 2) {
            line_num <- as.numeric(str_extract(line_text[1], "-?\\d+\\.?\\d*"))

            results <- results |>
              add_row(
                date = as.character(match_date),
                home = home,
                away = away,
                bet_type = "handicap",
                selection = "home",
                line = line_num,
                odds = as.numeric(odds_vals[1])
              ) |>
              add_row(
                date = as.character(match_date),
                home = home,
                away = away,
                bet_type = "handicap",
                selection = "away",
                line = -line_num,
                odds = as.numeric(odds_vals[2])
              )
          }
        }

        # Look for total goals section
        goals_tabs <- c(
          "button:contains('Mörk')",
          "button:contains('Stig')",
          "[data-tab='total']",
          ".market-selector:contains('Mörk')"
        )

        for (tab in goals_tabs) {
          tryCatch({
            if (length(detail_page$html_elements(tab)) > 0) {
              detail_page$click(tab)
              Sys.sleep(0.5)
              break
            }
          }, error = function(e) NULL)
        }

        # Extract total goals odds
        goals_rows <- detail_page$html_elements("[class*='total-row'], [class*='over-under']")

        for (row in goals_rows) {
          line_text <- row |> html_elements("[class*='line'], [class*='total']") |> html_text()
          odds_vals <- row |> html_elements("[class*='odds']") |> html_text()

          if (length(line_text) > 0 && length(odds_vals) >= 2) {
            line_num <- as.numeric(str_extract(line_text[1], "\\d+\\.?\\d*"))

            results <- results |>
              add_row(
                date = as.character(match_date),
                home = home,
                away = away,
                bet_type = "goals",
                selection = "over",
                line = line_num,
                odds = as.numeric(odds_vals[1])
              ) |>
              add_row(
                date = as.character(match_date),
                home = home,
                away = away,
                bet_type = "goals",
                selection = "under",
                line = line_num,
                odds = as.numeric(odds_vals[2])
              )
          }
        }

        detail_page$session$close()

      }, error = function(e) {
        message("Error getting detail odds for ", home, " vs ", away, ": ", e$message)
      })
    }
  }

  page$session$close()

  results |>
    filter(!is.na(odds))
}

#' Main function to get all odds
#' @export
#' @param sport Sport ID (6 = handball)
#' @param country Country code
#' @param competition Competition ID
#' @param verbose Print progress messages
#' @return Tibble with all odds in long format
get_all_odds <- function(sport, country, competition, verbose = TRUE) {
  if (verbose) message("Fetching odds for competition ", competition, "...")

  tryCatch({
    get_all_odds_inspect(sport, country, competition)
  }, error = function(e) {
    message("Error: ", e$message)
    tibble::tibble()
  })
}

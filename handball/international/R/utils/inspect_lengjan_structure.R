#' Diagnostic script to inspect Lengjan page structure
#'
#' Run this script to understand the DOM structure of Lengjan pages.
#' This will help refine the scraping selectors.
#'
#' Usage:
#' source("R/utils/inspect_lengjan_structure.R")
#' # Then check the output files in the working directory

library(rvest)
library(stringr)
library(glue)

# Configuration
sport <- 6
country <- "IS"
competition <- "1269"  # Men's Div 1

#' Inspect the main competition page
inspect_competition_page <- function() {
  url <- glue("https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}")

  message("Loading: ", url)
  page <- read_html_live(url)
  Sys.sleep(3)

  # Click show all if present
  tryCatch({
    page$click("._14isqi70._14isqi7q._14isqi7z.pi7fsa4")
    Sys.sleep(1)
  }, error = function(e) message("No 'show all' button found"))

  # Get all unique class names on the page
  all_elements <- page$html_elements("*")
  classes <- all_elements |>
    html_attr("class") |>
    na.omit() |>
    unique()

  message("\n=== UNIQUE CSS CLASSES ===")
  message("Found ", length(classes), " unique class combinations")

  # Save to file for inspection
  writeLines(classes, "lengjan_classes.txt")
  message("Saved to lengjan_classes.txt")

  # Find elements that look like match containers
  message("\n=== POTENTIAL MATCH CONTAINERS ===")
  container_patterns <- c("match", "event", "game", "lj1n6v", "leikur")

  for (pattern in container_patterns) {
    matching <- classes[str_detect(classes, pattern)]
    if (length(matching) > 0) {
      message("\nPattern '", pattern, "' matches:")
      for (cls in matching[1:min(5, length(matching))]) {
        message("  - ", cls)
      }
    }
  }

  # Find elements that look like odds
  message("\n=== POTENTIAL ODDS ELEMENTS ===")
  odds_patterns <- c("odds", "price", "uazl", "coefficient")

  for (pattern in odds_patterns) {
    matching <- classes[str_detect(classes, pattern)]
    if (length(matching) > 0) {
      message("\nPattern '", pattern, "' matches:")
      for (cls in matching[1:min(5, length(matching))]) {
        message("  - ", cls)
      }
    }
  }

  # Get all links (to find match detail pages)
  message("\n=== LINKS ON PAGE ===")
  links <- page$html_elements("a") |> html_attr("href") |> na.omit() |> unique()
  match_links <- links[str_detect(links, "/lengjan/[0-9]|leikur|match")]
  message("Potential match links: ", length(match_links))
  if (length(match_links) > 0) {
    message("First few:")
    for (link in match_links[1:min(5, length(match_links))]) {
      message("  - ", link)
    }
  }

  # Save full page HTML for inspection
  page_html <- page$html_elements("body") |> as.character()
  writeLines(page_html, "lengjan_page.html")
  message("\nSaved full page HTML to lengjan_page.html")

  page$session$close()

  invisible(list(classes = classes, links = links))
}

#' Inspect a single match detail page
#' @param match_path Path component like "/lengjan/12345"
inspect_match_page <- function(match_path = NULL) {

  # If no path provided, get first match from competition page
  if (is.null(match_path)) {
    url <- glue("https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}")
    page <- read_html_live(url)
    Sys.sleep(2)

    # Try to find a match link
    links <- page$html_elements("a") |> html_attr("href") |> na.omit()
    match_links <- links[str_detect(links, "/lengjan/[0-9]")]

    if (length(match_links) == 0) {
      # Try clicking on a match row
      page$click(".lj1n6v1")
      Sys.sleep(2)

      # Get current URL
      message("Clicked on match, current URL should show in browser")
      page$session$close()
      return(invisible(NULL))
    }

    match_path <- match_links[1]
    page$session$close()
  }

  full_url <- paste0("https://games.lotto.is", match_path)
  message("Loading match page: ", full_url)

  page <- read_html_live(full_url)
  Sys.sleep(3)

  # Get all classes on match detail page
  all_elements <- page$html_elements("*")
  classes <- all_elements |>
    html_attr("class") |>
    na.omit() |>
    unique()

  # Look for market/bet type selectors
  message("\n=== MARKET SELECTORS ===")
  market_patterns <- c("market", "tab", "forgjöf", "handicap", "mörk", "total", "stig")

  for (pattern in market_patterns) {
    matching <- classes[str_detect(tolower(classes), pattern)]
    if (length(matching) > 0) {
      message("\nPattern '", pattern, "' matches:")
      for (cls in matching[1:min(5, length(matching))]) {
        message("  - ", cls)
      }
    }
  }

  # Find all buttons (often used for market tabs)
  message("\n=== BUTTONS ===")
  buttons <- page$html_elements("button")
  button_text <- buttons |> html_text()
  message("Button labels: ", paste(unique(button_text), collapse = ", "))

  # Find all text that looks like betting lines (+/- numbers)
  message("\n=== POTENTIAL BETTING LINES ===")
  all_text <- page$html_elements("*") |> html_text()
  line_text <- all_text[str_detect(all_text, "^[+-]?\\d+\\.5$|^[+-]\\d+$")]
  message("Found lines: ", paste(unique(line_text)[1:min(10, length(unique(line_text)))], collapse = ", "))

  # Save match page HTML
  page_html <- page$html_elements("body") |> as.character()
  writeLines(page_html, "lengjan_match_page.html")
  message("\nSaved match page HTML to lengjan_match_page.html")

  page$session$close()

  invisible(classes)
}

#' Interactive exploration - click through tabs and report what's found
explore_match_markets <- function(match_path = NULL) {

  if (is.null(match_path)) {
    # Get first match URL
    url <- glue("https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}&competition={competition}")
    page <- read_html_live(url)
    Sys.sleep(2)

    links <- page$html_elements("a") |> html_attr("href") |> na.omit()
    match_links <- links[str_detect(links, "/lengjan/[0-9]")]

    if (length(match_links) == 0) {
      message("No match links found. Trying to click match row...")
      page$click(".lj1n6v1")
      Sys.sleep(2)
    } else {
      page$session$close()
      match_path <- match_links[1]
      page <- read_html_live(paste0("https://games.lotto.is", match_path))
      Sys.sleep(2)
    }
  } else {
    page <- read_html_live(paste0("https://games.lotto.is", match_path))
    Sys.sleep(2)
  }

  message("\n=== EXPLORING MATCH MARKETS ===")

  # Function to capture current odds state
  capture_odds <- function(label) {
    odds <- page$html_elements(".uazl1c1, [class*='odds'], [class*='price']") |> html_text()
    message("\n", label, ":")
    message("  Odds found: ", length(odds))
    if (length(odds) > 0) {
      message("  Values: ", paste(odds[1:min(10, length(odds))], collapse = ", "))
    }
    return(odds)
  }

  # Initial state
  initial_odds <- capture_odds("Initial page load")

  # Try clicking various potential market selectors
  selectors_to_try <- c(
    # Icelandic terms
    "button:contains('Forgjöf')",
    "button:contains('Mörk')",
    "button:contains('Stig')",
    "button:contains('Úrslit')",
    # Generic
    "[data-market]",
    ".market-tab",
    ".bet-type",
    # Tab-like elements
    "[role='tab']",
    ".tab"
  )

  for (selector in selectors_to_try) {
    tryCatch({
      elements <- page$html_elements(selector)
      if (length(elements) > 0) {
        message("\nFound selector: ", selector, " (", length(elements), " elements)")

        # Click and capture
        page$click(selector)
        Sys.sleep(1)
        capture_odds(paste("After clicking", selector))
      }
    }, error = function(e) NULL)
  }

  # Try clicking any unclicked buttons
  buttons <- page$html_elements("button")
  button_text <- buttons |> html_text() |> unique()

  for (btn_txt in button_text) {
    if (nchar(btn_txt) > 0 && nchar(btn_txt) < 30) {
      tryCatch({
        page$click(paste0("button:contains('", btn_txt, "')"))
        Sys.sleep(0.5)
        capture_odds(paste("After clicking button:", btn_txt))
      }, error = function(e) NULL)
    }
  }

  page$session$close()
  message("\n=== EXPLORATION COMPLETE ===")
}

# Run diagnostics
message("Starting Lengjan page structure inspection...")
message("This will create diagnostic files in your working directory.\n")

result <- inspect_competition_page()

message("\nNow inspecting a match detail page...")
inspect_match_page()

message("\n\nTo explore market tabs interactively, run:")
message("  explore_match_markets()")

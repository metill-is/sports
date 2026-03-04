#' Core Lengjan scraping functions with rate limiting
#'
#' Two-stage approach:
#'   1. scrape_competition() — loads competition list page, extracts 1x2 odds + match hrefs
#'   2. scrape_match_detail() — loads individual match page, extracts handicap + total odds
#'
#' Both functions include on.exit() browser cleanup and randomised delays.
#'
#' CSS class mapping (hashed by Lengjan's CSS Modules build — may change on deploy):
#'   Competition page:
#'     lj1n6v0  = match container (div)
#'     lj1n6v1  = match link (a) — href to detail page
#'     lj1n6v9  = team names (div > span, span, span)
#'     lj1n6vb  = odds container (div)
#'     lj1n6vd  = 1x2 odds list (ol)
#'     lj1n6ve  = "more markets" link (a)
#'   Detail page:
#'     zh0raz0  = market section (section) — identified by heading text
#'     h7cub57  = odds value display (div > p)
#'   Shared:
#'     uazl1c1  = odds button (button)

box::use(
  R/lengjan/parse[parse_lengjan_dates]
)

# --- Rate limiting helpers ---------------------------------------------------

#' @export
rate_limit_config <- list(
  page_delay_min = 2,
  page_delay_max = 5,
  click_delay_min = 1.5,
  click_delay_max = 3,
  backoff_base = 5,
  backoff_max = 30,
  max_retries = 2
)

#' @export
sleep_between_pages <- function() {
  Sys.sleep(stats::runif(1, rate_limit_config$page_delay_min, rate_limit_config$page_delay_max))
}

sleep_after_click <- function() {
  Sys.sleep(stats::runif(1, rate_limit_config$click_delay_min, rate_limit_config$click_delay_max))
}

sleep_backoff <- function(attempt) {
  delay <- min(
    rate_limit_config$backoff_base * 2^attempt,
    rate_limit_config$backoff_max
  ) + stats::runif(1, 0, 2)
  message("  Backing off for ", round(delay, 1), "s (attempt ", attempt + 1, ")")
  Sys.sleep(delay)
}

# --- CSS selectors (centralised) ---------------------------------------------
# These are hashed class names from Lengjan's CSS Modules build.
# If scraping breaks after a site deploy, update these by inspecting the DOM.

#' @export
selectors <- list(
  # Competition list page
  match_container = "div.lj1n6v0",
  match_link      = "a.lj1n6v1",
  teams           = ".lj1n6v9",
  odds_list       = "ol.lj1n6vd",
  odds_button     = "button.uazl1c1",
  odds_value      = ".h7cub57 p",
  more_markets    = "a.lj1n6ve",

  # Detail page (market sections identified by heading text, not class)
  market_section  = "section.zh0raz0"
)

# --- Stage 1: Competition page ------------------------------------------------

#' @export
#' Scrape 1x2 odds and match URLs from a competition list page
#' @param sport Numeric sport ID (e.g., 6 for handball)
#' @param country Country code (e.g., "IS", "ENG")
#' @param competition Competition ID string (e.g., "1269"). Optional.
#' @return List with $odds_1x2 (tibble) and $match_urls (character vector)
scrape_competition <- function(sport, country, competition = NULL) {
  box::use(
    glue[glue],
    rvest[read_html_live, html_elements, html_text, html_attr],
    purrr[map_chr, map_dbl],
    stringr[str_split, str_c, str_extract, str_trim, str_detect],
    tibble[tibble]
  )

  base_url <- "https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}"
  url <- glue(base_url)
  if (!is.null(competition)) url <- str_c(url, "&competition=", competition)

  message("  Loading competition page: ", url)

  page <- read_html_live(url)
  on.exit(tryCatch(page$session$close(), error = function(e) NULL))

  sleep_after_click()

  # Click "Sjá allt" button if present (expands truncated match list)
  # This button uses hashed classes — try multiple approaches
  tryCatch({
    show_all_candidates <- page$html_elements("button")
    for (btn_idx in seq_along(show_all_candidates)) {
      btn_text <- show_all_candidates[[btn_idx]] |> html_text() |> str_trim()
      if (str_detect(btn_text, "(?i)sj\u00e1 allt")) {
        page$click(str_c("button:nth-of-type(", btn_idx, ")"))
        sleep_after_click()
        break
      }
    }
  }, error = function(e) NULL)

  # --- Extract match data from containers ---
  containers <- page |> html_elements(selectors$match_container)
  n_matches <- length(containers)

  if (n_matches == 0) {
    message("  No matches found on page")
    return(list(odds_1x2 = tibble(), match_urls = character(0)))
  }

  # Pre-allocate
  home <- character(n_matches)
  away <- character(n_matches)
  date_strings <- character(n_matches)
  o_home <- character(n_matches)
  o_draw <- character(n_matches)
  o_away <- character(n_matches)
  match_hrefs <- character(n_matches)

  for (i in seq_len(n_matches)) {
    container <- containers[[i]]

    # --- Teams ---
    team_div <- container |> html_elements(selectors$teams)
    if (length(team_div) > 0) {
      spans <- team_div[[1]] |> html_elements("span") |> html_text()
      # Expected: c("Home", "-", "Away")
      if (length(spans) >= 3) {
        home[i] <- str_trim(spans[1])
        away[i] <- str_trim(spans[3])
      } else if (length(spans) >= 1) {
        # Fallback: split on " - "
        parts <- str_split(str_c(spans, collapse = ""), " - ")[[1]]
        home[i] <- if (length(parts) >= 1) str_trim(parts[1]) else NA_character_
        away[i] <- if (length(parts) >= 2) str_trim(parts[2]) else NA_character_
      }
    }

    # --- Date ---
    link <- container |> html_elements(selectors$match_link)
    if (length(link) > 0) {
      # First <p> in the link contains "3. mar19:45"
      date_p <- link[[1]] |> html_elements("p")
      if (length(date_p) > 0) {
        date_strings[i] <- date_p[[1]] |> html_text()
      }

      # Match detail URL
      href <- link[[1]] |> html_attr("href")
      if (!is.na(href) && nchar(href) > 0) {
        match_hrefs[i] <- str_c("https://games.lotto.is", href)
      }
    }

    # --- 1x2 odds ---
    # Primary: parse aria-label attributes ("1, stuðull: 1.39")
    odds_buttons <- container |> html_elements(selectors$odds_list) |>
      html_elements(selectors$odds_button)

    if (length(odds_buttons) >= 3) {
      labels <- odds_buttons |> html_attr("aria-label")
      odds_vals <- str_extract(labels, "[0-9]+\\.[0-9]+")
      o_home[i] <- odds_vals[1]
      o_draw[i] <- odds_vals[2]
      o_away[i] <- odds_vals[3]
    } else {
      # Fallback: extract from nested <p> elements
      odds_p <- container |> html_elements(selectors$odds_list) |>
        html_elements(selectors$odds_value) |> html_text()
      if (length(odds_p) >= 3) {
        o_home[i] <- odds_p[1]
        o_draw[i] <- odds_p[2]
        o_away[i] <- odds_p[3]
      }
    }
  }

  # Parse dates
  dates <- parse_lengjan_dates(date_strings)

  odds_1x2 <- tibble(
    dates = dates,
    home = home,
    away = away,
    o_home = o_home,
    o_draw = o_draw,
    o_away = o_away
  )

  # Drop rows with missing teams or odds
  odds_1x2 <- odds_1x2[
    !is.na(odds_1x2$home) & nchar(odds_1x2$home) > 0 &
    !is.na(odds_1x2$away) & nchar(odds_1x2$away) > 0,
  ]

  # Filter match URLs to non-empty

  match_urls <- match_hrefs[nchar(match_hrefs) > 0]

  message("  Found ", nrow(odds_1x2), " matches, ", length(match_urls), " detail links")

  list(
    odds_1x2 = odds_1x2,
    match_urls = match_urls
  )
}

# --- Stage 2: Match detail page -----------------------------------------------

#' @export
#' Scrape handicap and total goals odds from a match detail page
#'
#' Uses aria-controls IDs to target specific market sections:
#'   row-OU_FT = Over/Under Full Time (Yfir eða undir)
#'   row-HC_FT = Handicap Full Time (Forgjöf)
#'
#' @param match_url Full URL to a Lengjan match detail page
#' @param home Home team name (for labelling output)
#' @param away Away team name (for labelling output)
#' @param match_date Date of the match
#' @return List with $handicap (tibble or NULL) and $totals (tibble or NULL)
scrape_match_detail <- function(match_url, home = NA_character_, away = NA_character_, match_date = NA) {
  box::use(
    rvest[read_html_live, html_elements, html_text],
    stringr[str_detect, str_extract, str_trim, str_c],
    tibble[tibble],
    dplyr[bind_rows]
  )

  result <- list(handicap = NULL, totals = NULL)

  # Append marketTab=allMarkets so all section stubs are present in the DOM
  detail_url <- if (grepl("\\?", match_url)) {
    str_c(match_url, "&marketTab=allMarkets")
  } else {
    str_c(match_url, "?marketTab=allMarkets")
  }

  for (attempt in 0:rate_limit_config$max_retries) {
    page <- tryCatch(
      read_html_live(detail_url),
      error = function(e) {
        message("  Error loading ", detail_url, ": ", e$message)
        NULL
      }
    )

    if (!is.null(page)) {
      on.exit(tryCatch(page$session$close(), error = function(e) NULL))
      break
    }
    if (attempt < rate_limit_config$max_retries) sleep_backoff(attempt)
  }

  if (is.null(page)) return(result)

  # Wait for React hydration — the page loads HTML quickly but event handlers
  # take longer to bind. 3s is conservative but reliable.
  Sys.sleep(3)

  # --- Expand target market sections ---
  # Sections are collapsed by default. Each has an expand button with
  # aria-controls="row-{MARKET_ID}" and aria-expanded="false".
  # We only expand the markets we need (faster + fewer requests).
  market_ids <- list(
    totals   = "row-OU_FT",
    handicap = "row-HC_FT"
  )

  for (market_name in names(market_ids)) {
    market_id <- market_ids[[market_name]]
    btn_selector <- str_c("button[aria-controls=\"", market_id, "\"]")

    tryCatch({
      btns <- page$html_elements(btn_selector)
      if (length(btns) > 0) {
        page$click(btn_selector)
        # Wait for table to render after expansion
        Sys.sleep(stats::runif(1, 1.5, 2.5))
      }
    }, error = function(e) {
      message("  Could not expand ", market_name, " section: ", e$message)
    })
  }

  # --- Extract table data from expanded sections ---
  # After expanding, each section has a <table> with the odds.
  # Find all tables on the page and identify them by their parent section.
  sections <- page$html_elements(selectors$market_section)

  for (sec in sections) {
    # Identify section by heading text
    heading_p <- sec |> html_elements("p")
    if (length(heading_p) == 0) next
    heading_text <- heading_p[[1]] |> html_text() |> str_trim()

    if (heading_text == "Yfir e\u00f0a undir") {
      tryCatch({
        result$totals <- extract_table_market(sec, match_date, home, away, type = "totals")
      }, error = function(e) {
        message("  Totals extraction failed for ", home, " v ", away, ": ", e$message)
      })
    }

    if (heading_text == "Forgj\u00f6f") {
      tryCatch({
        result$handicap <- extract_table_market(sec, match_date, home, away, type = "handicap")
      }, error = function(e) {
        message("  Handicap extraction failed for ", home, " v ", away, ": ", e$message)
      })
    }
  }

  result
}


# --- Helper: extract odds from a table-based market section -------------------

#' Extract odds from a <table> inside a market section
#' @param section_node An rvest node for the <section> element
#' @param match_date Date of the match
#' @param home Home team name
#' @param away Away team name
#' @param type "totals" or "handicap"
#' @return tibble with odds rows, or NULL if no data
extract_table_market <- function(section_node, match_date, home, away, type) {
  box::use(
    rvest[html_elements, html_text],
    stringr[str_trim, str_extract],
    tibble[tibble],
    dplyr[bind_rows]
  )

  table_node <- section_node |> html_elements("table")
  if (length(table_node) == 0) return(NULL)

  rows <- table_node[[1]] |> html_elements("tbody tr")
  if (length(rows) == 0) return(NULL)

  row_list <- lapply(rows, function(row) {
    # Line value: in <th> (e.g., "1.5", "2.5" for totals; "1-0", "0-1" for handicap)
    th <- row |> html_elements("th")
    line_text <- if (length(th) > 0) th[[1]] |> html_text() |> str_trim() else NA_character_

    # Odds: in <td> cells, each containing .h7cub57 > p with the value
    tds <- row |> html_elements("td")
    odds_vals <- vapply(tds, function(td) {
      p_els <- td |> html_elements(".h7cub57 p")
      if (length(p_els) > 0) {
        p_els[[1]] |> html_text() |> str_trim()
      } else {
        # Fallback: aria-label on the button
        btn <- td |> html_elements("button")
        if (length(btn) > 0) {
          label <- btn[[1]] |> html_text() |> str_trim()
          str_extract(label, "[0-9]+\\.[0-9]+")
        } else {
          NA_character_
        }
      }
    }, character(1))

    if (type == "totals" && length(odds_vals) >= 2) {
      tibble(
        dates = as.Date(match_date),
        home = home,
        away = away,
        limit = as.numeric(line_text),
        o_over = odds_vals[1],
        o_under = odds_vals[2]
      )
    } else if (type == "handicap" && length(odds_vals) >= 2) {
      # Handicap can be 2-way (1, 2) or 3-way (1, X, 2)
      if (length(odds_vals) == 3) {
        tibble(
          dates = as.Date(match_date),
          home = home,
          away = away,
          line = line_text,
          o_home = odds_vals[1],
          o_draw = odds_vals[2],
          o_away = odds_vals[3]
        )
      } else {
        tibble(
          dates = as.Date(match_date),
          home = home,
          away = away,
          line = line_text,
          o_home = odds_vals[1],
          o_draw = NA_character_,
          o_away = odds_vals[2]
        )
      }
    } else {
      NULL
    }
  })

  result <- bind_rows(row_list)
  if (nrow(result) == 0) return(NULL)
  result
}

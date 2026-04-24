#' Core Lengjan scraping functions with rate limiting
#'
#' Two-stage approach:
#'   1. scrape_competition() -- loads competition list page, extracts 1x2 odds + match hrefs
#'   2. scrape_match_detail() -- loads individual match page, extracts handicap + total odds
#'
#' CSS class mapping (hashed by Lengjan's CSS Modules build -- may change on deploy):
#'   Competition page:
#'     lj1n6v0  = match container (div)
#'     lj1n6v1  = match link (a) -- href to detail page
#'     lj1n6v9  = team names (div > span, span, span)
#'     lj1n6vb  = odds container (div)
#'     lj1n6vd  = 1x2 odds list (ol)
#'     lj1n6ve  = "more markets" link (a)
#'   Detail page:
#'     zh0raz0  = market section (section) -- identified by inner button aria-controls
#'     h7cub57  = odds value display (div > p)
#'   Shared:
#'     uazl1c1  = odds button (button)

# --- Rate limiting helpers ---------------------------------------------------

rate_limit_config <- list(
  page_delay_min = 2,
  page_delay_max = 5,
  click_delay_min = 1.5,
  click_delay_max = 3,
  backoff_base = 5,
  backoff_max = 30,
  max_retries = 2
)

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

selectors <- list(
  match_container = "div.lj1n6v0",
  match_link      = "a.lj1n6v1",
  teams           = ".lj1n6v9",
  odds_list       = "ol.lj1n6vd",
  odds_button     = "button.uazl1c1",
  odds_value      = ".h7cub57 p",
  more_markets    = "a.lj1n6ve",
  market_section  = "section.zh0raz0"
)

# --- Stage 1: Competition page ------------------------------------------------

#' Scrape 1x2 odds and match URLs from a competition list page
#' @param sport Numeric sport ID (e.g., 1 for football)
#' @param country Country code (e.g., "ENG")
#' @param competition Competition ID string (e.g., "296")
#' @return List with $odds_1x2 (tibble) and $match_urls (character vector)
scrape_competition <- function(sport, country, competition = NULL) {
  base_url <- "https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}"
  url <- glue::glue(base_url)
  if (!is.null(competition)) url <- stringr::str_c(url, "&competition=", competition)

  message("  Loading competition page: ", url)

  page <- rvest::read_html_live(url)
  on.exit(tryCatch(page$session$close(), error = function(e) NULL))

  sleep_after_click()

  # Click "Sja allt" button if present (expands truncated match list)
  tryCatch(
    {
      show_all_candidates <- page$html_elements("button")
      for (btn_idx in seq_along(show_all_candidates)) {
        btn_text <- show_all_candidates[[btn_idx]] |>
          rvest::html_text() |>
          stringr::str_trim()
        if (stringr::str_detect(btn_text, "(?i)sj\u00e1 allt")) {
          page$click(stringr::str_c("button:nth-of-type(", btn_idx, ")"))
          sleep_after_click()
          break
        }
      }
    },
    error = function(e) NULL
  )

  # --- Extract match data from containers ---
  containers <- page |> rvest::html_elements(selectors$match_container)
  n_matches <- length(containers)

  if (n_matches == 0) {
    message("  No matches found on page")
    return(list(odds_1x2 = tibble::tibble(), match_urls = character(0)))
  }

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
    team_div <- container |> rvest::html_elements(selectors$teams)
    if (length(team_div) > 0) {
      spans <- team_div[[1]] |>
        rvest::html_elements("span") |>
        rvest::html_text()
      if (length(spans) >= 3) {
        home[i] <- stringr::str_trim(spans[1])
        away[i] <- stringr::str_trim(spans[3])
      } else if (length(spans) >= 1) {
        parts <- stringr::str_split(
          stringr::str_c(spans, collapse = ""), " - "
        )[[1]]
        home[i] <- if (length(parts) >= 1) stringr::str_trim(parts[1]) else NA_character_
        away[i] <- if (length(parts) >= 2) stringr::str_trim(parts[2]) else NA_character_
      }
    }

    # --- Date ---
    link <- container |> rvest::html_elements(selectors$match_link)
    if (length(link) > 0) {
      date_p <- link[[1]] |> rvest::html_elements("p")
      if (length(date_p) > 0) {
        date_strings[i] <- date_p[[1]] |> rvest::html_text()
      }
      href <- link[[1]] |> rvest::html_attr("href")
      if (!is.na(href) && nchar(href) > 0) {
        match_hrefs[i] <- stringr::str_c("https://games.lotto.is", href)
      }
    }

    # --- 1x2 odds ---
    odds_buttons <- container |>
      rvest::html_elements(selectors$odds_list) |>
      rvest::html_elements(selectors$odds_button)

    if (length(odds_buttons) >= 3) {
      labels <- odds_buttons |> rvest::html_attr("aria-label")
      odds_vals <- stringr::str_extract(labels, "[0-9]+\\.[0-9]+")
      o_home[i] <- odds_vals[1]
      o_draw[i] <- odds_vals[2]
      o_away[i] <- odds_vals[3]
    } else {
      odds_p <- container |>
        rvest::html_elements(selectors$odds_list) |>
        rvest::html_elements(selectors$odds_value) |>
        rvest::html_text()
      if (length(odds_p) >= 3) {
        o_home[i] <- odds_p[1]
        o_draw[i] <- odds_p[2]
        o_away[i] <- odds_p[3]
      }
    }
  }

  dates <- parse_lengjan_dates(date_strings)

  odds_1x2 <- tibble::tibble(
    dates = dates,
    home = home,
    away = away,
    o_home = o_home,
    o_draw = o_draw,
    o_away = o_away
  )

  odds_1x2 <- odds_1x2[
    !is.na(odds_1x2$home) & nchar(odds_1x2$home) > 0 &
      !is.na(odds_1x2$away) & nchar(odds_1x2$away) > 0,
  ]

  match_urls <- match_hrefs[nchar(match_hrefs) > 0]

  message("  Found ", nrow(odds_1x2), " matches, ", length(match_urls), " detail links")

  list(
    odds_1x2 = odds_1x2,
    match_urls = match_urls
  )
}

# --- Stage 2: Match detail page -----------------------------------------------

#' Scrape handicap and total goals odds from a match detail page
#' @param match_url Full URL to a Lengjan match detail page
#' @param home Home team name (for labelling output)
#' @param away Away team name (for labelling output)
#' @param match_date Date of the match
#' @return List with $handicap (tibble or NULL) and $totals (tibble or NULL)
scrape_match_detail <- function(match_url, home = NA_character_, away = NA_character_, match_date = NA) {
  result <- list(handicap = NULL, totals = NULL)

  detail_url <- if (grepl("\\?", match_url)) {
    stringr::str_c(match_url, "&marketTab=allMarkets")
  } else {
    stringr::str_c(match_url, "?marketTab=allMarkets")
  }

  for (attempt in 0:rate_limit_config$max_retries) {
    page <- tryCatch(
      rvest::read_html_live(detail_url),
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

  if (is.null(page)) {
    return(result)
  }

  # Wait for React hydration
  Sys.sleep(3)

  # --- Expand target market sections ---
  market_ids <- list(
    totals   = "row-OU_FT",
    handicap = "row-HC_FT"
  )

  for (market_name in names(market_ids)) {
    market_id <- market_ids[[market_name]]
    btn_selector <- stringr::str_c("button[aria-controls=\"", market_id, "\"]")

    tryCatch(
      {
        btns <- page$html_elements(btn_selector)
        if (length(btns) > 0) {
          page$click(btn_selector)
          Sys.sleep(stats::runif(1, 1.5, 2.5))
        }
      },
      error = function(e) {
        message("  Could not expand ", market_name, " section: ", e$message)
      }
    )
  }

  # --- Extract table data from expanded sections ---
  # Match sections by aria-controls on the expand button (stable across heading
  # renames -- Lengjan has shipped both "Yfir e<U+00F0>a undir" and "Yfir/Undir").
  sections <- page$html_elements(selectors$market_section)

  for (sec in sections) {
    btn <- sec |> rvest::html_elements("button[aria-controls]")
    if (length(btn) == 0) next
    aria_controls <- btn[[1]] |> rvest::html_attr("aria-controls")
    if (is.na(aria_controls)) next

    if (identical(aria_controls, "row-OU_FT")) {
      tryCatch(
        {
          result$totals <- extract_table_market(sec, match_date, home, away, type = "totals")
        },
        error = function(e) {
          message("  Totals extraction failed for ", home, " v ", away, ": ", e$message)
        }
      )
    } else if (identical(aria_controls, "row-HC_FT")) {
      tryCatch(
        {
          result$handicap <- extract_table_market(sec, match_date, home, away, type = "handicap")
        },
        error = function(e) {
          message("  Handicap extraction failed for ", home, " v ", away, ": ", e$message)
        }
      )
    }
  }

  result
}

# --- Helper: extract odds from a table-based market section -------------------

extract_table_market <- function(section_node, match_date, home, away, type) {
  table_node <- section_node |> rvest::html_elements("table")
  if (length(table_node) == 0) {
    return(NULL)
  }

  rows <- table_node[[1]] |> rvest::html_elements("tbody tr")
  if (length(rows) == 0) {
    return(NULL)
  }

  row_list <- lapply(rows, function(row) {
    th <- row |> rvest::html_elements("th")
    line_text <- if (length(th) > 0) {
      th[[1]] |>
        rvest::html_text() |>
        stringr::str_trim()
    } else {
      NA_character_
    }

    tds <- row |> rvest::html_elements("td")
    odds_vals <- vapply(tds, function(td) {
      p_els <- td |> rvest::html_elements(".h7cub57 p")
      if (length(p_els) > 0) {
        p_els[[1]] |>
          rvest::html_text() |>
          stringr::str_trim()
      } else {
        btn <- td |> rvest::html_elements("button")
        if (length(btn) > 0) {
          label <- btn[[1]] |>
            rvest::html_text() |>
            stringr::str_trim()
          stringr::str_extract(label, "[0-9]+\\.[0-9]+")
        } else {
          NA_character_
        }
      }
    }, character(1))

    if (type == "totals" && length(odds_vals) >= 2) {
      tibble::tibble(
        dates = as.Date(match_date),
        home = home,
        away = away,
        limit = as.numeric(line_text),
        o_over = odds_vals[1],
        o_under = odds_vals[2]
      )
    } else if (type == "handicap" && length(odds_vals) >= 2) {
      if (length(odds_vals) == 3) {
        tibble::tibble(
          dates = as.Date(match_date),
          home = home,
          away = away,
          line = line_text,
          o_home = odds_vals[1],
          o_draw = odds_vals[2],
          o_away = odds_vals[3]
        )
      } else {
        tibble::tibble(
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

  result <- dplyr::bind_rows(row_list)
  if (nrow(result) == 0) {
    return(NULL)
  }
  result
}

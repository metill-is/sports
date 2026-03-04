#' Fetch current 1x2, Asian Handicap and total-goals odds from Lengjan for a country.
#'
#' @param country Country code, e.g. "ENG" for England.
#' @param over_lines Character vector of over/under lines to scrape (e.g. c("2.5", "3.5")).
#' @return A tibble with match metadata and odds for 1x2, handicap and total-goals markets.
#' @export
get_football_markets <- function(country = "ENG", over_lines = c("2.5", "3.5")) {
  box::use(
    glue[glue],
    rvest[read_html_live, html_elements, html_text, html_attr],
    purrr[map_chr, map_dfr],
    stringr[str_split, str_detect, str_sub, str_c, str_replace, str_trim],
    lubridate[dmy],
    tibble[tibble]
  )
  
  sport <- 1
  # First fetch the listing page for the chosen country (all competitions)
  list_url <- glue("https://games.lotto.is/getraunaleikir/lengjan?sport={sport}&country={country}")
  listing <- read_html_live(list_url)
  
  # Extract basic fixture info as before
  teams <- listing |>
    html_elements("._469dd00._469dd0o._469dd0v.lj1n6v9") |>
    html_text() |>
    str_split(" - ")
  home  <- teams |> map_chr(1)
  away  <- teams |> map_chr(2)
  
  dates <- listing |>
    html_elements("._469dd00._469dd0t._469dd0v") |>
    html_text()
  dates <- dates[str_detect(dates, "\\.")] |>
      str_pad(width = 12, side = "left", pad = "0") |> 
    str_sub(1, 7) |>
    str_c(" ", format(Sys.Date(), "%Y")) |>
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
    str_replace("des", "12") |> 
    dmy(locale = "IS_is")
  
  # Extract 1x2 prices (three odds per match)
  odds1x2 <- listing |>
    html_elements(".lj1n6vd") |>
    html_elements(".uazl1c1.uazl1c5.uazl1ca") |>
    html_text()
  
  idx  <- seq_along(home) - 1
  o_home <- odds1x2[1 + 3 * idx]
  o_draw <- odds1x2[2 + 3 * idx]
  o_away <- odds1x2[3 + 3 * idx]
  
  # Extract the match links (to drill down into handicap / totals markets)
  match_links <- listing |>
    html_elements("a.lj1n6v1, a[aria-label*='markaðir']") |>  # anchors for each fixture
    html_attr("href") |>
    unique() |>
    (\(x) str_c("https://games.lotto.is", x))()
  
  # Function to parse handicap and totals on an individual match page
  parse_match_markets <- function(url, home_team, away_team, match_date) {
    match_page <- read_html_live(url)
    Sys.sleep(0.5)  # small pause to avoid hammering the server
    
    # Prepare a list to store market results
    rows <- list()
    
    ## --- Asian Handicap -----------------------------------------------------
    # Find all buttons whose aria-label starts with "Asíuforgjöf" and click them
    ah_buttons <- match_page$html_elements("button[aria-label^='Asíuforgjöf']")
    for (btn in ah_buttons) {
      btn$click()
      Sys.sleep(0.2)
    }
    
    match_page$html_elements()
    # After expanding, selections are in .uazl1c1.uazl1c5.uazl1ca; labels include the line
    ah_nodes <- match_page$html_elements("div:has(button[aria-label^='Asíuforgjöf'])")
    if (length(ah_nodes) > 0) {
      labels <- ah_nodes |> html_attr("aria-label")
      prices <- ah_nodes |> html_text()
      for (i in seq_along(labels)) {
        rows[[length(rows) + 1]] <- tibble(
          date    = match_date,
          home    = home_team,
          away    = away_team,
          market  = "Asian Handicap",
          outcome = str_trim(str_replace(labels[i], "stuðull:.*", "")),
          odds    = prices[i]
        )
      }
    }
    
    ## --- Over/under totals --------------------------------------------------
    for (line in over_lines) {
      # expand the over/under section for this line (Icelandic labels use comma as decimal)
      label_text <- glue("Yfir/undir {line}")
      ou_button  <- match_page$html_elements(glue("button:has-text('{label_text}')"))
      if (length(ou_button) > 0) {
        ou_button[[1]]$click()
        Sys.sleep(0.2)
        # within this section, selections should again be .uazl1c1.uazl1c5.uazl1ca
        ou_nodes <- match_page$html_elements(
          glue("div:has(button:has-text('{label_text}')) .uazl1c1.uazl1c5.uazl1ca")
        )
        if (length(ou_nodes) > 0) {
          ou_labels <- ou_nodes |> html_attr("aria-label")
          ou_prices <- ou_nodes |> html_text()
          for (j in seq_along(ou_labels)) {
            rows[[length(rows) + 1]] <- tibble(
              date    = match_date,
              home    = home_team,
              away    = away_team,
              market  = glue("Over/Under {line}"),
              outcome = str_trim(str_replace(ou_labels[j], "stuðull:.*", "")),
              odds    = ou_prices[j]
            )
          }
        }
      }
    }
    if (length(rows) == 0) return(NULL)
    do.call(rbind, rows)
  }
  
  # Iterate through all matches and parse markets
  extra_markets <- map_dfr(
    seq_along(match_links),
    \(i) parse_match_markets(match_links[i], home[i], away[i], dates[i])
  )
  
  # Combine the 1x2 odds with the extra markets
  base_df <- tibble(
    date   = dates,
    home   = home,
    away   = away,
    market = "1X2",
    outcome = c(rep("1", length(home)), rep("X", length(home)), rep("2", length(home))),
    odds   = c(o_home, o_draw, o_away)
  )
  full_df <- dplyr::bind_rows(base_df, extra_markets)
  full_df
}

# Example usage:
# Download Premier League (and other English competitions) odds including Asian handicap and totals
eng_markets <- get_football_markets(country = "ENG", over_lines = c("2.5", "3.5"))
print(eng_markets)

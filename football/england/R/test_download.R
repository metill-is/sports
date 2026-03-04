box::use(
  R / common / download_data,
  R / common / leagues,
  rvest[read_html_live]
)

# Test with Eliteserien 2024
test_league <- list(
  name = "eliteserien",
  years = 2024
)

# Generate URL
url <- download_data$make_results_url(
  test_league$name,
  2024,
  2025
)

print(paste("Testing URL:", url))

# Try to load the page
tryCatch(
  {
    page <- read_html_live(url)
    Sys.sleep(2)

    # Check if page loaded
    print("Page loaded successfully")

    # Try to find elements with correct selectors
    home_elements <- page$html_elements(
      ".event__homeParticipant strong, .event__homeParticipant span"
    )
    away_elements <- page$html_elements(
      ".event__awayParticipant strong, .event__awayParticipant span"
    )
    score_home_elements <- page$html_elements(".event__score--home")
    score_away_elements <- page$html_elements(".event__score--away")
    date_elements <- page$html_elements(".event__time")

    print(paste("Found", length(home_elements), "home team elements"))
    print(paste("Found", length(away_elements), "away team elements"))
    print(paste("Found", length(score_home_elements), "home score elements"))
    print(paste("Found", length(score_away_elements), "away score elements"))
    print(paste("Found", length(date_elements), "date elements"))

    # Test the read_page function
    if (length(home_elements) > 0 && length(away_elements) > 0) {
      print("Testing read_page function...")
      result <- download_data$read_page(page)
      print(paste("Successfully read", nrow(result), "matches"))
      if (nrow(result) > 0) {
        print("Sample data:")
        print(head(result, 3))
      }
    }

    # Close page
    page$session$close()
  },
  error = function(e) {
    print(paste("Error:", e$message))
  }
)

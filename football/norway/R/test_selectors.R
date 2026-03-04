box::use(
  R / common / download_data,
  rvest[read_html_live, html_elements, html_text]
)

# Test with Eliteserien 2024
url <- "https://www.livesport.com/en/soccer/norway/eliteserien-2024/results/"

print(paste("Testing URL:", url))

# Try to load the page
tryCatch(
  {
    page <- read_html_live(url)
    Sys.sleep(3)

    print("Page loaded successfully, testing different selectors...")

    # Test various possible selectors for team names
    selectors_to_test <- c(
      ".event__participant--home",
      ".event__participant--away",
      ".participant--home",
      ".participant--away",
      ".event__home",
      ".event__away",
      ".event-participant--home",
      ".event-participant--away",
      ".home-team",
      ".away-team",
      ".team-home",
      ".team-away",
      "[data-testid*='home']",
      "[data-testid*='away']",
      ".event__match .event__participant",
      ".participant__participantName",
      ".participantName",
      ".event .participant"
    )

    for (selector in selectors_to_test) {
      elements <- page$html_elements(selector)
      if (length(elements) > 0) {
        print(paste(
          "✓ Found",
          length(elements),
          "elements with selector:",
          selector
        ))
        # Show first few results
        if (length(elements) >= 2) {
          sample_text <- html_text(elements[1:2])
          print(paste("  Sample text:", paste(sample_text, collapse = " | ")))
        }
      } else {
        print(paste("✗ No elements found with selector:", selector))
      }
    }

    # Also try to find all classes that contain "participant" or "team"
    print("\n--- Looking for classes containing 'participant' or 'team' ---")
    all_elements <- page$html_elements(
      "*[class*='participant'], *[class*='team']"
    )
    if (length(all_elements) > 0) {
      print(paste(
        "Found",
        length(all_elements),
        "elements with 'participant' or 'team' in class"
      ))

      # Get unique class names
      classes <- character(0)
      for (i in 1:min(20, length(all_elements))) {
        elem_classes <- page$html_elements("*")[[i]]$get_attribute("class")
        if (!is.null(elem_classes) && length(elem_classes) > 0) {
          classes <- c(classes, elem_classes)
        }
      }
      unique_classes <- unique(classes)
      participant_classes <- unique_classes[grepl(
        "participant|team",
        unique_classes,
        ignore.case = TRUE
      )]
      if (length(participant_classes) > 0) {
        print("Relevant classes found:")
        for (cls in participant_classes[
          1:min(10, length(participant_classes))
        ]) {
          print(paste("  ", cls))
        }
      }
    }

    # Close page
    page$session$close()
  },
  error = function(e) {
    print(paste("Error:", e$message))
  }
)

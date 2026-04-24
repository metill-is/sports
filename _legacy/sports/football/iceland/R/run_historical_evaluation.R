#### Packages ####
library(tidyverse)
library(lubridate)
library(glue)
library(here)

# Source the historical evaluation function
source(here("R", "historical_evaluation.R"))

run_historical_evaluation <- function(sex = "male") {
  # Read data
  d <- read_csv(here("data", sex, "data.csv"))

  # Get all unique game dates
  game_dates <- d |>
    filter(
      timabil == 2025,
      division %in% c(1)
    ) |>
    arrange(dags) |>
    pull(dags) |>
    unique() |>
    rev()

  # Create results directory
  if (!dir.exists(here("results", sex, "historical"))) {
    dir.create(
      here("results", sex, "historical"),
      showWarnings = FALSE,
      recursive = TRUE
    )
  }

  # Loop over dates in reverse
  for (i in seq_along(game_dates)) {
    end_date <- game_dates[i]

    # Skip if we've already processed this date
    if (
      file.exists(
        here(
          "results",
          sex,
          "historical",
          end_date,
          "posterior_goals.csv"
        )
      )
    ) {
      next
    }

    # Fit model and save predictions
    tryCatch(
      {
        message(glue("Processing {format(end_date, '%Y-%m-%d')}"))
        fit_historical_model(
          start_date = clock::date_build(2021, 1, 1),
          end_date = end_date,
          sex = sex
        )
      },
      error = function(e) {
        message(glue(
          "Error processing {format(end_date, '%Y-%m-%d')}: {e$message}"
        ))
      }
    )
  }
}

# Example usage:
run_historical_evaluation(sex = "male")
run_historical_evaluation(sex = "female")

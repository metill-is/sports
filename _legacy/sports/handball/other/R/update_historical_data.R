box::use(
  R / utils / leagues,
  R / utils / download_data
)

# Function to read the JSON tracking file
read_update_log <- function() {
  json_file <- "data/last_updated.json"
  if (file.exists(json_file)) {
    jsonlite::fromJSON(json_file)
  } else {
    list()
  }
}

# Function to write the JSON tracking file
write_update_log <- function(update_log) {
  json_file <- "data/last_updated.json"
  jsonlite::write_json(update_log, json_file, pretty = TRUE, auto_unbox = TRUE)
}

# Function to update the log for a specific country
update_country_log <- function(country) {
  update_log <- read_update_log()
  update_log[[country]] <- as.character(Sys.Date())
  write_update_log(update_log)
}

# Function to check if a country was already updated today
is_updated_today <- function(country) {
  update_log <- read_update_log()
  if (country %in% names(update_log)) {
    last_update <- as.Date(update_log[[country]])
    return(last_update == Sys.Date())
  }
  return(FALSE)
}

for (league in leagues$leagues) {
  country <- league$country

  # Check if already updated today
  if (is_updated_today(country)) {
    print(paste("Skipping", country, "- already updated today"))
    next
  }

  print(paste("Starting download of", country))

  # Download the data
  download_data$update_historical_results(
    league
  )

  # Update the tracking log
  update_country_log(country)

  print(paste("Completed download of", country))
}

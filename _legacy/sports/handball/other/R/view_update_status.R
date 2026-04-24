#' View the last update status for all countries
#'
#' This function reads the JSON tracking file and displays when each country
#' was last updated in a human-readable format.
#'
#' @export
view_update_status <- function() {
  json_file <- "data/last_updated.json"

  if (!file.exists(json_file)) {
    cat("No update tracking file found. Run update_historical_data.R first.\n")
    return(invisible(NULL))
  }

  update_log <- jsonlite::fromJSON(json_file)

  if (length(update_log) == 0) {
    cat("No countries have been updated yet.\n")
    return(invisible(NULL))
  }

  cat("Last update dates for each country:\n")
  cat("===================================\n")

  today <- Sys.Date()

  for (country in names(update_log)) {
    last_updated <- as.Date(update_log[[country]])
    days_ago <- as.numeric(today - last_updated)

    status <- if (days_ago == 0) {
      "Today"
    } else if (days_ago == 1) {
      "1 day ago"
    } else {
      paste(days_ago, "days ago")
    }

    cat(sprintf(
      "%-15s: %s (%s)\n",
      stringr::str_to_title(gsub("-", " ", country)),
      format(last_updated, "%Y-%m-%d"),
      status
    ))
  }

  invisible(update_log)
}

#' Get the last update date for a specific country
#'
#' @param country The country name to check
#' @return Date of last update, or NULL if not found
#' @export
get_country_last_update <- function(country) {
  json_file <- "data/last_updated.json"

  if (!file.exists(json_file)) {
    return(NULL)
  }

  update_log <- jsonlite::fromJSON(json_file)

  if (country %in% names(update_log)) {
    return(as.Date(update_log[[country]]))
  } else {
    return(NULL)
  }
}

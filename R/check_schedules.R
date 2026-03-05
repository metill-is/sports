# Scan schedule.csv files to identify active competitions.
# Writes config/active_competitions.json for _targets.R filtering.
#
# Usage: Rscript R/check_schedules.R [--lookahead 14]
#
# A competition is "active" if any of its daily schedule.csv files
# contain matches within the next `lookahead` days.
#
# Cold-start safety: if NO schedule files exist at all, the JSON is
# not written — _targets.R scrapes everything.

library(readr)
library(jsonlite)

# -- CLI args ------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
lookahead <- 14L
if ("--lookahead" %in% args) {
  idx <- which(args == "--lookahead")
  lookahead <- as.integer(args[idx + 1])
}

# -- Config --------------------------------------------------------------------

here <- function(...) file.path(getwd(), ...)
config <- yaml::yaml.load(read_file(here("config", "competitions.yml")))
comp_keys <- setdiff(names(config), "defaults")

today <- Sys.Date()
horizon <- today + lookahead

# -- Date parsing --------------------------------------------------------------

#' Parse DD.MM. HH:MM dates with year inference
#'
#' If the inferred date is >6 months in the past, bump to next year.
parse_schedule_dates <- function(date_strings) {
  # Extract day and month
  matches <- regmatches(date_strings, regexpr("^(\\d{2})\\.(\\d{2})\\.", date_strings))
  valid <- nchar(matches) > 0
  if (!any(valid)) return(as.Date(character(0)))

  matches <- matches[valid]
  day <- as.integer(substr(matches, 1, 2))
  month <- as.integer(substr(matches, 4, 5))

  year <- as.integer(format(today, "%Y"))
  dates <- as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")

  # Year inference: if >6 months in the past, assume next year
  dates[!is.na(dates) & dates < (today - 180)] <- dates[!is.na(dates) & dates < (today - 180)] + 365
  dates[!is.na(dates)]
}

# -- Path resolution -----------------------------------------------------------

#' Get schedule.csv paths for a competition (daily leagues only)
resolve_schedule_paths <- function(comp) {
  paths <- character()

  if (!is.null(comp$leagues)) {
    # Football: flat league list
    for (league in comp$leagues) {
      paths <- c(paths,
        here("data", comp$livesport_sport, comp$country, "male", league, "schedule.csv")
      )
    }
  } else if (!is.null(comp$divisions)) {
    # Handball: nested by sex
    for (sex in names(comp$divisions)) {
      for (div in comp$divisions[[sex]]) {
        paths <- c(paths,
          here("data", comp$livesport_sport, comp$country, sex, div$label, "schedule.csv")
        )
      }
    }
  }

  paths
}

# -- Main scan -----------------------------------------------------------------

n_files_found <- 0L
active <- list()

for (key in comp_keys) {
  comp <- config[[key]]
  paths <- resolve_schedule_paths(comp)

  existing <- paths[file.exists(paths)]
  missing_dirs <- paths[!file.exists(dirname(paths))]

  if (length(missing_dirs) > 0) {
    warning(key, ": data directory missing for ",
            paste(basename(dirname(missing_dirs)), collapse = ", "))
  }

  if (length(existing) == 0) {
    message(key, ": no schedule files found — treating as active")
    active[[key]] <- TRUE
    next
  }

  n_files_found <- n_files_found + length(existing)

  # Check each schedule file for upcoming matches
  has_upcoming <- FALSE
  for (path in existing) {
    sched <- tryCatch(
      read_csv(path, col_types = cols(.default = "c"), n_max = 500),
      error = function(e) NULL
    )
    if (is.null(sched) || nrow(sched) == 0 || !"date" %in% names(sched)) next

    dates <- parse_schedule_dates(sched$date)
    if (any(dates >= today & dates <= horizon)) {
      has_upcoming <- TRUE
      break
    }
  }

  active[[key]] <- has_upcoming
  status <- if (has_upcoming) "ACTIVE" else "inactive"
  message(key, ": ", status, " (", length(existing), " schedule files)")
}

# -- Cold-start guard ----------------------------------------------------------

if (n_files_found == 0) {
  message("\nCold start: no schedule files found anywhere — skipping JSON write")
  message("All competitions will be scraped on next tar_make()")
  quit(status = 0)
}

# -- Write JSON ----------------------------------------------------------------

n_active <- sum(vapply(active, isTRUE, logical(1)))
n_inactive <- length(active) - n_active

result <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
  lookahead_days = lookahead,
  active = active
)

json_path <- here("config", "active_competitions.json")
tmp_path <- paste0(json_path, ".tmp")
write_json(result, tmp_path, auto_unbox = TRUE, pretty = TRUE)
file.rename(tmp_path, json_path)

message("\nWrote ", json_path)
message("Active: ", n_active, " / Inactive: ", n_inactive, " / Total: ", length(active))

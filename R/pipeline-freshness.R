#' Decide whether a (league, sex) pair needs refitting.
#'
#' Returns `TRUE` when there is at least one completed match with
#' `match_date` strictly later than the most recent `fit_date` partition
#' under `data/beliefs/archive/`. Returns `TRUE` when no fit exists yet.
#' Returns `FALSE` when no results exist (cannot fit on empty data) or
#' when no game has been played since the last fit.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @return Logical scalar.
#' @export
needs_refit <- function(static, sex, root = here::here("data")) {
  results <- read_table(
    "results",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(results) == 0L) {
    return(FALSE)
  }

  completed <- dplyr::filter(
    results, !is.na(.data$home_score), !is.na(.data$away_score)
  )
  if (nrow(completed) == 0L) {
    return(FALSE)
  }
  latest_match <- max(completed$match_date)

  archive_dir <- fs::path(
    root, "beliefs", "archive",
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  if (!fs::dir_exists(archive_dir)) {
    return(TRUE)
  }

  fit_dirs <- fs::dir_ls(archive_dir, type = "directory")
  if (length(fit_dirs) == 0L) {
    return(TRUE)
  }

  fit_dates <- as.Date(stringr::str_remove(fs::path_file(fit_dirs), "^fit_date="))
  last_fit <- max(fit_dates)

  latest_match > last_fit
}

#' Are there any matches scheduled in the next `days` days?
#'
#' Reads `data/facts/schedules/` for the (sport, country, sex) partition.
#' Returns `FALSE` when the schedule directory is missing or empty.
#'
#' @param static Per-league static slice with `$sport` and `$country`.
#' @param sex `"male"` or `"female"`.
#' @param root Filesystem root.
#' @param days Horizon in days (default 14).
#' @return Logical scalar.
#' @export
has_upcoming_games <- function(static, sex,
                               root = here::here("data"),
                               days = 14L) {
  sched_root <- fs::path(root, "facts", "schedules")
  if (!fs::dir_exists(sched_root)) {
    return(FALSE)
  }

  schedules <- read_table(
    "schedules",
    root = root,
    filter = list(sport = static$sport, country = static$country, sex = sex)
  )
  if (nrow(schedules) == 0L) {
    return(FALSE)
  }

  today <- Sys.Date()
  any(
    schedules$match_date >= today &
      schedules$match_date <= today + as.integer(days)
  )
}

#' @include ingest.R
NULL

# Fetch-attempt bookkeeping for the federation ingest (spec section 7).
#
# This replaces the config/active_competitions.json gate on ingest_one_league().
# That gate deadlocked: it was derived solely from data/facts/schedules rows,
# which only ingest itself can write, so a league whose fixtures had all been
# played could never write the rows that would mark it active again.
#
# The gate's stated justification is "saves a chromote launch", which is real
# (R/ingest-hsi-handball.R calls rvest::read_html_live()). That cost is
# recoverable from FETCH state -- when did we last try -- instead of FETCHED
# data. Fetch state has no closed loop, so a dormant league resumes on the
# next poll with no human action.

#' Path to the ingest attempt log.
#' @keywords internal
#' @noRd
ingest_log_path <- function(root = here::here("data")) {
  file.path(root, "health", "ingest_log.json")
}

#' Read the ingest attempt log.
#'
#' @return Named list keyed `"<league_key>/<sex>"`. An empty named list when
#'   the file is absent or unreadable -- callers must then attempt the fetch
#'   (fail open), never skip it.
#' @export
read_ingest_log <- function(root = here::here("data")) {
  path <- ingest_log_path(root)
  if (!file.exists(path)) {
    return(stats::setNames(list(), character()))
  }
  out <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(out)) {
    return(stats::setNames(list(), character()))
  }
  out
}

#' Record one ingest attempt.
#'
#' @param key League key, e.g. `"handball_iceland"`.
#' @param sex `"male"` / `"female"`.
#' @param n_rows Rows the fetch returned. Zero is meaningful; an ERRORED
#'   fetch must not be recorded at all, so a broken scraper never accrues a
#'   zero streak and never earns a backoff.
#' @return The record, invisibly.
#' @export
record_ingest_attempt <- function(key, sex, n_rows, now = Sys.time(),
                                  root = here::here("data")) {
  stopifnot(length(key) == 1L, length(sex) == 1L)
  n_rows <- as.integer(n_rows)
  log <- read_ingest_log(root)
  id <- paste0(key, "/", sex)
  prev <- log[[id]]

  stamp <- format(as.POSIXct(now, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  prev_streak <- if (is.null(prev$zero_streak)) 0L else as.integer(prev$zero_streak)

  record <- list(
    last_attempt_at = stamp,
    last_rows = n_rows,
    zero_streak = if (n_rows > 0L) 0L else prev_streak + 1L,
    last_nonzero_at = if (n_rows > 0L) stamp else prev$last_nonzero_at
  )

  log[[id]] <- record
  dir.create(dirname(ingest_log_path(root)), recursive = TRUE,
             showWarnings = FALSE)
  writeLines(
    jsonlite::toJSON(log, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    ingest_log_path(root)
  )
  invisible(record)
}

#' Should this cell's fetch be skipped as dormant?
#'
#' Pure. Skips only when the last attempt was recent AND the last several
#' attempts all returned nothing. Both conditions are required: recency alone
#' would throttle a healthy league, and a zero streak alone would strand a
#' league that has been dormant for months at exactly the moment its season
#' restarts.
#'
#' Fails OPEN (returns FALSE, meaning attempt) on anything it cannot read.
#' @keywords internal
#' @noRd
.ingest_backoff <- function(entry, now, min_interval_hours = 24,
                            zero_streak_threshold = 3L) {
  if (!is.list(entry) || length(entry) == 0L) {
    return(FALSE)
  }
  streak <- entry$zero_streak
  if (is.null(streak) || is.na(suppressWarnings(as.integer(streak)))) {
    return(FALSE)
  }
  if (as.integer(streak) < zero_streak_threshold) {
    return(FALSE)
  }
  last <- suppressWarnings(as.POSIXct(
    entry$last_attempt_at, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"
  ))
  if (length(last) != 1L || is.na(last)) {
    return(FALSE)
  }
  hours <- as.numeric(difftime(
    as.POSIXct(now, tz = "UTC"), last, units = "hours"
  ))
  isTRUE(hours < min_interval_hours)
}

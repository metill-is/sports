#' Decide whether a (league, sex) pair needs refitting.
#'
#' Returns `TRUE` when there is at least one completed match with
#' `match_date` strictly later than the most recent `fit_date` partition
#' under EITHER `data/beliefs/archive/` OR `data/beliefs/extracts/`
#' (whichever is fresher). The two-source check matters because
#' `fit_league()` skips the legacy `beliefs_archive` write for football
#' iceland (Phase 3b, 2026-05-04) — extracts/ is the canonical per-fit
#' accretive store for that league. Returns `TRUE` when no fit exists
#' yet OR when `beliefs/latest/` is missing/empty for the cell (so a
#' wiped canonical partition forces a refit even if archive still
#' carries yesterday's fit — defensive complement to the atomic
#' stage-then-rename in `write_table()`, audit 2026-05-15 §H + I2).
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

  # If beliefs/latest/ is missing or empty for this cell, force a refit
  # regardless of what archive says. This catches the failure mode where a
  # past write got wiped (manual rm, disk full, pre-atomic-fix Arrow crash)
  # but archive still has a fit_date partition — without this guard
  # decide_league() would silently emit empty recommendations indefinitely.
  latest_dir <- fs::path(
    root, "beliefs", "latest",
    paste0("sport=", static$sport),
    paste0("country=", static$country),
    paste0("sex=", sex)
  )
  if (!fs::dir_exists(latest_dir) ||
    length(fs::dir_ls(latest_dir, glob = "*.parquet")) == 0L) {
    return(TRUE)
  }

  # Collect fit_date partitions from both legacy archive/ and the newer
  # extracts/ tree. Either may be empty (archive/ is empty for football
  # iceland post-Phase-3b; extracts/ is empty for any league × sex that
  # hasn't been fit since its extraction layer shipped). The freshest
  # partition across both stores wins.
  collect_fit_dates <- function(subdir) {
    cell_dir <- fs::path(
      root, "beliefs", subdir,
      paste0("sport=", static$sport),
      paste0("country=", static$country),
      paste0("sex=", sex)
    )
    if (!fs::dir_exists(cell_dir)) {
      return(as.Date(character()))
    }
    fit_dirs <- fs::dir_ls(cell_dir, type = "directory")
    if (length(fit_dirs) == 0L) {
      return(as.Date(character()))
    }
    as.Date(stringr::str_remove(fs::path_file(fit_dirs), "^fit_date="))
  }

  fit_dates <- c(collect_fit_dates("archive"), collect_fit_dates("extracts"))
  if (length(fit_dates) == 0L) {
    return(TRUE)
  }
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

#' @include storage.R
NULL

#' Generate `config/active_competitions.json` from fixture data.
#'
#' Reads `data/facts/schedules/` (Parquet, partitioned by sport/country/sex/
#' season). A league is "active" if at least one fixture falls in
#' `[today, today + lookahead_days]`. Cold-start safe: missing schedule data
#' marks every league active to avoid silently skipping all scrapes.
#'
#' @param leagues Output of `load_leagues()` (or filtered subset).
#' @param lookahead_days Window size in days.
#' @param root Storage root.
#' @param out_path Path to write JSON.
#' @return invisible(out_path).
#' @export
generate_active_competitions <- function(leagues, lookahead_days = 7L,
                                         root = here::here("data"),
                                         out_path = here::here(
                                           "config", "active_competitions.json"
                                         )) {
  stopifnot(is.list(leagues), length(leagues) > 0L)

  schedule_root <- file.path(root, "facts", "schedules")
  # Helper: cold-start fall-back. Used both when the schedules dir is missing
  # AND when the Parquet read errors. Silent-disable-all-scrapes on read error
  # would be the opposite of the cold-start fail-safe philosophy.
  default_all_active <- function(reason) {
    cli::cli_alert_warning("{reason} - defaulting all leagues to active")
    out <- as.list(rep(TRUE, length(leagues)))
    names(out) <- names(leagues)
    out
  }

  degraded <- FALSE
  if (!dir.exists(schedule_root)) {
    active <- default_all_active(sprintf("No schedules at %s", schedule_root))
    degraded <- TRUE
  } else {
    today <- Sys.Date()
    horizon <- today + as.integer(lookahead_days)
    schedules <- tryCatch(
      read_table("schedules", root = root),
      error = function(e) NULL
    )
    if (is.null(schedules)) {
      active <- default_all_active("Failed to read data/facts/schedules/")
      degraded <- TRUE
    } else {
      active <- vapply(leagues, function(l) {
        hits <- schedules$sport == l$sport &
          schedules$country == l$country &
          schedules$match_date >= today &
          schedules$match_date <= horizon
        isTRUE(any(hits))
      }, logical(1))
      active <- as.list(active)
    }
  }

  payload <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    lookahead_days = as.integer(lookahead_days),
    degraded = degraded,
    active = active
  )
  # Auto-create the parent dir so callers don't have to. jsonlite::write_json
  # otherwise fails with "cannot open the connection".
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(payload, out_path, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success(
    "Wrote {out_path} ({sum(unlist(active))}/{length(active)} active)"
  )
  invisible(out_path)
}

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
  if (!dir.exists(schedule_root)) {
    cli::cli_alert_warning(
      "No schedules at {schedule_root} - defaulting all leagues to active"
    )
    active <- as.list(rep(TRUE, length(leagues)))
    names(active) <- names(leagues)
  } else {
    today <- Sys.Date()
    horizon <- today + as.integer(lookahead_days)
    schedules <- tryCatch(
      read_table("schedules", root = root),
      error = function(e) {
        cli::cli_alert_warning("Read schedules failed: {conditionMessage(e)}")
        tibble::tibble(
          sport = character(0), country = character(0),
          match_date = as.Date(character(0))
        )
      }
    )
    active <- vapply(leagues, function(l) {
      hits <- schedules$sport == l$sport &
        schedules$country == l$country &
        schedules$match_date >= today &
        schedules$match_date <= horizon
      isTRUE(any(hits))
    }, logical(1))
    active <- as.list(active)
  }

  payload <- list(
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    lookahead_days = as.integer(lookahead_days),
    active = active
  )
  jsonlite::write_json(payload, out_path, pretty = TRUE, auto_unbox = TRUE)
  cli::cli_alert_success(
    "Wrote {out_path} ({sum(unlist(active))}/{length(active)} active)"
  )
  invisible(out_path)
}

.ingest_registry <- new.env(parent = emptyenv())

#' Register an ingest source module.
#' @param name Source name referenced in leagues.yml's data_source field.
#' @param module A list with `fetch_results` and `fetch_schedule` functions.
#' @keywords internal
#' @noRd
register_ingest_source <- function(name, module) {
  stopifnot(
    is.list(module),
    is.function(module$fetch_results),
    is.function(module$fetch_schedule)
  )
  assign(name, module, envir = .ingest_registry)
  invisible(NULL)
}

#' Remove a registered ingest source (used by tests).
#' @keywords internal
#' @noRd
unregister_ingest_source <- function(name) {
  if (exists(name, envir = .ingest_registry, inherits = FALSE)) {
    rm(list = name, envir = .ingest_registry)
  }
  invisible(NULL)
}

#' Look up a source module by name; errors if not registered.
#' @keywords internal
#' @noRd
get_ingest_source <- function(name) {
  if (!exists(name, envir = .ingest_registry, inherits = FALSE)) {
    stop("Ingest source not registered: ", name, call. = FALSE)
  }
  get(name, envir = .ingest_registry)
}

#' Ingest results + schedules for one (league, sex) pair.
#'
#' Reads `league$data_source$results` and `league$data_source$schedule` to pick
#' the right source module, calls `fetch_results` / `fetch_schedule`, and writes
#' to `data/facts/\{results,schedules\}/` via `write_table()`.
#'
#' @param league A single entry from `load_leagues()`.
#' @param sex "male" or "female".
#' @param root Data root. Defaults to `here::here("data")`.
#' @param seasons Optional integer vector to pass through to `fetch_results`.
#' @return invisible(NULL)
#' @export
ingest_league <- function(league, sex,
                          root = here::here("data"),
                          seasons = NULL) {
  results_mod <- get_ingest_source(league$data_source$results)
  schedule_mod <- get_ingest_source(league$data_source$schedule)

  results <- results_mod$fetch_results(league, sex, seasons = seasons)
  schedule <- schedule_mod$fetch_schedule(league, sex)

  if (nrow(results) > 0) write_table(results, "results", root = root)
  if (nrow(schedule) > 0) write_table(schedule, "schedules", root = root)

  invisible(NULL)
}

# Idempotent: tar_source() walks R/ alphabetically, so ingest-*.R sources
# *before* ingest.R (- sorts before .). _targets.R works around that with an
# explicit source(ingest.R) first, but tar_source() then re-sources ingest.R
# at the end -- which would clobber a freshly-populated registry. Guard the
# init so the second pass is truly harmless.
if (!exists(".ingest_registry", inherits = FALSE)) {
  .ingest_registry <- new.env(parent = emptyenv())
}

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
#' @return Integer count of rows written (results + schedules), invisibly.
#' @export
ingest_league <- function(league, sex,
                          root = here::here("data"),
                          seasons = NULL) {
  results_mod <- get_ingest_source(league$data_source$results)
  schedule_mod <- get_ingest_source(league$data_source$schedule)

  results <- results_mod$fetch_results(league, sex, seasons = seasons)
  schedule <- schedule_mod$fetch_schedule(league, sex)

  # Use upsert semantics so re-ingests that return a strict subset of an
  # earlier fetch (e.g. HSI retry dropping G66 history) do not clobber the
  # larger partition on disk. See R/storage.R::upsert_table().
  if (nrow(results) > 0) upsert_table(results, "results", root = root)
  if (nrow(schedule) > 0) upsert_table(schedule, "schedules", root = root)

  invisible(as.integer(nrow(results) + nrow(schedule)))
}

#' Schedule-active gate used by all DAG wrappers.
#'
#' Reads `config/active_competitions.json` and returns TRUE if the league has
#' near-term fixtures (or if the file omits the league entirely — fail-safe to
#' active). Used by `ingest_one_*` and the Task 4-5 fit/decide/publish wrappers
#' so the gate is applied consistently.
#'
#' @keywords internal
#' @noRd
.is_league_active <- function(active_path, key) {
  active <- jsonlite::fromJSON(active_path)
  !isFALSE(active$active[[key]])
}

#' Run federation ingest for a single league across all configured sexes.
#'
#' Wrapper used by `_targets.R`'s per-league `ingest_<key>` targets. Reads
#' the active_competitions JSON to short-circuit when there are no near-term
#' fixtures (saves a chromote launch).
#'
#' @param leagues Output of `load_leagues()` (full config; the wrapper picks
#'   `leagues[[key]]` itself).
#' @param key League key (e.g. `"football_iceland"`).
#' @param active_path Path to `config/active_competitions.json`.
#' @return Integer count of rows fetched (results + schedule combined),
#'   summed across the league's sexes. Not equal to rows newly written —
#'   `upsert_table()` deduplicates on disk. Use only as a "did anything
#'   happen" indicator.
#' @export
ingest_one_league <- function(leagues, key, active_path) {
  if (!.is_league_active(active_path, key)) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  league <- leagues[[key]]
  total <- 0L
  for (sex in league$sexes) {
    total <- total + ingest_league(league, sex, seasons = NULL)
  }
  total
}

#' Run Lengjan odds ingest for a single league (DAG wrapper).
#'
#' @inheritParams ingest_one_league
#' @return Number of odds rows written (integer).
#' @export
ingest_one_lengjan <- function(leagues, key, active_path) {
  if (!.is_league_active(active_path, key)) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  as.integer(ingest_lengjan_odds(leagues[key]))
}

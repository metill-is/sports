# Idempotent: source order between this file and ingest-*.R isn't guaranteed
# (the per-source register_ingest_source() calls run at package load time);
# guard the init so a second pass is harmless.
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

  # kickoff_time is captured in the shared KSÍ parser and rides along on
  # results, but the results schema has no such column -- drop it so results
  # stays clean for every source (a no-op when the source never set it). On
  # the schedule side, non-KSÍ sources (KKÍ / HSÍ) post no time, so backfill
  # NA to satisfy the schedules schema's now-required kickoff_time column.
  results$kickoff_time <- NULL
  if (nrow(schedule) > 0 && !("kickoff_time" %in% names(schedule))) {
    schedule$kickoff_time <- NA_character_
  }

  # Populate the league round (matchweek) by dense-ranking match_date within
  # each (sport, country, sex, season, division) cell -- the scrapers cannot
  # supply it (KSÍ exposes no round). Applied to results only: results carry the
  # full played-season history so the rank is correct, whereas the schedule holds
  # only future fixtures and would rank from 1 instead of continuing the season
  # (cup rows stay NA either way). See R/derive-round.R.
  results <- derive_league_round(results)

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
#' Called by `scripts/01_ingest_results.R` for each active league. Reads
#' the active_competitions JSON to short-circuit when there are no near-term
#' fixtures (saves a chromote launch).
#'
#' Takes the per-league "static" slice (sport, country, sexes, active,
#' stan_model, data_source) rather than the full leagues config.
#'
#' @param static Per-league static slice (sport, country, sexes, active,
#'   stan_model, data_source).
#' @param key League key (e.g. `"football_iceland"`).
#' @param active_path Path to `config/active_competitions.json`.
#' @return Integer count of rows fetched (results + schedule combined),
#'   summed across the league's sexes. Not equal to rows newly written —
#'   `upsert_table()` deduplicates on disk. Use only as a "did anything
#'   happen" indicator.
#' @export
ingest_one_league <- function(static, key, active_path) {
  if (!.is_league_active(active_path, key)) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  total <- 0L
  for (sex in static$sexes) {
    total <- total + ingest_league(static, sex, seasons = NULL)
  }
  total
}

#' Run Lengjan odds ingest for a single league (DAG wrapper).
#'
#' Takes the static + lengjan slices separately so that betting-only changes
#' don't bust this target's cache.
#'
#' @param static Per-league static slice.
#' @param lengjan Per-league `lengjan` slice (competitions + team_names).
#' @param key League key.
#' @param active_path Path to `config/active_competitions.json`.
#' @return Number of odds rows written (integer).
#' @export
ingest_one_lengjan <- function(static, lengjan, key, active_path) {
  if (!.is_league_active(active_path, key)) {
    cli::cli_alert_info("{key}: skipped (no active fixtures)")
    return(0L)
  }
  league <- static
  league$lengjan <- lengjan
  tryCatch(
    as.integer(ingest_lengjan_odds(stats::setNames(list(league), key))),
    lengjan_fetch_error = function(e) {
      # A navigate/fetch timeout that survived every retry is transient and
      # external (Lengjan-side latency or runner-network), not a scraper bug.
      # Treat this league as 0 rows so the run exits clean instead of red-Xing
      # the workflow on a blip; real staleness still escalates via the
      # healthcheck's match-proximity odds_freshness check. Parse failures raise
      # plain errors (no lengjan_fetch_error class) and so still abort the run.
      cli::cli_alert_warning(
        "{key}: Lengjan fetch timed out after retries ({conditionMessage(e)}); skipping this run. odds_freshness escalates if a fixture is imminent."
      )
      0L
    }
  )
}

#' Should an odds scrape fail loudly for returning nothing in-season?
#'
#' TRUE only when at least one in-season league was scraped yet the whole run
#' wrote zero odds rows -- a systemic scraper failure (e.g. every Lengjan
#' match-detail fetch timing out, the 2026-05-29 outage). A single league's
#' emptiness is tolerated (odds may simply not be posted yet), so this keys on
#' the run-wide total, not per league. A manual `force` run suppresses it.
#' @noRd
odds_scrape_empty_failure <- function(n_inseason, total_rows, force = FALSE) {
  n_inseason > 0L && total_rows == 0L && !isTRUE(force)
}

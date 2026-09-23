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
#' to `data/facts/\{results,schedules\}/` via `write_table()`. Team names listed
#' in `league$data_source$team_aliases[[sex]]` are rewritten to their stored
#' spelling first.
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

  # A federation can rename a club between seasons (KKÍ: "Þór Akureyri" in
  # 2021, "Þór Ak." since). Normalise before the upsert, whose natural key
  # includes the team names -- a renamed row written unaliased is a second
  # team to prepare_data(), not a correction of the first. The map is per sex:
  # the same spelling can name a different side in the other sex's league.
  aliases <- league$data_source$team_aliases[[sex]]
  results <- .apply_team_aliases(results, aliases)
  schedule <- .apply_team_aliases(schedule, aliases)

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
  #
  # Schedules additionally use per-division snapshot semantics: a scrape is
  # the authoritative statement of each covered division's future fixtures,
  # so a rescheduled match retracts its old-dated ghost row (the natural key
  # includes match_date, so plain upsert would keep both dates forever —
  # the duplicated-fixture bug of 2026-06). Scope is per-division because
  # the fetchers degrade to a partial frame when one competition request
  # fails: an absent division must mean "not scraped", never "no fixtures".
  if (nrow(results) > 0) upsert_table(results, "results", root = root)
  if (nrow(schedule) > 0) {
    upsert_table(
      schedule, "schedules",
      root = root, snapshot_future_by = "division"
    )
  }

  invisible(as.integer(nrow(results) + nrow(schedule)))
}

#' Rewrite source team names to their stored spelling.
#'
#' @param df A results or schedules frame (may be empty).
#' @param aliases One sex's `data_source$team_aliases` map: a named list or
#'   vector, `{source_name: stored_name}`. `NULL` or empty is a no-op.
#' @return `df` with `home_team` / `away_team` rewritten as character.
#' @keywords internal
#' @noRd
.apply_team_aliases <- function(df, aliases) {
  if (length(aliases) == 0L || nrow(df) == 0L) {
    return(df)
  }
  map <- unlist(aliases)
  for (col in c("home_team", "away_team")) {
    # as.character(): indexing `map` by a factor would use its integer codes.
    x <- as.character(df[[col]])
    hit <- x %in% names(map)
    x[hit] <- unname(map[x[hit]])
    df[[col]] <- x
  }
  df
}

#' Bring stored results + schedules into line with `team_aliases`.
#'
#' [ingest_league()] aliases newly fetched rows only, and [upsert_table()] keys
#' on team names, so rows already on disk keep the source spelling until this
#' runs. Run it (`scripts/renormalise_team_aliases.R --apply`) in the same
#' change that adds an alias; `tests/testthat/test-ingest-integration.R` stays
#' red until you do.
#'
#' Two passes. The first plans every (league, table, sex, season) partition
#' that holds an alias key and aborts if aliasing any of them would collapse
#' two rows onto one natural key; only then does the second write, one
#' partition at a time through [write_table()]'s staged replace. An abort
#' therefore leaves the store untouched.
#'
#' @param leagues Named list from [load_leagues()].
#' @param root Data root.
#' @param apply Write the rewritten partitions. `FALSE` (the default) only
#'   reports.
#' @return Invisibly, one row per moved row: `league`, `table`, `sex`,
#'   `season`, `match_date`, `division` and the `home_` / `away_` names
#'   `before` and `after`. Zero rows when the store is already normalised.
#' @keywords internal
#' @noRd
renormalise_team_aliases <- function(leagues, root = here::here("data"),
                                     apply = FALSE) {
  planned <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    by_sex <- lg$data_source$team_aliases
    for (table in c("results", "schedules")) {
      for (sx in names(by_sex)) {
        aliases <- by_sex[[sx]]
        if (length(aliases) == 0L) next
        rows <- read_table(
          table,
          root = root,
          filter = list(sport = lg$sport, country = lg$country, sex = sx)
        )
        if (nrow(rows) == 0L) next
        keys <- names(unlist(aliases))
        stale <- rows$home_team %in% keys | rows$away_team %in% keys
        for (season in sort(unique(rows$season[stale]))) {
          before <- rows[rows$season == season, , drop = FALSE]
          after <- .apply_team_aliases(before, aliases)
          if (anyDuplicated(after[, natural_key_for(table)]) > 0L) {
            cli::cli_abort(
              "{key} {table} {sx}/{season}: aliasing creates duplicate natural keys; nothing was written.",
              call = NULL
            )
          }
          # `!=` is NA for a missing name; %in% TRUE counts those as unmoved.
          moved <- (before$home_team != after$home_team) %in% TRUE |
            (before$away_team != after$away_team) %in% TRUE
          planned[[length(planned) + 1L]] <- list(
            table = table,
            after = after,
            diff = tibble::tibble(
              league = key, table = table, sex = sx,
              season = as.integer(season),
              match_date = before$match_date[moved],
              division = before$division[moved],
              home_before = before$home_team[moved],
              home_after = after$home_team[moved],
              away_before = before$away_team[moved],
              away_after = after$away_team[moved]
            )
          )
        }
      }
    }
  }

  if (isTRUE(apply)) {
    for (p in planned) write_table(p$after, p$table, root = root)
  }
  diff <- dplyr::bind_rows(lapply(planned, `[[`, "diff"))
  if (nrow(diff) == 0L) {
    diff <- tibble::tibble(
      league = character(), table = character(), sex = character(),
      season = integer(), match_date = as.Date(character()),
      division = character(), home_before = character(),
      home_after = character(), away_before = character(),
      away_after = character()
    )
  }
  invisible(diff)
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

#' Abort when a fetched page's dates disagree with the season requested.
#'
#' A federation tournament id is a literal that is correct when written and
#' silently wrong later; nothing downstream notices, because a season-stamped
#' hive partition accepts any rows it is handed. This guard is what makes that
#' staleness loud. Icelandic winter seasons labelled `season` span calendar
#' years `season - 1` (autumn) and `season` (spring), so more than `tol` of the
#' parsed `match_date` years falling outside that pair means the page fetched is
#' not the season asked for -- a stale slug, a mis-mapped id, or a federation
#' URL-scheme change.
#'
#' Signals class `sports_season_stamp_error` so callers that otherwise degrade
#' fetch failures to warnings can re-raise it rather than swallow it.
#'
#' @param rows Tibble with a `match_date` Date column, or NULL. Zero rows and
#'   all-NA dates pass -- emptiness is a separate concern with its own checks.
#' @param season Integer season requested.
#' @param source Label naming the fetch, used in the abort message.
#' @param tol Maximum tolerated fraction of out-of-span calendar years.
#' @return `rows`, invisibly.
#' @keywords internal
#' @noRd
.assert_season_stamp <- function(rows, season, source = "unknown", tol = 0.05) {
  if (is.null(rows) || nrow(rows) == 0L) {
    return(invisible(rows))
  }
  years <- as.integer(format(rows$match_date, "%Y"))
  years <- years[!is.na(years)]
  if (length(years) == 0L) {
    return(invisible(rows))
  }

  season <- as.integer(season)
  allowed <- c(season - 1L, season)
  bad_frac <- mean(!(years %in% allowed))

  if (bad_frac > tol) {
    observed <- paste(sort(unique(years)), collapse = ", ")
    cli::cli_abort(
      c(
        "Season stamp mismatch for {source}: asked for season {season}.",
        "x" = paste0(
          "{round(100 * bad_frac, 1)}% of {length(years)} parsed dates fall ",
          "outside {allowed[1]}/{allowed[2]}."
        ),
        "i" = "Observed calendar years: {observed}.",
        "i" = "The tournament id registered for this (sex, division, season) is stale or wrong."
      ),
      class = "sports_season_stamp_error"
    )
  }

  invisible(rows)
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
ingest_one_league <- function(static, key, active_path,
                              root = here::here("data"),
                              force = FALSE,
                              offseason_min_interval_hours = 24,
                              now = Sys.time()) {
  # `active_path` is retained for call-site compatibility but deliberately no
  # longer consulted. It gated on config/active_competitions.json, which is
  # derived solely from data/facts/schedules rows -- rows only this function
  # can write. A league whose fixtures had all been played could therefore
  # never write the rows that would mark it active again (spec section 7).
  # `.is_league_active()` survives for ingest_one_lengjan(), where the gate is
  # correct: odds genuinely do not exist outside a fixture window, and that
  # loop is not closed.
  log <- read_ingest_log(root)
  total <- 0L

  for (sex in static$sexes) {
    id <- paste0(key, "/", sex)
    if (!isTRUE(force) && .ingest_backoff(
      log[[id]], now,
      min_interval_hours = offseason_min_interval_hours
    )) {
      cli::cli_alert_info(
        "{key}/{sex}: dormant, last tried
         {log[[id]]$last_attempt_at} -- treating as off-season."
      )
      next
    }

    # An ERRORED fetch propagates: a broken scraper must red-X CI, and must
    # never be recorded, or it would accrue a zero streak and earn itself a
    # backoff that hides it.
    n <- ingest_league(static, sex, root = root, seasons = NULL)
    n <- if (is.null(n) || is.na(n)) 0L else as.integer(n)

    prev_streak <- if (is.null(log[[id]]$zero_streak)) {
      0L
    } else {
      as.integer(log[[id]]$zero_streak)
    }
    if (n == 0L && prev_streak == 0L) {
      # The one genuinely ambiguous case -- season end, or a scraper that has
      # started returning nothing without erroring -- so it is made loud
      # rather than silently absorbed.
      cli::cli_warn(c(
        "{key}/{sex}: fetch returned 0 rows for the first time.",
        "i" = "Last non-empty fetch: {log[[id]]$last_nonzero_at %||% 'never'}.",
        "i" = "Season end, or a silently-empty scraper. Both look like this."
      ))
    } else if (n == 0L) {
      cli::cli_alert_info(
        "{key}/{sex}: 0 rows ({prev_streak + 1L} in a row) -- off-season."
      )
    }

    record_ingest_attempt(key, sex, n, now = now, root = root)
    log <- read_ingest_log(root)
    total <- total + n
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
#' @param betting Per-league `betting` slice, or `NULL`. Odds are scraped from
#'   `betting.mode` "scrape" up (spec 2026-09-23 WS1); at "off" the scrape is
#'   refused outright. Trailing and defaulted so existing four-arg calls keep
#'   working.
#' @return Number of odds rows written (integer).
#' @export
ingest_one_lengjan <- function(static, lengjan, key, active_path,
                               betting = NULL) {
  # Betting ladder (spec 2026-09-23 WS1): odds are scraped from mode "scrape"
  # up. Checked before the activation gate, so a league at "off" never touches
  # Lengjan even when it has fixtures today. An absent `betting` slice is
  # "auto" (football carries neither mode nor enabled).
  if (!betting_mode_at_least(list(betting = betting), "scrape")) {
    cli::cli_alert_info("{key}: skipped (betting.mode: off)")
    return(0L)
  }
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

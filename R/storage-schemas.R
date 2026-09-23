#' Arrow schemas for every storage table
#'
#' Referenced by every read/write primitive in R/storage.R. Column-naming
#' convention follows the spec section 3.3 (English underscore_case,
#' `home_team`/`away_team`, `match_date` not `date`, `p` for probabilities).
#'
#' @return Named list of arrow::Schema.
#' @export
schemas <- function() {
  ts <- arrow::timestamp(unit = "s", timezone = "UTC")

  list(
    results = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      season      = arrow::int32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      home_score  = arrow::int32(),
      away_score  = arrow::int32(),
      division    = arrow::string(),
      round       = arrow::int32()
    ),
    schedules = arrow::schema(
      sport        = arrow::string(),
      country      = arrow::string(),
      sex          = arrow::string(),
      season       = arrow::int32(),
      match_date   = arrow::date32(),
      home_team    = arrow::string(),
      away_team    = arrow::string(),
      division     = arrow::string(),
      round        = arrow::int32(),
      # Nullable "HH:MM" kick-off time captured from KSÍ (the federation source
      # that exposes it); NA for sources/rows without a posted time. Added
      # 2026-05-30 so placement can later be scheduled against actual kick-offs.
      kickoff_time = arrow::string()
    ),
    odds = arrow::schema(
      sport          = arrow::string(),
      country        = arrow::string(),
      scraped_at     = ts,
      match_date     = arrow::date32(),
      home_team      = arrow::string(),
      away_team      = arrow::string(),
      market         = arrow::string(),
      outcome        = arrow::string(),
      line           = arrow::float64(),
      odds           = arrow::float64(),
      # Nullable, added 2026-09-23 with the Lengjan JSON-API scraper (spec
      # 2026-09-23 WS2). Writers may omit them -- optional_columns() fills typed
      # NA -- so DOM-scraper rows and all earlier history carry NA.
      sex            = arrow::string(),
      event_id       = arrow::string(),
      competition_id = arrow::string(),
      kickoff_at     = ts
    ),
    beliefs_latest = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      fit_date    = arrow::date32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      draw_id     = arrow::int32(),
      home_goals  = arrow::float64(),
      away_goals  = arrow::float64()
    ),
    beliefs_archive = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      fit_date    = arrow::date32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      draw_id     = arrow::int32(),
      home_goals  = arrow::float64(),
      away_goals  = arrow::float64()
    ),
    beliefs_by_round = arrow::schema(
      sport        = arrow::string(),
      country      = arrow::string(),
      sex          = arrow::string(),
      season       = arrow::int32(),
      round_cutoff = arrow::int32(),
      fit_date     = arrow::date32(),
      match_date   = arrow::date32(),
      home_team    = arrow::string(),
      away_team    = arrow::string(),
      draw_id      = arrow::int32(),
      home_goals   = arrow::float64(),
      away_goals   = arrow::float64()
    ),
    candidates = arrow::schema(
      run_id      = ts,
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      p           = arrow::float64(),
      odds        = arrow::float64(),
      ev          = arrow::float64(),
      kelly_raw   = arrow::float64(),
      stage       = arrow::string()
    ),
    recommendations = arrow::schema(
      run_id      = ts,
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      p           = arrow::float64(),
      odds        = arrow::float64(),
      ev          = arrow::float64(),
      kelly       = arrow::float64(),
      bet_amount  = arrow::float64()
    ),
    ledger = arrow::schema(
      placed_at   = ts,
      match_date  = arrow::date32(),
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      odds_placed = arrow::float64(),
      p           = arrow::float64(),
      kelly       = arrow::float64(),
      bet_amount  = arrow::float64(),
      settled     = arrow::bool(),
      win         = arrow::bool(),
      pnl         = arrow::float64()
    ),
    fit_diagnostics = arrow::schema(
      sport           = arrow::string(),
      country         = arrow::string(),
      sex             = arrow::string(),
      fit_date        = arrow::date32(),
      n_obs           = arrow::int32(),
      n_divergent     = arrow::int32(),
      total_iter      = arrow::int32(),
      div_frac        = arrow::float64(),
      n_max_treedepth = arrow::int32(),
      treedepth_frac  = arrow::float64(),
      min_ebfmi       = arrow::float64(),
      max_rhat        = arrow::float64(),
      min_ess_bulk    = arrow::float64(),
      min_ess_tail    = arrow::float64(),
      adapt_delta     = arrow::float64(),
      iter_sampling   = arrow::int32(),
      chains          = arrow::int32(),
      passed          = arrow::bool()
    )
  )
}

#' Nullable columns a writer may omit, with the typed NA each is filled with.
#'
#' Schema evolution without touching every writer: `write_table()` and
#' `upsert_table()` add any of these a frame lacks before validating, so legacy
#' writers (the DOM scraper, test fixtures, the ETL) keep working and legacy
#' partitions merge cleanly with new rows.
#'
#' @param table Table name.
#' @return Named list: column name -> length-1 typed NA; `list()` when the
#'   table has no optional columns.
#' @noRd
optional_columns <- function(table) {
  switch(table,
    odds = list(
      sex = NA_character_,
      event_id = NA_character_,
      competition_id = NA_character_,
      kickoff_at = .POSIXct(NA_real_, tz = "UTC")
    ),
    list()
  )
}

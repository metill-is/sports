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
      sport       = arrow::string(),
      country     = arrow::string(),
      sex         = arrow::string(),
      season      = arrow::int32(),
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      division    = arrow::string(),
      round       = arrow::int32()
    ),
    odds = arrow::schema(
      sport       = arrow::string(),
      country     = arrow::string(),
      scraped_at  = ts,
      match_date  = arrow::date32(),
      home_team   = arrow::string(),
      away_team   = arrow::string(),
      market      = arrow::string(),
      outcome     = arrow::string(),
      line        = arrow::float64(),
      odds        = arrow::float64()
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
    )
  )
}

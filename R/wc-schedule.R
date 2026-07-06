# Vendored FIFA 2026 kickoff schedule -> ordering metadata for the forecast.
# The martj42 source the model uses carries only a date (no kickoff time, no
# match number), so within-day order is sourced here. See
# scripts/wc/fetch_schedule.R for provenance.

# fixturedownload team names -> martj42 names (the model's spelling).
# Non-ASCII keys built with intToUtf8() so the R source file stays pure ASCII,
# avoiding locale-dependent parse failures (devtools::load_all reads files in
# the native encoding, which is C/ASCII on macOS in some R configurations).
# intToUtf8() produces strings in the same encoding as read.csv(encoding="UTF-8"),
# so alias lookup succeeds regardless of the session locale.
.wc_schedule_aliases <- local({
  cote <- intToUtf8(c(67L, 244L, 116L, 101L, 32L, 100L, 39L, 73L, 118L, 111L, 105L, 114L, 101L))
  turkiye <- intToUtf8(c(84L, 252L, 114L, 107L, 105L, 121L, 101L))
  c(
    "Cabo Verde"     = "Cape Verde",
    "Congo DR"       = "DR Congo",
    "Czechia"        = "Czech Republic",
    "IR Iran"        = "Iran",
    "Korea Republic" = "South Korea",
    "USA"            = "United States"
  ) |>
    c(setNames(c("Ivory Coast", "Turkey"), c(cote, turkiye)))
})

.wc_alias <- function(x) {
  hit <- x %in% names(.wc_schedule_aliases)
  x[hit] <- unname(.wc_schedule_aliases[x[hit]])
  x
}

#' Unordered team-pair key (immune to home/away disagreement across sources).
#' @param a,b Character vectors of equal length.
#' @return Character vector of "<lo> | <hi>" keys.
#' @export
.wc_pair_key <- function(a, b) {
  # " | " separator: no country name contains it, so distinct pairs cannot
  # collide the way an empty separator would ("AB"+"C" vs "A"+"BC").
  paste(pmin(a, b), pmax(a, b), sep = " | ")
}

#' Load the vendored 2026 World Cup kickoff schedule (group stage).
#'
#' @param schedule_csv Path to the vendored fixturedownload UTC CSV.
#' @return Tibble: `match_no` (int), `kickoff` (POSIXct UTC), `home`, `away`
#'   (alias-applied martj42 names), `pair_key`. 72 group-stage rows.
#' @export
wc_schedule <- function(schedule_csv = here::here(
                          "data", "wc", "structure", "wc2026_schedule.csv"
                        )) {
  d <- utils::read.csv(
    schedule_csv,
    check.names = FALSE,
    colClasses = "character",
    encoding = "UTF-8"
  )
  d <- d[d[["Round Number"]] %in% c("1", "2", "3"), , drop = FALSE]
  home <- .wc_alias(trimws(d[["Home Team"]]))
  away <- .wc_alias(trimws(d[["Away Team"]]))
  kickoff <- as.POSIXct(d[["Date"]], format = "%d/%m/%Y %H:%M", tz = "UTC")
  out <- tibble::tibble(
    match_no = as.integer(d[["Match Number"]]),
    kickoff  = kickoff,
    home     = home,
    away     = away,
    pair_key = .wc_pair_key(home, away)
  )
  if (nrow(out) != 72L) {
    cli::cli_abort("wc_schedule: expected 72 group fixtures, got {nrow(out)}.")
  }
  if (anyNA(out$match_no) || anyNA(out$kickoff)) {
    cli::cli_abort("wc_schedule: NA in match_no or kickoff after parse.")
  }
  if (length(unique(out$pair_key)) != 72L) {
    cli::cli_abort("wc_schedule: pair_key not unique across 72 fixtures.")
  }
  out
}

# Venue timezones (IANA), keyed by canonical venue key. martj42 dates fixtures
# by the stadium-local calendar day, so the vendored UTC kickoffs must be
# rendered in the venue's timezone before any date comparison (Seattle's R16
# tie kicks off 07-07 00:00 UTC yet is a 6 July match).
.wc_venue_tz <- c(
  atlanta       = "America/New_York",
  boston        = "America/New_York",
  dallas        = "America/Chicago",
  guadalajara   = "America/Mexico_City",
  houston       = "America/Chicago",
  kansas_city   = "America/Chicago",
  los_angeles   = "America/Los_Angeles",
  mexico_city   = "America/Mexico_City",
  miami         = "America/New_York",
  monterrey     = "America/Monterrey",
  new_york      = "America/New_York",
  philadelphia  = "America/New_York",
  san_francisco = "America/Los_Angeles",
  seattle       = "America/Los_Angeles",
  toronto       = "America/Toronto",
  vancouver     = "America/Vancouver"
)

# fixturedownload `Location` -> venue key.
.wc_location_venue <- c(
  "Atlanta Stadium"                = "atlanta",
  "BC Place Vancouver"             = "vancouver",
  "Boston Stadium"                 = "boston",
  "Dallas Stadium"                 = "dallas",
  "Guadalajara Stadium"            = "guadalajara",
  "Houston Stadium"                = "houston",
  "Kansas City Stadium"            = "kansas_city",
  "Los Angeles Stadium"            = "los_angeles",
  "Mexico City Stadium"            = "mexico_city",
  "Miami Stadium"                  = "miami",
  "Monterrey Stadium"              = "monterrey",
  "New York/New Jersey Stadium"    = "new_york",
  "Philadelphia Stadium"           = "philadelphia",
  "San Francisco Bay Area Stadium" = "san_francisco",
  "Seattle Stadium"                = "seattle",
  "Toronto Stadium"                = "toronto"
)

# martj42 `city` -> venue key. martj42 is inconsistent within itself (the same
# stadium appears as both "Arlington" and "Dallas"), so municipality and metro
# spellings both map.
.wc_city_venue <- c(
  "Arlington"       = "dallas",
  "Atlanta"         = "atlanta",
  "Boston"          = "boston",
  "Dallas"          = "dallas",
  "East Rutherford" = "new_york",
  "Foxborough"      = "boston",
  "Guadalajara"     = "guadalajara",
  "Guadalupe"       = "monterrey",
  "Houston"         = "houston",
  "Inglewood"       = "los_angeles",
  "Kansas City"     = "kansas_city",
  "Los Angeles"     = "los_angeles",
  "Mexico City"     = "mexico_city",
  "Miami"           = "miami",
  "Miami Gardens"   = "miami",
  "Monterrey"       = "monterrey",
  "New Jersey"      = "new_york",
  "New York"        = "new_york",
  "Philadelphia"    = "philadelphia",
  "San Francisco"   = "san_francisco",
  "Santa Clara"     = "san_francisco",
  "Seattle"         = "seattle",
  "Toronto"         = "toronto",
  "Vancouver"       = "vancouver",
  "Zapopan"         = "guadalajara"
)

#' Load the vendored 2026 World Cup knockout slots (matches 73-104).
#'
#' The knockout complement of [wc_schedule()]: fixturedownload's rows for the
#' 32 bracket slots, which carry venue + UTC kickoff but no team names (the
#' vendored snapshot predates bracket resolution). `local_date` is the
#' stadium-local calendar date of the kickoff — the convention martj42 uses
#' for `date` — which is what [wc_correct_knockout_dates()] compares against.
#'
#' @param schedule_csv Path to the vendored fixturedownload UTC CSV.
#' @return Tibble: `match_no` (int), `round` (fixturedownload label), `venue`
#'   (canonical key), `kickoff` (POSIXct UTC), `local_date` (Date). 32 rows.
#' @export
wc_knockout_slots <- function(schedule_csv = here::here(
                                "data", "wc", "structure", "wc2026_schedule.csv"
                              )) {
  d <- utils::read.csv(
    schedule_csv,
    check.names = FALSE,
    colClasses = "character",
    encoding = "UTF-8"
  )
  d <- d[!(d[["Round Number"]] %in% c("1", "2", "3")), , drop = FALSE]
  loc <- trimws(d[["Location"]])
  unknown <- setdiff(unique(loc), names(.wc_location_venue))
  if (length(unknown) > 0L) {
    cli::cli_abort("wc_knockout_slots: unmapped Location value(s): {.val {unknown}}.")
  }
  venue <- unname(.wc_location_venue[loc])
  kickoff <- as.POSIXct(d[["Date"]], format = "%d/%m/%Y %H:%M", tz = "UTC")
  local_date <- as.Date(vapply(
    seq_along(kickoff),
    function(i) format(kickoff[[i]], "%Y-%m-%d", tz = .wc_venue_tz[[venue[[i]]]]),
    character(1)
  ))
  out <- tibble::tibble(
    match_no = as.integer(d[["Match Number"]]),
    round = d[["Round Number"]],
    venue = venue,
    kickoff = kickoff,
    local_date = local_date
  )
  if (nrow(out) != 32L || anyNA(out$match_no) || anyNA(out$kickoff)) {
    cli::cli_abort("wc_knockout_slots: expected 32 clean knockout slots, got {nrow(out)}.")
  }
  out
}

#' Correct unplayed knockout fixture dates against the official calendar.
#'
#' martj42 lists upcoming fixtures as `NA`-score rows whose `date` is
#' unreliable (2026-07-06 incident: all four remaining R16 ties carried the
#' round's first day, so the published forecast — and the platform matchday
#' reel, which selects by exact `match_date` — showed 4 matches on 6 July
#' instead of 2). Its `city` IS reliable, so venue + approximate date
#' identifies the official slot: each unplayed 2026 WC knockout row is re-dated
#' to the stadium-local date of the nearest official slot at its venue. Played
#' rows first consume their slots, so an off-by-a-couple-days date cannot
#' anchor to a slot the tournament has already used (Dallas hosts R32 and R16
#' ties only three days apart). Rows the mapping cannot place uniquely
#' (unmapped city, ambiguous tie, nothing within `max_shift_days`) keep their
#' martj42 date with a warning — degraded beats aborting the unattended
#' forecast.
#'
#' Runs BEFORE the manual-results overlay in [wc_ingest_internationals()], and
#' scripts/wc/list_missing.R applies it to its scaffold too, so overlay rows
#' key on corrected dates.
#'
#' @param raw martj42-schema data frame (`date`, `home_team`, `away_team`,
#'   `home_score`, `away_score`, `tournament`, `city`, ...).
#' @param slots Output of [wc_knockout_slots()].
#' @param schedule Output of [wc_schedule()] (the 72 group pairings, which are
#'   never touched).
#' @param max_shift_days Reject an official slot whose local date is further
#'   than this from martj42's date. Default 3.
#' @return `raw` with corrected `date` on unplayed 2026 WC knockout rows.
#' @export
wc_correct_knockout_dates <- function(raw, slots = wc_knockout_slots(),
                                      schedule = wc_schedule(),
                                      max_shift_days = 3L) {
  is_wc <- raw$tournament == "FIFA World Cup" & format(raw$date, "%Y") == "2026"
  is_wc[is.na(is_wc)] <- FALSE
  is_ko <- is_wc &
    !(.wc_pair_key(raw$home_team, raw$away_team) %in% schedule$pair_key)
  if (!any(is_ko)) {
    return(raw)
  }
  scored <- !is.na(raw$home_score) & !is.na(raw$away_score)
  venue <- unname(.wc_city_venue[trimws(as.character(raw$city))])

  nearest <- function(available, i) {
    cand <- which(available$venue == venue[[i]])
    if (length(cand) == 0L) {
      return(list(status = "none"))
    }
    dist <- abs(as.integer(available$local_date[cand] - raw$date[[i]]))
    if (min(dist) > max_shift_days) {
      return(list(status = "too_far"))
    }
    hit <- cand[dist == min(dist)]
    if (length(hit) > 1L) {
      return(list(status = "ambiguous"))
    }
    list(status = "ok", hit = hit)
  }

  available <- slots
  played_idx <- which(is_ko & scored & !is.na(venue))
  for (i in played_idx[order(raw$date[played_idx])]) {
    m <- nearest(available, i)
    if (identical(m$status, "ok")) {
      available <- available[-m$hit, , drop = FALSE]
    }
  }

  upcoming_idx <- which(is_ko & !scored)
  unknown <- unique(raw$city[upcoming_idx][is.na(venue[upcoming_idx])])
  unknown <- unknown[!is.na(unknown)]
  if (length(unknown) > 0L) {
    cli::cli_warn(
      "wc_correct_knockout_dates: unmapped martj42 city value(s) \\
      {.val {unknown}} -- keeping martj42 dates for fixtures there."
    )
  }
  for (i in upcoming_idx[order(raw$date[upcoming_idx])]) {
    if (is.na(venue[[i]])) next
    label <- sprintf(
      "%s vs %s (%s, %s)",
      raw$home_team[[i]], raw$away_team[[i]], raw$city[[i]], raw$date[[i]]
    )
    m <- nearest(available, i)
    if (identical(m$status, "none")) {
      cli::cli_warn(
        "wc_correct_knockout_dates: no unconsumed official slot at \\
        {.val {raw$city[[i]]}} -- keeping {label}."
      )
      next
    }
    if (identical(m$status, "too_far")) {
      cli::cli_warn(
        "wc_correct_knockout_dates: no official slot within \\
        {max_shift_days} days -- keeping {label}."
      )
      next
    }
    if (identical(m$status, "ambiguous")) {
      cli::cli_warn(
        "wc_correct_knockout_dates: ambiguous official slot for {label} \\
        -- keeping the martj42 date."
      )
      next
    }
    new_date <- available$local_date[[m$hit]]
    if (new_date != raw$date[[i]]) {
      cli::cli_alert_info(
        "Official-calendar date correction: {raw$home_team[[i]]} vs \\
        {raw$away_team[[i]]} {raw$date[[i]]} -> {new_date} \\
        (match {available$match_no[[m$hit]]}, {raw$city[[i]]})."
      )
      raw$date[[i]] <- new_date
    }
    available <- available[-m$hit, , drop = FALSE]
  }
  raw
}

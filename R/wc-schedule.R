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

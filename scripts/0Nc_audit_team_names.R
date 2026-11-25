#!/usr/bin/env Rscript
# scripts/0Nc_audit_team_names.R -- LOCAL ONLY, read-only.
#
# Audits Lengjan participant names against a league's lengjan.team_names
# before its odds move to the JSON API (spec 2026-09-23 WS3). Sources:
#   - API: participants of events listed in the league's configured
#     competitions, from the API test fixture, a live current-program
#     (--live: one request) and, when a local copy of archived Lengjan
#     payloads is configured (LENGJAN_ARCHIVE_DIR or --archive-dir), one
#     archived program per --archive-every-hours (default 6).
#   - DOM: every name the DOM scraper wrote to data/facts/odds for the league
#     (no sex: checked against both maps).
# It also lists every API participant name with stray whitespace, in any
# sport: the DOM's text extraction trims and collapses spaces, the API does
# not, so such a name keys differently on the two scrapers.
#
# Usage:
#   Rscript scripts/0Nc_audit_team_names.R                  # football_iceland
#   Rscript scripts/0Nc_audit_team_names.R --league football_iceland --live
#   Options: --archive-dir <dir>, --archive-every-hours 6, --out <dir> (CSVs; never under data/)

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
options(width = 150)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
out_dir <- odds_compare_out_dir(flag("out"))
key <- flag("league", "football_iceland")
league <- load_leagues()[[key]]
if (is.null(league)) stop("Unknown league: ", key)
comps <- league$lengjan$competitions %||% list()
comp_ids <- vapply(comps, function(cp) as.character(cp$id), character(1))
comp_sex <- stats::setNames(
  vapply(comps, function(cp) as.character(cp$sex), character(1)), comp_ids
)
sport_id <- .lengjan_sport_id(league$sport)

# --- API programs -------------------------------------------------------------
programs <- list(fixture = list(
  when = "2026-09-23 (test fixture)",
  events = parse_lengjan_program(jsonlite::fromJSON(
    here::here("tests", "testthat", "fixtures", "lengjan-api", "current-program.json"),
    simplifyVector = FALSE
  ))
))
if ("--live" %in% args) {
  programs$live <- list(
    when = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    events = parse_lengjan_program(lengjan_fetch_program())
  )
}
archive_dir <- tryCatch(.lengjan_archive_dir(flag("archive-dir")), error = function(e) "")
if (nzchar(archive_dir) && dir.exists(archive_dir)) {
  every_h <- as.numeric(flag("archive-every-hours", "6"))
  ix <- lengjan_archive_index(archive_dir)
  ix <- ix[ix$status %in% c("ok", "partial") & !is.na(ix$program_file), , drop = FALSE]
  slot <- floor(as.numeric(ix$ts) / (every_h * 3600))
  ix <- ix[!duplicated(slot), , drop = FALSE]
  for (i in seq_len(nrow(ix))) {
    body <- .lengjan_read_bodies(archive_dir, ix$program_file[[i]])
    if (is.character(body)) next
    programs[[paste0("archive_", i)]] <- list(
      when = format(ix$ts[[i]], "%Y-%m-%d %H:%M UTC", tz = "UTC"),
      events = parse_lengjan_program(body[[1]])
    )
  }
  cli::cli_alert_info("Read {nrow(ix)} archived program{?s}, one per {every_h} h.")
} else {
  cli::cli_alert_info("No local archive configured; API names come from the fixture{if ('--live' %in% args) ' and the live program' else ''}.")
}

all_events <- dplyr::bind_rows(lapply(names(programs), function(p) {
  ev <- programs[[p]]$events
  ev$source <- if (startsWith(p, "archive_")) "archive" else p
  ev
}))

cli::cli_h1("Team-name audit: {key}")

cli::cli_h2("Configured competitions seen on the API")
mine <- all_events[all_events$sport_id == sport_id &
  all_events$competition_id %in% comp_ids, , drop = FALSE]
seen_comps <- if (nrow(mine) == 0L) {
  tibble::tibble(competition_id = character())
} else {
  mine |>
    dplyr::group_by(.data$competition_id) |>
    dplyr::summarise(
      api_name = dplyr::first(.data$competition_name),
      sources = paste(sort(unique(.data$source)), collapse = ","),
      events = dplyr::n_distinct(.data$event_id),
      first_kickoff = min(.data$kickoff_at), last_kickoff = max(.data$kickoff_at),
      .groups = "drop"
    )
}
comp_table <- tibble::tibble(
  competition_id = comp_ids,
  config_name = vapply(comps, function(cp) as.character(cp$name), character(1)),
  sex = unname(comp_sex)
) |>
  dplyr::left_join(seen_comps, by = "competition_id")
print(as.data.frame(comp_table))

cli::cli_h2("API participant names in configured competitions")
api_names <- if (nrow(mine) == 0L) {
  tibble::tibble(team = character(), sex = character())
} else {
  tidyr::pivot_longer(
    mine[, c("event_id", "competition_id", "source", "kickoff_at", "home_team", "away_team")],
    c("home_team", "away_team"),
    values_to = "team"
  ) |>
    dplyr::mutate(sex = unname(comp_sex[.data$competition_id])) |>
    dplyr::group_by(.data$team, .data$sex) |>
    dplyr::summarise(
      competitions = paste(sort(unique(.data$competition_id)), collapse = ","),
      sources = paste(sort(unique(.data$source)), collapse = ","),
      events = dplyr::n_distinct(.data$event_id), .groups = "drop"
    )
}
api_audit <- audit_lengjan_team_names(api_names, league)
print(as.data.frame(api_audit))

cli::cli_h2("Names the DOM scraper wrote (data/facts/odds), checked against both maps")
dom <- tryCatch(
  read_table("odds", filter = list(sport = league$sport, country = league$country)),
  error = function(e) tibble::tibble()
)
dom_names <- if (nrow(dom) == 0L) {
  tibble::tibble(team = character(), sex = character())
} else {
  tidyr::pivot_longer(
    dom[, c("match_date", "home_team", "away_team")],
    c("home_team", "away_team"),
    values_to = "team"
  ) |>
    dplyr::group_by(.data$team) |>
    dplyr::summarise(
      rows = dplyr::n(), first_match = min(.data$match_date),
      last_match = max(.data$match_date), .groups = "drop"
    ) |>
    dplyr::mutate(sex = NA_character_)
}
dom_audit <- audit_lengjan_team_names(dom_names, league)
print(as.data.frame(dplyr::count(dom_audit, .data$status, .data$sex_hit)))
not_mapped <- dom_audit[dom_audit$status != "mapped", , drop = FALSE]
if (nrow(not_mapped) > 0L) print(as.data.frame(not_mapped))

cli::cli_h2("Configured renderings never seen")
coverage <- team_name_rendering_coverage(league, c(api_names$team, dom_names$team))
print(as.data.frame(dplyr::count(coverage, .data$sex, .data$seen)))
print(as.data.frame(coverage[!coverage$seen, , drop = FALSE]))

cli::cli_h2("API participant names with stray whitespace (any sport)")
raw <- unique(c(all_events$home_team, all_events$away_team))
raw <- raw[!is.na(raw)]
stray <- raw[raw != stringr::str_squish(raw)]
stray_tbl <- all_events[all_events$home_team %in% stray | all_events$away_team %in% stray, ,
  drop = FALSE
] |>
  dplyr::distinct(.data$event_id, .keep_all = TRUE) |>
  dplyr::select(
    "sport_id", "country_code", "competition_id", "competition_name",
    "home_team", "away_team"
  )
cli::cli_alert_info(
  "{length(stray)} of {length(raw)} distinct participant names carry stray whitespace."
)
print(as.data.frame(dplyr::mutate(
  stray_tbl,
  home_team = sprintf("[%s]", .data$home_team),
  away_team = sprintf("[%s]", .data$away_team)
)))

if (!is.null(out_dir)) {
  fs::dir_create(out_dir)
  readr::write_csv(comp_table, file.path(out_dir, "competitions.csv"))
  readr::write_csv(api_audit, file.path(out_dir, "api-names.csv"))
  readr::write_csv(dom_audit, file.path(out_dir, "dom-names.csv"))
  readr::write_csv(coverage, file.path(out_dir, "rendering-coverage.csv"))
  readr::write_csv(stray_tbl, file.path(out_dir, "stray-whitespace.csv"))
  cli::cli_alert_info("CSVs written to {out_dir}")
}

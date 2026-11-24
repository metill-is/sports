#!/usr/bin/env Rscript
# scripts/0Nc_compare_odds_sources.R -- LOCAL ONLY, never on CI.
#
# Shadow comparison of Lengjan's two odds scrapers (spec 2026-09-23 WS3): an
# API snapshot, the Chromote DOM scrape, then a second API snapshot, all in
# memory. Every DOM row is judged against the two snapshots
# (compare_odds_sources()); nothing is written to data/. Proposed judging rule
# and the cutover checklist: docs/runbooks/odds-source-cutover.md.
#
# Requests per run: one current-program, then markets for the listed events
# (20 per request) twice, plus one browser page per competition and one per
# detail page opened (--max-detail per competition).
#
# Usage:
#   Rscript scripts/0Nc_compare_odds_sources.R                    # football_iceland
#   Rscript scripts/0Nc_compare_odds_sources.R --league football_iceland
#   Rscript scripts/0Nc_compare_odds_sources.R --competition 296 --country england
#       # throwaway league: one Lengjan competition (off-season shadow runs)
#   Options: --sport football (with --competition), --max-detail 10,
#            --out <dir> (a report folder, never under data/)

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
options(width = 150)
if (nzchar(Sys.getenv("GITHUB_ACTIONS")) || nzchar(Sys.getenv("CI"))) {
  stop("0Nc_compare_odds_sources.R runs locally only, never on CI.")
}
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
max_detail <- as.numeric(flag("max-detail", "Inf"))
out_dir <- flag("out")
if (!is.null(out_dir)) {
  data_dir <- normalizePath(here::here("data"), mustWork = FALSE)
  out_abs <- normalizePath(out_dir, mustWork = FALSE)
  if (startsWith(paste0(out_abs, "/"), paste0(data_dir, "/"))) {
    stop("--out must not be under data/: this script writes nothing there.")
  }
}

if (!is.null(flag("competition"))) {
  comp <- flag("competition")
  leagues <- list(shadow = list(
    sport = flag("sport", "football"),
    country = flag("country", "england"),
    lengjan = list(source = "api", competitions = list(
      list(id = comp, name = paste("competition", comp), sex = "male")
    ))
  ))
} else {
  key <- flag("league", "football_iceland")
  all_leagues <- load_leagues()
  if (!key %in% names(all_leagues)) stop("Unknown league: ", key)
  leagues <- all_leagues[key]
  if (length(leagues[[1]]$lengjan$competitions %||% list()) == 0L) {
    stop(key, " has no lengjan.competitions to compare.")
  }
}

cli::cli_h1("Lengjan odds: DOM scraper vs JSON API (in memory)")
events <- parse_lengjan_program(lengjan_fetch_program())
before <- lengjan_api_snapshot(leagues, events = events)
cli::cli_alert_info(
  "API before: {nrow(before$odds)} rows, {nrow(before$events)} listed event{?s} at {format(before$taken_at, tz = 'UTC')} UTC"
)
if (nrow(before$events) == 0L) {
  cli::cli_alert_warning("Lengjan lists no event in these competitions; nothing to compare.")
  quit(save = "no", status = 0L)
}

dom_start <- Sys.time()
dom <- scrape_lengjan_dom(leagues, scraped_at = dom_start, max_detail = max_detail)
dom_end <- Sys.time()
cli::cli_alert_info(
  "DOM: {nrow(dom$rows)} rows from {sum(dom$visits$detail == 'ok')} detail page{?s}, {format(dom_start, tz = 'UTC')} to {format(dom_end, tz = 'UTC')} UTC"
)

after <- lengjan_api_snapshot(leagues, events = events)
cli::cli_alert_info(
  "API after: {nrow(after$odds)} rows at {format(after$taken_at, tz = 'UTC')} UTC"
)

cmp <- compare_odds_sources(
  dom$rows, before$odds, after$odds,
  visits = dom$visits, api_events = events,
  api_closed = dplyr::bind_rows(before$closed, after$closed)
)
timing <- tibble::tibble(
  api_before = before$taken_at, dom_start = dom_start, dom_end = dom_end,
  api_after = after$taken_at
)

cli::cli_h2("Summary")
print(as.data.frame(cmp$summary))
print(as.data.frame(timing))
cli::cli_h2("Competition pages")
print(as.data.frame(dom$pages))
cli::cli_h2("Rows by market")
by_market <- dplyr::full_join(
  dplyr::count(cmp$dom, .data$market, .data$verdict, name = "n") |>
    tidyr::pivot_wider(names_from = "verdict", values_from = "n", names_prefix = "dom_"),
  dplyr::count(cmp$api_only, .data$market, name = "api_only"),
  by = "market"
)
print(as.data.frame(by_market))
cli::cli_h2("Causes")
print(as.data.frame(odds_comparison_causes(cmp)))
if (nrow(cmp$mispaired) > 0L) {
  cli::cli_h2("Mispaired detail pages (link's event is another match)")
  print(as.data.frame(cmp$mispaired[, c(
    "competition_id", "position", "event_id", "home_team", "away_team",
    "api_home_team", "api_away_team"
  )]))
}
fails <- cmp$dom[cmp$dom$verdict == "fail", , drop = FALSE]
if (nrow(fails) > 0L) {
  cli::cli_h2("Failing DOM rows (first 40)")
  print(as.data.frame(utils::head(fails, 40L)))
}
if (nrow(cmp$duplicates) > 0L) {
  cli::cli_h2("Keys listed twice on one side")
  print(as.data.frame(cmp$duplicates))
}

if (cmp$summary$pass) {
  cli::cli_alert_success("PASS: all {cmp$summary$dom_rows} DOM rows have an API row at the same price in a bracketing snapshot; {cmp$summary$shared_detail_matches} shared match{?es} compared on detail rows.")
} else {
  cli::cli_alert_danger("FAIL ({cmp$summary$fail_reason}): {cmp$summary$dom_fail} of {cmp$summary$dom_rows} DOM row{?s} without a same-priced API row; {cmp$summary$shared_detail_matches} shared match{?es} with detail rows.")
}

if (!is.null(out_dir)) {
  fs::dir_create(out_dir)
  stamp <- format(dom_start, "%Y%m%dT%H%M%SZ", tz = "UTC")
  saveRDS(
    list(
      leagues = leagues, timing = timing, comparison = cmp, dom = dom,
      api_before = before, api_after = after
    ),
    file.path(out_dir, paste0("compare-", stamp, ".rds"))
  )
  readr::write_csv(
    dplyr::bind_cols(timing, cmp$summary),
    file.path(out_dir, paste0("compare-", stamp, "-summary.csv"))
  )
  readr::write_csv(
    odds_comparison_causes(cmp),
    file.path(out_dir, paste0("compare-", stamp, "-causes.csv"))
  )
  cli::cli_alert_info("Report written to {out_dir}")
}

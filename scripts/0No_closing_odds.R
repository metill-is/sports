# Closing-odds table from a local copy of the raw Lengjan archive. LOCAL ONLY:
# no workflow may run it (tests/testthat/test-lengjan-closing.R checks).
#
#   Rscript scripts/0No_closing_odds.R [--from 2026-11-16] [--to YYYY-MM-DD]
#       [--out DIR] [--root DIR] [--window-days 3] [--within 15]
#
# Reads the archive named by LENGJAN_ARCHIVE_DIR (docs/runbooks/odds-archive.md),
# takes each Icelandic event's last captured prices before kick-off for every
# market (lengjan_closing_odds()), joins them to the results under --root
# (default data/; lengjan_closing_results()) and measures, against every
# Icelandic event listed with markets, how many closed with a capture within
# --within minutes of kick-off (lengjan_closing_coverage()). Writes to --out,
# else SPORTS_CLOSING_ODDS_DIR, else the git-ignored
# research/results/closing-odds/; never under data/:
#   closing_odds.parquet  one row per event, market and selection, with result
#   coverage.csv          one row per listed event
#   coverage.md           the summary and every event that missed
options(width = 150, warn = 1)
invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressMessages(devtools::load_all(quiet = TRUE))

arg <- function(name, default = NULL) {
  a <- commandArgs(trailingOnly = TRUE)
  i <- match(paste0("--", name), a)
  if (is.na(i)) default else a[i + 1L]
}
from <- arg("from")
to <- arg("to")
out <- closing_odds_out_dir(arg("out"))
root <- arg("root", here::here("data"))
window_days <- as.integer(arg("window-days", "3"))
within <- as.numeric(arg("within", "15"))

ix <- lengjan_archive_index()
ix <- ix[.lengjan_in_window(ix$ts, from, to), , drop = FALSE]
if (nrow(ix) == 0L) stop("No archive capture in the window.", call. = FALSE)
as_of <- max(ix$ts)
cli::cli_alert_info("{nrow(ix)} captures, {format(min(ix$ts))} to {format(as_of)} UTC")

rows <- read_lengjan_archive(from = from, to = to, table = "odds_markets")
rows <- rows[rows$country_code %in% "IS", , drop = FALSE]
closing <- lengjan_closing_odds(rows, as_of = as_of)
joined <- lengjan_closing_results(
  closing, read_table("results", root = root),
  match_date_window_days = window_days
)
listings <- lengjan_archive_events(from = from, to = to, country_code = "IS")
cov <- lengjan_closing_coverage(listings, joined, as_of = as_of, within_minutes = within)

dir.create(out, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(joined, file.path(out, "closing_odds.parquet"))
utils::write.csv(cov, file.path(out, "coverage.csv"), row.names = FALSE, na = "")

count <- function(d) {
  tibble::tibble(
    events = nrow(d),
    within = sum(d$status == "within"),
    stale = sum(d$status == "stale"),
    no_detail = sum(d$status == "no_detail"),
    joined = sum(d$result_join %in% c("exact", "date_window"))
  )
}
by_comp <- dplyr::bind_rows(lapply(split(cov, paste(cov$sport, cov$competition_id)), function(d) {
  dplyr::bind_cols(
    tibble::tibble(sport = d$sport[[1]], competition = paste(d$competition_id[[1]], d$competition_name[[1]])),
    count(d)
  )
}))
total <- dplyr::bind_cols(tibble::tibble(sport = "all", competition = ""), count(cov))
md_table <- function(d) {
  d[] <- lapply(d, function(x) ifelse(is.na(x), "", as.character(x)))
  c(
    paste0("| ", paste(names(d), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )
}
missed <- cov[cov$status != "within" | !cov$result_join %in% c("exact", "date_window"), , drop = FALSE]
missed_tbl <- tibble::tibble(
  kickoff = format(missed$kickoff_at, "%Y-%m-%d %H:%M"),
  event = missed$event_id, sport = missed$sport,
  match = paste(missed$home_team, "-", missed$away_team),
  status = missed$status, minutes_before = round(missed$minutes_before, 1),
  live_flagged = missed$live_flagged, result_join = missed$result_join
)
writeLines(c(
  "# Closing-odds coverage",
  "",
  paste0(
    "Captures ", format(min(ix$ts), "%Y-%m-%d %H:%M"), " to ", format(as_of, "%Y-%m-%d %H:%M"),
    " UTC (", nrow(ix), "). Written ", format(Sys.time(), "%Y-%m-%d %H:%M %Z"), "."
  ),
  paste0(
    "An event counts when it was listed with markets and kicked off by the last capture. ",
    "`within`: its closing capture came at most ", within, " minutes before kick-off; ",
    "`stale`: earlier; `no_detail`: no market-detail capture at all. ",
    "`joined`: matched to a result (exact date or a unique result within ", window_days, " days)."
  ),
  "",
  md_table(dplyr::bind_rows(by_comp, total)),
  "",
  paste0("Closing rows: ", nrow(joined), " for ", length(unique(joined$event_id)), " events."),
  "",
  "## Events that missed (not within, or not joined)",
  "",
  if (nrow(missed_tbl) > 0L) md_table(missed_tbl) else "None."
), file.path(out, "coverage.md"))

print(dplyr::bind_rows(by_comp, total))
cli::cli_alert_success("Wrote closing_odds.parquet, coverage.csv and coverage.md to {out}")

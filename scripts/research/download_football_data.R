#!/usr/bin/env Rscript
# Download historical results and odds from football-data.co.uk for offline
# backtesting. Research use only: the files are never committed and nothing
# derived from them is published (see the README the script writes).
#
# Usage:
#   Rscript scripts/research/download_football_data.R --out DIR
#   Rscript scripts/research/download_football_data.R --out DIR --seasons 2526,2627 --divs E0,SC0
#   Rscript scripts/research/download_football_data.R --out DIR --no-extra --dry-run
#
#   --out DIR     Where the files go (or set FOOTBALL_DATA_DIR). Keep it outside the repo.
#   --seasons     Comma-separated season codes ("2526" = 2025/26). Default: 1993/94 to now.
#   --divs        Comma-separated division codes. Default: the 22 main divisions.
#   --no-extra    Skip the one-file-per-country "extra leagues" (Norway, Sweden, ...).
#   --refresh     Re-download past seasons that are already on disk.
#   --dry-run     List what would be fetched and stop.
#
# A past season is fetched once. The current season, the extra-league files and
# notes.txt are refreshed on every run. Requests are spaced one second apart.

options(width = 120)

base_url <- "https://football-data.co.uk"
user_agent <- "football-data-research-downloader (R httr2)"
pause_secs <- 1
# WHY: the site answers a division-season that does not exist with 404 or with 300 plus an HTML page.
missing_statuses <- c(300L, 404L)

main_divs <- c(
  "E0", "E1", "E2", "E3", "EC", "SC0", "SC1", "SC2", "SC3", "D1", "D2",
  "I1", "I2", "SP1", "SP2", "F1", "F2", "N1", "B1", "P1", "T1", "G1"
)
extra_leagues <- c(
  "ARG", "AUT", "BRA", "CHN", "DNK", "FIN", "IRL", "JPN",
  "MEX", "NOR", "POL", "ROU", "RUS", "SWE", "SUI", "USA"
)

season_code <- function(start_year) {
  sprintf("%02d%02d", start_year %% 100, (start_year + 1) %% 100)
}

current_start_year <- function(today = Sys.Date()) {
  year <- as.integer(format(today, "%Y"))
  if (as.integer(format(today, "%m")) >= 7L) year else year - 1L
}

parse_args <- function(args) {
  value_of <- function(flag) {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) NULL else args[[i + 1L]]
  }
  split_csv <- function(x) if (is.null(x)) NULL else strsplit(x, ",", fixed = TRUE)[[1]]
  out <- value_of("--out")
  if (is.null(out)) out <- Sys.getenv("FOOTBALL_DATA_DIR", unset = "")
  if (!nzchar(out)) {
    stop("Give the output directory with --out DIR or FOOTBALL_DATA_DIR.", call. = FALSE)
  }
  list(
    out = path.expand(out),
    seasons = split_csv(value_of("--seasons")) %||% season_code(1993:current_start_year()),
    divs = split_csv(value_of("--divs")) %||% main_divs,
    extra = !"--no-extra" %in% args,
    refresh = "--refresh" %in% args,
    dry_run = "--dry-run" %in% args
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

build_targets <- function(opts) {
  current <- season_code(current_start_year())
  seasons <- expand.grid(div = opts$divs, season = opts$seasons, stringsAsFactors = FALSE)
  targets <- data.frame(
    url = sprintf("%s/mmz4281/%s/%s.csv", base_url, seasons$season, seasons$div),
    path = file.path("main", seasons$season, paste0(seasons$div, ".csv")),
    always = seasons$season == current
  )
  if (opts$extra) {
    targets <- rbind(targets, data.frame(
      url = sprintf("%s/new/%s.csv", base_url, extra_leagues),
      path = file.path("extra", paste0(extra_leagues, ".csv")),
      always = TRUE
    ))
  }
  rbind(targets, data.frame(url = paste0(base_url, "/notes.txt"), path = "notes.txt", always = TRUE))
}

fetch_one <- function(url, dest) {
  req <- httr2::request(url) |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3, is_transient = \(r) httr2::resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L)) |>
    httr2::req_error(is_error = \(r) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(status = NA_integer_, bytes = 0L, note = conditionMessage(resp)))
  }
  status <- httr2::resp_status(resp)
  type <- httr2::resp_content_type(resp) %||% ""
  # WHY: a missing division-season answers 404 with an HTML page; never save HTML as a .csv.
  if (status != 200L || !type %in% c("text/csv", "text/plain", "application/csv")) {
    return(list(status = status, bytes = 0L, note = paste("not saved, content type", type)))
  }
  body <- httr2::resp_body_raw(resp)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest, ".part")
  writeBin(body, tmp)
  file.rename(tmp, dest)
  list(status = status, bytes = length(body), note = "saved")
}

write_readme <- function(out) {
  writeLines(c(
    "Source: https://football-data.co.uk (results and bookmaker odds; column key in notes.txt).",
    "Fetched by scripts/research/download_football_data.R in the sports repo.",
    "",
    "Use: offline backtesting and methods research only.",
    "Do not commit these files, redistribute them, or publish products derived from them.",
    "The site states its data is intended for private individuals, not commercial products.",
    "",
    "Layout: main/<season>/<division>.csv, one file per division-season (\"2526\" = 2025/26);",
    "extra/<COUNTRY>.csv, one file per country holding every season; manifest.csv, one row per URL."
  ), file.path(out, "README.txt"))
}

update_manifest <- function(out, log) {
  path <- file.path(out, "manifest.csv")
  if (file.exists(path)) {
    old <- utils::read.csv(path, stringsAsFactors = FALSE)
    log <- rbind(old[!old$url %in% log$url, names(log), drop = FALSE], log)
  }
  utils::write.csv(log[order(log$path), ], path, row.names = FALSE)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  targets <- build_targets(opts)
  dest <- file.path(opts$out, targets$path)
  on_disk <- file.exists(dest) & file.size(dest) > 0
  # WHY: many early division-seasons do not exist; do not ask for a known 404 on every run.
  known_missing <- rep(FALSE, nrow(targets))
  manifest_path <- file.path(opts$out, "manifest.csv")
  if (file.exists(manifest_path)) {
    manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
    known_missing <- targets$url %in% manifest$url[manifest$status %in% missing_statuses]
  }
  todo <- targets$always | opts$refresh | !(on_disk | known_missing)
  cat(sprintf(
    "%d files in scope, %d already on disk, %d to fetch (about %.0f min at one request per %g s).\n",
    nrow(targets), sum(on_disk & !todo), sum(todo), sum(todo) * pause_secs / 60, pause_secs
  ))
  if (opts$dry_run) {
    cat(utils::head(targets$url[todo], 10), sep = "\n")
    return(invisible())
  }
  dir.create(opts$out, recursive = TRUE, showWarnings = FALSE)
  write_readme(opts$out)
  idx <- which(todo)
  log <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    i <- idx[[k]]
    res <- fetch_one(targets$url[[i]], dest[[i]])
    log[[k]] <- data.frame(
      url = targets$url[[i]], path = targets$path[[i]], status = res$status, bytes = res$bytes,
      note = res$note, fetched_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    )
    if (k %% 50L == 0L || k == length(idx)) {
      cat(sprintf("  %d / %d fetched\n", k, length(idx)))
    }
    Sys.sleep(pause_secs)
  }
  log <- do.call(rbind, log)
  update_manifest(opts$out, log)
  saved <- log$note == "saved"
  cat(sprintf(
    "DONE: %d saved (%.1f MB), %d do not exist on the site, %d failed. Manifest: %s\n",
    sum(saved), sum(log$bytes) / 1e6, sum(!saved & log$status %in% missing_statuses),
    sum(!saved & !log$status %in% missing_statuses), file.path(opts$out, "manifest.csv")
  ))
  failed <- log[!saved & !log$status %in% missing_statuses, c("url", "status", "note")]
  if (nrow(failed) > 0L) print(failed, row.names = FALSE)
}

main()

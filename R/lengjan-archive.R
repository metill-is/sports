#' @include lengjan-api.R ingest-lengjan-odds.R config.R
NULL

# Reader for a local copy of the raw Lengjan odds archive, format version 1
# (docs/runbooks/odds-archive.md):
#   <dir>/manifest/YYYY-MM.jsonl                   one JSON object per capture
#   <dir>/raw/YYYY/MM/DD/<ts>-program.json.gz      a current-program body
#   <dir>/raw/YYYY/MM/DD/<ts>-markets-NNN.json.gz  a markets body
# Read-only: nothing here writes to the facts store or touches the network.
.LENGJAN_ARCHIVE_VERSION <- 1L

#' Resolve the archive directory: argument, option, then environment variable.
#' @noRd
.lengjan_archive_dir <- function(dir = NULL) {
  dir <- dir %||% getOption("sports.lengjan_archive_dir")
  if (is.null(dir) || !nzchar(dir)) {
    dir <- Sys.getenv("LENGJAN_ARCHIVE_DIR", unset = "")
  }
  if (!nzchar(dir)) {
    stop(
      "No Lengjan archive directory: pass `dir`, set option ",
      "`sports.lengjan_archive_dir` or environment variable ",
      "`LENGJAN_ARCHIVE_DIR` (see docs/runbooks/odds-archive.md).",
      call. = FALSE
    )
  }
  dir
}

#' Index the captures of a local Lengjan odds archive.
#'
#' Reads every `manifest/YYYY-MM.jsonl` file: one line per capture. A line
#' that cannot be read (a truncated last line, say) or that carries another
#' format version is skipped with a warning. An archive with no manifest yet
#' gives a zero-row index; a directory that does not exist is an error.
#'
#' @param dir Archive directory. Defaults to option
#'   `sports.lengjan_archive_dir`, then environment variable
#'   `LENGJAN_ARCHIVE_DIR`.
#' @return Tibble sorted by `ts`, one row per capture: `ts` (POSIXct, UTC),
#'   `reason`, `status` (`ok`, `partial`, `error` or `skipped`),
#'   `program_file` and `markets_files` (paths relative to `dir`; the latter a
#'   list column of character vectors), `n_events`, `n_detail_events`,
#'   `n_requests` (integers) and `error`.
#' @examples
#' \dontrun{
#' ix <- lengjan_archive_index()
#' table(ix$status)
#' }
#' @export
lengjan_archive_index <- function(dir = NULL) {
  dir <- .lengjan_archive_dir(dir)
  if (!dir.exists(dir)) {
    stop("Lengjan archive directory does not exist: ", dir, call. = FALSE)
  }
  files <- sort(list.files(
    file.path(dir, "manifest"),
    pattern = "^[0-9]{4}-[0-9]{2}[.]jsonl$", full.names = TRUE
  ))
  recs <- list()
  for (f in files) {
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (i in which(nzchar(trimws(lines)))) {
      rec <- .lengjan_manifest_record(lines[[i]])
      if (is.character(rec)) {
        cli::cli_warn(
          "Skipping line {i} of manifest {.file {basename(f)}}: {rec}."
        )
        next
      }
      recs[[length(recs) + 1L]] <- rec
    }
  }
  if (length(recs) == 0L) {
    return(empty_lengjan_archive_index())
  }

  chr <- function(get) {
    vapply(recs, function(r) .scalar_chr(get(r)), character(1))
  }
  int <- function(get) {
    vapply(recs, function(r) as.integer(.scalar_chr(get(r))), integer(1))
  }
  out <- tibble::tibble(
    ts = .POSIXct(vapply(recs, function(r) r$ts, numeric(1)), tz = "UTC"),
    reason = chr(function(r) r$reason),
    status = chr(function(r) r$status),
    program_file = chr(function(r) r$program$file),
    n_events = int(function(r) r$program$n_events),
    markets_files = lapply(recs, function(r) {
      vapply(r$markets %||% list(), function(m) {
        .scalar_chr(m$file)
      }, character(1))
    }),
    n_detail_events = int(function(r) r$n_detail_events),
    n_requests = int(function(r) r$n_requests),
    error = chr(function(r) r$error)
  )
  out[order(out$ts), , drop = FALSE]
}

#' Parse one manifest line: the capture record, or a string saying why the
#' line is not one.
#'
#' `parse_json()`, not `fromJSON()`: the latter would fetch a line that
#' happened to be a URL, and this reader makes no network request.
#' @noRd
.lengjan_manifest_record <- function(line) {
  rec <- tryCatch(
    jsonlite::parse_json(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(rec) || is.null(names(rec))) {
    return("not a JSON object")
  }
  v <- suppressWarnings(as.integer(.scalar_chr(rec$v)))
  if (!identical(v, .LENGJAN_ARCHIVE_VERSION)) {
    return(paste0(
      "format version ", .scalar_chr(rec$v), ", this reader handles version ",
      .LENGJAN_ARCHIVE_VERSION
    ))
  }
  ts <- lubridate::ymd_hms(.scalar_chr(rec$ts), tz = "UTC", quiet = TRUE)
  if (is.na(ts)) {
    return("no parseable `ts`")
  }
  rec$ts <- as.numeric(ts)
  rec
}

#' @noRd
.scalar_chr <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x)) {
    return(NA_character_)
  }
  as.character(x)
}

#' @noRd
empty_lengjan_archive_index <- function() {
  tibble::tibble(
    ts = as.POSIXct(character(), tz = "UTC"),
    reason = character(), status = character(),
    program_file = character(), n_events = integer(),
    markets_files = list(),
    n_detail_events = integer(), n_requests = integer(),
    error = character()
  )
}

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

#' Odds rows from the main market embedded in Lengjan's current program.
#'
#' Every event in the program carries its main market inline, so a program
#' body alone gives a full-time 1X2 price for each listed event without a
#' market-detail request. The embedded market goes through
#' [parse_lengjan_markets()] and [lengjan_markets_to_odds()], so the rule is
#' the live scrape's: a primary, open `3WAY` market maps to moneyline
#' home/draw/away for selections `1`/`X`/`2` priced above 1. Embedded `2WAY`
#' and `NWAY` markets are dropped. Events come from all four program arrays,
#' first copy of each id.
#'
#' @param prog Parsed JSON of a current-program body.
#' @return Tibble: event_id, market, outcome, line (always `NA`), odds.
#' @examples
#' \dontrun{
#' parse_lengjan_program_markets(lengjan_fetch_program())
#' }
#' @export
parse_lengjan_program_markets <- function(prog) {
  evs <- .lengjan_program_events(prog)
  evs <- evs[vapply(evs, function(e) is.list(e$market), logical(1))]
  groups <- lapply(evs, function(e) {
    market <- e$market
    market$eventId <- e$id
    list(name = "program", markets = list(market))
  })
  lengjan_markets_to_odds(parse_lengjan_markets(groups))
}

#' The program's events across its four arrays, first copy of each id.
#' @noRd
.lengjan_program_events <- function(prog) {
  if (!is.list(prog) || !all(c("events", "liveSoon") %in% names(prog))) {
    stop(
      "Unexpected current-program shape (no events/liveSoon arrays); ",
      "Lengjan's API may have changed.",
      call. = FALSE
    )
  }
  evs <- c(prog$events, prog$liveSoon, prog$popular, prog$liveNow)
  ids <- vapply(evs, function(e) as.character(e$id), character(1))
  evs[!duplicated(ids)]
}

#' Read a local Lengjan odds archive into odds-shaped rows.
#'
#' Parses the raw bodies each capture points at with the functions the live
#' scrape uses ([parse_lengjan_program()], [parse_lengjan_markets()],
#' [lengjan_markets_to_odds()]) and returns one row per capture, event,
#' market, outcome and line. Every captured event comes back, whether or not
#' `leagues` knows its competition; nothing is de-duplicated across captures,
#' so an event listed in two captures has two sets of rows, told apart by
#' `scraped_at`. Team names stay as Lengjan renders them.
#'
#' Read-only: the rows are not validated against `schemas()$odds`, nothing is
#' written to the facts store and no network request is made.
#'
#' A capture whose status is `error` is skipped with a note naming it, a
#' `skipped` one silently. A capture naming a file that is missing from the
#' local copy or cannot be read (an interrupted pull) is skipped with a
#' warning naming the file. A body that reads but has an unexpected shape
#' stops the read with an error naming the capture.
#'
#' @param dir Archive directory. Defaults to option
#'   `sports.lengjan_archive_dir`, then environment variable
#'   `LENGJAN_ARCHIVE_DIR`.
#' @param from,to Inclusive bounds on the capture time: a `POSIXct`, a `Date`
#'   or a string (`"2026-11-19"`, `"2026-11-19T18:00:00Z"`); strings without
#'   an offset are UTC. A `to` given as a date covers that whole day. `NULL`
#'   leaves the side unbounded.
#' @param what `"detail"` reads the market-detail bodies: moneyline, total
#'   and spread rows for the events a capture fetched detail for. `"program"`
#'   reads only the program body: a moneyline row per selection of every
#'   listed event's embedded main market
#'   ([parse_lengjan_program_markets()]).
#' @param leagues League definitions, used only to fill `sex`.
#' @return Tibble with the columns of the odds table (`sport`, `country`,
#'   `scraped_at`, `match_date`, `home_team`, `away_team`, `market`,
#'   `outcome`, `line`, `odds`, `sex`, `event_id`, `competition_id`,
#'   `kickoff_at`) followed by `sport_id`, `sport_name`, `country_code` and
#'   `competition_name` from the payload. `scraped_at` is the capture time.
#'   `sport` and `country` are `NA` for a sport id or country code the
#'   package does not know; `sex` is `NA` unless `leagues` lists the
#'   competition under the row's sport. `match_date` is the UTC date of
#'   `kickoff_at`; both are `NA` for an event whose kickoff does not parse
#'   (the live scrape treats that as an error; a research read keeps the
#'   prices). Zero rows, same columns, when no capture falls in the window.
#' @examples
#' \dontrun{
#' odds <- read_lengjan_archive(from = "2026-11-19", to = "2026-11-20")
#' main <- read_lengjan_archive(from = "2026-11-19", what = "program")
#' }
#' @export
read_lengjan_archive <- function(dir = NULL, from = NULL, to = NULL,
                                 what = c("detail", "program"),
                                 leagues = load_leagues()) {
  what <- match.arg(what)
  dir <- .lengjan_archive_dir(dir)
  ix <- lengjan_archive_index(dir)
  ix <- ix[.lengjan_in_window(ix$ts, from, to), , drop = FALSE]
  comp_sex <- .lengjan_competition_sex(leagues)

  rows <- list(empty_lengjan_archive())
  for (i in seq_len(nrow(ix))) {
    status <- ix$status[[i]]
    if (status %in% "skipped") next
    stamp <- format(ix$ts[[i]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    if (!status %in% c("ok", "partial")) {
      cli::cli_alert_warning(
        "Lengjan archive: capture {stamp} has status {.val {status}}; not read."
      )
      next
    }
    files <- ix$program_file[[i]]
    if (what == "detail") files <- c(files, ix$markets_files[[i]])
    bodies <- .lengjan_read_bodies(dir, files, stamp)
    if (is.null(bodies)) next
    rows[[length(rows) + 1L]] <- tryCatch(
      .lengjan_capture_rows(bodies, what, ix$ts[[i]], comp_sex),
      error = function(e) {
        stop(
          "Lengjan archive capture ", stamp, ": ", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }
  dplyr::bind_rows(rows)
}

#' Odds-shaped rows for one capture. `bodies` is the parsed program body
#' followed, in detail mode, by the capture's markets bodies.
#' @noRd
.lengjan_capture_rows <- function(bodies, what, scraped_at, comp_sex) {
  prog <- bodies[[1L]]
  odds <- if (what == "detail") {
    groups <- unlist(bodies[-1L], recursive = FALSE)
    lengjan_markets_to_odds(parse_lengjan_markets(groups))
  } else {
    parse_lengjan_program_markets(prog)
  }
  # WHY: parse_lengjan_program() costs about 3 ms per event, and a program
  # lists about a thousand; only events with odds rows survive the join.
  prog <- .lengjan_program_subset(prog, unique(odds$event_id))
  events <- parse_lengjan_program(prog)
  listed <- .lengjan_program_events(prog)
  sport_names <- stats::setNames(
    vapply(listed, function(e) .scalar_chr(e$sportName), character(1)),
    vapply(listed, function(e) as.character(e$id), character(1))
  )

  j <- dplyr::inner_join(odds, events, by = "event_id")
  j <- j[!is.na(j$home_team) & !is.na(j$away_team), , drop = FALSE]
  sport <- .lengjan_sport_from_id(j$sport_id)
  tibble::tibble(
    sport = sport,
    country = .lengjan_country_from_code(j$country_code),
    scraped_at = rep(scraped_at, nrow(j)),
    match_date = as.Date(j$kickoff_at, tz = "UTC"),
    home_team = j$home_team, away_team = j$away_team,
    market = j$market, outcome = j$outcome, line = j$line, odds = j$odds,
    sex = unname(comp_sex[paste(sport, j$competition_id)]),
    event_id = j$event_id, competition_id = j$competition_id,
    kickoff_at = j$kickoff_at,
    sport_id = j$sport_id,
    sport_name = unname(sport_names[j$event_id]),
    country_code = j$country_code,
    competition_name = j$competition_name
  )
}

#' The program with its event arrays cut down to the given event ids.
#' @noRd
.lengjan_program_subset <- function(prog, event_ids) {
  arrays <- c("events", "liveSoon", "popular", "liveNow")
  for (k in intersect(arrays, names(prog))) {
    prog[[k]] <- Filter(
      function(e) as.character(e$id) %in% event_ids, prog[[k]]
    )
  }
  prog
}

#' Read a capture's gzipped JSON bodies, in order; NULL (after a warning
#' naming the file) when one is missing from the local copy or unreadable.
#' @noRd
.lengjan_read_bodies <- function(dir, files, stamp) {
  bodies <- vector("list", length(files))
  for (k in seq_along(files)) {
    file <- files[[k]]
    path <- file.path(dir, file)
    problem <- NULL
    if (is.na(file) || !file.exists(path)) {
      problem <- "is not on disk"
    } else {
      body <- tryCatch(
        jsonlite::fromJSON(gzfile(path), simplifyVector = FALSE),
        error = function(e) e
      )
      if (inherits(body, "error")) {
        problem <- paste0("could not be read (", conditionMessage(body), ")")
      } else {
        bodies[k] <- list(body)
      }
    }
    if (!is.null(problem)) {
      cli::cli_warn(
        "Lengjan archive: skipping capture {stamp}: {.file {file}} {problem}."
      )
      return(NULL)
    }
  }
  bodies
}

#' Which capture times fall inside the inclusive `from`/`to` window.
#' @noRd
.lengjan_in_window <- function(ts, from, to) {
  keep <- rep(TRUE, length(ts))
  if (!is.null(from)) {
    keep <- keep & ts >= .lengjan_time_bound(from, "from")$time
  }
  if (!is.null(to)) {
    bound <- .lengjan_time_bound(to, "to")
    keep <- keep & if (bound$whole_day) {
      ts < bound$time + 86400
    } else {
      ts <= bound$time
    }
  }
  keep
}

#' A `from`/`to` value as a UTC instant, flagged when it named a whole day.
#' @noRd
.lengjan_time_bound <- function(x, arg) {
  if (length(x) == 1L && !is.na(x)) {
    if (inherits(x, "POSIXt")) {
      return(list(time = as.POSIXct(x), whole_day = FALSE))
    }
    if (inherits(x, "Date")) {
      return(list(time = as.POSIXct(format(x), tz = "UTC"), whole_day = TRUE))
    }
    if (is.character(x)) {
      time <- lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE)
      if (is.na(time)) time <- lubridate::ymd_hm(x, tz = "UTC", quiet = TRUE)
      if (!is.na(time)) {
        return(list(time = time, whole_day = FALSE))
      }
      day <- lubridate::ymd(x, tz = "UTC", quiet = TRUE)
      if (!is.na(day)) {
        return(list(time = day, whole_day = TRUE))
      }
    }
  }
  stop(
    "`", arg, "` must be a single POSIXct, Date or date(-time) string, ",
    "e.g. \"2026-11-19\" or \"2026-11-19T18:00:00Z\".",
    call. = FALSE
  )
}

#' `sex` per configured Lengjan competition, named "<sport> <competition id>".
#' @noRd
.lengjan_competition_sex <- function(leagues) {
  keys <- character()
  sexes <- character()
  for (lg in leagues) {
    for (cp in lg$lengjan$competitions %||% list()) {
      keys <- c(keys, paste(.scalar_chr(lg$sport), .scalar_chr(cp$id)))
      sexes <- c(sexes, .scalar_chr(cp$sex))
    }
  }
  stats::setNames(sexes, keys)[!duplicated(keys)]
}

#' Canonical sport for Lengjan sport ids; NA for an id the package lacks.
#' @noRd
.lengjan_sport_from_id <- function(sport_id) {
  sports <- c("football", "basketball", "handball")
  sports[match(sport_id, vapply(sports, .lengjan_sport_id, integer(1)))]
}

#' Canonical country for Lengjan country codes; NA for an unknown code.
#' @noRd
.lengjan_country_from_code <- function(code) {
  names(.LENGJAN_COUNTRY_CODES)[match(code, .LENGJAN_COUNTRY_CODES)]
}

#' @noRd
empty_lengjan_archive <- function() {
  dplyr::bind_cols(
    empty_lengjan_api_odds(),
    tibble::tibble(
      sport_id = integer(), sport_name = character(),
      country_code = character(), competition_name = character()
    )
  )
}

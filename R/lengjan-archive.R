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
#' that cannot be read (a truncated last line, say), that carries another
#' format version or that has no parseable `ts` is skipped; the skipped lines
#' are reported together in one warning. An archive with no manifest yet
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
  lines <- lapply(files, function(f) {
    text <- readLines(f, warn = FALSE, encoding = "UTF-8")
    keep <- which(nzchar(trimws(text)))
    list(text = text[keep], where = paste0(basename(f), " line ", keep))
  })
  where <- unlist(lapply(lines, `[[`, "where"))
  recs <- lapply(unlist(lapply(lines, `[[`, "text")), .lengjan_manifest_record)
  if (length(recs) == 0L) {
    return(empty_lengjan_archive_index())
  }

  problem <- vapply(recs, function(r) {
    if (is.character(r)) r else NA_character_
  }, character(1))
  # WHY: one vectorised parse; a ymd_hms() call per line costs about 2.5 ms,
  # which is 100 s for a year of captures at 15 minutes.
  ts <- .lengjan_parse_utc(vapply(recs, function(r) {
    if (is.character(r)) NA_character_ else .scalar_chr(r$ts)
  }, character(1)))
  problem[is.na(problem) & is.na(ts)] <- "no parseable `ts`"
  skipped <- !is.na(problem)
  if (any(skipped)) {
    warning(
      .lengjan_skip_report(
        "manifest line", paste0(where, ": ", problem)[skipped]
      ),
      call. = FALSE
    )
  }
  recs <- recs[!skipped]
  ts <- ts[!skipped]
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
    ts = ts,
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
  rec
}

#' One warning text for everything a read skipped: a count, then the first
#' `max` items, so an interrupted pull does not emit a warning per file.
#' @noRd
.lengjan_skip_report <- function(what, items, max = 10L) {
  n <- length(items)
  paste0(
    "Lengjan archive: skipped ", n, " ", what, if (n != 1L) "s", ":\n",
    paste0("  - ", items[seq_len(min(n, max))], collapse = "\n"),
    if (n > max) paste0("\n  ... and ", n - max, " more")
  )
}

#' @noRd
.lengjan_parse_utc <- function(x) {
  lubridate::ymd_hms(x, tz = "UTC", quiet = TRUE)
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
#' An event in the program carries its main market inline once it has one, so
#' a program body alone gives a full-time 1X2 price for those events without
#' a market-detail request. The embedded market goes through
#' [parse_lengjan_markets()] and [lengjan_markets_to_odds()], so the rule is
#' the live scrape's: a primary, open `3WAY` market maps to moneyline
#' home/draw/away for selections `1`/`X`/`2` priced above 1. Embedded `2WAY`
#' and `NWAY` markets are dropped, so two-way sports and outrights give no
#' row, and so is a market flagged `live` (in play): the live scrape asks
#' for `live=false` markets only, and an in-play price would otherwise pass
#' for a pre-match one. Events come from all four program arrays, first copy
#' of each id.
#'
#' @param prog Parsed JSON of a current-program body.
#' @return Tibble: event_id, market, outcome, line (always `NA`), odds.
#' @examples
#' \dontrun{
#' parse_lengjan_program_markets(lengjan_fetch_program())
#' }
#' @export
parse_lengjan_program_markets <- function(prog) {
  groups <- .lengjan_program_market_groups(prog, in_play = FALSE)
  lengjan_markets_to_odds(parse_lengjan_markets(groups))
}

#' The program's embedded main markets as market groups (group name
#' `"program"`), one per event that has one, each stamped with its event id.
#' `in_play = FALSE` drops a market flagged `live`.
#' @noRd
.lengjan_program_market_groups <- function(prog, in_play) {
  evs <- .lengjan_program_events(prog)
  evs <- evs[vapply(evs, function(e) {
    is.list(e$market) && (in_play || !isTRUE(e$market$live))
  }, logical(1))]
  lapply(evs, function(e) {
    market <- e$market
    market$eventId <- e$id
    list(name = "program", markets = list(market))
  })
}

#' The program's events across its four arrays, first copy of each id. An
#' event with no id is dropped, as parse_lengjan_program() in effect does.
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
  ids <- vapply(evs, function(e) .scalar_chr(e$id), character(1))
  evs[!is.na(ids) & !duplicated(ids)]
}

#' Read a local Lengjan odds archive into odds-shaped rows.
#'
#' Parses the raw bodies each capture points at with the functions the live
#' scrape uses ([parse_lengjan_program()], [parse_lengjan_markets()],
#' [lengjan_markets_to_odds()] or [lengjan_markets_long()]) and returns one
#' row per capture, event, market, outcome and line (`table = "odds"`) or one
#' row per capture and selection of every market (`table = "odds_markets"`).
#' A captured event comes back whether or not `leagues` knows its
#' competition, as long as it has a row to give (an event with none has no
#' row). Nothing is de-duplicated across captures, so an event listed in two
#' captures has two sets of rows, told apart by `scraped_at`. Team names stay
#' as Lengjan renders them.
#'
#' Read-only: the rows are not validated against `schemas()`, nothing is
#' written to the facts store and no network request is made.
#'
#' A capture whose status is `error` is skipped with a note naming it, a
#' `skipped` one silently. A capture is also skipped when it names a file
#' that is missing from the local copy or cannot be read (an interrupted
#' pull), or when a body has an unexpected shape (Lengjan's API changed).
#' Those captures are reported together in one warning naming each capture
#' and what was wrong with it; the rest are returned.
#'
#' @param dir Archive directory. Defaults to option
#'   `sports.lengjan_archive_dir`, then environment variable
#'   `LENGJAN_ARCHIVE_DIR`.
#' @param from,to Inclusive bounds on the capture time: a `POSIXct`, a `Date`
#'   or a string (`"2026-11-19"`, `"2026-11-19T18:00:00Z"`); strings without
#'   an offset are UTC. A `to` given as a date covers that whole day. `NULL`
#'   leaves the side unbounded.
#' @param what `"detail"` reads the market-detail bodies, for the events a
#'   capture fetched detail for. `"program"` reads only the program body: each
#'   listed event's embedded main market. With `table = "odds"` that gives
#'   moneyline rows where the embedded market is an open, pre-match primary
#'   `3WAY` ([parse_lengjan_program_markets()]); with `table = "odds_markets"`
#'   every embedded market's open, priced selections, in-play ones included
#'   and flagged by `is_live`, with `market_group` `"program"` (the program
#'   carries no group).
#' @param leagues League definitions, used only to fill `sex`.
#' @param table `"odds"` returns the odds table's columns: moneyline, total
#'   and spread only. `"odds_markets"` returns the odds_markets table's
#'   columns: every open selection priced above 1 of every market, with
#'   Lengjan's market id, version, code (`externalId`), type, group and name.
#' @return Tibble with the columns of the chosen table (`schemas()$odds` or
#'   `schemas()$odds_markets`) followed by `sport_id`, `sport_name`,
#'   `country_code` and `competition_name` from the payload. `scraped_at` is
#'   the capture time. `sport` and `country` are `NA` for a sport id or
#'   country code the package does not know; `sex` is `NA` unless `leagues`
#'   lists the competition under the row's sport. `match_date` is the UTC
#'   date of `kickoff_at`; both are `NA` for an event whose kickoff does not
#'   parse (the live scrape treats that as an error; a research read keeps
#'   the prices). Zero rows, same columns, when no capture falls in the
#'   window.
#' @examples
#' \dontrun{
#' odds <- read_lengjan_archive(from = "2026-11-19", to = "2026-11-20")
#' main <- read_lengjan_archive(from = "2026-11-19", what = "program")
#' every <- read_lengjan_archive(from = "2026-11-22", table = "odds_markets")
#' }
#' @export
read_lengjan_archive <- function(dir = NULL, from = NULL, to = NULL,
                                 what = c("detail", "program"),
                                 leagues = load_leagues(),
                                 table = c("odds", "odds_markets")) {
  what <- match.arg(what)
  table <- match.arg(table)
  dir <- .lengjan_archive_dir(dir)
  ix <- lengjan_archive_index(dir)
  ix <- ix[.lengjan_in_window(ix$ts, from, to), , drop = FALSE]
  comp_sex <- .lengjan_competition_sex(leagues)

  rows <- list(empty_lengjan_archive(table))
  skipped <- character()
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
    got <- .lengjan_read_bodies(dir, files)
    if (!is.character(got)) {
      got <- tryCatch(
        .lengjan_capture_rows(got, what, table, ix$ts[[i]], comp_sex),
        error = function(e) conditionMessage(e)
      )
    }
    if (is.character(got)) {
      skipped <- c(skipped, paste0(stamp, ": ", got))
      next
    }
    rows[[length(rows) + 1L]] <- got
  }
  if (length(skipped) > 0L) {
    warning(.lengjan_skip_report("capture", skipped), call. = FALSE)
  }
  dplyr::bind_rows(rows)
}

#' Rows of `table`'s shape for one capture. `bodies` is the parsed program
#' body followed, in detail mode, by the capture's markets bodies.
#' @noRd
.lengjan_capture_rows <- function(bodies, what, table, scraped_at, comp_sex) {
  prog <- bodies[[1L]]
  groups <- if (what == "detail") {
    unlist(bodies[-1L], recursive = FALSE)
  } else {
    .lengjan_program_market_groups(prog, in_play = table == "odds_markets")
  }
  markets <- parse_lengjan_markets(groups)
  rows <- if (table == "odds") {
    lengjan_markets_to_odds(markets)
  } else {
    lengjan_markets_long(markets)
  }
  # WHY: parse_lengjan_program() costs about 3 ms per event, and a program
  # lists about a thousand; only events with rows survive the join.
  prog <- .lengjan_program_subset(prog, unique(rows$event_id))
  events <- parse_lengjan_program(prog)
  listed <- .lengjan_program_events(prog)
  sport_names <- stats::setNames(
    vapply(listed, function(e) .scalar_chr(e$sportName), character(1)),
    vapply(listed, function(e) .scalar_chr(e$id), character(1))
  )

  j <- dplyr::inner_join(rows, events, by = "event_id")
  j <- j[!is.na(j$home_team) & !is.na(j$away_team), , drop = FALSE]
  sport <- .lengjan_sport_from_id(j$sport_id)
  match <- tibble::tibble(
    sport = sport,
    country = .lengjan_country_from_code(j$country_code),
    scraped_at = rep(scraped_at, nrow(j)),
    match_date = as.Date(j$kickoff_at, tz = "UTC"),
    home_team = j$home_team, away_team = j$away_team
  )
  ids <- tibble::tibble(
    sex = unname(comp_sex[paste(sport, j$competition_id)]),
    event_id = j$event_id, competition_id = j$competition_id,
    kickoff_at = j$kickoff_at
  )
  payload <- tibble::tibble(
    sport_id = j$sport_id,
    sport_name = unname(sport_names[j$event_id]),
    country_code = j$country_code,
    competition_name = j$competition_name
  )
  if (table == "odds") {
    odds <- j[, c("market", "outcome", "line", "odds"), drop = FALSE]
    dplyr::bind_cols(match, odds, ids, payload)
  } else {
    dplyr::bind_cols(match, ids, j[, .LENGJAN_MARKET_COLUMNS, drop = FALSE], payload)
  }
}

#' Events listed in a local Lengjan odds archive, capture by capture.
#'
#' Reads only each capture's program body, so it sees every listed event of
#' the country, whether or not a capture fetched its market detail: the
#' denominator for [lengjan_closing_coverage()]. An event's country is the
#' program's `countryCode`, or its `countryName` where the code is empty (an
#' Icelandic event's `liveSoon` copy carries `countryName` `"IS"` and no
#' code), as in [parse_lengjan_program()]. Captures are read and skipped as
#' in [read_lengjan_archive()].
#'
#' @inheritParams read_lengjan_archive
#' @param country_code Lengjan country code to keep (`"IS"` for Iceland).
#' @return Tibble, one row per capture and listed event, sorted by
#'   `scraped_at` then `event_id`: `scraped_at`, `event_id`, `sport` (NA for a
#'   sport the package does not know), `sport_id`, `competition_id`,
#'   `competition_name`, `country_code`, `kickoff_at`, `home_team`,
#'   `away_team` (NA for an outright), `market_count` and `is_live` (Lengjan's
#'   event-level live flag: live betting will be offered).
#' @examples
#' \dontrun{
#' listings <- lengjan_archive_events(from = "2026-11-16")
#' }
#' @export
lengjan_archive_events <- function(dir = NULL, from = NULL, to = NULL,
                                   country_code = "IS") {
  dir <- .lengjan_archive_dir(dir)
  ix <- lengjan_archive_index(dir)
  ix <- ix[.lengjan_in_window(ix$ts, from, to) &
    ix$status %in% c("ok", "partial"), , drop = FALSE]
  rows <- list(empty_lengjan_archive_events())
  skipped <- character()
  for (i in seq_len(nrow(ix))) {
    stamp <- format(ix$ts[[i]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    got <- .lengjan_read_bodies(dir, ix$program_file[[i]])
    if (!is.character(got)) {
      got <- tryCatch(
        .lengjan_listed_events(got[[1L]], country_code, ix$ts[[i]]),
        error = function(e) conditionMessage(e)
      )
    }
    if (is.character(got)) {
      skipped <- c(skipped, paste0(stamp, ": ", got))
      next
    }
    rows[[length(rows) + 1L]] <- got
  }
  if (length(skipped) > 0L) {
    warning(.lengjan_skip_report("capture", skipped), call. = FALSE)
  }
  out <- dplyr::bind_rows(rows)
  out[order(out$scraped_at, out$event_id), , drop = FALSE]
}

#' One program body's events of one country, as lengjan_archive_events() rows.
#' @noRd
.lengjan_listed_events <- function(prog, country_code, scraped_at) {
  evs <- .lengjan_program_events(prog)
  country <- vapply(evs, function(e) {
    cc <- .scalar_chr(e$countryCode)
    if (is.na(cc) || !nzchar(cc)) cc <- .scalar_chr(e$countryName)
    cc
  }, character(1))
  evs <- evs[country %in% country_code]
  if (length(evs) == 0L) {
    return(empty_lengjan_archive_events())
  }
  live <- stats::setNames(
    vapply(evs, function(e) isTRUE(e$live), logical(1)),
    vapply(evs, function(e) .scalar_chr(e$id), character(1))
  )
  ids <- names(live)
  # WHY: parse only this country's events; a program lists about a thousand.
  ev <- parse_lengjan_program(.lengjan_program_subset(prog, ids))
  tibble::tibble(
    scraped_at = rep(scraped_at, nrow(ev)),
    event_id = ev$event_id,
    sport = .lengjan_sport_from_id(ev$sport_id),
    sport_id = ev$sport_id,
    competition_id = ev$competition_id,
    competition_name = ev$competition_name,
    country_code = ev$country_code,
    kickoff_at = ev$kickoff_at,
    home_team = ev$home_team,
    away_team = ev$away_team,
    market_count = ev$market_count,
    is_live = unname(live[ev$event_id])
  )
}

#' @noRd
empty_lengjan_archive_events <- function() {
  tibble::tibble(
    scraped_at = as.POSIXct(character(), tz = "UTC"),
    event_id = character(), sport = character(), sport_id = integer(),
    competition_id = character(), competition_name = character(),
    country_code = character(),
    kickoff_at = as.POSIXct(character(), tz = "UTC"),
    home_team = character(), away_team = character(),
    market_count = integer(), is_live = logical()
  )
}

#' The program with its event arrays cut down to the given event ids.
#' @noRd
.lengjan_program_subset <- function(prog, event_ids) {
  arrays <- c("events", "liveSoon", "popular", "liveNow")
  for (k in intersect(arrays, names(prog))) {
    keep <- vapply(prog[[k]], function(e) {
      .scalar_chr(e$id) %in% event_ids
    }, logical(1))
    prog[[k]] <- prog[[k]][keep]
  }
  prog
}

#' Read a capture's gzipped JSON bodies, in order: a list of parsed bodies,
#' or a string naming the first file that is missing from the local copy or
#' unreadable.
#' @noRd
.lengjan_read_bodies <- function(dir, files) {
  bodies <- vector("list", length(files))
  for (k in seq_along(files)) {
    file <- files[[k]]
    path <- file.path(dir, file)
    if (is.na(file)) {
      return("the manifest names no file")
    }
    if (!file.exists(path)) {
      return(paste(file, "is not on disk"))
    }
    body <- tryCatch(
      jsonlite::fromJSON(gzfile(path), simplifyVector = FALSE),
      error = function(e) e
    )
    if (inherits(body, "error")) {
      return(paste0(file, " could not be read (", conditionMessage(body), ")"))
    }
    bodies[k] <- list(body)
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
empty_lengjan_archive <- function(table = "odds") {
  dplyr::bind_cols(
    if (table == "odds") empty_lengjan_api_odds() else empty_lengjan_api_market_rows(),
    tibble::tibble(
      sport_id = integer(), sport_name = character(),
      country_code = character(), competition_name = character()
    )
  )
}

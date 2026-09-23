#' @include ingest-lengjan-odds.R storage.R config.R
NULL

# Lengjan's public JSON API (spec 2026-09-23 WS2, finding L1). The site's own
# Next.js front end calls these; no login, no browser. current-program lists
# every event; markets returns all markets for up to 20 events per call (L2:
# past index 20 the server's query parser turns eventIds into an object and
# answers HTTP 400).
.LENGJAN_API_BASE <- "https://games.lotto.is/api/proxy/lengjan"
.LENGJAN_MARKETS_BATCH <- 20L
.LENGJAN_UA <- "sports-pipeline (+https://github.com/metill-is/sports)"

#' One-line seam over httr2::req_perform() so tests can fail the transport.
#' @noRd
.lengjan_perform <- function(req) httr2::req_perform(req)

#' GET one Lengjan API path and return the parsed JSON (lists, not simplified).
#'
#' Any transport failure or non-2xx status (after httr2's retries) is raised
#' as `lengjan_fetch_error`, the class `ingest_one_lengjan()` already
#' soft-fails to 0 rows, so an API blip behaves like a DOM navigate timeout.
#' @noRd
lengjan_api_get <- function(path, query = list()) {
  req <- httr2::request(.LENGJAN_API_BASE) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(.LENGJAN_UA) |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_timeout(30L)
  resp <- tryCatch(.lengjan_perform(req), error = function(e) e)
  if (inherits(resp, "error")) {
    stop(structure(
      class = c("lengjan_fetch_error", "error", "condition"),
      list(
        message = paste0("Lengjan API /", path, ": ", conditionMessage(resp)),
        call = NULL
      )
    ))
  }
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Fetch Lengjan's current program (every listed event).
#'
#' @return Parsed JSON: a list with `events`, `liveSoon`, `popular`, ...
#' @export
lengjan_fetch_program <- function() {
  lengjan_api_get("current-program")
}

#' Fetch all markets for Lengjan events, at most 20 per request.
#'
#' @param event_ids Character vector of Lengjan event ids.
#' @param batch_size Ids per request (at most 20: the server rejects more).
#' @param pause_s Seconds between requests (politeness; 0 in tests).
#' @return List of market groups: the requests' JSON arrays, concatenated.
#' @export
lengjan_fetch_markets <- function(event_ids, batch_size = .LENGJAN_MARKETS_BATCH,
                                  pause_s = 1) {
  ids <- unique(as.character(event_ids))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0L) {
    return(list())
  }
  stopifnot(batch_size >= 1L, batch_size <= .LENGJAN_MARKETS_BATCH)
  chunks <- split(ids, ceiling(seq_along(ids) / batch_size))
  out <- list()
  for (k in seq_along(chunks)) {
    chunk <- chunks[[k]]
    query <- stats::setNames(
      as.list(chunk), sprintf("eventIds[%d]", seq_along(chunk) - 1L)
    )
    query$live <- "false"
    out <- c(out, lengjan_api_get("markets", query))
    if (k < length(chunks) && pause_s > 0) Sys.sleep(pause_s)
  }
  out
}

#' Flatten Lengjan's current-program JSON into one row per event.
#'
#' Unions `events`, `liveSoon`, `popular` and `liveNow` -- an Icelandic event
#' with no open market yet sits only in `liveSoon` (spec L7) -- keeping the
#' first copy of each event id. `country_code` falls back to `countryName`:
#' Icelandic events carry `countryName: "IS"` and no `countryCode` (L4).
#'
#' @param prog Parsed JSON from [lengjan_fetch_program()].
#' @return Tibble: event_id, sport_id, competition_id, competition_name,
#'   country_code, kickoff_at (POSIXct UTC), home_team, away_team, market_count.
#' @export
parse_lengjan_program <- function(prog) {
  if (!is.list(prog) || !all(c("events", "liveSoon") %in% names(prog))) {
    stop(
      "parse_lengjan_program: unexpected current-program shape ",
      "(no events/liveSoon arrays); Lengjan's API may have changed.",
      call. = FALSE
    )
  }
  evs <- c(prog$events, prog$liveSoon, prog$popular, prog$liveNow)
  if (length(evs) == 0L) {
    return(empty_lengjan_events())
  }
  out <- dplyr::bind_rows(lapply(evs, .lengjan_event_row))
  out[!duplicated(out$event_id), , drop = FALSE]
}

#' @noRd
.lengjan_event_row <- function(e) {
  parts <- e$participants %||% list()
  side <- function(k) {
    for (p in parts) {
      if (identical(as.integer(p$homeOrAway), k)) {
        return(as.character(p$name))
      }
    }
    NA_character_
  }
  cc <- e$countryCode
  if (is.null(cc) || !nzchar(cc)) cc <- e$countryName %||% NA_character_
  tibble::tibble(
    event_id = as.character(e$id),
    sport_id = as.integer(e$sportId),
    competition_id = as.character(e$compId),
    competition_name = trimws(as.character(e$compName %||% NA_character_)),
    country_code = as.character(cc),
    kickoff_at = lubridate::ymd_hms(
      e$datePlayed %||% NA_character_, tz = "UTC", quiet = TRUE
    ),
    home_team = side(1L),
    away_team = side(2L),
    market_count = as.integer(e$marketCount %||% 0L)
  )
}

#' @noRd
empty_lengjan_events <- function() {
  tibble::tibble(
    event_id = character(), sport_id = integer(), competition_id = character(),
    competition_name = character(), country_code = character(),
    kickoff_at = as.POSIXct(character(), tz = "UTC"),
    home_team = character(), away_team = character(), market_count = integer()
  )
}

#' Flatten Lengjan's markets JSON into one row per selection.
#'
#' @param groups Parsed JSON from [lengjan_fetch_markets()]: market groups,
#'   each with `name` ("OU_FT", "HC_FT", "single-<id>", ...) and `markets`.
#' @return Tibble: event_id, group, type, type_name, primary, status,
#'   special_value, selection, odds (decimal: the API's integer / 100, L3).
#' @export
parse_lengjan_markets <- function(groups) {
  rows <- list()
  for (g in groups) {
    if (!is.list(g) || is.null(g$markets)) {
      stop(
        "parse_lengjan_markets: market group without a `markets` array; ",
        "Lengjan's API may have changed.",
        call. = FALSE
      )
    }
    for (m in g$markets) {
      sels <- m$selections %||% list()
      if (length(sels) == 0L) next
      rows[[length(rows) + 1L]] <- tibble::tibble(
        event_id = as.character(m$eventId),
        group = as.character(g$name %||% NA_character_),
        type = as.character(m$type %||% NA_character_),
        type_name = as.character(m$typeName %||% NA_character_),
        primary = isTRUE(m$primary),
        status = as.character(m$status %||% NA_character_),
        special_value = as.character(m$specialValue %||% NA_character_),
        selection = vapply(sels, function(s) {
          as.character(s$name %||% NA_character_)
        }, character(1)),
        odds = vapply(sels, function(s) {
          as.numeric(s$odds %||% NA_real_)
        }, numeric(1)) / 100
      )
    }
  }
  if (length(rows) == 0L) {
    return(empty_lengjan_markets())
  }
  dplyr::bind_rows(rows)
}

#' @noRd
empty_lengjan_markets <- function() {
  tibble::tibble(
    event_id = character(), group = character(), type = character(),
    type_name = character(), primary = logical(), status = character(),
    special_value = character(), selection = character(), odds = numeric()
  )
}

#' Map parsed Lengjan markets onto the canonical odds vocabulary.
#'
#' The event's primary 3WAY (full-time 1X2) -> moneyline home/draw/away;
#' group OU_FT -> total over/under at `special_value`; group HC_FT (3-way
#' handicap) -> spread home/draw/away at `special_value`, which is home's
#' signed handicap ("Forgjof 1-0" -> 1, as [parse_handicap()] encodes it).
#' Everything else -- half-time, double chance, Asian handicap, BTTS -- is
#' dropped, as is any selection that is not open or whose price is not a
#' valid decimal (> 1): one suspended market must not abort the whole write
#' through validate_values().
#'
#' @param markets Output of [parse_lengjan_markets()].
#' @return Tibble: event_id, market, outcome, line, odds.
#' @export
lengjan_markets_to_odds <- function(markets) {
  m <- markets[markets$status %in% "open" & is.finite(markets$odds) &
    markets$odds > 1, , drop = FALSE]
  three_way <- c("1" = "home", "X" = "draw", "2" = "away")
  over_under <- c(Yfir = "over", Undir = "under")

  ml <- m[m$primary & m$type_name %in% "3WAY" &
    m$selection %in% names(three_way), , drop = FALSE]
  ml$market <- rep("moneyline", nrow(ml))
  ml$outcome <- unname(three_way[ml$selection])
  ml$line <- rep(NA_real_, nrow(ml))

  tot <- m[m$group %in% "OU_FT" & m$type_name %in% "OU" &
    m$selection %in% names(over_under), , drop = FALSE]
  tot$market <- rep("total", nrow(tot))
  tot$outcome <- unname(over_under[tot$selection])
  tot$line <- suppressWarnings(as.numeric(tot$special_value))

  hc <- m[m$group %in% "HC_FT" & m$type_name %in% "HC" &
    m$selection %in% names(three_way), , drop = FALSE]
  hc$market <- rep("spread", nrow(hc))
  hc$outcome <- unname(three_way[hc$selection])
  hc$line <- suppressWarnings(as.numeric(hc$special_value))

  out <- dplyr::bind_rows(ml, tot, hc)
  out <- out[out$market == "moneyline" | is.finite(out$line), , drop = FALSE]
  out[, c("event_id", "market", "outcome", "line", "odds"), drop = FALSE]
}

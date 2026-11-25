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
# Longest wait a server's Retry-After may ask for: httr2 sleeps whatever the
# header says, uncapped, and a run has a 45-minute job timeout.
.LENGJAN_RETRY_AFTER_MAX_S <- 60

#' One-line seam over httr2::req_perform() so tests can fail the transport.
#' @noRd
.lengjan_perform <- function(req) httr2::req_perform(req)

#' HTTP statuses worth another try: rate limiting and the server-side
#' failures a proxy returns while its backend restarts.
#' @noRd
.lengjan_is_transient <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

#' Seconds to wait before a retry: the server's Retry-After, capped at
#' `.LENGJAN_RETRY_AFTER_MAX_S`; NA (no header) leaves httr2's own backoff.
#' @noRd
.lengjan_retry_after <- function(resp) {
  after <- httr2::resp_retry_after(resp)
  if (is.na(after)) {
    return(NA_real_)
  }
  max(0, min(after, .LENGJAN_RETRY_AFTER_MAX_S))
}

#' GET one Lengjan API path and return the parsed JSON (lists, not simplified).
#'
#' Retries transport failures and HTTP 429, 500, 502, 503 and 504, three
#' tries in all, waiting at most 60 s for a Retry-After. Any transport failure
#' or non-2xx status left after that is raised as `lengjan_fetch_error`, the
#' class `ingest_one_lengjan()` already soft-fails to 0 rows, so an API blip
#' behaves like a DOM navigate timeout.
#' @noRd
lengjan_api_get <- function(path, query = list()) {
  req <- httr2::request(.LENGJAN_API_BASE) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!query) |>
    httr2::req_user_agent(.LENGJAN_UA) |>
    httr2::req_retry(
      max_tries = 3L, retry_on_failure = TRUE,
      is_transient = .lengjan_is_transient, after = .lengjan_retry_after
    ) |>
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

#' A current-program fetcher that asks Lengjan once, then serves that body.
#'
#' `scripts/02_scrape_odds.R` scrapes league by league, and each API league
#' used to fetch the whole program again. A run makes one of these and passes
#' it to every league, so they share one request. A failed fetch is not kept:
#' the next league asks again.
#'
#' @param fetch The fetcher to wrap.
#' @return A function of no arguments that returns the parsed program.
#' @examples
#' \dontrun{
#' program <- lengjan_program_once()
#' identical(program(), program()) # one request
#' }
#' @export
lengjan_program_once <- function(fetch = lengjan_fetch_program) {
  force(fetch)
  body <- NULL
  function() {
    if (is.null(body)) body <<- fetch()
    body
  }
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
#' @return Tibble: event_id, market_id, market_version, group, type,
#'   market_code (the payload's `externalId`: a stable code for the market
#'   type), type_name, market_name (Lengjan's Icelandic label), primary, live
#'   (the market is in play), status, special_value, closes_at (POSIXct UTC,
#'   from `dateClosed`), selection, odds (decimal: the API's integer / 100,
#'   L3). Every selection is kept, open or not, priced or not.
#' @export
parse_lengjan_markets <- function(groups) {
  ms <- list()
  for (g in groups) {
    if (!is.list(g) || is.null(g$markets)) {
      stop(
        "parse_lengjan_markets: market group without a `markets` array; ",
        "Lengjan's API may have changed.",
        call. = FALSE
      )
    }
    for (m in g$markets) {
      if (length(m$selections) == 0L) next
      m$group <- g$name
      ms[[length(ms) + 1L]] <- m
    }
  }
  if (length(ms) == 0L) {
    return(empty_lengjan_markets())
  }
  # WHY: one tibble at the end, not one per market. An archive capture holds
  # thousands of markets, and building a tibble costs about 0.3 ms.
  n <- vapply(ms, function(m) length(m$selections), integer(1))
  field <- function(name) {
    rep(vapply(ms, function(m) .scalar_chr(m[[name]]), character(1)), n)
  }
  flag <- function(name) {
    rep(vapply(ms, function(m) isTRUE(m[[name]]), logical(1)), n)
  }
  sels <- unlist(lapply(ms, `[[`, "selections"), recursive = FALSE)
  tibble::tibble(
    event_id = field("eventId"),
    market_id = field("id"),
    market_version = field("version"),
    group = field("group"),
    type = field("type"),
    market_code = field("externalId"),
    type_name = field("typeName"),
    market_name = field("name"),
    primary = flag("primary"),
    live = flag("live"),
    status = field("status"),
    special_value = field("specialValue"),
    closes_at = lubridate::ymd_hms(field("dateClosed"), tz = "UTC", quiet = TRUE),
    selection = vapply(sels, function(s) {
      as.character(s$name %||% NA_character_)
    }, character(1)),
    odds = vapply(sels, function(s) {
      as.numeric(s$odds %||% NA_real_)
    }, numeric(1)) / 100
  )
}

#' One scalar as a string; NA for a missing, empty, list or longer value.
#'
#' A double is written in full: as.character() gives "3e+09" for a round id
#' or version that arrives as a JSON number.
#' @noRd
.scalar_chr <- function(x) {
  if (is.null(x) || length(x) != 1L || is.list(x)) {
    return(NA_character_)
  }
  if (is.double(x) && !is.na(x)) {
    return(format(x, scientific = FALSE, trim = TRUE, digits = 15))
  }
  as.character(x)
}

#' @noRd
empty_lengjan_markets <- function() {
  tibble::tibble(
    event_id = character(), market_id = character(),
    market_version = character(), group = character(), type = character(),
    market_code = character(), type_name = character(),
    market_name = character(), primary = logical(), live = logical(),
    status = character(), special_value = character(),
    closes_at = as.POSIXct(character(), tz = "UTC"),
    selection = character(), odds = numeric()
  )
}

#' Selections that are open and carry a valid decimal price (> 1).
#'
#' One suspended market or a missing price must not abort the whole write
#' through validate_values(), so they are dropped row by row.
#' @noRd
.lengjan_open_priced <- function(markets) {
  markets[markets$status %in% "open" & is.finite(markets$odds) &
    markets$odds > 1, , drop = FALSE]
}

#' Map parsed Lengjan markets onto the canonical odds vocabulary.
#'
#' The event's primary 3WAY (full-time 1X2) -> moneyline home/draw/away;
#' group OU_FT -> total over/under at `special_value`; group HC_FT (3-way
#' handicap) -> spread home/draw/away at `special_value`, which is home's
#' signed handicap ("Forgjof 1-0" -> 1, as [parse_handicap()] encodes it).
#' Everything else -- half-time, double chance, Asian handicap, BTTS -- is
#' dropped here and kept by [lengjan_markets_long()], as is any selection
#' that is not open or whose price is not a valid decimal (> 1): one
#' suspended market must not abort the whole write through validate_values().
#'
#' @param markets Output of [parse_lengjan_markets()].
#' @return Tibble: event_id, market, outcome, line, odds.
#' @export
lengjan_markets_to_odds <- function(markets) {
  out <- .lengjan_canonical_rows(.lengjan_open_priced(markets))
  out[, c("event_id", "market", "outcome", "line", "odds"), drop = FALSE]
}

#' The canonical mapping of [lengjan_markets_to_odds()] without its open and
#' priced filter: every column of `m` is kept, `status` included, so the
#' shadow comparison can tell a suspended selection from a missing one.
#' @noRd
.lengjan_canonical_rows <- function(m) {
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
  out[out$market == "moneyline" | is.finite(out$line), , drop = FALSE]
}

#' Every open, priced selection of every market, in the odds_markets
#' vocabulary.
#'
#' The lossless counterpart of [lengjan_markets_to_odds()]: one row per
#' selection that is open and priced above 1, whatever the market. `line` is
#' `special_value` as a number (a total's 2.5, a handicap's signed -1), NA for
#' a market without one; `special_value` keeps the raw string. A market listed
#' in two groups gives one set of rows (the first): event, market id and
#' selection are the odds_markets key within a scrape.
#'
#' @param markets Output of [parse_lengjan_markets()].
#' @return Tibble: event_id, market_id, market_version, market_code,
#'   market_type, market_group, market_name, special_value, line, selection,
#'   odds, is_primary, is_live, closes_at.
#' @export
lengjan_markets_long <- function(markets) {
  m <- .lengjan_open_priced(markets)
  # WHY: upsert_table() de-duplicates only against stored rows, so a duplicate
  # within the first write of a day would be stored and dropped by the next.
  m <- m[!duplicated(m[, c("event_id", "market_id", "selection")]), , drop = FALSE]
  tibble::tibble(
    event_id = m$event_id,
    market_id = m$market_id,
    market_version = m$market_version,
    market_code = m$market_code,
    market_type = m$type_name,
    market_group = m$group,
    market_name = m$market_name,
    special_value = m$special_value,
    line = suppressWarnings(as.numeric(m$special_value)),
    selection = m$selection,
    odds = m$odds,
    is_primary = m$primary,
    is_live = m$live,
    closes_at = m$closes_at
  )
}

#' Per-selection columns of the odds_markets table, after the match columns.
#' @noRd
.LENGJAN_MARKET_COLUMNS <- c(
  "market_id", "market_version", "market_code", "market_type",
  "market_group", "market_name", "special_value", "line", "selection",
  "odds", "is_primary", "is_live", "closes_at"
)

#' Rows of one league's events, joined to their program fields.
#'
#' An event belongs to the league when its sport matches and its competition
#' id is one of the league's `lengjan.competitions`; the row takes that
#' competition's `sex`. An event with no home or away team is dropped. A
#' selected event with no parseable kickoff would be written with match_date
#' NA, never joinable and never reported (final review F3).
#' `bad_kickoff = "stop"` makes that an error naming the event: a plain
#' error, not lengjan_fetch_error, so it is never soft-failed; the scrape
#' script's run_per_league() contains it to this league. `"drop"` drops the
#' event's rows with a warning naming it instead.
#'
#' @param rows Rows keyed by `event_id` (odds or long market rows).
#' @return `rows` joined to `events`, with a `sex` column.
#' @noRd
.lengjan_league_rows <- function(events, rows, league,
                                 bad_kickoff = c("stop", "drop")) {
  bad_kickoff <- match.arg(bad_kickoff)
  comps <- league$lengjan$competitions %||% list()
  comp_ids <- vapply(comps, function(cp) as.character(cp$id), character(1))
  comp_sex <- stats::setNames(
    vapply(comps, function(cp) as.character(cp$sex), character(1)),
    comp_ids
  )
  ev <- events[events$sport_id == .lengjan_sport_id(league$sport) &
    events$competition_id %in% comp_ids, , drop = FALSE]
  j <- dplyr::inner_join(rows, ev, by = "event_id")
  j <- j[!is.na(j$home_team) & !is.na(j$away_team), , drop = FALSE]
  bad <- unique(j$event_id[is.na(j$kickoff_at)])
  if (length(bad) > 0L) {
    msg <- paste0(
      "Lengjan API: no parseable kickoff (datePlayed) for ",
      league$sport, " event(s) ", paste(bad, collapse = ", "),
      "; Lengjan's API may have changed."
    )
    if (bad_kickoff == "stop") stop(msg, call. = FALSE)
    warning(msg, " Their market rows are not written.", call. = FALSE)
    j <- j[!j$event_id %in% bad, , drop = FALSE]
  }
  j$sex <- unname(comp_sex[j$competition_id])
  j
}

#' Canonical odds rows for one league from a parsed program and markets.
#'
#' An event belongs to the league when its sport matches and its competition
#' id is one of the league's `lengjan.competitions`; the row takes that
#' competition's `sex`. Team names stay as Lengjan renders them -- decide maps
#' them to canonical per sex ([normalise_lengjan_team_names()]).
#' `match_date` is the kickoff's UTC date: Iceland keeps UTC all year. A
#' selected event with no parseable kickoff is an error naming the event, not
#' a row with an NA `match_date`.
#'
#' @param events Output of [parse_lengjan_program()].
#' @param markets Output of [parse_lengjan_markets()].
#' @param league A league definition carrying `sport`, `country`, `lengjan`.
#' @param scraped_at Single timestamp for the run.
#' @return Tibble matching `schemas()$odds`.
#' @export
lengjan_api_odds_rows <- function(events, markets, league, scraped_at) {
  .lengjan_odds_shape(
    events, lengjan_markets_to_odds(markets), league, scraped_at
  )
}

#' Odds-table rows for one league from canonical rows keyed by `event_id`.
#' @noRd
.lengjan_odds_shape <- function(events, odds, league, scraped_at,
                                bad_kickoff = "stop") {
  j <- .lengjan_league_rows(events, odds, league, bad_kickoff = bad_kickoff)
  if (nrow(j) == 0L) {
    return(empty_lengjan_api_odds())
  }
  tibble::tibble(
    sport = league$sport, country = league$country,
    scraped_at = rep(scraped_at, nrow(j)),
    match_date = as.Date(j$kickoff_at, tz = "UTC"),
    home_team = j$home_team, away_team = j$away_team,
    market = j$market, outcome = j$outcome, line = j$line, odds = j$odds,
    sex = j$sex,
    event_id = j$event_id, competition_id = j$competition_id,
    kickoff_at = j$kickoff_at
  )
}

#' @noRd
empty_lengjan_api_odds <- function() {
  tibble::tibble(
    sport = character(), country = character(),
    scraped_at = as.POSIXct(character(), tz = "UTC"),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), odds = numeric(),
    sex = character(), event_id = character(), competition_id = character(),
    kickoff_at = as.POSIXct(character(), tz = "UTC")
  )
}

#' Every market's rows for one league, for the odds_markets table.
#'
#' Selects the league's events as [lengjan_api_odds_rows()] does (same sport
#' and competition test, same `sex`) and keeps every open, priced selection
#' of every market ([lengjan_markets_long()]). An event with no parseable
#' kickoff is dropped with a warning naming it, not an error: the long table
#' holds markets the odds table never keeps, so an error here would reach
#' events the odds table never checks.
#'
#' @inheritParams lengjan_api_odds_rows
#' @return Tibble matching `schemas()$odds_markets`.
#' @export
lengjan_api_market_rows <- function(events, markets, league, scraped_at) {
  j <- .lengjan_league_rows(events, lengjan_markets_long(markets), league,
    bad_kickoff = "drop"
  )
  if (nrow(j) == 0L) {
    return(empty_lengjan_api_market_rows())
  }
  dplyr::bind_cols(
    tibble::tibble(
      sport = league$sport, country = league$country,
      scraped_at = rep(scraped_at, nrow(j)),
      match_date = as.Date(j$kickoff_at, tz = "UTC"),
      home_team = j$home_team, away_team = j$away_team,
      sex = j$sex,
      event_id = j$event_id, competition_id = j$competition_id,
      kickoff_at = j$kickoff_at
    ),
    j[, .LENGJAN_MARKET_COLUMNS, drop = FALSE]
  )
}

#' @noRd
empty_lengjan_api_market_rows <- function() {
  dplyr::bind_cols(
    tibble::tibble(
      sport = character(), country = character(),
      scraped_at = as.POSIXct(character(), tz = "UTC"),
      match_date = as.Date(character()),
      home_team = character(), away_team = character(),
      sex = character(), event_id = character(),
      competition_id = character(),
      kickoff_at = as.POSIXct(character(), tz = "UTC")
    ),
    lengjan_markets_long(empty_lengjan_markets())[, .LENGJAN_MARKET_COLUMNS]
  )
}

#' One in-memory API snapshot of leagues' odds, for the shadow comparison.
#'
#' Fetches markets for the leagues' listed events that have open markets
#' (the selection [ingest_lengjan_api()] makes) and maps them as the API
#' scrape would write them. Writes nothing. Pass `events` to reuse a parsed
#' program, so a before-and-after pair costs one program request.
#'
#' @param leagues Named list of league definitions (each with `lengjan`).
#' @param events Output of [parse_lengjan_program()]; NULL fetches it.
#' @param fetch_program,fetch_markets Injectable fetchers, as for
#'   [ingest_lengjan_api()].
#' @param now Clock, called once the markets have arrived.
#' @return List: `taken_at` (POSIXct), `events` (the program rows of the
#'   leagues' competitions), `odds` (the rows [lengjan_api_odds_rows()] gives,
#'   stamped `taken_at`) and `closed` (the same shape, for selections in those
#'   markets that are not open or carry no valid price: what the API scrape
#'   leaves out).
#' @export
lengjan_api_snapshot <- function(leagues, events = NULL,
                                 fetch_program = lengjan_fetch_program,
                                 fetch_markets = lengjan_fetch_markets,
                                 now = Sys.time) {
  stopifnot(is.list(leagues), length(leagues) > 0L)
  if (is.null(events)) events <- parse_lengjan_program(fetch_program())
  mine <- rep(FALSE, nrow(events))
  for (lg in leagues) {
    ids <- vapply(lg$lengjan$competitions %||% list(), function(cp) {
      as.character(cp$id)
    }, character(1))
    mine <- mine | (events$sport_id == .lengjan_sport_id(lg$sport) &
      events$competition_id %in% ids)
  }
  events <- events[mine, , drop = FALSE]
  wanted <- events$event_id[events$market_count > 0L]
  markets <- if (length(wanted) == 0L) {
    empty_lengjan_markets()
  } else {
    parse_lengjan_markets(fetch_markets(wanted))
  }
  taken_at <- now()

  open <- .lengjan_open_priced(markets)
  mapped <- .lengjan_canonical_rows(markets)
  is_open <- paste(mapped$event_id, mapped$market_id, mapped$selection) %in%
    paste(open$event_id, open$market_id, open$selection)
  closed <- mapped[!is_open, c("event_id", "market", "outcome", "line", "odds"),
    drop = FALSE
  ]
  per_league <- function(build) {
    dplyr::bind_rows(c(list(empty_lengjan_api_odds()), lapply(leagues, build)))
  }
  list(
    taken_at = taken_at,
    events = events,
    odds = per_league(function(lg) {
      lengjan_api_odds_rows(events, markets, lg, taken_at)
    }),
    closed = per_league(function(lg) {
      .lengjan_odds_shape(events, closed, lg, taken_at, bad_kickoff = "drop")
    })
  )
}

#' Scrape odds from Lengjan's JSON API for leagues and upsert them.
#'
#' One program request, then markets for the leagues' open events in batches
#' of 20 -- typically 2-4 requests in all. The API counterpart of
#' [ingest_lengjan_odds()] (the Chromote DOM scraper), chosen per league by
#' `lengjan.source: api` in [ingest_one_lengjan()].
#'
#' Writes two tables from the same responses: `odds`, the three markets
#' decide maps ([lengjan_api_odds_rows()]), and `odds_markets`, every open,
#' priced selection of every market ([lengjan_api_market_rows()]). `odds` is
#' built and written first, exactly as before `odds_markets` existed, so
#' nothing about the second table can stop the first: an unparseable kickoff
#' on an event with an odds-table market writes neither table; on an event
#' with only other markets it drops that event's market rows with a warning.
#' A failed `odds_markets` write raises after `odds` is written: the league
#' fails loudly, and the scrape workflow still commits the odds.
#'
#' @param leagues Named list of league definitions (each with `lengjan`).
#' @param scraped_at Single timestamp for the whole run.
#' @param root Storage root.
#' @param fetch_program,fetch_markets Injectable fetchers; tests pass fixtures,
#'   and the scrape script passes one [lengjan_program_once()] to every league.
#' @return Number of `odds` rows written (invisible integer).
#' @export
ingest_lengjan_api <- function(leagues, scraped_at = Sys.time(),
                               root = here::here("data"),
                               fetch_program = lengjan_fetch_program,
                               fetch_markets = lengjan_fetch_markets) {
  stopifnot(is.list(leagues), length(leagues) > 0L)
  events <- parse_lengjan_program(fetch_program())

  wanted <- character(0)
  for (lg in leagues) {
    ids <- vapply(lg$lengjan$competitions %||% list(), function(cp) {
      as.character(cp$id)
    }, character(1))
    hit <- events$sport_id == .lengjan_sport_id(lg$sport) &
      events$competition_id %in% ids & events$market_count > 0L
    wanted <- c(wanted, events$event_id[hit])
  }
  wanted <- unique(wanted)
  if (length(wanted) == 0L) {
    cli::cli_alert_info("Lengjan API: no open markets for {.val {names(leagues)}}")
    return(invisible(0L))
  }

  markets <- parse_lengjan_markets(fetch_markets(wanted))
  per_league <- function(build) {
    dplyr::bind_rows(lapply(leagues, build,
      events = events, markets = markets, scraped_at = scraped_at
    ))
  }
  rows <- per_league(lengjan_api_odds_rows)
  upsert_table(rows, "odds", root = root)
  # WHY after the odds write: decide reads odds, and the long table covers
  # markets and events the odds mapping never reaches (PR #119 review).
  market_rows <- per_league(lengjan_api_market_rows)
  upsert_table(market_rows, "odds_markets", root = root)
  if (nrow(rows) == 0L && nrow(market_rows) == 0L) {
    cli::cli_alert_info("Lengjan API: {length(wanted)} event{?s} but no open, priced selections")
    return(invisible(0L))
  }
  n_markets <- nrow(dplyr::distinct(market_rows[, c("event_id", "market_id")]))
  cli::cli_alert_success(paste(
    "Lengjan API: wrote {nrow(rows)} odds rows and {nrow(market_rows)} market",
    "rows ({n_markets} market{?s}) for {length(wanted)} event{?s}"
  ))
  invisible(nrow(rows))
}

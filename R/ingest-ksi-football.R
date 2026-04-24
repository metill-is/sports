#' @include ingest.R
NULL

#' KSÍ competition ID registry.
#'
#' Layout: `KSI_IDS[[sex]][[division_slug]][[as.character(season)]]`.
#'
#' - `sex`: "male" or "female"
#' - `division_slug`: "div1" (Besta deild / Bónusdeild), "div2" (1. deild),
#'   "div3" (2. deild), "div4" (3. deild), "div5" (4. deild), "cup" (Mjölnir /
#'   bikarkeppni), plus playoff tournaments that rotate each year.
#' - Season: integer year (Icelandic football seasons align with the calendar
#'   year, running May-October).
#'
#' Values are the `id` query parameter on
#' `www.ksi.is/oll-mot/mot?id=<id>&banner-tab=matches-and-results`. The old
#' `stakt-mot/?motnumer=` URL scheme has been retired. Update at the start of
#' each season by browsing `www.ksi.is/oll-mot` and copying the `?id=` param
#' from each competition page.
#'
#' @keywords internal
#' @noRd
KSI_IDS <- list(
  male = list(
    div1 = list(`2025` = 190366L, `2026` = 7025510L),
    div2 = list(`2025` = 190359L, `2026` = 7025540L),
    div3 = list(`2025` = 190365L, `2026` = 7025548L),
    div4 = list(`2025` = 190363L, `2026` = 7025551L),
    div5 = list(`2025` = 190358L, `2026` = 7025560L),
    cup = list(`2025` = 190367L, `2026` = 7059683L),
    div1_upper_playoffs = list(`2026` = 7025527L),
    div1_lower_playoffs = list(`2026` = 7025532L),
    div2_playoffs = list(`2026` = 7025545L)
  ),
  female = list(
    div1 = list(`2025` = 190372L, `2026` = 7025645L),
    div2 = list(`2025` = 190375L, `2026` = 7025676L),
    div3 = list(`2025` = 190364L, `2026` = 7025681L),
    cup = list(`2025` = 190356L, `2026` = 7058831L),
    div1_upper_playoffs = list(`2026` = 7025657L),
    div1_lower_playoffs = list(`2026` = 7025661L)
  )
)

#' Map internal division slug to canonical division label.
#'
#' Labels match the legacy `football/iceland/` data files.
#' @keywords internal
#' @noRd
KSI_DIVISION_LABELS <- c(
  div1 = "BD",
  div2 = "LD1",
  div3 = "LD2",
  div4 = "LD3",
  div5 = "LD4",
  cup = "CUP",
  div1_upper_playoffs = "BD_UPPER_PO",
  div1_lower_playoffs = "BD_LOWER_PO",
  div2_playoffs = "LD1_PO"
)

#' Build a KSÍ competition page URL.
#'
#' @param id Integer competition ID.
#' @return Character URL.
#' @keywords internal
#' @noRd
ksi_url <- function(id) {
  sprintf(
    "https://www.ksi.is/oll-mot/mot?id=%s&banner-tab=matches-and-results",
    id
  )
}

#' Fetch a KSÍ competition page.
#'
#' KSÍ pages are server-rendered Astro — plain `httr2::req_perform` + `rvest`
#' returns the fully populated match list. No chromote needed. Response body
#' is decoded explicitly as UTF-8 so Icelandic team names and month
#' abbreviations survive the round-trip.
#'
#' @param id Integer competition ID.
#' @return xml_document on success; NULL on HTTP failure (caller warns + skips).
#' @keywords internal
#' @noRd
fetch_ksi_page <- function(id) {
  url <- ksi_url(id)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("sports-pipeline (+https://github.com/metill-is/sports)") |>
      httr2::req_retry(max_tries = 3L) |>
      httr2::req_timeout(30L) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp) || httr2::resp_status(resp) != 200L) {
    return(NULL)
  }
  body <- httr2::resp_body_raw(resp)
  rvest::read_html(body, encoding = "UTF-8")
}

#' Icelandic month-name → 2-digit month number map.
#'
#' KSÍ displays months in Icelandic long form ("apríl", "maí", "júní",
#' "ágúst"). We normalise via regex replacement before `lubridate::dmy()`,
#' avoiding a process-wide `Sys.setlocale()` which is fragile across
#' `devtools::load_all()`, CI runners, and R CMD check.
#'
#' Keys are constructed with `stringi::stri_unescape_unicode()` instead of
#' R's `\uxxxx` literal escapes. Under a C locale, `\u00ed` in an R source
#' file can be parsed into the ASCII mojibake string `<U+00ED>` rather than
#' the UTF-8 bytes `c3 ad`, breaking regex matching against live UTF-8 input.
#' `stri_unescape_unicode()` guarantees the proper UTF-8 bytes regardless of
#' locale.
#' @keywords internal
#' @noRd
ksi_build_month_map <- function() {
  raw_keys <- c(
    "jan\\u00faar", "febr\\u00faar", "mars", "apr\\u00edl",
    "ma\\u00ed", "j\\u00fan\\u00ed", "j\\u00fal\\u00ed",
    "\\u00e1g\\u00fast", "september", "okt\\u00f3ber",
    "n\\u00f3vember", "desember"
  )
  values <- sprintf("%02d", 1:12)
  keys <- stringi::stri_unescape_unicode(raw_keys)
  stats::setNames(values, keys)
}
KSI_MONTH_MAP <- ksi_build_month_map()

#' Parse a KSÍ date fragment ("Sun 26. apríl 18:00") into a Date.
#'
#' KSÍ displays each match-row date as "<day-name> <day>. <month-name>  <time>"
#' (double-space before the time). We strip the day-name prefix and the
#' trailing time, map the Icelandic month name to a 2-digit number via
#' `KSI_MONTH_MAP`, then delegate to `lubridate::dmy()`.
#'
#' @param date_raw Character vector of raw date strings.
#' @param year Integer year to append.
#' @return Date vector.
#' @keywords internal
#' @noRd
parse_ksi_date <- function(date_raw, year) {
  # Ensure inputs are tagged UTF-8 so regex matching with UTF-8 month keys
  # doesn't fall back to byte-exact comparison with mojibake strings.
  Encoding(date_raw) <- "UTF-8"

  cleaned <- date_raw |>
    sub("^[^ ]+ ", "", x = _) |>
    sub("  .*$", "", x = _) |>
    trimws() |>
    tolower()

  # stringr's `\b` word-boundary doesn't treat "í", "ú", "ó", "á" as word
  # characters; we anchor each month with explicit space / end lookarounds.
  for (m in names(KSI_MONTH_MAP)) {
    pattern <- paste0("(?<=\\s)", m, "(?=\\s|$)")
    cleaned <- stringr::str_replace(cleaned, pattern, KSI_MONTH_MAP[[m]])
  }

  cleaned <- paste(cleaned, year)
  suppressWarnings(lubridate::dmy(cleaned))
}

#' Extract the season year from a parsed KSÍ page.
#'
#' Reads the Tímabil `<select>` dropdown (id `timabil-select-filter-select`).
#' Options include an empty placeholder plus 4-digit year values; we pick the
#' maximum valid year as the canonical season for the page. Falls back to the
#' current calendar year if the dropdown is missing.
#' @keywords internal
#' @noRd
extract_ksi_season_year <- function(page) {
  sel <- rvest::html_element(page, "select#timabil-select-filter-select")
  if (inherits(sel, "xml_missing") || is.na(sel)) {
    return(as.integer(format(Sys.Date(), "%Y")))
  }
  opts <- rvest::html_elements(sel, "option")
  years <- opts |>
    rvest::html_attr("value") |>
    (\(x) x[nchar(x) == 4L])() |>
    as.integer()
  if (length(years) == 0L) {
    return(as.integer(format(Sys.Date(), "%Y")))
  }
  max(years, na.rm = TRUE)
}

#' Extract raw match tuples from a KSÍ match-list page.
#'
#' KSÍ renders each match row as a DOM subtree that includes:
#' - a date column (`div.flex.flex-col > div`) with weekday + date + time,
#' - a score/placeholder span (`span.body-4.whitespace-nowrap`), and
#' - two `<a>` links (home + away team names) in the same parent as the score.
#'
#' The score span reads "H - A" for played matches and "-" for scheduled.
#'
#' @param page Parsed xml_document from `fetch_ksi_page()` or a fixture.
#' @return Tibble with raw columns `date_raw`, `home_team`, `away_team`,
#'   `home_score` (int or NA), `away_score` (int or NA). One row per match.
#' @keywords internal
#' @noRd
extract_ksi_matches <- function(page) {
  empty <- tibble::tibble(
    date_raw = character(),
    home_team = character(),
    away_team = character(),
    home_score = integer(),
    away_score = integer()
  )

  score_nodes <- rvest::html_elements(page, "span.body-4.whitespace-nowrap")
  if (length(score_nodes) == 0L) {
    return(empty)
  }

  rows <- lapply(score_nodes, function(node) {
    score_text <- rvest::html_text(node) |> trimws()
    parent <- xml2::xml_parent(node)
    links <- rvest::html_elements(parent, "a")
    if (length(links) < 2L) {
      return(NULL)
    }
    home <- rvest::html_text(links[[1]]) |> trimws()
    away <- rvest::html_text(links[[2]]) |> trimws()

    row <- xml2::xml_parent(parent)
    first_col <- rvest::html_element(row, "div.flex.flex-col")
    date_raw <- if (inherits(first_col, "xml_missing") || is.na(first_col)) {
      ""
    } else {
      date_el <- rvest::html_element(first_col, "div")
      if (inherits(date_el, "xml_missing") || is.na(date_el)) {
        ""
      } else {
        rvest::html_text(date_el) |> trimws()
      }
    }

    score_parts <- stringr::str_match(score_text, "^([0-9]+) - ([0-9]+)$")
    if (!is.na(score_parts[1, 1L])) {
      tibble::tibble(
        date_raw = date_raw,
        home_team = home,
        away_team = away,
        home_score = as.integer(score_parts[1, 2L]),
        away_score = as.integer(score_parts[1, 3L])
      )
    } else {
      tibble::tibble(
        date_raw = date_raw,
        home_team = home,
        away_team = away,
        home_score = NA_integer_,
        away_score = NA_integer_
      )
    }
  })

  rows <- rows[!vapply(rows, is.null, logical(1L))]
  if (length(rows) == 0L) {
    return(empty)
  }
  dplyr::bind_rows(rows)
}

#' Empty canonical-schema results tibble.
#' @keywords internal
#' @noRd
ksi_empty_results <- function() {
  tibble::tibble(
    sport = character(),
    country = character(),
    sex = character(),
    season = integer(),
    match_date = as.Date(character()),
    home_team = character(),
    away_team = character(),
    home_score = integer(),
    away_score = integer(),
    division = character(),
    round = integer()
  )
}

#' Empty canonical-schema schedule tibble.
#' @keywords internal
#' @noRd
ksi_empty_schedule <- function() {
  tibble::tibble(
    sport = character(),
    country = character(),
    sex = character(),
    season = integer(),
    match_date = as.Date(character()),
    home_team = character(),
    away_team = character(),
    division = character(),
    round = integer()
  )
}

#' Parse a KSÍ match-list page into canonical results schema.
#'
#' Pure function — accepts an `xml_document`, returns a tibble matching
#' `schemas()$results`. Fixture-testable: no network.
#'
#' @param html xml_document from `fetch_ksi_page()` or
#'   `rvest::read_html(<fixture>)`.
#' @param sport,country,sex,division,season Context columns applied to every
#'   row of the output.
#' @param played_only If TRUE, drop rows without a valid score.
#' @return Tibble with canonical results columns.
#' @keywords internal
#' @noRd
parse_ksi_results_page <- function(html, sport, country, sex, division, season,
                                   played_only = FALSE) {
  raw <- extract_ksi_matches(html)
  if (nrow(raw) == 0L) {
    return(ksi_empty_results())
  }

  match_date <- parse_ksi_date(raw$date_raw, season)

  out <- tibble::tibble(
    sport = sport,
    country = country,
    sex = sex,
    season = as.integer(season),
    match_date = match_date,
    home_team = raw$home_team,
    away_team = raw$away_team,
    home_score = raw$home_score,
    away_score = raw$away_score,
    division = division,
    round = NA_integer_
  )

  keep <- !is.na(out$match_date) &
    !is.na(out$home_team) & nzchar(out$home_team) &
    !is.na(out$away_team) & nzchar(out$away_team)
  if (played_only) {
    keep <- keep & !is.na(out$home_score) & !is.na(out$away_score)
  }
  out[keep, , drop = FALSE]
}

#' Parse a KSÍ match-list page into canonical schedule schema.
#'
#' Schedule rows are match-list rows without a score — reuses
#' `parse_ksi_results_page()` and drops played matches.
#'
#' @inheritParams parse_ksi_results_page
#' @return Tibble with canonical schedules columns (no score fields).
#' @keywords internal
#' @noRd
parse_ksi_schedule_page <- function(html, sport, country, sex, division, season) {
  all_matches <- parse_ksi_results_page(
    html,
    sport = sport, country = country, sex = sex,
    division = division, season = season,
    played_only = FALSE
  )
  if (nrow(all_matches) == 0L) {
    return(ksi_empty_schedule())
  }

  upcoming <- all_matches[is.na(all_matches$home_score), , drop = FALSE]
  upcoming[, c(
    "sport", "country", "sex", "season", "match_date",
    "home_team", "away_team", "division", "round"
  )]
}

#' Iterate over all configured (division, season) pairs for one sex.
#'
#' @param sex "male" or "female".
#' @param seasons Optional integer vector restricting seasons.
#' @param sleep_seconds Polite delay between HTTP requests.
#' @return Combined tibble (canonical results schema). Empty tibble if every
#'   page failed or returned no matches.
#' @keywords internal
#' @noRd
fetch_ksi_all <- function(sex, seasons = NULL, sleep_seconds = 1) {
  stopifnot(sex %in% names(KSI_IDS))

  sex_map <- KSI_IDS[[sex]]
  frames <- list()

  for (div in names(sex_map)) {
    division_label <- KSI_DIVISION_LABELS[[div]]
    season_map <- sex_map[[div]]
    season_keys <- names(season_map)
    if (!is.null(seasons)) {
      season_keys <- intersect(season_keys, as.character(seasons))
    }

    for (season_key in season_keys) {
      id <- season_map[[season_key]]
      season_int <- as.integer(season_key)

      parsed <- tryCatch(
        {
          Sys.sleep(sleep_seconds)
          page <- fetch_ksi_page(id)
          if (is.null(page)) {
            NULL
          } else {
            parse_ksi_results_page(
              page,
              sport = "football",
              country = "iceland",
              sex = sex,
              division = division_label,
              season = season_int,
              played_only = FALSE
            )
          }
        },
        error = function(e) {
          cli::cli_warn(c(
            "KSI fetch failed for {sex}/{div}/{season_key}",
            "i" = "{conditionMessage(e)}"
          ))
          NULL
        }
      )
      if (is.null(parsed) || nrow(parsed) == 0L) next
      frames[[length(frames) + 1L]] <- parsed
    }
  }

  if (length(frames) == 0L) {
    return(ksi_empty_results())
  }
  dplyr::bind_rows(frames)
}

#' Source-module entrypoint: results for a (league, sex).
#'
#' Iterates all KSI_IDS for the requested sex, returning only matches with a
#' parsed score. Historical seasons with stale IDs return empty and are
#' skipped via `tryCatch`; warnings surface via `cli::cli_warn`.
#'
#' @param league Unused (source-module signature parity); KSÍ wiring is fixed
#'   internally.
#' @param sex "male" or "female".
#' @param seasons Optional integer vector restricting seasons.
#' @keywords internal
#' @noRd
fetch_results_ksi <- function(league, sex, seasons = NULL) {
  all_matches <- fetch_ksi_all(sex, seasons = seasons)
  if (nrow(all_matches) == 0L) {
    return(ksi_empty_results())
  }
  all_matches[!is.na(all_matches$home_score) &
    !is.na(all_matches$away_score), , drop = FALSE]
}

#' Source-module entrypoint: schedule (upcoming only) for a (league, sex).
#'
#' Returns only future fixtures (match_date >= today) without scores. Restricts
#' the scan to the current calendar year by default — historical IDs in
#' KSI_IDS do not expose future matches, so fetching them is pure overhead.
#' @keywords internal
#' @noRd
fetch_schedule_ksi <- function(league, sex, seasons = NULL) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(seasons)) {
    # Upcoming matches only live on the current-or-next season page.
    seasons <- c(current_year, current_year + 1L)
  }
  all_matches <- fetch_ksi_all(sex, seasons = seasons)
  if (nrow(all_matches) == 0L) {
    return(ksi_empty_schedule())
  }
  upcoming <- all_matches[is.na(all_matches$home_score) &
    !is.na(all_matches$match_date) &
    all_matches$match_date >= Sys.Date(), , drop = FALSE]
  upcoming[, c(
    "sport", "country", "sex", "season", "match_date",
    "home_team", "away_team", "division", "round"
  )]
}

register_ingest_source(
  "ksi_football",
  list(
    fetch_results = fetch_results_ksi,
    fetch_schedule = fetch_schedule_ksi
  )
)

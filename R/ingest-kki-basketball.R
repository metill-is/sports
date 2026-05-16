#' @include ingest.R
NULL

#' Baskethotel API key (publicly visible in widget URLs).
#' @keywords internal
#' @noRd
BASKETHOTEL_API <- "a0d07178160bf749eb6e5e761fc623fe42e2bb57"

#' Nested season ID registry for KKI basketball.
#'
#' Layout: `KKI_SEASON_IDS[[sex]][[div]][[as.character(season)]]`.
#' Divisions: div1 = Bónusdeild (BD), div2 = 1. Deild (1D).
#'
#' Season ID values are the `season_id` query parameter on
#' `widgets.baskethotel.com/widget-service/export/view/schedule_and_results`.
#'
#' Season convention: "2026" = the 2025–2026 season (i.e. the calendar year
#' the season ends). All 24 IDs for 2021–2026 verified via XLSX download on
#' 2026-04-24; see `Sports/Baskethotel Season IDs.md` in the Metill vault
#' for the full reference (including the widget-500 discovery method for
#' deeper history back to 2014-2015).
#'
#' No separate cup / playoffs season IDs: KKÍ packages post-season as extra
#' rounds inside the regular-season `season_id` export ("Deildarkeppni").
#'
#' Caveat: the 2025 male div1 ID `190366` that appeared in legacy notes is
#' **invalid** — the XLSX comes back header-only (6230 bytes). The correct
#' 2024–2025 male div1 is `128582`.
#' @keywords internal
#' @noRd
KKI_SEASON_IDS <- list(
  male = list(
    div1 = list(
      `2021` = 118319L, `2022` = 121197L, `2023` = 124655L,
      `2024` = 127358L, `2025` = 128582L, `2026` = 130403L
    ),
    div2 = list(
      `2021` = 118315L, `2022` = 121191L, `2023` = 124650L,
      `2024` = 127380L, `2025` = 128589L, `2026` = 130402L
    )
  ),
  female = list(
    div1 = list(
      `2021` = 118325L, `2022` = 121199L, `2023` = 124654L,
      `2024` = 127289L, `2025` = 128585L, `2026` = 130422L
    ),
    div2 = list(
      `2021` = 118317L, `2022` = 121195L, `2023` = 124651L,
      `2024` = 127381L, `2025` = 128590L, `2026` = 130421L
    )
  )
)

#' Map internal div slug to canonical division label.
#' @keywords internal
#' @noRd
KKI_DIVISION_LABELS <- c(div1 = "BD", div2 = "1D")

#' Build a Baskethotel widget export URL.
#' @param season_id Integer season identifier.
#' @param type "results_only" or "schedule_only".
#' @keywords internal
#' @noRd
baskethotel_url <- function(season_id, type = c("results_only", "schedule_only")) {
  type <- match.arg(type)
  sprintf(
    paste0(
      "https://widgets.baskethotel.com/widget-service/export/view/",
      "schedule_and_results?api=%s&season_id=%s&lang=is&month=all&type=%s"
    ),
    BASKETHOTEL_API, season_id, type
  )
}

#' Download a Baskethotel XLSX export to a temp file.
#'
#' Validates that the response body starts with the XLSX magic bytes ("PK")
#' before returning — Baskethotel silently returns an HTML error page for
#' invalid keys / season IDs, which `readxl::read_excel` would then parse
#' into a confusing error.
#'
#' @return Path to a temp `.xlsx` file.
#' @keywords internal
#' @noRd
download_baskethotel_xlsx <- function(season_id,
                                      type = c("results_only", "schedule_only")) {
  type <- match.arg(type)
  url <- baskethotel_url(season_id, type)

  resp <- httr2::request(url) |>
    httr2::req_user_agent("sports-pipeline (+https://github.com/metill-is/sports)") |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_timeout(60L) |>
    httr2::req_perform()

  body <- httr2::resp_body_raw(resp)
  if (length(body) < 2L || !identical(as.raw(c(0x50, 0x4B)), body[1:2])) {
    stop(
      "Baskethotel did not return an XLSX file for season_id=", season_id,
      " type=", type,
      " (first bytes: ", paste(body[1:min(8L, length(body))], collapse = " "), ")",
      call. = FALSE
    )
  }

  tmp <- tempfile(fileext = ".xlsx")
  writeBin(body, tmp)
  tmp
}

#' Parse a Baskethotel results/schedule XLSX into canonical schema.
#'
#' Pure function — takes a file path and context, returns a tibble matching
#' `schemas()$results` (even for schedule exports; the caller drops score
#' columns). Fixture-testable: no network.
#'
#' @param path Path to XLSX file.
#' @param sport,country,sex,division,season Context columns applied to every
#'   row of the output.
#' @return Tibble with canonical results columns.
#' @keywords internal
#' @noRd
parse_baskethotel_xlsx <- function(path, sport, country, sex, division, season) {
  raw <- readxl::read_excel(path, sheet = 1L, skip = 1L, col_types = "text")

  # Column names from live inspection of sample_male_div1_2026.xlsx:
  #   id, vikudagur, dags., timi, heimalid, gestalid, leikvollur, stig, ahorfendur
  # Position-based rename is robust against encoding of Icelandic column names.
  if (ncol(raw) < 8L) {
    stop(
      "Expected at least 8 columns in Baskethotel export, got ", ncol(raw),
      call. = FALSE
    )
  }
  names(raw)[1:8] <- c(
    "id", "vikudagur", "dags", "timi",
    "heimalid", "gestalid", "leikvollur", "stig"
  )

  # Drop divider rows (all NA) and any row missing a date.
  cleaned <- raw[!is.na(raw$dags) & nzchar(raw$dags), , drop = FALSE]

  # Split "home-away" scores. "0-0" in schedule exports becomes NA via filter
  # below; genuinely drawn 0-0 results are impossible in basketball.
  scores <- stringr::str_match(cleaned$stig, "^\\s*(\\d+)\\s*-\\s*(\\d+)\\s*$")
  home_score <- suppressWarnings(as.integer(scores[, 2L]))
  away_score <- suppressWarnings(as.integer(scores[, 3L]))

  # Schedule exports use "0-0" for unplayed matches. Treat those as NA so the
  # caller can differentiate played vs unplayed.
  is_placeholder <- !is.na(home_score) & home_score == 0L &
    !is.na(away_score) & away_score == 0L
  home_score[is_placeholder] <- NA_integer_
  away_score[is_placeholder] <- NA_integer_

  match_date <- lubridate::dmy(cleaned$dags)

  out <- tibble::tibble(
    sport = sport,
    country = country,
    sex = sex,
    season = as.integer(season),
    match_date = match_date,
    home_team = cleaned$heimalid,
    away_team = cleaned$gestalid,
    home_score = home_score,
    away_score = away_score,
    division = division,
    round = NA_integer_
  )

  # Drop rows with missing dates / teams (extra safety against stray separator rows).
  out[!is.na(out$match_date) &
    !is.na(out$home_team) & nzchar(out$home_team) &
    !is.na(out$away_team) & nzchar(out$away_team), , drop = FALSE]
}

#' Download + parse every (div, season) pair for a sex.
#' @param league League config entry (unused beyond presence — KKI-specific
#'   metadata is baked into this module).
#' @param sex "male" or "female".
#' @param seasons Optional integer vector to restrict to specific seasons.
#' @param type "results_only" or "schedule_only".
#' @return Combined tibble across all available (div, season) pairs.
#' @keywords internal
#' @noRd
fetch_kki <- function(league, sex, seasons = NULL,
                      type = c("results_only", "schedule_only")) {
  type <- match.arg(type)
  stopifnot(sex %in% names(KKI_SEASON_IDS))

  sex_map <- KKI_SEASON_IDS[[sex]]
  frames <- list()

  for (div in names(sex_map)) {
    division_label <- KKI_DIVISION_LABELS[[div]]
    season_map <- sex_map[[div]]
    season_keys <- names(season_map)
    if (!is.null(seasons)) {
      season_keys <- intersect(season_keys, as.character(seasons))
    }

    for (season_key in season_keys) {
      season_id <- season_map[[season_key]]
      season_int <- as.integer(season_key)

      path <- tryCatch(
        download_baskethotel_xlsx(season_id, type = type),
        error = function(e) {
          cli::cli_warn(c(
            "Baskethotel download failed for {sex}/{div}/{season_key}",
            "i" = "{conditionMessage(e)}"
          ))
          NULL
        }
      )
      if (is.null(path)) next

      parsed <- parse_baskethotel_xlsx(
        path,
        sport = "basketball",
        country = "iceland",
        sex = sex,
        division = division_label,
        season = season_int
      )
      frames[[length(frames) + 1L]] <- parsed
    }
  }

  if (length(frames) == 0L) {
    return(parse_baskethotel_xlsx_empty())
  }
  dplyr::bind_rows(frames)
}

#' Return an empty canonical-schema tibble.
#' @keywords internal
#' @noRd
parse_baskethotel_xlsx_empty <- function() {
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

#' Source-module entrypoint: results for a (league, sex).
#' @keywords internal
#' @noRd
fetch_results_kki <- function(league, sex, seasons = NULL) {
  fetch_kki(league, sex, seasons = seasons, type = "results_only")
}

#' Source-module entrypoint: schedule (upcoming only) for a (league, sex).
#'
#' Schedule exports from Baskethotel can contain played + unplayed matches
#' depending on timing. Downstream callers expect `schedules` to hold only
#' future fixtures, so filter to rows with `is.na(home_score)`.
#' @keywords internal
#' @noRd
fetch_schedule_kki <- function(league, sex, seasons = NULL) {
  raw <- fetch_kki(league, sex, seasons = seasons, type = "schedule_only")
  if (nrow(raw) == 0L) {
    return(tibble::tibble(
      sport = character(),
      country = character(),
      sex = character(),
      season = integer(),
      match_date = as.Date(character()),
      home_team = character(),
      away_team = character(),
      division = character(),
      round = integer()
    ))
  }

  # Mirror the KSI / HSI scrapers: drop unplayed rows whose kickoff is in the
  # past (stale fixtures that never resolved — e.g. the 2024 ÍA orphan
  # surfaced in the 2026-05-15 audit). Schedule downstream must contain only
  # genuinely-upcoming matches; played-but-unscored matches will land via the
  # next results fetch.
  upcoming <- raw[is.na(raw$home_score) &
    !is.na(raw$match_date) &
    raw$match_date >= Sys.Date(), , drop = FALSE]
  upcoming[, c(
    "sport", "country", "sex", "season", "match_date",
    "home_team", "away_team", "division", "round"
  )]
}

register_ingest_source(
  "kki_basketball",
  list(
    fetch_results = fetch_results_kki,
    fetch_schedule = fetch_schedule_kki
  )
)

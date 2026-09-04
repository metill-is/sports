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
#' 2027 (the 2026-27 season) was resolved from the kki.is motayfirlit season
#' selector rather than probed, then VERIFIED live on 2026-09-04 by fetching
#' each schedule export: male div1 132568 (132 fixtures, 12 teams,
#' 2026-10-08..2027-03-30), male div2 132571 (132, 12), female div1 132567
#' (90, 10, 2026-09-29..2027-03-02), female div2 132570 (132, 12). Row counts
#' are exactly n*(n-1), a full double round robin, and every date falls in
#' {2026, 2027} so `.assert_season_stamp()` accepts them.
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
      `2024` = 127358L, `2025` = 128582L, `2026` = 130403L,
      `2027` = 132568L
    ),
    div2 = list(
      `2021` = 118315L, `2022` = 121191L, `2023` = 124650L,
      `2024` = 127380L, `2025` = 128589L, `2026` = 130402L,
      `2027` = 132571L
    )
  ),
  female = list(
    div1 = list(
      `2021` = 118325L, `2022` = 121199L, `2023` = 124654L,
      `2024` = 127289L, `2025` = 128585L, `2026` = 130422L,
      `2027` = 132567L
    ),
    div2 = list(
      `2021` = 118317L, `2022` = 121195L, `2023` = 124651L,
      `2024` = 127381L, `2025` = 128590L, `2026` = 130421L,
      `2027` = 132570L
    )
  )
)

#' Map internal div slug to canonical division label.
#' @keywords internal
#' @noRd
KKI_DIVISION_LABELS <- c(div1 = "BD", div2 = "1D")

#' Stable KKI competition identifiers, per (sex, division).
#'
#' `league_id` names the competition and does NOT change between seasons;
#' `season_id` (see [KKI_SEASON_IDS]) rotates every July. Keying the registry
#' on the stable half is what stops the ingest going silently blind each
#' autumn: a missing `season_id` is now resolvable from kki.is rather than
#' being a hand-edited integer that nobody remembers to bump (spec section 6,
#' finding N3).
#'
#' Read live from kki.is on 2026-09-02 and cross-validated: for each of the
#' four, the season selector's 2025-26 option equals the `season_id` this repo
#' already holds under `KKI_SEASON_IDS[[sex]][[div]][["2026"]]`
#' (190 -> 130403, 191 -> 130402, 189 -> 130422, 231 -> 130421). That
#' agreement is what licenses trusting the same page for an unknown season.
#'
#' Source URL shape:
#'   https://kki.is/motamal/leikir-og-urslit/motayfirlit/Leikir?league_id=<id>
#' @keywords internal
#' @noRd
KKI_LEAGUE_IDS <- list(
  male = list(
    div1 = 190L,  # Bonusdeild karla
    div2 = 191L   # 1. deild karla
  ),
  female = list(
    div1 = 189L,  # Bonusdeild kvenna
    div2 = 231L   # 1. deild kvenna
  )
)

#' Resolve the stable KKI `league_id` for a (sex, division) cell.
#'
#' Aborts rather than returning NA on an unresolved cell: a NA id would build
#' a syntactically valid URL that quietly returns nothing, which is the exact
#' silent-blindness this registry exists to prevent.
#'
#' @param sex "male" or "female".
#' @param division A key of [KKI_DIVISION_LABELS] ("div1" / "div2").
#' @return Integer scalar.
#' @keywords internal
#' @noRd
kki_league_id <- function(sex, division) {
  if (!sex %in% names(KKI_LEAGUE_IDS)) {
    cli::cli_abort("unknown KKI sex: {.val {sex}}", call = NULL)
  }
  by_div <- KKI_LEAGUE_IDS[[sex]]
  if (!division %in% names(by_div)) {
    cli::cli_abort(
      "unknown KKI division: {.val {division}}",
      call = NULL
    )
  }
  id <- by_div[[division]]
  if (length(id) != 1L || is.na(id)) {
    cli::cli_abort(
      c(
        "KKI league_id for {.val {sex}}/{.val {division}} has not been resolved.",
        "i" = "Discover it from the kki.is motayfirlit page and record it in
               KKI_LEAGUE_IDS."
      ),
      call = NULL
    )
  }
  as.integer(id)
}

#' KKI motayfirlit page URL for a competition (all seasons).
#'
#' The page's season selector is JS-rendered, so this must be fetched with
#' `rvest::read_html_live()` (chromote), not a plain `httr` GET -- the same
#' constraint the HSI scraper already lives with.
#' @keywords internal
#' @noRd
kki_motayfirlit_url <- function(league_id) {
  sprintf(
    paste0(
      "https://kki.is/motamal/leikir-og-urslit/motayfirlit/",
      "Leikir?league_id=%d"
    ),
    as.integer(league_id)
  )
}

#' Parse the season selector out of a KKI motayfirlit page.
#'
#' Pure and fixture-tested: no network. The page carries three unnamed
#' `<select>` elements (season, `stig` / stage, `leikdagur` / matchday), so
#' they cannot be told apart by attribute. The season one is identified by the
#' SHAPE of its option labels -- `YYYY-YYYY` -- which is stable across the
#' other two (whose labels are Icelandic words and bare round numbers).
#'
#' Season numbering follows this repo's convention throughout: the CLOSING
#' calendar year. Label `2026-2027` is season `2027`.
#'
#' NB the page also exposes a stage dimension (`Deildarkeppni` vs
#' `Urslitakeppni`) which this repo does not yet capture -- KKI packages the
#' playoffs as extra rounds inside the same `season_id`, which is why
#' basketball BD rows carry urslitakeppni matches. Capturing that stage split
#' is deliberately deferred to the publish workstream that consumes it.
#'
#' @param html A parsed document, or a string/path accepted by
#'   `rvest::read_html()`.
#' @return Tibble with columns `season` (integer, closing year), `season_id`
#'   (integer) and `label` (character), newest first. Zero rows when the page
#'   has no season selector.
#' @keywords internal
#' @noRd
parse_kki_season_options <- function(html) {
  doc <- if (inherits(html, "xml_document")) html else rvest::read_html(html)
  empty <- tibble::tibble(
    season = integer(), season_id = integer(), label = character()
  )

  opts <- rvest::html_elements(doc, "select option")
  if (length(opts) == 0L) {
    return(empty)
  }
  label <- trimws(rvest::html_text(opts))
  value <- trimws(rvest::html_attr(opts, "value"))

  # `YYYY-YYYY` is what distinguishes the season selector from the stage and
  # matchday ones. A blank value is the "Veldu timabil" placeholder.
  keep <- !is.na(value) & nzchar(value) &
    grepl("^[0-9]{4}\\s*-\\s*[0-9]{4}$", label)
  if (!any(keep)) {
    return(empty)
  }

  label <- label[keep]
  tibble::tibble(
    season = as.integer(sub("^[0-9]{4}\\s*-\\s*", "", label)),
    season_id = as.integer(value[keep]),
    label = label
  ) |>
    dplyr::arrange(dplyr::desc(.data$season))
}

#' Current KKI season, as the closing calendar year.
#'
#' The Icelandic basketball season spans October to May, so from July onwards
#' the current season is the one closing next year. Matches
#' `hsi_current_season()`, and the `KKI_SEASON_IDS` key convention where
#' "2026" is the 2025-26 season.
#' @keywords internal
#' @noRd
kki_current_season <- function(today = Sys.Date()) {
  yr <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))
  if (mo >= 7L) yr + 1L else yr
}

#' Resolve a KKI `season_id` for a (sex, division, season).
#'
#' Registry first (hand-verified, offline), then the federation provenance
#' cache. Returns NULL when neither knows -- callers treat NULL as
#' do-not-fetch and report it, rather than fetching nothing silently.
#' @keywords internal
#' @noRd
kki_season_id <- function(sex, division, season,
                          path = federation_seasons_path()) {
  key <- as.character(as.integer(season))
  registry <- KKI_SEASON_IDS[[sex]][[division]][[key]]
  if (!is.null(registry) && !is.na(registry)) {
    return(as.integer(registry))
  }
  cached <- tryCatch(
    federation_season_id("kki", sex, division, as.integer(season), path = path),
    error = function(e) NULL
  )
  if (!is.null(cached) && !is.na(cached)) {
    return(as.integer(cached))
  }
  NULL
}

#' KKI (sex, division) cells with no resolvable id for `season`.
#'
#' The KKI analogue of `hsi_unresolved_seasons()`. A cell listed here is one
#' the ingest will skip, so it is reported rather than being indistinguishable
#' from an off-season.
#' @keywords internal
#' @noRd
kki_unresolved_seasons <- function(season, sexes = c("male", "female"),
                                   path = federation_seasons_path()) {
  rows <- list()
  for (sex in sexes) {
    for (div in names(KKI_DIVISION_LABELS)) {
      if (is.null(kki_season_id(sex, div, season, path = path))) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          sex = sex, division = div, season = as.integer(season)
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      sex = character(), division = character(), season = integer()
    ))
  }
  dplyr::bind_rows(rows)
}

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

  # Default to the CURRENT season only. This used to iterate every registry
  # key, so a routine ingest re-downloaded five historical seasons that can
  # never change -- 2 sexes x 2 types x 2 divisions x 6 seasons = 48 XLSX
  # downloads a day. Pass `seasons` explicitly to backfill.
  target_seasons <- if (is.null(seasons)) {
    kki_current_season()
  } else {
    as.integer(seasons)
  }

  frames <- list()

  for (div in names(KKI_DIVISION_LABELS)) {
    division_label <- KKI_DIVISION_LABELS[[div]]

    for (season_int in target_seasons) {
      season_int <- as.integer(season_int)
      season_id <- kki_season_id(sex, div, season_int)

      if (is.null(season_id)) {
        # Report, do not skip silently. An unresolved id is indistinguishable
        # from an off-season in the log otherwise, which is how the registry
        # going stale each July stayed invisible.
        cli::cli_warn(c(
          "KKI {sex}/{div} season={season_int}: no season_id resolved -- skipped.",
          "i" = "Refresh from the kki.is motayfirlit selector
                 (parse_kki_season_options)."
        ))
        next
      }

      path <- tryCatch(
        download_baskethotel_xlsx(season_id, type = type),
        error = function(e) {
          cli::cli_warn(c(
            "Baskethotel download failed for {sex}/{div}/{season_int}",
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

      # Deliberately OUTSIDE the tryCatch above: a re-raise from inside a
      # class-specific handler is caught by that same tryCatch's `error=`
      # handler, which would degrade this abort back into the warning it
      # exists to escape.
      .assert_season_stamp(
        parsed, season_int,
        source = sprintf("kki %s/%s season_id=%s", sex, div, season_id)
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

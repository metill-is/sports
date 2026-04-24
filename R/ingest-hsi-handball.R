#' @include ingest.R
NULL

#' HSÍ handball source URL templates.
#'
#' Layout: HSI_URLS[[sex]][[division_slug]][[kind]] where
#' - sex: "male" or "female"
#' - division_slug: "div1" (Olísdeild), "div2" (Grill 66), "cup" (male only),
#'   "playoffs"
#' - kind: "url" (current season page)
#'
#' Current-season pages render both results and schedule tables. For
#' tournament-style pages (cup, playoffs), HSÍ exposes tournament IDs that
#' rotate each year and require updating at the start of each season; same for
#' the `-2025-26` suffix on the league pages.
#' @keywords internal
#' @noRd
HSI_URLS <- list(
  male = list(
    div1 = "https://www.hsi.is/olis-deild-karla-2025-26",
    div2 = "https://www.hsi.is/grill-66-deild-karla-2025-26",
    cup = "https://www.hsi.is/tournament/8437",
    playoffs = "https://www.hsi.is/tournament/8427"
  ),
  female = list(
    div1 = "https://www.hsi.is/olis-deild-kvenna-1",
    div2 = "https://www.hsi.is/grill-66-deild-kvenna-2025-26",
    playoffs = "https://www.hsi.is/tournament/8430"
  )
)

#' Map internal division slug to canonical division label.
#'
#' Matches the labels used in legacy `handball/iceland/` data files:
#' - Olísdeild = OD
#' - Grill 66 deild = G66
#' - Cup = CUP
#' - Playoffs = PO
#' @keywords internal
#' @noRd
HSI_DIVISION_LABELS <- c(
  div1 = "OD",
  div2 = "G66",
  cup = "CUP",
  playoffs = "PO"
)

#' Historical HSÍ tournament IDs per (sex, division, season).
#'
#' Layout: `HSI_HISTORICAL_IDS[[sex]][[division]][[as.character(season)]]` →
#' integer `mot_nr` used in `https://www.hsi.is/tournament/{mot_nr}`.
#'
#' Season labels follow the same convention as `hsi_current_season()` — the
#' closing calendar year of the season span (e.g. 2025 = Sept 2024 – May 2025).
#' Values extracted from legacy `_legacy/sports/handball/iceland/R/utils/
#' {male,female}/download_historical_data_div{1,2,cup}.R`.
#'
#' Notes / caveats:
#' - Male cup history is omitted: the legacy `download_historical_data_cup.R`
#'   copy-pasted the div1 IDs for 2021–2024 (a bug — it would have scraped the
#'   Olísdeild tournament and written rows labelled "cup"). The 2025–26 cup is
#'   already the current tournament in `HSI_URLS[[male]][[cup]]`.
#' - Female div2 2025 is omitted: the legacy file has "2025" = 7644, which is
#'   a copy-paste from male div2 (same ID) — the genuine female Grill 66 2024–25
#'   tournament ID is not recoverable from the legacy source.
#' - Current-season tournament IDs live in `HSI_URLS` (league pages, not
#'   tournament pages) and are handled separately.
#' @keywords internal
#' @noRd
HSI_HISTORICAL_IDS <- list(
  male = list(
    div1 = list(
      "2021" = 5260L,
      "2022" = 5640L,
      "2023" = 6149L,
      "2024" = 6983L,
      "2025" = 7641L
    ),
    div2 = list(
      "2021" = 5262L,
      "2022" = 5643L,
      "2023" = 6143L,
      "2024" = 6981L,
      "2025" = 7644L
    )
  ),
  female = list(
    div1 = list(
      "2021" = 5261L,
      "2022" = 5641L,
      "2023" = 6146L,
      "2024" = 6982L,
      "2025" = 7642L
    ),
    div2 = list(
      "2021" = 5263L,
      "2022" = 5642L,
      "2023" = 6148L,
      "2024" = 6980L
    )
  )
)

#' Build the tournament URL for a historical HSÍ (sex, division, season).
#' @keywords internal
#' @noRd
hsi_historical_url <- function(sex, division, season) {
  ids_sex <- HSI_HISTORICAL_IDS[[sex]]
  if (is.null(ids_sex)) {
    return(NULL)
  }
  ids_div <- ids_sex[[division]]
  if (is.null(ids_div)) {
    return(NULL)
  }
  mot_nr <- ids_div[[as.character(season)]]
  if (is.null(mot_nr)) {
    return(NULL)
  }
  sprintf("https://www.hsi.is/tournament/%d", as.integer(mot_nr))
}

#' Icelandic month-abbreviation → 2-digit month number map.
#'
#' HSÍ date strings look like "Fim. 12. mar. 26" — day, abbreviated month
#' name, 2-digit year. We normalise month names via case-insensitive
#' replacement before `lubridate::dmy()`.
#' @keywords internal
#' @noRd
HSI_MONTH_MAP <- c(
  jan = "01", feb = "02", mar = "03", apr = "04",
  "ma\u00ed" = "05", "j\u00fan" = "06", "j\u00fal" = "07",
  "\u00e1g\u00fa" = "08", sept = "09", sep = "09",
  okt = "10", "n\u00f3v" = "11", des = "12"
)

#' Build an HSÍ page URL for a given (sex, division) pair.
#'
#' The `kind` arg is kept for API parity with other ingest modules but unused
#' for HSÍ — the current-season page renders both results and schedule tables.
#' @keywords internal
#' @noRd
hsi_url <- function(sex, division, season = NULL,
                    kind = c("results", "schedule")) {
  kind <- match.arg(kind)
  if (!sex %in% names(HSI_URLS)) {
    stop("Unknown sex for HSI: ", sex, call. = FALSE)
  }
  if (!division %in% names(HSI_URLS[[sex]])) {
    stop("Unknown division for HSI sex=", sex, ": ", division, call. = FALSE)
  }
  HSI_URLS[[sex]][[division]]
}

#' Fetch an HSÍ page via headless Chromium.
#'
#' HSÍ's website is a client-side-rendered Drupal + JS app. Plain `curl` /
#' `rvest::read_html()` receives a shell HTML without match tables. Use
#' `rvest::read_html_live()` (which shells out to chromote) and capture the
#' fully rendered `outerHTML`. Returns an `xml_document` ready for
#' `rvest::html_table()`.
#'
#' Implementation notes:
#' - Waits up to ~5 attempts (2 s each) for tables to materialise.
#' - Extracts `outerHTML` as raw UTF-8 bytes so Icelandic characters survive
#'   downstream `janitor::make_clean_names()`; without explicit byte handling
#'   rvest's string conversion drops non-ASCII.
#' @keywords internal
#' @noRd
fetch_hsi_html <- function(url, min_tables = 2L, max_attempts = 5L,
                           wait_seconds = 2) {
  page <- rvest::read_html_live(url)
  on.exit(
    try(page$session$close(), silent = TRUE),
    add = TRUE
  )

  tables <- list()
  for (attempt in seq_len(max_attempts)) {
    Sys.sleep(wait_seconds)
    tables <- rvest::html_table(page)
    if (length(tables) >= min_tables) break
  }
  if (length(tables) < min_tables) {
    stop(
      "HSI page returned ", length(tables),
      " table(s), expected >= ", min_tables, ": ", url,
      call. = FALSE
    )
  }

  # Extract the rendered DOM as UTF-8 bytes, so Icelandic characters in the
  # column headers survive the read_html round-trip.
  outer <- page$session$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  Encoding(outer) <- "UTF-8"
  rvest::read_html(charToRaw(outer), encoding = "UTF-8")
}

#' Strip the "column name" prefix from each cell in a rvest-parsed HSÍ table.
#'
#' HSÍ's HTML pattern (responsive stacked rows on mobile) renders cells like
#' `<td data-label="Dagsetning">Fim. 12. mar. 26</td>`, which `html_table()`
#' flattens to "DagsetningFim. 12. mar. 26". The legacy scraper strips the
#' leading column-name prefix from every cell to recover the value.
#' @keywords internal
#' @noRd
hsi_strip_col_prefix <- function(df) {
  for (nm in names(df)) {
    df[[nm]] <- stringr::str_replace(df[[nm]], paste0("^", nm), "")
  }
  df
}

#' Parse an Icelandic HSÍ date string to a Date.
#'
#' HSÍ formats dates as "Fim. 12. mar. 26" — weekday abbr, day, month abbr,
#' 2-digit year. We strip the weekday (first 5 chars), remove dots, lowercase
#' and map the Icelandic month abbreviations to 2-digit numbers, then delegate
#' to `lubridate::dmy()`. Returns NA on unparseable strings.
#' @keywords internal
#' @noRd
hsi_parse_date <- function(x) {
  stripped <- stringr::str_sub(x, 6L, -1L) |>
    stringr::str_replace_all("\\.", "") |>
    tolower() |>
    stringr::str_trim()

  for (m in names(HSI_MONTH_MAP)) {
    stripped <- stringr::str_replace(stripped, paste0("\\b", m, "\\b"), HSI_MONTH_MAP[[m]])
  }
  suppressWarnings(lubridate::dmy(stripped))
}

#' Parse HSÍ results tables from a rendered page into canonical schema.
#'
#' Pure function — accepts an `xml_document`, returns a tibble matching
#' `schemas()$results`. Fixture-testable: no network.
#'
#' Looks for a results table with columns "Dagsetning", "Lið", "Niðurstöður".
#' Drops any rows without a parseable score (those belong to the schedule).
#'
#' @param html xml_document from `fetch_hsi_html()` or
#'   `rvest::read_html(<fixture>)`.
#' @param sport,country,sex,division,season Context columns applied to every
#'   row of the output.
#' @return Tibble with canonical results columns.
#' @keywords internal
#' @noRd
parse_hsi_results_page <- function(html, sport, country, sex, division, season) {
  tables <- rvest::html_table(html)

  empty <- hsi_empty_results()
  if (length(tables) < 2L) {
    return(empty)
  }

  # Locate the results table. HSI renders the league standings as table 1 and
  # previous matches (with a Niurstur column after clean_names+translit) as
  # table 2. We fall back to any table that contains the expected columns.
  tbl <- NULL
  for (i in seq_along(tables)) {
    candidate <- tables[[i]]
    cleaned_names <- janitor::make_clean_names(
      names(candidate),
      transliterations = "Latin-ASCII"
    )
    if (all(c("dagsetning", "lid", "nidurstodur") %in% cleaned_names)) {
      names(candidate) <- cleaned_names
      tbl <- candidate
      break
    }
  }
  if (is.null(tbl)) {
    return(empty)
  }

  # Strip "ColumnName" prefix that HSI renders in each cell value.
  tbl <- hsi_strip_col_prefix(tbl)

  # Project to the columns we need.
  tbl <- tbl[, c("dagsetning", "lid", "nidurstodur"), drop = FALSE]

  # Drop empty rows.
  tbl <- tbl[
    !is.na(tbl$lid) & nzchar(tbl$lid) &
      !is.na(tbl$nidurstodur) & nzchar(tbl$nidurstodur), ,
    drop = FALSE
  ]
  if (nrow(tbl) == 0L) {
    return(empty)
  }

  # Split "Home - Away" into team names.
  team_split <- stringr::str_split_fixed(tbl$lid, " - ", 2L)
  home_team <- stringr::str_trim(team_split[, 1L])
  away_team <- stringr::str_trim(team_split[, 2L])

  # Parse scores from "HH - AA".
  score_split <- stringr::str_match(
    tbl$nidurstodur,
    "\\s*(\\d+)\\s*-\\s*(\\d+)\\s*"
  )
  home_score <- suppressWarnings(as.integer(score_split[, 2L]))
  away_score <- suppressWarnings(as.integer(score_split[, 3L]))

  match_date <- hsi_parse_date(tbl$dagsetning)

  out <- tibble::tibble(
    sport = sport,
    country = country,
    sex = sex,
    season = as.integer(season),
    match_date = match_date,
    home_team = home_team,
    away_team = away_team,
    home_score = home_score,
    away_score = away_score,
    division = division,
    round = NA_integer_
  )

  # Drop rows where teams or date failed to parse, or no score present.
  keep <- !is.na(out$match_date) &
    !is.na(out$home_team) & nzchar(out$home_team) &
    !is.na(out$away_team) & nzchar(out$away_team) &
    !is.na(out$home_score) & !is.na(out$away_score)
  out[keep, , drop = FALSE]
}

#' Parse HSÍ schedule tables from a rendered page into canonical schema.
#'
#' Schedule tables on HSÍ appear after the results table (table index 3+ on
#' league pages, after a results table on tournament pages). Lacks score
#' columns — we pull date, round, and teams only.
#'
#' @inheritParams parse_hsi_results_page
#' @return Tibble with canonical schedules columns (no scores).
#' @keywords internal
#' @noRd
parse_hsi_schedule_page <- function(html, sport, country, sex, division, season) {
  tables <- rvest::html_table(html)
  empty <- hsi_empty_schedule()
  if (length(tables) < 2L) {
    return(empty)
  }

  # Find a table that has dagsetning + lid but NOT nidurstodur (schedule has
  # no score column), else fall back to the last table.
  sched_idx <- NULL
  for (i in seq_along(tables)) {
    cleaned_names <- janitor::make_clean_names(
      names(tables[[i]]),
      transliterations = "Latin-ASCII"
    )
    if (all(c("dagsetning", "lid") %in% cleaned_names) &&
      !("nidurstodur" %in% cleaned_names)) {
      sched_idx <- i
      break
    }
  }
  if (is.null(sched_idx)) {
    return(empty)
  }

  tbl <- tables[[sched_idx]]
  names(tbl) <- janitor::make_clean_names(
    names(tbl),
    transliterations = "Latin-ASCII"
  )
  tbl <- hsi_strip_col_prefix(tbl)

  # Keep rows with a non-empty lid (drop placeholder "-" rows).
  tbl <- tbl[
    !is.na(tbl$lid) & nzchar(tbl$lid) &
      !stringr::str_detect(tbl$lid, "^-$|rslit|\u00farslit"), ,
    drop = FALSE
  ]
  if (nrow(tbl) == 0L) {
    return(empty)
  }

  team_split <- stringr::str_split_fixed(tbl$lid, " - ", 2L)
  home_team <- stringr::str_trim(team_split[, 1L])
  away_team <- stringr::str_trim(team_split[, 2L])

  match_date <- hsi_parse_date(tbl$dagsetning)

  out <- tibble::tibble(
    sport = sport,
    country = country,
    sex = sex,
    season = as.integer(season),
    match_date = match_date,
    home_team = home_team,
    away_team = away_team,
    division = division,
    round = NA_integer_
  )

  keep <- !is.na(out$match_date) &
    !is.na(out$home_team) & nzchar(out$home_team) &
    !is.na(out$away_team) & nzchar(out$away_team)
  out[keep, , drop = FALSE]
}

#' Empty canonical-schema results tibble.
#' @keywords internal
#' @noRd
hsi_empty_results <- function() {
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
hsi_empty_schedule <- function() {
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

#' Divisions to ingest for a given sex.
#'
#' Male: Olísdeild + Grill 66 + Cup + Playoffs.
#' Female: Olísdeild + Grill 66 + Playoffs.
#' @keywords internal
#' @noRd
hsi_divisions_for_sex <- function(sex) {
  switch(sex,
    male = c("div1", "div2", "cup", "playoffs"),
    female = c("div1", "div2", "playoffs"),
    stop("Unknown sex for HSI: ", sex, call. = FALSE)
  )
}

#' Derive the current HSÍ season integer.
#'
#' Icelandic handball seasons run Sept-May and are labelled by the final
#' calendar year (e.g., 2025-26 → season 2026). Matches played in July or
#' later count toward the next season; earlier matches count toward the
#' current calendar year's season.
#' @keywords internal
#' @noRd
hsi_current_season <- function(today = Sys.Date()) {
  yr <- as.integer(format(today, "%Y"))
  mo <- as.integer(format(today, "%m"))
  if (mo >= 7L) yr + 1L else yr
}

#' Delay between consecutive HSÍ tournament-page fetches (seconds).
#'
#' Legacy `download_historical_data_*.R` used `Sys.sleep(15)` per page. Each
#' `fetch_hsi_html()` call already spends ~10s in chromote boot + wait loop,
#' so a 3-second inter-page sleep is adequate to avoid hammering HSÍ.
#' @keywords internal
#' @noRd
HSI_HISTORICAL_SLEEP_SECS <- 3

#' Fetch a single HSÍ page and parse into results rows.
#'
#' Factored out of `fetch_results_hsi` so the current-season league URL and
#' per-season historical tournament URLs share the same fetch + parse + error
#' handling path.
#' @keywords internal
#' @noRd
hsi_fetch_and_parse <- function(url, sex, div, division_label, season) {
  tryCatch(
    {
      html <- fetch_hsi_html(url)
      parse_hsi_results_page(
        html,
        sport = "handball",
        country = "iceland",
        sex = sex,
        division = division_label,
        season = season
      )
    },
    error = function(e) {
      cli::cli_warn(c(
        "HSI results fetch failed for {sex}/{div} season={season}",
        "i" = "{conditionMessage(e)}"
      ))
      NULL
    }
  )
}

#' Source-module entrypoint: results for a (league, sex).
#'
#' Iterates over all configured divisions for the requested sex, fetching and
#' parsing each HSÍ page, and combines into a single canonical tibble. Any
#' per-(division, season) failure is logged via `cli::cli_warn` and skipped so
#' one tournament-page glitch does not take down the league ingest.
#'
#' When `seasons` is `NULL`, hits only the current-season league pages in
#' `HSI_URLS`. When `seasons` is specified, iterates over the requested seasons:
#' - Current season → league URL from `HSI_URLS`
#' - Past seasons → tournament URLs from `HSI_HISTORICAL_IDS`, with a short
#'   inter-page sleep (`HSI_HISTORICAL_SLEEP_SECS`) to avoid overwhelming HSÍ.
#'
#' Historical tournament pages use the same table structure as current-season
#' pages per the legacy `read_page()` implementations, so the existing
#' `parse_hsi_results_page()` parser applies unchanged.
#'
#' @param league Unused (included for source-module signature parity); HSI
#'   URL wiring is fixed internally.
#' @param sex "male" or "female".
#' @param seasons Optional integer vector restricting seasons. When `NULL`,
#'   only the current season is fetched.
#' @keywords internal
#' @noRd
fetch_results_hsi <- function(league, sex, seasons = NULL) {
  current <- hsi_current_season()
  divisions <- hsi_divisions_for_sex(sex)

  # Decide which seasons to hit, per division. Current-season pages live in
  # HSI_URLS; historical pages live in HSI_HISTORICAL_IDS (no historical cup
  # data — see HSI_HISTORICAL_IDS docstring).
  requested <- if (is.null(seasons)) current else as.integer(seasons)

  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]

    for (season in requested) {
      url <- NULL
      if (season == current) {
        url <- HSI_URLS[[sex]][[div]]
      } else {
        url <- hsi_historical_url(sex, div, season)
      }
      if (is.null(url)) next # No mapping for this (sex, div, season).

      parsed <- hsi_fetch_and_parse(url, sex, div, division_label, season)
      if (!is.null(parsed)) frames[[length(frames) + 1L]] <- parsed

      # Sleep between historical tournament fetches to avoid hammering HSÍ.
      if (season != current) Sys.sleep(HSI_HISTORICAL_SLEEP_SECS)
    }
  }

  if (length(frames) == 0L) {
    return(hsi_empty_results())
  }
  dplyr::bind_rows(frames)
}

#' Source-module entrypoint: schedule (upcoming only) for a (league, sex).
#'
#' Iterates over all configured divisions, fetches each HSÍ page, and extracts
#' the schedule table (if present). HSÍ league pages render the schedule as a
#' 3rd table when there are unplayed matches; tournament pages render it as
#' the last table. `parse_hsi_schedule_page` picks the first table that has
#' `(dagsetning, lid)` without a score column.
#' @keywords internal
#' @noRd
fetch_schedule_hsi <- function(league, sex) {
  current <- hsi_current_season()
  divisions <- hsi_divisions_for_sex(sex)
  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]
    url <- HSI_URLS[[sex]][[div]]

    parsed <- tryCatch(
      {
        html <- fetch_hsi_html(url)
        parse_hsi_schedule_page(
          html,
          sport = "handball",
          country = "iceland",
          sex = sex,
          division = division_label,
          season = current
        )
      },
      error = function(e) {
        cli::cli_warn(c(
          "HSI schedule fetch failed for {sex}/{div}",
          "i" = "{conditionMessage(e)}"
        ))
        NULL
      }
    )
    if (is.null(parsed)) next
    frames[[length(frames) + 1L]] <- parsed
  }

  if (length(frames) == 0L) {
    return(hsi_empty_schedule())
  }
  # Keep only future fixtures (schedule tables can contain played rounds
  # with empty result cells for the away-win / bye cases).
  combined <- dplyr::bind_rows(frames)
  combined[combined$match_date >= Sys.Date(), , drop = FALSE]
}

register_ingest_source(
  "hsi_handball",
  list(
    fetch_results = fetch_results_hsi,
    fetch_schedule = fetch_schedule_hsi
  )
)

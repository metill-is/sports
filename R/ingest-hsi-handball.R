#' @include ingest.R
#' @include federation-seasons.R
NULL

#' HSÍ tournament ids, keyed by (sex, division, season).
#'
#' Layout: `HSI_TOURNAMENT_IDS[[sex]][[division]][["<season>"]]` -> integer
#' `mot_nr` used in `https://www.hsi.is/tournament/{mot_nr}`. Seasons are
#' labelled by the closing calendar year (2025 = Sept 2024 - May 2025), the
#' same convention as [hsi_current_season()].
#'
#' This replaces the previous split between `HSI_URLS` (dated league slugs for
#' whichever season happened to be current when a human last edited the file)
#' and `HSI_HISTORICAL_IDS` (tournament ids for everything older). That split
#' was the defect: the slug and the season stamp came from two independent
#' sources and drifted apart every July, and `https://www.hsi.is/olis-deild-
#' karla-2026-27` is a 404 because HSÍ now serves `/tournament/<id>` only.
#' One shape, one key, one lookup.
#'
#' Provenance for every seeded value lives in `config/federation-seasons.json`,
#' not in this comment -- see [read_federation_seasons()]. `cup` and `playoffs`
#' have no 2027 entry: HSÍ has not created those tournaments yet, and
#' [hsi_unresolved_seasons()] reports the gap rather than a guessed id filling
#' it. Historical 2021-2025 values came from the legacy
#' `_legacy/sports/handball/iceland/R/utils/{male,female}/download_historical_
#' data_div{1,2}.R`.
#' @keywords internal
#' @noRd
HSI_TOURNAMENT_IDS <- list(
  male = list(
    div1 = list(
      "2021" = 5260L, "2022" = 5640L, "2023" = 6149L,
      "2024" = 6983L, "2025" = 7641L, "2027" = 9142L
    ),
    div2 = list(
      "2021" = 5262L, "2022" = 5643L, "2023" = 6143L,
      "2024" = 6981L, "2025" = 7644L, "2027" = 9140L
    ),
    cup = list(
      "2026" = 8437L
    ),
    playoffs = list(
      "2026" = 8427L
    )
  ),
  female = list(
    div1 = list(
      "2021" = 5261L, "2022" = 5641L, "2023" = 6146L,
      "2024" = 6982L, "2025" = 7642L, "2027" = 9141L
    ),
    div2 = list(
      "2021" = 5263L, "2022" = 5642L, "2023" = 6148L,
      "2024" = 6980L, "2025" = 7643L, "2027" = 9143L
    ),
    playoffs = list(
      "2026" = 8430L
    )
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

#' Resolve an HSÍ tournament id for a (sex, division, season) triple.
#'
#' Registry first, `config/federation-seasons.json` cache second, `NULL` third.
#' `NULL` means do not fetch, which is the fail-safe direction -- an
#' unregistered triple must never fall back to "some other season's page".
#' Unknown sexes and divisions resolve to `NULL` rather than aborting, so a
#' config typo skips one cell instead of taking the whole ingest down.
#' @keywords internal
#' @noRd
hsi_tournament_id <- function(sex, division, season) {
  key <- as.character(as.integer(season))
  from_registry <- HSI_TOURNAMENT_IDS[[sex]][[division]][[key]]
  if (!is.null(from_registry)) {
    return(as.integer(from_registry))
  }
  federation_season_id("hsi", sex, division, season)
}

#' Build the HSÍ tournament URL for a (sex, division, season) triple.
#'
#' @return Character URL, or `NULL` when the triple has no resolvable id.
#' @keywords internal
#' @noRd
hsi_url <- function(sex, division, season) {
  id <- hsi_tournament_id(sex, division, season)
  if (is.null(id) || is.na(id)) {
    return(NULL)
  }
  sprintf("https://www.hsi.is/tournament/%d", id)
}

#' Reachable HSÍ (sex, division) pairs with no resolvable id for a season.
#'
#' Before the season-keyed registry, `playoffs` and `cup` were fetched
#' unconditionally from the current-season table, so they were scraped for
#' whatever season happened to be current -- which is why `data/facts/results`
#' holds PO rows for 2026 only. Under the registry they are ordinary
#' (sex, division, season) triples, and HSÍ does not create the úrslitakeppni
#' or the 2026-27 bikar tournaments until later in the season.
#'
#' Deferring them is correct; deferring them silently is not, because the
#' absence is indistinguishable from ordinary off-season emptiness. This
#' function names the gap so [fetch_results_hsi()] can warn and a later health
#' check can raise it.
#'
#' @param season Integer season to check.
#' @param sexes Sexes to check.
#' @return Tibble with columns `sex`, `division`, `season`.
#' @keywords internal
#' @noRd
hsi_unresolved_seasons <- function(season, sexes = c("male", "female")) {
  rows <- list()
  for (sex in sexes) {
    for (div in hsi_divisions_for_sex(sex)) {
      if (is.null(hsi_tournament_id(sex, div, season))) {
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

#' Poll a table-returning callback until the last table's row count is stable.
#'
#' HSÍ tournament pages render table shells (standings + results) quickly, but
#' the results `tbody` is populated asynchronously as chromote continues the
#' JS load. A naïve exit condition on `length(tables) >= min_tables` returns
#' the page mid-hydration with partial rows. This helper instead counts rows
#' in the LAST table (the results table) and exits once that count has not
#' changed across two consecutive polls — tbody growth is monotonic during
#' hydration, so stability implies fully-loaded.
#'
#' Pure function: network and Chromium live behind `get_tables`, which is why
#' the unit tests in `tests/testthat/test-poll-tables.R` can exercise the
#' decision logic with a simple stub.
#'
#' @param get_tables A zero-arg function returning a list of data frames. Each
#'   call triggers a fresh `html_table()` read from the live page.
#' @param min_tables Minimum table count before row-stability checks begin.
#' @param min_rows Row count of the last table must meet this threshold before
#'   stability counts as an acceptable exit. Default 1 rejects stable-empty.
#'   Set to 0 to accept genuinely empty tournaments and exit early.
#' @param max_attempts Hard cap on poll attempts.
#' @param wait_seconds Seconds to wait before each poll.
#' @param sleep_fn Sleep implementation (override for tests so they don't
#'   actually sleep).
#' @return The list of tables from the final poll.
#' @keywords internal
#' @noRd
poll_hsi_tables <- function(get_tables,
                            min_tables = 2L,
                            min_rows = 1L,
                            max_attempts = 12L,
                            wait_seconds = 5,
                            sleep_fn = Sys.sleep) {
  tables <- list()
  last_count <- -1L

  for (attempt in seq_len(max_attempts)) {
    sleep_fn(wait_seconds)
    tables <- get_tables()
    if (length(tables) < min_tables) next

    current_count <- nrow(tables[[length(tables)]])
    if (current_count >= min_rows && current_count == last_count) break
    last_count <- current_count
  }

  tables
}

#' Fetch an HSÍ page via headless Chromium.
#'
#' HSÍ's website is a client-side-rendered Drupal + JS app. Plain `curl` /
#' `rvest::read_html()` receives a shell HTML without match tables. Use
#' `rvest::read_html_live()` (which shells out to chromote) and capture the
#' fully rendered `outerHTML`. Returns an `xml_document` ready for
#' `rvest::html_table()`.
#'
#' Retry policy: polls up to `max_attempts` times with `wait_seconds` between
#' each poll (default 12 × 5 s = 60 s cap). Exits early once the LAST table's
#' row count is stable across two consecutive polls — see [poll_hsi_tables()]
#' for why element-count polling is insufficient.
#'
#' Icelandic-character preservation: extracts `outerHTML` as raw UTF-8 bytes
#' so accented column headers survive downstream `janitor::make_clean_names()`;
#' without explicit byte handling rvest's string conversion drops non-ASCII.
#' @keywords internal
#' @noRd
fetch_hsi_html <- function(url, min_tables = 2L, min_rows = 1L,
                           max_attempts = 12L, wait_seconds = 5) {
  page <- rvest::read_html_live(url)
  on.exit(
    try(page$session$close(), silent = TRUE),
    add = TRUE
  )

  tables <- poll_hsi_tables(
    get_tables = function() rvest::html_table(page),
    min_tables = min_tables,
    min_rows = min_rows,
    max_attempts = max_attempts,
    wait_seconds = wait_seconds
  )

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

#' Fetch a single HSÍ tournament page and parse into results rows.
#'
#' The ordinary-failure handler degrades a fetch error to a warning so one bad
#' tournament page cannot take down a league's ingest. A season-stamp mismatch
#' is deliberately NOT an ordinary failure: it means the id we hold is wrong,
#' and warning about it would write the wrong rows anyway.
#'
#' The guard therefore runs OUTSIDE the `tryCatch`, not as a
#' `sports_season_stamp_error =` handler that re-raises. `tryCatch()` nests its
#' handlers -- the last one given is established outermost -- so a `stop(e)`
#' from inside a specific handler is caught by that same call's `error =`
#' handler and silently degraded to the warning it was trying to escape.
#' Verified, not assumed: the re-raise form returns NULL with a warning.
#' @keywords internal
#' @noRd
hsi_fetch_and_parse <- function(url, sex, div, division_label, season) {
  rows <- tryCatch(
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
  if (is.null(rows)) {
    return(NULL)
  }
  .assert_season_stamp(
    rows, season,
    source = sprintf("hsi %s/%s results (%s)", sex, div, url)
  )
  rows
}

#' Source-module entrypoint: results for a (league, sex).
#'
#' Iterates the configured divisions for the requested sex and, for each
#' requested season, resolves a `/tournament/<id>` URL via [hsi_url()]. There is
#' no current-vs-historical branch any more: every season is the same shape of
#' lookup against the same registry, so "this season" stops being a special
#' case that a human has to re-point every July.
#'
#' An unresolvable (sex, division, season) is skipped with a warning naming it,
#' never silently -- see [hsi_unresolved_seasons()].
#'
#' @param league Unused (source-module signature parity).
#' @param sex "male" or "female".
#' @param seasons Optional integer vector. `NULL` means the current season only.
#' @param sleep_fn Sleep implementation. Injected rather than mocked, because
#'   `local_mocked_bindings()` cannot bind base functions -- the same seam
#'   [poll_hsi_tables()] already uses.
#' @keywords internal
#' @noRd
fetch_results_hsi <- function(league, sex, seasons = NULL,
                              sleep_fn = Sys.sleep) {
  requested <- if (is.null(seasons)) {
    hsi_current_season()
  } else {
    as.integer(seasons)
  }
  divisions <- hsi_divisions_for_sex(sex)
  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]

    for (season in requested) {
      url <- hsi_url(sex, div, season)
      if (is.null(url)) {
        cli::cli_warn(
          "HSI: no tournament id for {sex}/{div} season={season} -- skipped."
        )
        next
      }

      parsed <- hsi_fetch_and_parse(url, sex, div, division_label, season)
      if (!is.null(parsed)) frames[[length(frames) + 1L]] <- parsed

      sleep_fn(HSI_HISTORICAL_SLEEP_SECS)
    }
  }

  if (length(frames) == 0L) {
    return(hsi_empty_results())
  }
  dplyr::bind_rows(frames)
}

#' Source-module entrypoint: schedule (upcoming only) for a (league, sex).
#'
#' Same registry lookup as [fetch_results_hsi()], for the current season only,
#' with the same season-stamp guard: a schedule scraped off a stale page is as
#' wrong as results scraped off one, and schedules feed the fixture window that
#' drives odds and decide. The guard sits outside the fetch `tryCatch` for the
#' reason documented on [hsi_fetch_and_parse()].
#'
#' @param league Unused (source-module signature parity).
#' @param sex "male" or "female".
#' @keywords internal
#' @noRd
fetch_schedule_hsi <- function(league, sex) {
  current <- hsi_current_season()
  divisions <- hsi_divisions_for_sex(sex)
  frames <- list()

  for (div in divisions) {
    division_label <- HSI_DIVISION_LABELS[[div]]
    url <- hsi_url(sex, div, current)
    if (is.null(url)) {
      cli::cli_warn(
        "HSI: no tournament id for {sex}/{div} season={current} -- schedule skipped."
      )
      next
    }

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
    # Outside the tryCatch on purpose -- see hsi_fetch_and_parse().
    .assert_season_stamp(
      parsed, current,
      source = sprintf("hsi %s/%s schedule (%s)", sex, div, url)
    )
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

#' Title patterns mapping an HSÍ tournament title to (sex, division).
#'
#' Ordered: the first match wins. The Grill 66 and Olís patterns are disjoint,
#' but the ordering is kept explicit so adding a pattern later cannot silently
#' shadow one. Icelandic characters are written as `\uXXXX` escapes, the same
#' convention `HSI_MONTH_MAP` uses, so the source stays ASCII.
#' @keywords internal
#' @noRd
HSI_TITLE_PATTERNS <- tibble::tibble(
  pattern = c(
    "^Ol\u00eds\\s*deild\\s+karla",
    "^Grill\\s*66\\s*deild\\s+karla",
    "^Ol\u00eds\\s*deild\\s+kvenna",
    "^Grill\\s*66\\s*deild\\s+kvenna",
    "^\u00darslitakeppni\\s+karla",
    "^\u00darslitakeppni\\s+kvenna",
    "^(Coca[- ]?Cola\\s+)?[Bb]ikar\\s*(keppni)?\\s+karla"
  ),
  sex = c("male", "male", "female", "female", "male", "female", "male"),
  division = c("div1", "div2", "div1", "div2", "playoffs", "playoffs", "cup")
)

#' Map tournament titles to (sex, division); NA where no pattern matches.
#'
#' An unmappable title is dropped by the caller rather than guessed at -- the
#' whole point of discovery is that it is more trustworthy than a guess.
#' @keywords internal
#' @noRd
.hsi_match_title <- function(titles) {
  sex <- rep(NA_character_, length(titles))
  division <- rep(NA_character_, length(titles))
  for (i in seq_len(nrow(HSI_TITLE_PATTERNS))) {
    hit <- is.na(sex) &
      stringr::str_detect(titles, HSI_TITLE_PATTERNS$pattern[[i]])
    hit[is.na(hit)] <- FALSE
    sex[hit] <- HSI_TITLE_PATTERNS$sex[[i]]
    division[hit] <- HSI_TITLE_PATTERNS$division[[i]]
  }
  tibble::tibble(title = titles, sex = sex, division = division)
}

#' The page title of a rendered HSÍ page, without the " | HSÍ" suffix.
#' @keywords internal
#' @noRd
hsi_page_title <- function(html) {
  raw <- rvest::html_element(html, "title") |> rvest::html_text2()
  stringr::str_trim(stringr::str_replace(raw, "\\s*\\|\\s*HS\u00cd\\s*$", ""))
}

#' Parse `/tournament/<id>` links and their titles out of a rendered page.
#'
#' Pure function -- network lives in [hsi_discover_tournaments()], so the title
#' mapping stays fixture-testable.
#'
#' @param html xml_document.
#' @return Tibble with `id` (integer) and `title` (character), deduplicated.
#' @keywords internal
#' @noRd
parse_hsi_tournament_index <- function(html) {
  links <- rvest::html_elements(html, "a[href*='/tournament/']")
  if (length(links) == 0L) {
    return(tibble::tibble(id = integer(), title = character()))
  }
  hrefs <- rvest::html_attr(links, "href")
  ids <- suppressWarnings(as.integer(
    stringr::str_match(hrefs, "/tournament/(\\d+)")[, 2L]
  ))
  titles <- stringr::str_trim(rvest::html_text2(links))

  out <- tibble::tibble(id = ids, title = titles)
  out <- out[!is.na(out$id) & nzchar(out$title), , drop = FALSE]
  dplyr::distinct(out, .data$id, .keep_all = TRUE)
}

#' Discover HSÍ tournament ids for a season off the live site.
#'
#' This is what stops the registry being the thing that goes stale: it reads
#' the ids HSÍ is actually serving today, rather than the ones a human typed
#' last September. Merge the result with [refresh_federation_seasons()]; the
#' registry then becomes a verified cache rather than the sole source.
#'
#' Two stages, because the index alone is not enough. Measured on the live
#' page (2026-09-03) the nav's link text is sex-free -- "Olísdeildin",
#' "Grill 66 deildin", "Powerade bikarinn", each appearing twice, once per sex
#' -- so no pattern over the index can say which row is the women's. Each
#' unmappable id is therefore resolved from its own tournament page's
#' `<title>`, which does carry it ("Olís deild karla 2025-26"). An id that
#' stays unmappable is dropped, never guessed.
#'
#' HSÍ's site is client-side rendered, so this goes through the existing
#' chromote-backed [fetch_hsi_html()] -- a plain `httr` GET returns a shell.
#'
#' @param index_url Tournament index / navigation page.
#' @param season Season to attribute the discovered ids to. The caller is
#'   asserting "this index is showing season N"; `.assert_season_stamp()` is
#'   what checks that assertion the first time each id is fetched.
#' @param sleep_fn Sleep implementation between per-tournament fetches; see
#'   [fetch_results_hsi()].
#' @return Tibble shaped like the provenance cache.
#' @importFrom rlang .data
#' @keywords internal
#' @noRd
hsi_discover_tournaments <- function(index_url = "https://www.hsi.is/mot",
                                     season = hsi_current_season(),
                                     sleep_fn = Sys.sleep) {
  html <- fetch_hsi_html(index_url, min_tables = 0L, min_rows = 0L)
  idx <- parse_hsi_tournament_index(html)
  if (nrow(idx) == 0L) {
    return(.federation_seasons_empty())
  }
  mapped <- .hsi_match_title(idx$title)
  titles <- idx$title

  for (i in which(is.na(mapped$sex))) {
    url <- sprintf("https://www.hsi.is/tournament/%d", idx$id[[i]])
    page_title <- tryCatch(
      hsi_page_title(fetch_hsi_html(url, min_tables = 0L, min_rows = 0L)),
      error = function(e) {
        cli::cli_warn(c(
          "HSI discovery could not read the title of {url}",
          "i" = "{conditionMessage(e)}"
        ))
        NA_character_
      }
    )
    if (!is.na(page_title)) {
      hit <- .hsi_match_title(page_title)
      mapped$sex[[i]] <- hit$sex[[1L]]
      mapped$division[[i]] <- hit$division[[1L]]
      titles[[i]] <- page_title
    }
    sleep_fn(HSI_HISTORICAL_SLEEP_SECS)
  }

  out <- tibble::tibble(
    federation = "hsi",
    sex = mapped$sex,
    division = mapped$division,
    season = as.integer(season),
    id = idx$id,
    title = titles,
    source = "live",
    discovered_at = format(Sys.Date()),
    verified = TRUE,
    note = NA_character_
  )
  out[!is.na(out$sex) & !is.na(out$division), , drop = FALSE]
}

register_ingest_source(
  "hsi_handball",
  list(
    fetch_results = fetch_results_hsi,
    fetch_schedule = fetch_schedule_hsi
  )
)

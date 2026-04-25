#' @include storage.R config.R
NULL

# --- CSS selectors (matches legacy _legacy/lengjan-odds/R/scrape.R) ----------
# Hashed by Lengjan's CSS Modules build; a deploy may invalidate them. The
# `aria-controls` IDs (row-OU_FT, row-HC_FT) on the detail page are more stable.
.lengjan_selectors <- list(
  match_container = "div.lj1n6v0",
  match_link      = "a.lj1n6v1",
  teams           = ".lj1n6v9",
  odds_list       = "ol.lj1n6vd",
  odds_button     = "button.uazl1c1",
  odds_value      = ".h7cub57 p",
  market_section  = "section.zh0raz0"
)

# Rate-limit constants for the chromote-driven detail-page loop. Mirrors the
# legacy `rate_limit_config$page_delay_*` values from
# _legacy/lengjan-odds/R/scrape.R.
.LENGJAN_DETAIL_DELAY_S <- c(min = 2, max = 5)

#' Parse a Lengjan competition page (1x2 odds + match links).
#'
#' Long-form output: one row per outcome (home/draw/away). `line` is NA for
#' the moneyline market. Date is parsed from Icelandic month abbreviations via
#' [parse_lengjan_dates()].
#'
#' @param html Parsed HTML (from `rvest::read_html()` or `read_html_live()`).
#' @param sport,country Strings written into the `sport` / `country` columns.
#' @return Tibble with columns
#'   `sport, country, match_date, home_team, away_team, market, outcome, line, odds`.
#' @keywords internal
#' @noRd
parse_competition_page <- function(html, sport, country) {
  matches <- rvest::html_elements(html, .lengjan_selectors$match_container)
  if (length(matches) == 0L) {
    return(tibble::tibble(
      sport = character(0), country = character(0),
      match_date = as.Date(character(0)),
      home_team = character(0), away_team = character(0),
      market = character(0), outcome = character(0),
      line = numeric(0), odds = numeric(0)
    ))
  }

  date_strs <- vapply(matches, function(m) {
    link <- rvest::html_element(m, .lengjan_selectors$match_link)
    if (inherits(link, "xml_missing")) {
      return(NA_character_)
    }
    date_p <- rvest::html_element(link, "p")
    if (inherits(date_p, "xml_missing")) NA_character_ else rvest::html_text2(date_p)
  }, character(1))
  match_dates <- parse_lengjan_dates(date_strs)

  rows <- purrr::map2(matches, match_dates, function(m, match_date) {
    teams_el <- rvest::html_elements(m, .lengjan_selectors$teams)
    if (length(teams_el) == 0L) {
      return(NULL)
    }
    teams <- rvest::html_text2(rvest::html_elements(teams_el, "span"))
    # Strip the separator span between the two teams. Lengjan flipped this
    # from "vs" to "-" sometime around 2026-04-25; accept either to stay
    # forward-compatible if they switch back.
    teams <- teams[!teams %in% c("vs", "-") & nzchar(teams)]
    if (length(teams) < 2L) {
      return(NULL)
    }

    odds_btns <- rvest::html_elements(m, .lengjan_selectors$odds_button)
    odds_vals <- suppressWarnings(as.numeric(rvest::html_text2(
      rvest::html_elements(odds_btns, .lengjan_selectors$odds_value)
    )))
    if (length(odds_vals) < 3L || any(is.na(odds_vals[1:3]))) {
      return(NULL)
    }

    tibble::tibble(
      sport = sport, country = country,
      match_date = match_date,
      home_team = teams[[1]], away_team = teams[[2]],
      market = "moneyline",
      outcome = c("home", "draw", "away"),
      line = NA_real_,
      odds = odds_vals[1:3]
    )
  })

  dplyr::bind_rows(rows)
}

#' Parse a Lengjan match-detail page (handicap + totals).
#'
#' Identifies sections by the `aria-controls` attribute on each section's
#' expand button: `row-HC_FT` is the handicap (spread) market, `row-OU_FT`
#' is the totals market. Yields long-form rows for each line/outcome pair.
#'
#' @param html Parsed HTML for the detail page.
#' @param match_meta Named list with `sport, country, home_team, away_team,
#'   match_date` to attach to the output.
#' @return Tibble with the same columns as [parse_competition_page()];
#'   empty tibble if no recognised sections are present.
#' @keywords internal
#' @noRd
parse_match_detail <- function(html, match_meta) {
  sections <- rvest::html_elements(html, .lengjan_selectors$market_section)

  out <- list()
  for (sec in sections) {
    btn <- rvest::html_element(sec, "button[aria-controls]")
    if (inherits(btn, "xml_missing")) next
    aria <- rvest::html_attr(btn, "aria-controls")
    if (is.na(aria)) next

    if (identical(aria, "row-HC_FT")) {
      market <- "spread"
      outcomes <- c("home", "away")
    } else if (identical(aria, "row-OU_FT")) {
      market <- "total"
      outcomes <- c("over", "under")
    } else {
      next
    }

    rows <- rvest::html_elements(sec, "tr")
    for (r in rows) {
      th_el <- rvest::html_element(r, "th")
      if (inherits(th_el, "xml_missing")) next
      line_str <- rvest::html_text2(th_el)
      line_val <- suppressWarnings(as.numeric(line_str))
      if (is.na(line_val)) next

      btns <- rvest::html_elements(r, .lengjan_selectors$odds_button)
      odds_vals <- suppressWarnings(as.numeric(rvest::html_text2(
        rvest::html_elements(btns, .lengjan_selectors$odds_value)
      )))
      if (length(odds_vals) < 2L) next

      out[[length(out) + 1L]] <- tibble::tibble(
        sport = match_meta$sport, country = match_meta$country,
        match_date = match_meta$match_date,
        home_team = match_meta$home_team, away_team = match_meta$away_team,
        market = market,
        outcome = outcomes,
        line = line_val,
        odds = odds_vals[1:2]
      )
    }
  }

  dplyr::bind_rows(out)
}

# --- Icelandic date parsing (verbatim from _legacy/lengjan-odds/R/parse.R) ---
# Lengjan format: "3. mar 19:45" (day. month-abbrev hour:min); year inferred.
# Non-ASCII month keys and the regex char-class are built via
# stringi::stri_unescape_unicode() and explicitly tagged UTF-8 to guarantee
# the right bytes under a C locale (mirrors R/ingest-ksi-football.R::
# ksi_build_month_map). Naive `\uXXXX` literals in source can get mangled
# when R parses the file in C locale.
build_lengjan_month_map <- function() {
  raw_keys <- c(
    "jan", "feb", "mar", "apr",
    "ma\\u00ed", "j\\u00fan", "j\\u00fal", "\\u00e1g\\u00fa",
    "sep", "okt", "n\\u00f3v", "des"
  )
  keys <- stringi::stri_unescape_unicode(raw_keys)
  Encoding(keys) <- "UTF-8"
  values <- sprintf("%02d", 1:12)
  stats::setNames(values, keys)
}
icelandic_months <- build_lengjan_month_map()

# Pre-built date-extract regex. Built once via paste0 so that the ASCII
# regex metacharacters survive R parsing intact while the Icelandic char
# class is supplied as a UTF-8 fragment from stri_unescape_unicode().
build_lengjan_date_pattern <- function() {
  ice_chars <- stringi::stri_unescape_unicode(
    "\\u00e1\\u00e9\\u00ed\\u00f3\\u00fa\\u00f0\\u00fe\\u00e6"
  )
  Encoding(ice_chars) <- "UTF-8"
  pat <- paste0("\\d+\\.\\s*[a-z", ice_chars, "]+")
  Encoding(pat) <- "UTF-8"
  pat
}
LENGJAN_DATE_PATTERN <- build_lengjan_date_pattern()

#' Parse Lengjan date strings into Date objects.
#'
#' Accepts strings like `"3. mar 19:45"` or `"4. feb 18:30"`. Extracts the
#' "day. month" portion, maps Icelandic abbreviations to two-digit numbers,
#' and infers the year dynamically. Dates more than 180 days in the future
#' are shifted back one year (handles January scrapes that see "des").
#'
#' @param date_strings Character vector.
#' @return Date vector (NA for unparseable entries).
#' @keywords internal
#' @noRd
parse_lengjan_dates <- function(date_strings) {
  # Tag inputs UTF-8 so regex matching against UTF-8 month keys does not fall
  # back to byte-exact comparison with mojibake strings (mirrors KSI fix).
  Encoding(date_strings) <- "UTF-8"

  cleaned <- stringr::str_extract(date_strings, LENGJAN_DATE_PATTERN)

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  for (abbr in names(icelandic_months)) {
    cleaned <- stringr::str_replace(cleaned, abbr, icelandic_months[[abbr]])
  }
  with_year <- stringr::str_c(cleaned, " ", current_year)
  parsed <- lubridate::dmy(with_year, quiet = TRUE)

  future_cutoff <- Sys.Date() + 180
  past_shift <- !is.na(parsed) & parsed > future_cutoff
  if (any(past_shift)) {
    with_year_prev <- stringr::str_c(cleaned[past_shift], " ", current_year - 1L)
    parsed[past_shift] <- lubridate::dmy(with_year_prev, quiet = TRUE)
  }
  parsed
}

#' Scrape live Lengjan odds for active leagues and write to facts/odds Parquet.
#'
#' Two-stage: (1) competition page yields 1x2 + match URLs; (2) match-detail
#' pages yield handicap + totals. All odds rows go into one long-form tibble
#' and a single `write_table("odds", ...)` append.
#'
#' @param leagues Filtered list (output of `filter_leagues(active_only = TRUE,
#'   has_lengjan = TRUE)`).
#' @param scraped_at Single timestamp for the entire run.
#' @param root Storage root.
#' @param chromote_session Existing session; NULL means create + close one.
#' @return Number of rows written (invisible).
#' @export
ingest_lengjan_odds <- function(leagues, scraped_at = Sys.time(),
                                root = here::here("data"),
                                chromote_session = NULL) {
  stopifnot(is.list(leagues), length(leagues) > 0L)

  owns_session <- is.null(chromote_session)
  if (owns_session) {
    chromote_session <- chromote::ChromoteSession$new()
  }
  on.exit(if (owns_session) chromote_session$close(), add = TRUE)

  rows <- list()
  for (key in names(leagues)) {
    league <- leagues[[key]]
    comps <- league$lengjan$competitions %||% list()
    if (length(comps) == 0L) next

    for (comp in comps) {
      cli::cli_alert_info("Scraping {key}: {comp$name} (id={comp$id})")
      url <- sprintf(
        "https://games.lotto.is/getraunaleikir/lengjan?sport=%d&country=%s&competition=%s",
        .lengjan_sport_id(league$sport),
        .lengjan_country_code(league$country),
        comp$id
      )
      html <- .lengjan_fetch(chromote_session, url)
      comp_rows <- parse_competition_page(
        html,
        sport = league$sport, country = league$country
      )

      match_links <- rvest::html_attr(
        rvest::html_elements(html, .lengjan_selectors$match_link), "href"
      )
      n_matches <- nrow(comp_rows) %/% 3L
      for (i in seq_along(match_links)) {
        if (i > n_matches) break
        match_meta <- list(
          sport = league$sport, country = league$country,
          home_team = comp_rows$home_team[1L + (i - 1L) * 3L],
          away_team = comp_rows$away_team[1L + (i - 1L) * 3L],
          match_date = comp_rows$match_date[1L + (i - 1L) * 3L]
        )
        # Append marketTab=allMarkets so the detail page renders handicap +
        # totals tables immediately. Without it the legacy code clicked a
        # "show more" button (selectors$more_markets); the rendered-page query
        # arg avoids that interaction. See _legacy/lengjan-odds/R/scrape.R.
        match_url <- sprintf(
          "https://games.lotto.is%s%smarketTab=allMarkets",
          match_links[[i]],
          if (grepl("\\?", match_links[[i]])) "&" else "?"
        )
        detail_html <- tryCatch(
          .lengjan_fetch(chromote_session, match_url),
          error = function(e) {
            cli::cli_alert_warning("Match detail fetch failed: {conditionMessage(e)}")
            NULL
          }
        )
        if (!is.null(detail_html)) {
          comp_rows <- dplyr::bind_rows(
            comp_rows,
            parse_match_detail(detail_html, match_meta)
          )
        }
        Sys.sleep(stats::runif(
          1, .LENGJAN_DETAIL_DELAY_S[["min"]], .LENGJAN_DETAIL_DELAY_S[["max"]]
        ))
      }

      rows[[key]] <- dplyr::bind_rows(rows[[key]], comp_rows)
    }
  }

  all_rows <- dplyr::bind_rows(rows) |>
    dplyr::mutate(scraped_at = !!scraped_at) |>
    dplyr::select(
      "sport", "country", "scraped_at", "match_date",
      "home_team", "away_team", "market", "outcome", "line", "odds"
    )

  if (nrow(all_rows) == 0L) {
    cli::cli_alert_info("No odds rows scraped")
    return(invisible(0L))
  }

  write_table(all_rows, "odds", root = root)
  cli::cli_alert_success("Wrote {nrow(all_rows)} odds rows to facts/odds/")
  invisible(nrow(all_rows))
}

.lengjan_sport_id <- function(sport) {
  c(football = 1L, basketball = 2L, handball = 6L)[[sport]]
}

.lengjan_country_code <- function(country) {
  # Canonical (lowercase) -> Lengjan country query parameter.
  # Currently only `iceland` is wired through `config/leagues.yml::active=true`;
  # the other codes are kept as a forward-compatible lookup for when
  # non-Icelandic leagues are reactivated in later plans. Removing them would
  # force re-derivation from `_legacy/lengjan-odds/config/competitions.yml`,
  # so keeping them here is intentional and not dead code.
  codes <- c(
    iceland = "IS", england = "ENG", italy = "IT", spain = "ES",
    denmark = "DK", germany = "DE", sweden = "SE", norway = "NO",
    france = "FR", international = "INT"
  )
  if (!country %in% names(codes)) {
    stop(
      "Unknown Lengjan country code for '", country,
      "'. Add it to .lengjan_country_code() in R/ingest-lengjan-odds.R."
    )
  }
  codes[[country]]
}

.lengjan_fetch <- function(session, url) {
  session$Page$navigate(url)
  session$Page$loadEventFired(timeout_ = 30000)
  Sys.sleep(2)
  rvest::read_html(session$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

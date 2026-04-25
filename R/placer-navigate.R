# Lengjan Navigation
# Functions for navigating to competitions, matches, and finding match IDs.

# ── Constants ─────────────────────────────────────────────────────────────────

.lengjan_base_url <- "https://games.lotto.is/getraunaleikir/lengjan"

# ── URL builders ──────────────────────────────────────────────────────────────

#' Build URL for a competition listing page
#'
#' @param sport_id Lengjan sport ID (integer)
#' @param competition_id Lengjan competition ID (integer)
#' @return A length-1 character URL
#' @export
competition_url <- function(sport_id, competition_id) {
  paste0(.lengjan_base_url, "?sport=", sport_id, "&competition=", competition_id)
}

#' Build URL for a match detail page
#'
#' @param match_id Lengjan match ID (character or integer)
#' @param sport_id Lengjan sport ID (integer)
#' @return A length-1 character URL
#' @export
match_url <- function(match_id, sport_id) {
  paste0(.lengjan_base_url, "/leikur?id=", match_id, "&sport=", sport_id)
}

# ── Match discovery ───────────────────────────────────────────────────────────

#' Extract match listings from a competition page
#'
#' Navigates to the competition page and extracts all visible matches
#' with their team names, 1x2 odds, and detail page URLs (which contain
#' the match_id needed for handicap/totals bets).
#'
#' @param session A ChromoteSession (authenticated)
#' @param sport_id Lengjan sport ID
#' @param competition_id Lengjan competition ID
#' @param rate_delay Seconds to wait between page loads (default 2-4, randomised)
#' @return A tibble with columns: home, away, match_id, detail_url,
#'   odds_home, odds_draw, odds_away
#' @export
extract_matches <- function(session, sport_id, competition_id,
                            rate_delay = c(2, 4)) {
  url <- competition_url(sport_id, competition_id)
  cli::cli_alert_info("Loading competition page: {url}")

  session$Page$navigate(url)
  Sys.sleep(sample_delay(rate_delay))

  # Click "Sjá allt" to expand truncated match list
  expand_all(session)

  match_data <- session$Runtime$evaluate(
    expression = match_extraction_js(),
    returnByValue = TRUE
  )

  if (is.null(match_data$result$value)) {
    cli::cli_alert_warning("No matches found on competition page.")
    return(tibble::tibble(
      home = character(),
      away = character(),
      match_id = character(),
      date = character(),
      odds_home = numeric(),
      odds_draw = numeric(),
      odds_away = numeric()
    ))
  }

  raw <- match_data$result$value
  if (is.character(raw)) {
    raw <- jsonlite::fromJSON(raw)
  }

  tibble::as_tibble(raw)
}

#' JavaScript to extract match data from the competition listing page
#'
#' DOM structure per match:
#'   div.lj1n6v0 (match container)
#'     a.lj1n6v1 (main link, text = "date...LeagueHome - Away")
#'     div.lj1n6vb (odds area)
#'       ol.lj1n6vd (odds list with buttons)
#'       a.lj1n6ve ("+N more markets" link)
#'
#' Odds buttons have aria-label: "1, stuðull: 2.28", "X, stuðull: 3.14", etc.
#' Team names are at the end of the main link text: "...Home - Away"
#'
#' @noRd
match_extraction_js <- function() {
  "
  (() => {
    const matches = [];
    const seen = new Set();

    // Find all match links (main link only, not the '+N' link)
    const links = document.querySelectorAll('a[href*=\"/leikur?id=\"]');

    links.forEach(link => {
      const href = link.getAttribute('href');
      const idMatch = href.match(/id=(\\d+)/);
      if (!idMatch) return;

      const matchId = idMatch[1];
      if (seen.has(matchId)) return;
      seen.add(matchId);

      // The match container is the parent div with both the link and odds
      const container = link.closest('div') ?
        link.closest('div').closest('div') || link.closest('div') :
        link.parentElement;

      // Team names are in the last child element (DIV) of the main link
      // Structure: a > [P date, P time, SPAN live, P league, DIV teams]
      let home = '', away = '';
      const lastChild = link.lastElementChild;
      if (lastChild) {
        const teamsText = lastChild.textContent.trim();
        const dashIdx = teamsText.indexOf(' - ');
        if (dashIdx > 0) {
          home = teamsText.substring(0, dashIdx).trim();
          away = teamsText.substring(dashIdx + 3).trim();
        }
      }

      // Find odds from aria-labels on buttons in the container
      let oddsHome = null, oddsDraw = null, oddsAway = null;
      const buttons = container ? container.querySelectorAll('button') : [];
      buttons.forEach(btn => {
        const label = btn.getAttribute('aria-label') || '';
        const oddsMatch = label.match(/stu\\u00f0ull:\\s*(\\d+\\.\\d+)/);
        if (!oddsMatch) return;
        const odds = parseFloat(oddsMatch[1]);
        if (label.startsWith('1,')) oddsHome = odds;
        else if (label.startsWith('X,')) oddsDraw = odds;
        else if (label.startsWith('2,')) oddsAway = odds;
      });

      matches.push({
        match_id: matchId,
        home: home,
        away: away,
        odds_home: oddsHome,
        odds_draw: oddsDraw,
        odds_away: oddsAway,
        detail_url: href
      });
    });

    return JSON.stringify(matches);
  })()
  "
}

# ── Post-extraction lookup ────────────────────────────────────────────────────

#' Find a match in the extracted tibble by team names
#'
#' Pure tibble lookup against the data returned by \code{extract_matches()}.
#' Matches exactly on both home and away team names (case-sensitive), honouring
#' home/away ordering so that e.g. (KR, FH) and (Valur, KR) resolve to
#' distinct rows.
#'
#' @param matches_df A tibble as returned by \code{extract_matches()}, with at
#'   least columns \code{home} and \code{away}.
#' @param home_lengjan The home team's Lengjan display name (character).
#' @param away_lengjan The away team's Lengjan display name (character).
#' @return A tibble — single-row on a hit, zero-row when no match found.
#' @export
find_match_in_extracted <- function(matches_df, home_lengjan, away_lengjan) {
  matches_df[matches_df$home == home_lengjan & matches_df$away == away_lengjan, ]
}

# ── Helpers ───────────────────────────────────────────────────────────────────

#' Click "Sjá allt" button to expand truncated match list (via JS)
#'
#' @param session A ChromoteSession (authenticated)
#' @return invisible(NULL)
#' @export
expand_all <- function(session) {
  js <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.trim().match(/Sjá allt/i)) {
          btn.click();
          return true;
        }
      }
      return false;
    })()
  "
  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  if (isTRUE(result$result$value)) {
    Sys.sleep(sample_delay(c(1.5, 3)))
    cli::cli_alert_info("Expanded match list (clicked 'Sjá allt').")
  }
  invisible(NULL)
}

#' Generate a random delay within a range
#'
#' Returns a single numeric value sampled uniformly from \code{range}.
#' Intended to be passed directly to \code{Sys.sleep()}.
#'
#' @param range Numeric vector of length 2: \code{c(min, max)} seconds.
#' @return A single numeric value.
#' @export
sample_delay <- function(range = c(2, 4)) {
  stats::runif(1, min = range[1], max = range[2])
}

# Lengjan Bet Placement
#
# Functions to place individual bets on Lengjan via chromote.
# Supports 1x2 (outcome), handicap (forgjof), and totals (yfir eda undir).
#
# Implements placement rules P3 (live Kelly recalculation) and P4 (+EV check).

# ── CDP click helper ──────────────────────────────────────────────────────────

#' Click at given x,y coordinates using CDP Input.dispatchMouseEvent
#'
#' Creates trusted events that React/Lengjan will recognise.
#' Dispatches mouseMoved -> mousePressed -> mouseReleased to mimic a real click.
#'
#' @param session ChromoteSession (authenticated)
#' @param x Numeric x coordinate
#' @param y Numeric y coordinate
#' @keywords internal
#' @noRd
cdp_click <- function(session, x, y) {
  x <- round(x)
  y <- round(y)
  session$Input$dispatchMouseEvent(
    type = "mouseMoved", x = x, y = y, button = "none"
  )
  session$Input$dispatchMouseEvent(
    type = "mousePressed", x = x, y = y, button = "left", clickCount = 1
  )
  session$Input$dispatchMouseEvent(
    type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1
  )
}

#' Select all text in a focused input and type a replacement value.
#'
#' Uses JS focus+select to select existing text, then CDP Input.insertText
#' to type the replacement (triggers React's synthetic input events).
#'
#' @param session ChromoteSession (authenticated)
#' @param text Character value to type
#' @keywords internal
#' @noRd
cdp_select_all_and_type <- function(session, text) {
  # Use JS to select all text in the focused/active element
  session$Runtime$evaluate(
    expression = "document.activeElement && document.activeElement.select()",
    returnByValue = TRUE
  )
  Sys.sleep(0.1)
  # Insert text via CDP — this replaces the selection and fires proper events
  session$Input$insertText(text = text)
}

# ── Kelly helpers (P3/P4) ─────────────────────────────────────────────────────

#' Proportionally adjust bet amount for odds drift (rule P3)
#'
#' When Lengjan's actual odds differ from the recommendation, scale the
#' original bet_amount by the ratio of simple Kelly at new vs old odds.
#' This preserves the joint Kelly optimiser's baseline.
#'
#' @param p Model probability
#' @param actual_odds Live odds from Lengjan
#' @param original_odds Recommended odds (from pipeline)
#' @param original_amount Recommended bet amount (from joint Kelly)
#' @return Rounded bet amount in kr
#' @export
recalculate_kelly_amount <- function(p, actual_odds, original_odds, original_amount) {
  old_raw <- (p * original_odds - 1) / (original_odds - 1)
  new_raw <- (p * actual_odds - 1) / (actual_odds - 1)
  new_raw <- max(0, new_raw)
  if (old_raw <= 0) {
    return(0L)
  }
  round(min(original_amount * (new_raw / old_raw), original_amount * 2), 0)
}

#' Check if a bet has positive expected value (rule P4)
#'
#' @param p Model probability
#' @param odds Decimal odds
#' @return TRUE if +EV (strictly positive expected value)
#' @export
is_positive_ev <- function(p, odds) {
  p * (odds - 1) - (1 - p) > 0
}

# ── DOM odds parser (Plan 5 Task 6 seam) ─────────────────────────────────────

#' Parse the actual odds from a Lengjan odds-button HTML fragment.
#'
#' Pure parser — no browser, no Chromote. Mirrors the regex chain embedded in
#' the JavaScript inside \code{click_market_button} / \code{click_table_button}
#' so that a Lengjan UI deploy that changes the odds-element structure
#' surfaces as a unit-test failure here, rather than silently producing wrong
#' odds at placement time. Used by both click helpers to verify the JS-reported
#' odds value against an independent R-side parse.
#'
#' Resolution order:
#' \enumerate{
#'   \item \code{aria-label} containing "stuðull: D.D" (market buttons,
#'     primary path).
#'   \item Plain text content matching \code{^[1X2]?(\\d+\\.\\d+)$} (market
#'     buttons, fallback for label-prefixed numeric text like "11.69").
#'   \item First decimal anywhere in plain text content (table buttons).
#' }
#'
#' @param html Character. The \code{outerHTML} of a single odds button element.
#' @return Numeric odds (e.g. 2.28), or \code{NA_real_} if no parse path matched.
#' @export
parse_actual_odds_from_dom <- function(html) {
  if (is.null(html) || length(html) == 0L) {
    return(NA_real_)
  }
  if (is.na(html) || !is.character(html) || !nzchar(html)) {
    return(NA_real_)
  }

  aria <- regmatches(
    html, regexec('aria-label="([^"]*)"', html)
  )[[1L]]
  if (length(aria) >= 2L) {
    m <- regmatches(
      aria[2L], regexec("stu\u00f0ull:\\s*(\\d+\\.\\d+)", aria[2L])
    )[[1L]]
    if (length(m) >= 2L) {
      return(as.numeric(m[2L]))
    }
  }

  text <- trimws(gsub("<[^>]+>", "", html))
  if (!nzchar(text)) {
    return(NA_real_)
  }

  cleaned <- sub("^[1X2]", "", text)
  m <- regmatches(cleaned, regexec("^(\\d+\\.\\d+)$", cleaned))[[1L]]
  if (length(m) >= 2L) {
    return(as.numeric(m[2L]))
  }

  m <- regmatches(text, regexec("(\\d+\\.\\d+)", text))[[1L]]
  if (length(m) >= 2L) {
    return(as.numeric(m[2L]))
  }

  NA_real_
}

#' Cross-check JS-reported odds against the R-side parser.
#'
#' Stops with a clear diagnostic if the parser disagrees with the JS or
#' fails to parse the chosen button HTML. Tolerant of older JS payloads that
#' don't carry the \code{html} field (warns but does not stop).
#'
#' @keywords internal
#' @noRd
verify_odds_match <- function(parsed, actual_odds) {
  html <- parsed$html
  if (is.null(html) || length(html) == 0L || !nzchar(html)) {
    cli::cli_alert_warning(
      "Click button JS did not return outerHTML; skipping R-side verification."
    )
    return(invisible(actual_odds))
  }
  verified <- parse_actual_odds_from_dom(html)
  if (is.na(verified)) {
    stop(sprintf(
      "parse_actual_odds_from_dom: could not parse odds from chosen button.\n  HTML head: %s",
      substr(html, 1L, 200L)
    ), call. = FALSE)
  }
  if (abs(verified - actual_odds) > 1e-9) {
    stop(sprintf(
      "Odds parser disagreement: JS reported %.4f, R parser found %.4f.\n  HTML head: %s",
      actual_odds, verified, substr(html, 1L, 200L)
    ), call. = FALSE)
  }
  invisible(actual_odds)
}

# ── Main entry point ──────────────────────────────────────────────────────────

#' Place a single bet on Lengjan
#'
#' Navigates to the match page, clicks the odds button, reads live odds,
#' validates via P3/P4 rules, then enters stake and confirms.
#'
#' @param session ChromoteSession (authenticated)
#' @param bet A single-row data.frame/tibble with recommendation fields
#' @param match_id Lengjan match ID (from extract_matches)
#' @param sport_id Lengjan sport ID
#' @param dry_run If TRUE, stop before clicking "Kaupa" (default FALSE)
#' @return List with status ("placed", "skipped", "dry_run", "error"),
#'   actual_odds (from Lengjan DOM), and amount (stake entered)
#' @export
place_bet <- function(session, bet, match_id, sport_id, dry_run = FALSE) {
  # Decide layer emits market names {moneyline, spread, total}; placer's legacy
  # internal names are {outcome, handicap, totals}. Translate at the boundary
  # so the placer body keeps its legacy vocabulary unchanged.
  market <- switch(bet$market,
    moneyline = "outcome",
    spread    = "handicap",
    total     = "totals",
    bet$market
  )
  result <- tryCatch(
    {
      if (market == "outcome") {
        place_outcome_bet(session, bet, match_id, sport_id, dry_run)
      } else if (market == "handicap") {
        place_handicap_bet(session, bet, match_id, sport_id, dry_run)
      } else if (market == "totals") {
        place_totals_bet(session, bet, match_id, sport_id, dry_run)
      } else {
        cli::cli_alert_warning("Unknown market: {market}")
        list(status = "skipped", reason = paste("Unknown market:", market))
      }
    },
    error = function(e) {
      cli::cli_alert_danger("Error placing bet: {e$message}")
      list(status = "error", reason = e$message)
    }
  )

  result
}

# ── Placement rules (P3/P4) ───────────────────────────────────────────────────

#' Validate live odds and recalculate stake if needed
#'
#' Called after clicking the odds button (which adds the selection to the
#' bet slip) but BEFORE entering stake and clicking "Kaupa".
#'
#' P4: Reject if no longer +EV at actual odds.
#' P3: Recalculate Kelly amount if odds drifted >1%.
#'
#' @param session ChromoteSession (authenticated)
#' @param bet Single-row bet tibble
#' @param actual_odds Numeric live odds read from DOM
#' @return On rejection: list(status, reason, actual_odds).
#'   On success: list(ok = TRUE, amount = <stake to enter>).
#' @keywords internal
#' @noRd
check_live_odds <- function(session, bet, actual_odds) {
  # P4: Must still be +EV at live odds
  if (!is_positive_ev(bet$probability, actual_odds)) {
    cli::cli_alert_warning(
      "No longer +EV at live odds {actual_odds} (p={bet$probability}) — skipping."
    )
    clear_bet_slip(session)
    return(list(status = "skipped", reason = "not_positive_ev", actual_odds = actual_odds))
  }

  # P3: Proportionally adjust stake if odds drifted >1%
  amount <- bet$bet_amount
  if (abs(actual_odds - bet$odds) / bet$odds > 0.01) {
    amount <- recalculate_kelly_amount(
      bet$probability, actual_odds, bet$odds, bet$bet_amount
    )
    if (amount < 200) {
      cli::cli_alert_warning("Kelly amount {amount} kr < 200 kr minimum at live odds — skipping.")
      clear_bet_slip(session)
      return(list(status = "skipped", reason = "kelly_too_small", actual_odds = actual_odds))
    }
    cli::cli_alert_info(
      "Odds drifted {bet$odds} → {actual_odds}: stake {bet$bet_amount} → {amount} kr"
    )
  }

  # All checks passed
  list(ok = TRUE, amount = amount)
}

# ── Outcome dispatch ──────────────────────────────────────────────────────────

#' Map a three-way outcome (home/draw/away) to its 1-indexed Lengjan button slot.
#'
#' Shared by `place_outcome_bet` (moneyline) and `place_handicap_bet` (spread).
#' The canonical outcome strings come from `R/decide-kelly.R::build_return_matrix`
#' and the `recommendations` Parquet schema — `home`, `draw`, `away`. Any other
#' string is a contract violation upstream and must abort placement immediately
#' rather than silently route to the wrong button.
#'
#' @param outcome Character. One of `"home"`, `"draw"`, `"away"`.
#' @param market Character used only in the error message.
#' @return Integer 1L/2L/3L.
#' @keywords internal
#' @noRd
outcome_3way_btn_index <- function(outcome, market) {
  switch(outcome,
    "home" = 1L,
    "draw" = 2L,
    "away" = 3L,
    stop("Invalid outcome for ", market, ": ", outcome, call. = FALSE)
  )
}

# ── Outcome (1x2) bets ────────────────────────────────────────────────────────

#' Place a 1x2 (outcome/moneyline) bet
#'
#' @param session ChromoteSession (authenticated)
#' @param bet Single-row bet tibble
#' @param match_id Lengjan match ID
#' @param sport_id Lengjan sport ID
#' @param dry_run If TRUE, stop before clicking "Kaupa"
#' @return List with status, actual_odds, amount
#' @export
place_outcome_bet <- function(session, bet, match_id, sport_id, dry_run) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli::cli_alert_info(
    "Navigating to {bet$home_team} vs {bet$away_team} for {bet$outcome} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  btn_index <- outcome_3way_btn_index(bet$outcome, "1x2")

  # Click odds button — returns actual odds from the DOM.
  # WHY \uxxxx escape: in a C locale R session, sprintf() converts non-ASCII
  # literals to <U+xxxx> placeholder text instead of the actual codepoint, so
  # the literal "Úrslit" arrives at the page's DOM as the literal 13-char
  # string "<U+00DA>rslit" and never matches. The escape forces the right
  # codepoint regardless of locale. See r-package-conventions.md.
  actual_odds <- click_market_button(
    session,
    section_label = "\u00darslit",
    btn_index = btn_index
  )

  # P3/P4 validation
  check <- check_live_odds(session, bet, actual_odds)
  if (!isTRUE(check$ok)) {
    return(check)
  }

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Handicap bets ─────────────────────────────────────────────────────────────

#' Place a handicap (spread) bet
#'
#' @param session ChromoteSession (authenticated)
#' @param bet Single-row bet tibble
#' @param match_id Lengjan match ID
#' @param sport_id Lengjan sport ID
#' @param dry_run If TRUE, stop before clicking "Kaupa"
#' @return List with status, actual_odds, amount
#' @export
place_handicap_bet <- function(session, bet, match_id, sport_id, dry_run) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli::cli_alert_info(
    "Navigating to {bet$home_team} vs {bet$away_team} for handicap {bet$outcome} ",
    "line={bet$line} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  expand_market_section(session, "Forgj\u00f6f")
  Sys.sleep(sample_delay(c(1, 2)))

  click_show_all(session)

  line_label <- handicap_to_lengjan_line(bet$line)

  btn_index <- outcome_3way_btn_index(bet$outcome, "handicap")

  actual_odds <- click_table_button(
    session,
    section_label = "Forgj\u00f6f",
    line_label = line_label,
    btn_index = btn_index
  )

  check <- check_live_odds(session, bet, actual_odds)
  if (!isTRUE(check$ok)) {
    return(check)
  }

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Totals bets ───────────────────────────────────────────────────────────────

#' Place a totals (over/under) bet
#'
#' @param session ChromoteSession (authenticated)
#' @param bet Single-row bet tibble
#' @param match_id Lengjan match ID
#' @param sport_id Lengjan sport ID
#' @param dry_run If TRUE, stop before clicking "Kaupa"
#' @return List with status, actual_odds, amount
#' @export
place_totals_bet <- function(session, bet, match_id, sport_id, dry_run) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli::cli_alert_info(
    "Navigating to {bet$home_team} vs {bet$away_team} for totals {bet$outcome} ",
    "line={bet$line} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  expand_market_section(session, "Yfir/Undir")
  Sys.sleep(sample_delay(c(1, 2)))

  line_label <- as.character(bet$line)

  btn_index <- switch(bet$outcome,
    "over"  = 1,
    "under" = 2,
    stop("Invalid outcome for totals: ", bet$outcome)
  )

  actual_odds <- click_table_button(
    session,
    section_label = "Yfir/Undir",
    line_label = line_label,
    btn_index = btn_index
  )

  check <- check_live_odds(session, bet, actual_odds)
  if (!isTRUE(check$ok)) {
    return(check)
  }

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Shared helpers ────────────────────────────────────────────────────────────

#' Expand a market section by clicking its "open" button
#'
#' @param session ChromoteSession (authenticated)
#' @param section_label Character label text of the market section
#' @keywords internal
#' @noRd
expand_market_section <- function(session, section_label) {
  js <- sprintf("
    (() => {
      // Find section by label (exact or startsWith match)
      const allElements = document.querySelectorAll('*');
      for (const el of allElements) {
        if (el.children.length === 0) {
          const t = el.textContent.trim();
          if (t === '%s' || t.startsWith('%s')) {
            const section = el.closest('[role=\"region\"], section');
            if (section) {
              // Find the expand/collapse button (aria-label='open')
              const btn = section.querySelector('button[aria-label=\"open\"]') ||
                          section.querySelector('button');
              if (btn) { btn.click(); return true; }
            }
          }
        }
      }
      return false;
    })()
  ", section_label, section_label)

  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  if (isTRUE(result$result$value)) {
    cli::cli_alert_info("Expanded '{section_label}' section.")
  } else {
    cli::cli_alert_warning("Could not find '{section_label}' section to expand.")
  }
}

#' Click an odds button in a list-based market section (1x2)
#'
#' Returns the actual odds found on the DOM so the caller can run
#' P3/P4 checks before confirming the bet.
#'
#' @param session ChromoteSession (authenticated)
#' @param section_label Character label of the market section (e.g. "Úrslit")
#' @param btn_index Integer index of the button to click (1 = home, 2 = tie, 3 = away)
#' @return Numeric: actual odds from Lengjan
#' @keywords internal
#' @noRd
click_market_button <- function(session, section_label, btn_index,
                                max_wait_seconds = 10) {
  js <- sprintf("
    (() => {
      // Find section by label (supports startsWith for variants like 'Úrslit leiksins')
      const allElements = document.querySelectorAll('*');
      let section = null;
      for (const el of allElements) {
        if (el.children.length === 0) {
          const t = el.textContent.trim();
          if (t === '%s' || t.startsWith('%s')) {
            section = el.closest('[role=\"region\"], section');
            if (section) break;
          }
        }
      }
      if (!section) return JSON.stringify({error: 'Section not found'});

      const buttons = section.querySelectorAll('li button, button');
      const oddsButtons = [];
      for (const btn of buttons) {
        // Try aria-label first ('1, stuðull: 2.28')
        const label = btn.getAttribute('aria-label') || '';
        const labelMatch = label.match(/stu\\u00f0ull:\\s*(\\d+\\.\\d+)/);
        if (labelMatch) {
          oddsButtons.push({element: btn, odds: parseFloat(labelMatch[1])});
        } else {
          // Fallback: button text may have label prefix like '11.69', 'X7.80', '22.39'
          const oddsEl = btn.querySelector('[class*=\"h7cub5\"] p, [class*=\"h7cub5\"]');
          const oddsText = oddsEl ? oddsEl.textContent.trim() : btn.textContent.trim();
          // Strip leading 1/X/2 prefix if present
          const cleaned = oddsText.replace(/^[1X2]/, '');
          const textMatch = cleaned.match(/^(\\d+\\.\\d+)$/);
          if (textMatch) {
            oddsButtons.push({element: btn, odds: parseFloat(textMatch[1])});
          }
        }
      }

      if (oddsButtons.length < %d) {
        return JSON.stringify({error: 'Not enough odds buttons', found: oddsButtons.length});
      }

      const target = oddsButtons[%d - 1];
      const rect = target.element.getBoundingClientRect();
      return JSON.stringify({
        odds: target.odds,
        x: rect.x + rect.width / 2,
        y: rect.y + rect.height / 2,
        html: target.element.outerHTML
      });
    })()
  ", section_label, section_label, btn_index, btn_index)

  # Poll the section selector — Lengjan's React app sometimes hydrates the
  # markets up to several seconds after navigation. Retry on transient
  # "Section not found" / "Not enough odds buttons" errors.
  deadline <- Sys.time() + max_wait_seconds
  parsed <- NULL
  repeat {
    result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
    parsed <- jsonlite::fromJSON(result$result$value)
    transient <- !is.null(parsed$error) &&
      parsed$error %in% c("Section not found", "Not enough odds buttons")
    if (!transient || Sys.time() >= deadline) break
    Sys.sleep(0.5)
  }

  if (!is.null(parsed$error)) {
    stop("Failed to click odds button: ", parsed$error)
  }

  actual_odds <- parsed$odds
  verify_odds_match(parsed, actual_odds)

  # Use CDP trusted click at the button coordinates
  cdp_click(session, parsed$x, parsed$y)
  cli::cli_alert_success("Clicked odds button: {actual_odds}")
  Sys.sleep(sample_delay(c(0.5, 1.5)))

  actual_odds
}

#' Click an odds button in a table-based market section (handicap/totals)
#'
#' Table structure:
#'   <table>
#'     <tbody>
#'       <tr>
#'         <th><div><p>59.5</p></div></th>  (line label)
#'         <td><div><button>...<p>1.65</p>...</button></div></td>  (over/home)
#'         <td><div><button>...<p>1.87</p>...</button></div></td>  (under/away)
#'       </tr>
#'     </tbody>
#'   </table>
#'
#' @param session ChromoteSession (authenticated)
#' @param section_label Character label of the market section
#' @param line_label Character label of the table row (e.g. "59.5", "1.5-0")
#' @param btn_index Integer index of the button in the row (1-based)
#' @return Numeric: actual odds from Lengjan
#' @keywords internal
#' @noRd
click_table_button <- function(session, section_label, line_label, btn_index) {
  js <- sprintf(
    "
    (() => {
      // Find section by label (supports partial match for variants)
      const allElements = document.querySelectorAll('*');
      let section = null;
      for (const el of allElements) {
        if (el.children.length === 0) {
          const t = el.textContent.trim();
          if (t === '%s' || t.startsWith('%s')) {
            section = el.closest('[role=\"region\"], section');
            if (section) break;
          }
        }
      }
      if (!section) return JSON.stringify({error: 'Section not found: %s'});

      const table = section.querySelector('table');
      if (!table) return JSON.stringify({error: 'No table in section'});

      // Find the row with matching line label
      const rows = table.querySelectorAll('tbody tr');
      let targetRow = null;
      for (const row of rows) {
        const th = row.querySelector('th');
        if (th && th.textContent.trim() === '%s') {
          targetRow = row;
          break;
        }
      }
      if (!targetRow) return JSON.stringify({error: 'Line not found: %s'});

      // Get all td cells with buttons in this row
      const tds = targetRow.querySelectorAll('td');
      const rowButtons = [];
      tds.forEach(td => {
        const btn = td.querySelector('button');
        if (btn) {
          const oddsText = btn.textContent.trim();
          const match = oddsText.match(/(\\d+\\.\\d+)/);
          if (match) {
            rowButtons.push({element: btn, odds: parseFloat(match[1])});
          }
        }
      });

      if (rowButtons.length < %d) {
        return JSON.stringify({
          error: 'Not enough buttons in row',
          found: rowButtons.length,
          line: '%s',
          rowHTML: targetRow.innerHTML.substring(0, 200)
        });
      }

      const target = rowButtons[%d - 1];
      const rect = target.element.getBoundingClientRect();
      return JSON.stringify({
        odds: target.odds,
        x: rect.x + rect.width / 2,
        y: rect.y + rect.height / 2,
        html: target.element.outerHTML
      });
    })()
  ", section_label, section_label, section_label, line_label, line_label,
    btn_index, line_label, btn_index
  )

  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  parsed <- jsonlite::fromJSON(result$result$value)

  if (!is.null(parsed$error)) {
    stop("Failed to click table button: ", parsed$error)
  }

  actual_odds <- parsed$odds
  verify_odds_match(parsed, actual_odds)

  # Use CDP trusted click
  cdp_click(session, parsed$x, parsed$y)
  cli::cli_alert_success("Clicked odds button: {actual_odds} (line {line_label})")
  Sys.sleep(sample_delay(c(0.5, 1.5)))

  actual_odds
}

#' Enter stake amount and optionally confirm the bet
#'
#' @param session ChromoteSession (authenticated)
#' @param amount Numeric stake in kr
#' @param dry_run If TRUE, stop before clicking "Kaupa"
#' @return List with status and amount
#' @keywords internal
#' @noRd
enter_stake_and_confirm <- function(session, amount, dry_run = FALSE) {
  # Wait for bet slip to appear after clicking an odds button
  Sys.sleep(sample_delay(c(1.5, 2.5)))

  # First expand the bet slip if it's minimised
  expand_bet_slip(session)
  Sys.sleep(sample_delay(c(0.5, 1)))

  # Find the stake input and get its coordinates
  js_find_input <- "
    (() => {
      const inputs = document.querySelectorAll('input');
      for (const input of inputs) {
        if (input.offsetParent !== null && /^\\d*$/.test(input.value)) {
          const rect = input.getBoundingClientRect();
          return JSON.stringify({
            found: true,
            currentValue: input.value,
            x: rect.x + rect.width / 2,
            y: rect.y + rect.height / 2
          });
        }
      }
      const allInputs = Array.from(inputs).map(i => ({
        type: i.type, value: i.value.substring(0, 20),
        visible: i.offsetParent !== null
      }));
      return JSON.stringify({found: false, inputs: allInputs});
    })()
  "

  result <- session$Runtime$evaluate(
    expression = js_find_input, returnByValue = TRUE
  )
  parsed <- jsonlite::fromJSON(result$result$value)

  if (!isTRUE(parsed$found)) {
    cli::cli_alert_warning("Stake input debug: {result$result$value}")
    stop("Could not find stake input.")
  }

  # Click the input to focus it, then select-all and type the new amount
  cdp_click(session, parsed$x, parsed$y)
  Sys.sleep(0.2)
  cdp_select_all_and_type(session, as.character(as.integer(amount)))

  cli::cli_alert_info("Set stake to {amount} kr.")
  Sys.sleep(sample_delay(c(0.5, 1)))

  if (dry_run) {
    cli::cli_alert_warning("[DRY RUN] Would click 'Kaupa' — stopping here.")
    clear_bet_slip(session)
    return(list(status = "dry_run", amount = amount))
  }

  js_find_buy <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.includes('Kaupa') && btn.offsetParent !== null) {
          const rect = btn.getBoundingClientRect();
          return JSON.stringify({x: rect.x + rect.width / 2, y: rect.y + rect.height / 2});
        }
      }
      return JSON.stringify({error: 'Kaupa button not found'});
    })()
  "

  buy_result <- session$Runtime$evaluate(
    expression = js_find_buy, returnByValue = TRUE
  )
  buy_parsed <- jsonlite::fromJSON(buy_result$result$value)
  if (!is.null(buy_parsed$error)) {
    stop(buy_parsed$error)
  }
  cdp_click(session, buy_parsed$x, buy_parsed$y)
  cli::cli_alert_info("Clicked 'Kaupa' — waiting for confirmation dialog...")

  Sys.sleep(sample_delay(c(1.5, 2.5)))

  # Lengjan shows a "Stadfesta Kaup" (Confirm Purchase) button after "Kaupa"
  js_find_confirm <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        const t = btn.textContent.trim();
        if ((t.includes('Sta\\u00f0festa') || t.includes('sta\\u00f0festa')) &&
            btn.offsetParent !== null) {
          const rect = btn.getBoundingClientRect();
          return JSON.stringify({x: rect.x + rect.width / 2, y: rect.y + rect.height / 2});
        }
      }
      return JSON.stringify({error: 'Confirm button not found'});
    })()
  "

  confirm_result <- session$Runtime$evaluate(
    expression = js_find_confirm, returnByValue = TRUE
  )
  confirm_parsed <- jsonlite::fromJSON(confirm_result$result$value)
  if (!is.null(confirm_parsed$error)) {
    cli::cli_alert_warning("No confirmation dialog found — bet may already be placed or failed.")
  } else {
    cdp_click(session, confirm_parsed$x, confirm_parsed$y)
    cli::cli_alert_success("Clicked 'Staðfesta Kaup' — bet confirmed.")
  }

  Sys.sleep(sample_delay(c(2, 3)))
  cli::cli_alert_success("Bet placed: {amount} kr.")

  list(status = "placed", amount = amount)
}

#' Click "Birta allt" (show all) button in a market section
#'
#' @param session ChromoteSession (authenticated)
#' @keywords internal
#' @noRd
click_show_all <- function(session) {
  js <- "
    (() => {
      const buttons = document.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.includes('Birta allt')) {
          btn.click();
          return true;
        }
      }
      return false;
    })()
  "
  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  if (isTRUE(result$result$value)) {
    cli::cli_alert_info("Clicked 'Birta allt' to show all lines.")
    Sys.sleep(sample_delay(c(1, 2)))
  }
}

# ── Bet slip interaction ──────────────────────────────────────────────────────

#' Expand the bet slip if it's minimised (click the "Opna" button)
#'
#' @param session ChromoteSession (authenticated)
#' @keywords internal
#' @noRd
expand_bet_slip <- function(session) {
  js <- "
    (() => {
      // The bet slip region contains a button with aria text 'Opna' (Open)
      const region = document.querySelector('[aria-label*=\"se\\u00f0il\"]');
      if (!region) return 'no_slip';
      const btn = region.querySelector('button');
      if (!btn) return 'no_button';
      // Check if the slip is already expanded (has a visible input)
      const input = region.querySelector('input');
      if (input && input.offsetParent !== null) return 'already_open';
      // Click to expand
      btn.click();
      return 'expanded';
    })()
  "
  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  status <- result$result$value
  if (status == "expanded") {
    cli::cli_alert_info("Expanded bet slip.")
    Sys.sleep(sample_delay(c(0.5, 1)))
  } else if (status == "no_slip") {
    cli::cli_alert_warning("No bet slip found on page — odds click may have failed.")
  }
  invisible(status)
}

#' Verify the bet slip appeared and contains the expected selection
#'
#' @param session ChromoteSession (authenticated)
#' @return TRUE if bet slip is visible with a selection, FALSE otherwise
#' @keywords internal
#' @noRd
verify_bet_slip <- function(session) {
  js <- "
    (() => {
      const region = document.querySelector('[aria-label*=\"se\\u00f0il\"]');
      if (!region) return JSON.stringify({visible: false, reason: 'no region'});
      const odds = region.querySelector('[aria-label*=\"Heildarstu\\u00f0ull\"], [class*=\"stu\\u00f0ull\"]');
      const text = region.textContent;
      const hasOdds = /\\d+[,.]\\d+/.test(text);
      return JSON.stringify({visible: true, hasOdds: hasOdds, snippet: text.substring(0, 100)});
    })()
  "
  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  parsed <- jsonlite::fromJSON(result$result$value)
  isTRUE(parsed$visible) && isTRUE(parsed$hasOdds)
}

#' Clear the bet slip by clicking "Hreinsa raddir"
#'
#' @param session ChromoteSession (authenticated)
#' @return Invisible logical: TRUE if the slip was cleared
#' @export
clear_bet_slip <- function(session) {
  js <- "
    (() => {
      const region = document.querySelector('[aria-label*=\"se\\u00f0il\"]');
      if (!region) return false;
      const buttons = region.querySelectorAll('button');
      for (const btn of buttons) {
        if (btn.textContent.includes('Hreinsa')) {
          btn.click();
          return true;
        }
      }
      // Try the individual delete button (X on each selection)
      for (const btn of buttons) {
        const label = btn.textContent.trim();
        if (label.includes('Ey\\u00f0a') || label === '') {
          const aria = btn.querySelector('[aria-label]');
          if (aria) { btn.click(); return true; }
        }
      }
      return false;
    })()
  "
  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  if (isTRUE(result$result$value)) {
    cli::cli_alert_info("Cleared bet slip.")
  }
  invisible(result$result$value)
}

# ── Handicap line conversion ──────────────────────────────────────────────────

#' Convert pipeline handicap value to Lengjan's "H-A" line format
#'
#' @param handicap Numeric handicap value (positive = home advantage)
#' @return Character string in Lengjan format (e.g. "1.5-0" or "0-1.5")
#' @export
handicap_to_lengjan_line <- function(handicap) {
  if (handicap >= 0) {
    paste0(handicap, "-0")
  } else {
    paste0("0-", abs(handicap))
  }
}

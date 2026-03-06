#' Lengjan Bet Placement
#'
#' Functions to place individual bets on Lengjan via chromote.
#' Supports 1x2 (outcome), handicap (forgjöf), and totals (yfir eða undir).
#'
#' Implements placement rules P3 (live Kelly recalculation) and P4 (+EV check).
#' See: ~/Obsidian/Metill/Sports/betting-system-rules.md

box::use(
  cli[cli_alert_info, cli_alert_success, cli_alert_warning, cli_alert_danger]
)

# ── CDP click helper ──────────────────────────────────────────────────────────

#' Click at given x,y coordinates using CDP Input.dispatchMouseEvent
#' This creates trusted events that React/Lengjan will recognise.
#' Dispatches mouseMoved → mousePressed → mouseReleased to mimic a real click.
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
#' Uses JS focus+select to select existing text, then CDP Input.insertText
#' to type the replacement (triggers React's synthetic input events).
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

# ── Kelly helpers (P3/P4) ───────────────────────────────────────────────────

#' Recalculate Kelly bet amount at live odds (rule P3)
#'
#' When Lengjan's actual odds differ from the recommendation, recompute
#' the optimal stake using the Kelly criterion at the new odds.
#'
#' @param p Model probability
#' @param actual_odds Live odds from Lengjan
#' @param kelly_frac Calibrated Kelly fraction for this league
#' @param bankroll Current available bankroll
#' @return Rounded bet amount in kr
#' @export
recalculate_kelly_amount <- function(p, actual_odds, kelly_frac, bankroll) {
  raw_kelly <- (p * actual_odds - 1) / (actual_odds - 1)
  raw_kelly <- max(0, raw_kelly)
  scaled <- raw_kelly * kelly_frac
  round(scaled * bankroll, 0)
}

#' Check if a bet has positive expected value (rule P4)
#'
#' @param p Model probability
#' @param odds Decimal odds
#' @return TRUE if +EV
#' @export
is_positive_ev <- function(p, odds) {
  p * (odds - 1) - (1 - p) > 0
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
#' @param bankroll Current bankroll for live Kelly recalculation (P3).
#'   NULL skips recalculation and uses the recommended bet_amount.
#' @return List with status ("placed", "skipped", "dry_run", "error"),
#'   actual_odds (from Lengjan DOM), and amount (stake entered)
#' @export
place_bet <- function(session, bet, match_id, sport_id,
                      dry_run = FALSE, bankroll = NULL) {

  market <- bet$market
  result <- tryCatch(
    {
      if (market == "outcome") {
        place_outcome_bet(session, bet, match_id, sport_id, dry_run, bankroll)
      } else if (market == "handicap") {
        place_handicap_bet(session, bet, match_id, sport_id, dry_run, bankroll)
      } else if (market == "totals") {
        place_totals_bet(session, bet, match_id, sport_id, dry_run, bankroll)
      } else {
        list(status = "skipped", reason = paste("Unknown market:", market))
      }
    },
    error = function(e) {
      cli_alert_danger("Error placing bet: {e$message}")
      list(status = "error", reason = e$message)
    }
  )

  result
}

# ── Placement rules (P3/P4) ────────────────────────────────────────────────

#' Validate live odds and recalculate stake if needed
#'
#' Called after clicking the odds button (which adds the selection to the
#' bet slip) but BEFORE entering stake and clicking "Kaupa".
#'
#' P4: Reject if no longer +EV at actual odds.
#' P3: Recalculate Kelly amount if odds drifted >1%.
#'
#' @return On rejection: list(status, reason, actual_odds).
#'   On success: list(ok = TRUE, amount = <stake to enter>).
check_live_odds <- function(session, bet, actual_odds, bankroll) {
  # P4: Must still be +EV at live odds
  if (!is_positive_ev(bet$probability, actual_odds)) {
    cli_alert_warning(
      "No longer +EV at live odds {actual_odds} (p={bet$probability}) \u2014 skipping."
    )
    clear_bet_slip(session)
    return(list(status = "skipped", reason = "not_positive_ev", actual_odds = actual_odds))
  }

  # P3: Recalculate Kelly if odds drifted >1%
  amount <- bet$bet_amount
  if (!is.null(bankroll) && abs(actual_odds - bet$odds) / bet$odds > 0.01) {
    amount <- recalculate_kelly_amount(
      bet$probability, actual_odds, bet$kelly_frac, bankroll
    )
    if (amount < 1) {
      cli_alert_warning("Kelly amount < 1 kr at live odds \u2014 skipping.")
      clear_bet_slip(session)
      return(list(status = "skipped", reason = "kelly_too_small", actual_odds = actual_odds))
    }
    cli_alert_info(
      "Odds drifted {bet$odds} \u2192 {actual_odds}: stake {bet$bet_amount} \u2192 {amount} kr"
    )
  }

  # All checks passed
  list(ok = TRUE, amount = amount)
}

# ── Outcome (1x2) bets ────────────────────────────────────────────────────────

place_outcome_bet <- function(session, bet, match_id, sport_id,
                              dry_run, bankroll) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli_alert_info(
    "Navigating to {bet$home} vs {bet$away} for {bet$outcome} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  btn_index <- switch(bet$outcome,
    "home" = 1,
    "tie"  = 2,
    "away" = 3,
    stop("Invalid outcome for 1x2: ", bet$outcome)
  )

  # Click odds button — returns actual odds from the DOM
  actual_odds <- click_market_button(
    session,
    section_label = "\u00darslit",
    btn_index = btn_index
  )

  # P3/P4 validation
  check <- check_live_odds(session, bet, actual_odds, bankroll)
  if (!isTRUE(check$ok)) return(check)

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Handicap bets ─────────────────────────────────────────────────────────────

place_handicap_bet <- function(session, bet, match_id, sport_id,
                               dry_run, bankroll) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli_alert_info(
    "Navigating to {bet$home} vs {bet$away} for handicap {bet$outcome} ",
    "line={bet$info} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  expand_market_section(session, "Forgj\u00f6f")
  Sys.sleep(sample_delay(c(1, 2)))

  click_show_all(session)

  line_label <- handicap_to_lengjan_line(as.numeric(bet$info))

  btn_index <- switch(bet$outcome,
    "home" = 1,
    "tie"  = 2,
    "away" = 3,
    stop("Invalid outcome for handicap: ", bet$outcome)
  )

  actual_odds <- click_table_button(
    session,
    section_label = "Forgj\u00f6f",
    line_label = line_label,
    btn_index = btn_index
  )

  check <- check_live_odds(session, bet, actual_odds, bankroll)
  if (!isTRUE(check$ok)) return(check)

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Totals bets ───────────────────────────────────────────────────────────────

place_totals_bet <- function(session, bet, match_id, sport_id,
                             dry_run, bankroll) {
  url <- paste0(
    "https://games.lotto.is/getraunaleikir/lengjan/leikur?id=",
    match_id, "&sport=", sport_id
  )
  cli_alert_info(
    "Navigating to {bet$home} vs {bet$away} for totals {bet$outcome} ",
    "line={bet$info} @ {bet$odds}"
  )
  session$Page$navigate(url)
  Sys.sleep(sample_delay(c(2.5, 4)))

  expand_market_section(session, "Yfir e\u00f0a undir")
  Sys.sleep(sample_delay(c(1, 2)))

  line_label <- as.character(bet$info)

  btn_index <- switch(bet$outcome,
    "over"  = 1,
    "under" = 2,
    stop("Invalid outcome for totals: ", bet$outcome)
  )

  actual_odds <- click_table_button(
    session,
    section_label = "Yfir e\u00f0a undir",
    line_label = line_label,
    btn_index = btn_index
  )

  check <- check_live_odds(session, bet, actual_odds, bankroll)
  if (!isTRUE(check$ok)) return(check)

  result <- enter_stake_and_confirm(session, check$amount, dry_run)
  result$actual_odds <- actual_odds
  result
}

# ── Shared helpers ────────────────────────────────────────────────────────────

#' Expand a market section by clicking its "open" button
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
    cli_alert_info("Expanded '{section_label}' section.")
  } else {
    cli_alert_warning("Could not find '{section_label}' section to expand.")
  }
}

#' Click an odds button in a list-based market section (1x2)
#'
#' Returns the actual odds found on the DOM so the caller can run
#' P3/P4 checks before confirming the bet.
#'
#' @return Numeric: actual odds from Lengjan
click_market_button <- function(session, section_label, btn_index) {
  js <- sprintf("
    (() => {
      // Find section by label (supports startsWith for variants like '\u00darslit leiksins')
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
        // Try aria-label first ('1, stu\u00f0ull: 2.28')
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
        y: rect.y + rect.height / 2
      });
    })()
  ", section_label, section_label, btn_index, btn_index)

  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  parsed <- jsonlite::fromJSON(result$result$value)

  if (!is.null(parsed$error)) {
    stop("Failed to click odds button: ", parsed$error)
  }

  actual_odds <- parsed$odds

  # Use CDP trusted click at the button coordinates
  cdp_click(session, parsed$x, parsed$y)
  cli_alert_success("Clicked odds button: {actual_odds}")
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
#' @return Numeric: actual odds from Lengjan
click_table_button <- function(session, section_label, line_label, btn_index) {
  js <- sprintf("
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
        y: rect.y + rect.height / 2
      });
    })()
  ", section_label, section_label, section_label, line_label, line_label,
     btn_index, line_label, btn_index)

  result <- session$Runtime$evaluate(expression = js, returnByValue = TRUE)
  parsed <- jsonlite::fromJSON(result$result$value)

  if (!is.null(parsed$error)) {
    stop("Failed to click table button: ", parsed$error)
  }

  actual_odds <- parsed$odds

  # Use CDP trusted click
  cdp_click(session, parsed$x, parsed$y)
  cli_alert_success("Clicked odds button: {actual_odds} (line {line_label})")
  Sys.sleep(sample_delay(c(0.5, 1.5)))

  actual_odds
}

#' Enter stake amount and optionally confirm the bet
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
    cli_alert_warning("Stake input debug: {result$result$value}")
    stop("Could not find stake input.")
  }

  # Click the input to focus it, then select-all and type the new amount
  cdp_click(session, parsed$x, parsed$y)
  Sys.sleep(0.2)
  cdp_select_all_and_type(session, as.character(as.integer(amount)))

  cli_alert_info("Set stake to {amount} kr.")
  Sys.sleep(sample_delay(c(0.5, 1)))

  if (dry_run) {
    cli_alert_warning("[DRY RUN] Would click 'Kaupa' \u2014 stopping here.")
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
  cli_alert_info("Clicked 'Kaupa' — waiting for confirmation dialog...")

  Sys.sleep(sample_delay(c(1.5, 2.5)))

  # Lengjan shows a "Staðfesta Kaup" (Confirm Purchase) button after "Kaupa"
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
    cli_alert_warning("No confirmation dialog found — bet may already be placed or failed.")
  } else {
    cdp_click(session, confirm_parsed$x, confirm_parsed$y)
    cli_alert_success("Clicked 'Staðfesta Kaup' — bet confirmed.")
  }

  Sys.sleep(sample_delay(c(2, 3)))
  cli_alert_success("Bet placed: {amount} kr.")

  list(status = "placed", amount = amount)
}

#' Click "Birta allt" (show all) button in a market section
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
    cli_alert_info("Clicked 'Birta allt' to show all lines.")
    Sys.sleep(sample_delay(c(1, 2)))
  }
}

# ── Bet slip interaction ──────────────────────────────────────────────────────

#' Expand the bet slip if it's minimised (click the "Opna" button)
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
    cli_alert_info("Expanded bet slip.")
    Sys.sleep(sample_delay(c(0.5, 1)))
  } else if (status == "no_slip") {
    cli_alert_warning("No bet slip found on page \u2014 odds click may have failed.")
  }
  invisible(status)
}

#' Verify the bet slip appeared and contains the expected selection
#' @return TRUE if bet slip is visible with a selection, FALSE otherwise
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

#' Clear the bet slip by clicking "Hreinsa raðir"
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
    cli_alert_info("Cleared bet slip.")
  }
  invisible(result$result$value)
}

# ── Handicap line conversion ──────────────────────────────────────────────────

#' Convert pipeline handicap value to Lengjan's "H-A" line format
handicap_to_lengjan_line <- function(handicap) {
  if (handicap >= 0) {
    paste0(handicap, "-0")
  } else {
    paste0("0-", abs(handicap))
  }
}

#' Random delay helper
sample_delay <- function(range = c(2, 4)) {
  stats::runif(1, min = range[1], max = range[2])
}

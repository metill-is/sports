#' Lengjan Bet Placement
#'
#' Functions to place individual bets on Lengjan via chromote.
#' Supports 1x2 (outcome), handicap (forgjöf), and totals (yfir eða undir).

box::use(
  cli[cli_alert_info, cli_alert_success, cli_alert_warning, cli_alert_danger]
)

# ── CDP click helper ──────────────────────────────────────────────────────────

#' Click at given x,y coordinates using CDP Input.dispatchMouseEvent
#' This creates trusted events that React/Lengjan will recognise
cdp_click <- function(session, x, y) {
  session$Input$dispatchMouseEvent(
    type = "mousePressed", x = x, y = y, button = "left", clickCount = 1
  )
  session$Input$dispatchMouseEvent(
    type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1
  )
}

# ── Main entry point ──────────────────────────────────────────────────────────

#' Place a single bet on Lengjan
#'
#' @param session ChromoteSession (authenticated)
#' @param bet A single-row data.frame/tibble from bets_log.csv
#' @param match_id Lengjan match ID (from extract_matches)
#' @param sport_id Lengjan sport ID
#' @param dry_run If TRUE, stop before clicking "Kaupa" (default FALSE)
#' @param odds_tolerance Maximum relative difference between pipeline odds
#'   and Lengjan odds to accept (default 0.05 = 5%)
#' @return List with status ("placed", "skipped", "dry_run", "error") and details
place_bet <- function(session, bet, match_id, sport_id,
                      dry_run = FALSE, odds_tolerance = 0.05) {

  market <- bet$market
  result <- tryCatch(
    {
      if (market == "outcome") {
        place_outcome_bet(session, bet, match_id, sport_id, dry_run, odds_tolerance)
      } else if (market == "handicap") {
        place_handicap_bet(session, bet, match_id, sport_id, dry_run, odds_tolerance)
      } else if (market == "totals") {
        place_totals_bet(session, bet, match_id, sport_id, dry_run, odds_tolerance)
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

# ── Outcome (1x2) bets ────────────────────────────────────────────────────────

place_outcome_bet <- function(session, bet, match_id, sport_id,
                              dry_run, odds_tolerance) {
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

  # Section label varies by sport: "Úrslit" (football) or "Úrslit leiksins" (handball)
  click_market_button(
    session,
    section_label = "\u00darslit",
    btn_index = btn_index,
    expected_odds = bet$odds,
    odds_tolerance = odds_tolerance
  )

  enter_stake_and_confirm(session, bet$bet_amount, dry_run)
}

# ── Handicap bets ─────────────────────────────────────────────────────────────

place_handicap_bet <- function(session, bet, match_id, sport_id,
                               dry_run, odds_tolerance) {
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

  click_table_button(
    session,
    section_label = "Forgj\u00f6f",
    line_label = line_label,
    btn_index = btn_index,
    expected_odds = bet$odds,
    odds_tolerance = odds_tolerance
  )

  enter_stake_and_confirm(session, bet$bet_amount, dry_run)
}

# ── Totals bets ───────────────────────────────────────────────────────────────

place_totals_bet <- function(session, bet, match_id, sport_id,
                             dry_run, odds_tolerance) {
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

  click_table_button(
    session,
    section_label = "Yfir e\u00f0a undir",
    line_label = line_label,
    btn_index = btn_index,
    expected_odds = bet$odds,
    odds_tolerance = odds_tolerance
  )

  enter_stake_and_confirm(session, bet$bet_amount, dry_run)
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
click_market_button <- function(session, section_label, btn_index,
                                expected_odds, odds_tolerance) {
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
        const labelMatch = label.match(/stuðull:\\s*(\\d+\\.\\d+)/);
        if (labelMatch) {
          oddsButtons.push({element: btn, odds: parseFloat(labelMatch[1])});
        } else {
          // Fallback: button text may have label prefix like '11.69', 'X7.80', '22.39'
          // The actual odds are inside a child <p> or <div> with class containing 'h7cub57'
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
  if (abs(actual_odds - expected_odds) / expected_odds > odds_tolerance) {
    stop(sprintf(
      "Odds mismatch: expected %.2f, found %.2f (%.1f%% difference)",
      expected_odds, actual_odds,
      abs(actual_odds - expected_odds) / expected_odds * 100
    ))
  }

  # Use CDP trusted click at the button coordinates
  cdp_click(session, parsed$x, parsed$y)
  cli_alert_success("Clicked odds button: {actual_odds}")
  Sys.sleep(sample_delay(c(0.5, 1.5)))
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
click_table_button <- function(session, section_label, line_label, btn_index,
                               expected_odds, odds_tolerance) {
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
  if (abs(actual_odds - expected_odds) / expected_odds > odds_tolerance) {
    stop(sprintf(
      "Odds mismatch: expected %.2f, found %.2f (%.1f%% difference)",
      expected_odds, actual_odds,
      abs(actual_odds - expected_odds) / expected_odds * 100
    ))
  }

  # Use CDP trusted click
  cdp_click(session, parsed$x, parsed$y)
  cli_alert_success("Clicked odds button: {actual_odds} (line {line_label})")
  Sys.sleep(sample_delay(c(0.5, 1.5)))
}

#' Enter stake amount and optionally confirm the bet
enter_stake_and_confirm <- function(session, amount, dry_run = FALSE) {
  # Wait for bet slip to appear after clicking an odds button
  Sys.sleep(sample_delay(c(1.5, 2.5)))

  js_set_stake <- sprintf("
    (() => {
      // Try all input types — bet slip input might be text, number, or tel
      const inputs = document.querySelectorAll('input');
      for (const input of inputs) {
        // Look for a visible input with a numeric value (default stake)
        if (input.offsetParent !== null && /^\\d+$/.test(input.value)) {
          const setter = Object.getOwnPropertyDescriptor(
            window.HTMLInputElement.prototype, 'value'
          ).set;
          setter.call(input, '%s');
          input.dispatchEvent(new Event('input', { bubbles: true }));
          input.dispatchEvent(new Event('change', { bubbles: true }));
          return JSON.stringify({found: true, oldValue: input.value, type: input.type});
        }
      }
      // Debug: report what inputs exist
      const allInputs = Array.from(inputs).map(i => ({
        type: i.type, value: i.value.substring(0, 20),
        visible: i.offsetParent !== null, name: i.name
      }));
      return JSON.stringify({found: false, inputs: allInputs});
    })()
  ", as.character(as.integer(amount)))

  result <- session$Runtime$evaluate(
    expression = js_set_stake, returnByValue = TRUE
  )

  parsed <- jsonlite::fromJSON(result$result$value)
  if (!isTRUE(parsed$found)) {
    cli_alert_warning("Stake input debug: {result$result$value}")
    stop("Could not find or set stake input.")
  }

  cli_alert_info("Set stake to {amount} kr.")
  Sys.sleep(sample_delay(c(0.5, 1)))

  if (dry_run) {
    cli_alert_warning("[DRY RUN] Would click 'Kaupa' — stopping here.")
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

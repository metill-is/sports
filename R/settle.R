#' @include storage.R
NULL

#' Compute win/pnl for placed bets given match results.
#'
#' Joins bets to results on (sport, country, sex, match_date, home_team,
#' away_team) and resolves win/pnl using the same per-market rules as
#' [build_return_matrix()] in `R/decide-kelly.R`:
#' - moneyline: home wins iff hg > ag; draw iff hg == ag; away iff hg < ag.
#' - spread: `line` is the home team's signed handicap (positive = home gets
#'   head start). Define `adj = (hg + line) - ag`. Then home wins iff
#'   adj > 0 (strict); away wins iff adj < 0 (strict); draw wins iff
#'   adj == 0. Home and away are mirror images of one adjusted margin --
#'   NOT two independent handicaps.
#' - total: over wins iff (hg + ag) > line (strict); under wins iff
#'   (hg + ag) < line (strict).
#'
#' Strict inequality on totals/spreads matches the EV computation that
#' justified placing the bet, so settled win-rate stays self-consistent
#' with the calibration multiplier the model used.
#'
#' Rows without a matching result retain `settled = FALSE`, `win = NA`,
#' `pnl = NA`. Rows with a result get `settled = TRUE` plus the resolved
#' `win` and `pnl`.
#'
#' When `match_date_window_days > 0`, bets that the strict equijoin leaves
#' unsettled get a second-pass lookup against results within
#' `match_date_window_days` of the bet's `match_date`, keyed on (sport,
#' country, sex, home_team, away_team). The fallback only fires when the
#' window contains exactly one candidate result for the bet — if a fixture
#' genuinely repeats inside the window (e.g. league leg + cup tie within
#' three days), every bet on that pairing is left unsettled rather than
#' attributed ambiguously. This unblocks ledger orphans caused by Lengjan
#' rescheduling a fixture after the bet was placed: the placer froze
#' `match_date` at the original kick-off (L3/L4), but the federation
#' results scraper records the played match at the new date.
#'
#' @param bets Tibble matching the ledger schema (must include `sport`,
#'   `country`, `sex`, `match_date`, `home_team`, `away_team`, `market`,
#'   `outcome`, `line`, `odds_placed`, `bet_amount`, `settled`, `win`, `pnl`).
#' @param results Tibble with columns `sport`, `country`, `sex`,
#'   `match_date`, `home_team`, `away_team`, `home_score`, `away_score`.
#' @param match_date_window_days Non-negative integer. `0` (default) gives
#'   the strict equijoin behaviour. Positive values enable the
#'   uniquely-matched fallback described above.
#' @return The `bets` tibble with `settled` / `win` / `pnl` updated where
#'   resolvable. Other columns are unchanged.
#' @export
compute_settlement <- function(bets, results, match_date_window_days = 0L) {
  if (nrow(bets) == 0L) {
    return(bets)
  }

  join_cols <- c(
    "sport", "country", "sex", "match_date", "home_team", "away_team"
  )
  res_slim <- results[, c(join_cols, "home_score", "away_score")]
  joined <- dplyr::left_join(bets, res_slim, by = join_cols)

  if (match_date_window_days > 0L && nrow(results) > 0L) {
    needs_fallback <- which(is.na(joined$home_score) | is.na(joined$away_score))
    for (i in needs_fallback) {
      cand <- results[
        results$sport == joined$sport[[i]] &
          results$country == joined$country[[i]] &
          results$sex == joined$sex[[i]] &
          results$home_team == joined$home_team[[i]] &
          results$away_team == joined$away_team[[i]] &
          abs(as.integer(results$match_date - joined$match_date[[i]])) <=
            match_date_window_days, ,
        drop = FALSE
      ]
      if (nrow(cand) == 1L) {
        joined$home_score[[i]] <- cand$home_score
        joined$away_score[[i]] <- cand$away_score
      }
    }
  }

  hg <- joined$home_score
  ag <- joined$away_score
  line <- joined$line
  market <- joined$market
  outcome <- joined$outcome

  win <- rep(NA, nrow(joined))
  for (i in seq_len(nrow(joined))) {
    if (is.na(hg[[i]]) || is.na(ag[[i]])) {
      next
    }
    win[[i]] <- switch(market[[i]],
      moneyline = switch(outcome[[i]],
        home = hg[[i]] > ag[[i]],
        draw = hg[[i]] == ag[[i]],
        away = hg[[i]] < ag[[i]],
        stop("Unknown moneyline outcome: ", outcome[[i]], call. = FALSE)
      ),
      spread = {
        if (is.na(line[[i]])) {
          stop("spread bet has missing line", call. = FALSE)
        }
        # WHY: `line` is the home team's signed handicap shared across all
        # outcomes of a parse_match_detail() row -- home and away are
        # mirror images under a single adjusted margin. The pre-2026-05-13
        # away formula `(ag + line) > hg` flipped the sign of the away
        # branch (same bug as build_return_matrix()).
        adj <- (hg[[i]] + line[[i]]) - ag[[i]]
        switch(outcome[[i]],
          home = adj > 0,
          away = adj < 0,
          draw = adj == 0,
          stop("Unknown spread outcome: ", outcome[[i]], call. = FALSE)
        )
      },
      total = {
        if (is.na(line[[i]])) {
          stop("total bet has missing line", call. = FALSE)
        }
        switch(outcome[[i]],
          over = (hg[[i]] + ag[[i]]) > line[[i]],
          under = (hg[[i]] + ag[[i]]) < line[[i]],
          stop("Unknown total outcome: ", outcome[[i]], call. = FALSE)
        )
      },
      stop("Unknown market: ", market[[i]], call. = FALSE)
    )
  }

  resolvable <- !is.na(win)
  bets$settled[resolvable] <- TRUE
  bets$win <- win
  bets$pnl[resolvable] <- ifelse(
    win[resolvable],
    bets$bet_amount[resolvable] * (bets$odds_placed[resolvable] - 1),
    -bets$bet_amount[resolvable]
  )
  bets
}

#' Settle resolvable rows in the canonical Parquet ledger.
#'
#' Reads the ledger and the results store, fills `settled` / `win` / `pnl`
#' on rows that newly have a matching result, and re-writes the ledger.
#' Already-settled rows are never re-evaluated (L4 ledger immutability).
#'
#' Calls [compute_settlement()] with `match_date_window_days = 3` so a bet
#' whose `match_date` no longer matches any result row (typically because
#' Lengjan rescheduled the kick-off after placement) still settles against
#' the played fixture as long as it lands within three days of the
#' originally-listed date *and* the window contains exactly one candidate
#' result. See the `compute_settlement()` docs for the unique-pairing
#' guard semantics.
#'
#' @param root Data root for the Parquet stores
#'   (default `here::here("data")`).
#' @param match_date_window_days Passed through to [compute_settlement()].
#'   Default `3L` — long enough to absorb the typical Icelandic-league
#'   reschedule horizon, short enough that cup-vs-league fixture repeats
#'   trigger the unique-pairing guard rather than mis-attribute.
#' @return Invisibly, the integer count of rows newly settled.
#' @export
settle_ledger <- function(root = here::here("data"),
                          match_date_window_days = 3L) {
  led <- tryCatch(
    read_table("ledger", root = root),
    error = function(e) tibble::tibble()
  )
  if (nrow(led) == 0L) {
    return(invisible(0L))
  }

  unsettled_mask <- is.na(led$settled) | !led$settled
  if (!any(unsettled_mask)) {
    return(invisible(0L))
  }

  results <- tryCatch(
    read_table("results", root = root),
    error = function(e) tibble::tibble()
  )
  if (nrow(results) == 0L) {
    return(invisible(0L))
  }

  to_settle <- led[unsettled_mask, , drop = FALSE]
  resolved <- compute_settlement(
    to_settle, results,
    match_date_window_days = match_date_window_days
  )
  newly_resolved_in_subset <- resolved$settled & !is.na(resolved$win) &
    !(to_settle$settled & !is.na(to_settle$win))
  if (!any(newly_resolved_in_subset)) {
    return(invisible(0L))
  }

  led_unsettled_idx <- which(unsettled_mask)
  newly_idx <- led_unsettled_idx[newly_resolved_in_subset]
  led$settled[newly_idx] <- TRUE
  led$win[newly_idx] <- resolved$win[newly_resolved_in_subset]
  led$pnl[newly_idx] <- resolved$pnl[newly_resolved_in_subset]

  changed_partitions <- dplyr::distinct(
    led[newly_idx, c("sport", "country"), drop = FALSE]
  )
  for (i in seq_len(nrow(changed_partitions))) {
    sp <- changed_partitions$sport[[i]]
    co <- changed_partitions$country[[i]]
    part_mask <- led$sport == sp & led$country == co
    write_table(led[part_mask, , drop = FALSE], "ledger", root = root)
  }

  invisible(as.integer(sum(newly_resolved_in_subset)))
}

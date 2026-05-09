#' @include storage.R
NULL

#' Compute win/pnl for placed bets given match results.
#'
#' Joins bets to results on (sport, country, sex, match_date, home_team,
#' away_team) and resolves win/pnl using the same per-market rules as
#' [build_return_matrix()] in `R/decide-kelly.R`:
#' - moneyline: home wins iff hg > ag; draw iff hg == ag; away iff hg < ag.
#' - spread: home wins iff (hg + line) > ag (strict); away wins iff
#'   (ag + line) > hg (strict); draw wins iff (hg + line) == ag.
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
#' @param bets Tibble matching the ledger schema (must include `sport`,
#'   `country`, `sex`, `match_date`, `home_team`, `away_team`, `market`,
#'   `outcome`, `line`, `odds_placed`, `bet_amount`, `settled`, `win`, `pnl`).
#' @param results Tibble with columns `sport`, `country`, `sex`,
#'   `match_date`, `home_team`, `away_team`, `home_score`, `away_score`.
#' @return The `bets` tibble with `settled` / `win` / `pnl` updated where
#'   resolvable. Other columns are unchanged.
#' @export
compute_settlement <- function(bets, results) {
  if (nrow(bets) == 0L) {
    return(bets)
  }

  join_cols <- c(
    "sport", "country", "sex", "match_date", "home_team", "away_team"
  )
  res_slim <- results[, c(join_cols, "home_score", "away_score")]
  joined <- dplyr::left_join(bets, res_slim, by = join_cols)

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
        switch(outcome[[i]],
          home = (hg[[i]] + line[[i]]) > ag[[i]],
          away = (ag[[i]] + line[[i]]) > hg[[i]],
          draw = (hg[[i]] + line[[i]]) == ag[[i]],
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
#' @param root Data root for the Parquet stores
#'   (default `here::here("data")`).
#' @return Invisibly, the integer count of rows newly settled.
#' @export
settle_ledger <- function(root = here::here("data")) {
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
  resolved <- compute_settlement(to_settle, results)
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

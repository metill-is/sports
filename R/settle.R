#' @include storage.R
NULL

#' Compute win/pnl for placed bets given match results.
#'
#' Joins bets to results on (sport, country, sex, match_date, home_team,
#' away_team) and resolves win/pnl using the same per-market rules as
#' [build_return_matrix()] in `R/decide-kelly.R` (with `tie_threshold = T`):
#' - moneyline: home wins iff (hg - ag) > T; draw iff |hg - ag| <= T;
#'   away iff (ag - hg) > T.
#' - spread: `line` is the home team's signed handicap (positive = home gets
#'   head start). Define `adj = (hg + line) - ag`. Then home wins iff
#'   adj > T; away wins iff -adj > T; draw wins iff |adj| <= T. Home and
#'   away are mirror images of one adjusted margin -- NOT two independent
#'   handicaps.
#' - total: over wins iff (hg + ag) > line (strict); under wins iff
#'   (hg + ag) < line (strict).
#'
#' For integer-scored sports with `T = 0` (football and basketball default,
#' and handball *for the settle side* since recorded results are integer),
#' the behaviour is equality-as-tie — exactly what the equality form
#' encoded. The threshold matters only at decide-time on the continuous
#' Stan posterior. K6 self-consistency holds because the same comparison
#' is applied with the same threshold on both sides.
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
#' @param tie_threshold Numeric \eqn{\ge 0}. Tie/push band — see the
#'   `build_return_matrix()` docstring. Default `0` preserves the exact
#'   equality semantics for integer-scored realisations. `settle_ledger()`
#'   looks up the threshold per-(sport, country) from `config/leagues.yml`.
#' @return The `bets` tibble with `settled` / `win` / `pnl` updated where
#'   resolvable. Other columns are unchanged.
#' @export
compute_settlement <- function(bets, results, match_date_window_days = 0L,
                               tie_threshold = 0) {
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
    diff <- hg[[i]] - ag[[i]]
    win[[i]] <- switch(market[[i]],
      moneyline = switch(outcome[[i]],
        home = diff > tie_threshold,
        draw = abs(diff) <= tie_threshold,
        away = -diff > tie_threshold,
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
        # branch (same bug as build_return_matrix()). `tie_threshold` keeps
        # the spread push band consistent with moneyline.
        adj <- (hg[[i]] + line[[i]]) - ag[[i]]
        switch(outcome[[i]],
          home = adj > tie_threshold,
          away = -adj > tie_threshold,
          draw = abs(adj) <= tie_threshold,
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

  # K6 self-consistency: settle uses the same tie_threshold the decide layer
  # used for the EV that justified the bet. Per-(sport, country) lookup from
  # leagues.yml — unknown leagues (e.g. paused European cells whose old bets
  # remain in the ledger) default to 0, which matches the pre-fix equality
  # behaviour and is correct for integer-scored realisations regardless.
  leagues_cfg <- tryCatch(load_leagues(), error = function(e) list())
  tt_for <- function(sport, country) {
    for (lg in leagues_cfg) {
      if (identical(lg$sport, sport) && identical(lg$country, country)) {
        return(lg$betting$scoring$tie_threshold %||% 0)
      }
    }
    0
  }

  groups <- dplyr::distinct(to_settle[, c("sport", "country"), drop = FALSE])
  resolved <- to_settle
  for (gi in seq_len(nrow(groups))) {
    sp <- groups$sport[[gi]]
    co <- groups$country[[gi]]
    gmask <- to_settle$sport == sp & to_settle$country == co
    g_resolved <- compute_settlement(
      to_settle[gmask, , drop = FALSE], results,
      match_date_window_days = match_date_window_days,
      tie_threshold = tt_for(sp, co)
    )
    resolved$settled[gmask] <- g_resolved$settled
    resolved$win[gmask] <- g_resolved$win
    resolved$pnl[gmask] <- g_resolved$pnl
  }

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

#' Void (refund) a single unsettled ledger bet.
#'
#' Records a bookmaker void/refund on the canonical Parquet ledger. A void is
#' *not* a graded outcome: the match was never played (postponed / abandoned)
#' and Lengjan refunded the stake, so there is no scraped result for
#' [settle_ledger()] / [compute_settlement()] to join against. This is the only
#' sanctioned way to resolve such a row — the settle layer joins exclusively to
#' results and will never touch a match that has no result.
#'
#' Identifies exactly one ledger row by the natural key (`sport`, `country`,
#' `sex`, `match_date`, `home_team`, `away_team`, `market`), optionally narrowed
#' by `line` / `outcome` / `placed_at` when the key alone is ambiguous (e.g. two
#' total bets on one fixture at different lines). Flips **only**
#' `settled = TRUE`, `win = NA`, `pnl = 0`; every frozen field (`odds_placed`,
#' `bet_amount`, `outcome`, `line`, `match_date`, `p`, `kelly`, ...) is left
#' untouched (L3/L4). A void is a stake refund, not a loss — hence `pnl = 0`,
#' and `win = NA` rather than `FALSE` so a void never pollutes the calibration
#' win-rate.
#'
#' Errors (never silently mutates) when the key matches zero rows, matches more
#' than one row, or matches a row that is already settled (L4 — settled rows are
#' immutable; never re-settle).
#'
#' Mirrors the partition-replace write at the tail of [settle_ledger()]:
#' `write_table(..., "ledger")` overwrites whole `(sport, country)` partitions,
#' so the full partition slice is re-written, not the single voided row (which
#' would wipe its partition-mates). Like [settle_ledger()] this never touches
#' git — the script-layer wrapper or the documented one-liner in
#' `docs/runbooks/orphaned-bet.md` calls [commit_ledger_changes()] afterwards.
#'
#' @param root Data root for the Parquet stores (default `here::here("data")`).
#' @param sport,country,sex,match_date,home_team,away_team,market The natural
#'   key identifying the bet. `match_date` is coerced with [as.Date()].
#' @param line,outcome,placed_at Optional disambiguators, each `NULL` (ignored)
#'   by default. Supply one or more when the natural key alone matches more
#'   than one ledger row.
#' @return Invisibly, the single voided ledger row (post-flip) as a one-row
#'   tibble.
#' @export
void_bet <- function(root = here::here("data"),
                     sport, country, sex, match_date,
                     home_team, away_team, market,
                     line = NULL, outcome = NULL, placed_at = NULL) {
  led <- tryCatch(read_table("ledger", root = root), error = function(e) NULL)
  if (is.null(led) || nrow(led) == 0L) {
    stop("void_bet: ledger is empty or missing; nothing to void.", call. = FALSE)
  }

  match_date <- as.Date(match_date)
  mask <- led$sport == sport & led$country == country & led$sex == sex &
    led$match_date == match_date & led$home_team == home_team &
    led$away_team == away_team & led$market == market
  if (!is.null(line)) {
    mask <- mask & !is.na(led$line) & led$line == line
  }
  if (!is.null(outcome)) {
    mask <- mask & led$outcome == outcome
  }
  if (!is.null(placed_at)) {
    mask <- mask & led$placed_at == placed_at
  }

  idx <- which(mask)
  n <- length(idx)
  if (n == 0L) {
    stop(
      "void_bet: matched 0 ledger rows for the given key. ",
      "Check the natural-key fields against the ledger.",
      call. = FALSE
    )
  }
  if (n > 1L) {
    stop(
      sprintf(
        paste0(
          "void_bet: key is ambiguous -- matched %d rows. ",
          "Disambiguate with line / outcome / placed_at."
        ),
        n
      ),
      call. = FALSE
    )
  }
  if (isTRUE(led$settled[[idx]])) {
    stop(
      "void_bet: the matched ledger row is already settled ",
      "(L4 -- settled rows are immutable; never re-settle).",
      call. = FALSE
    )
  }

  led$settled[idx] <- TRUE
  led$win[idx] <- NA
  led$pnl[idx] <- 0

  part_mask <- led$sport == sport & led$country == country
  write_table(led[part_mask, , drop = FALSE], "ledger", root = root)

  invisible(led[idx, , drop = FALSE])
}

# Null-coalescing (R 4.4+ has a base `%||%` but the package targets R >= 4.0).
`%||%` <- function(x, y) if (is.null(x)) y else x

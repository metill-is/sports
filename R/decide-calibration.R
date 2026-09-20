#' @include storage.R
NULL

#' Bayesian Beta-Binomial calibration multiplier for one (league, sex).
#'
#' Reads settled bets from `data/decisions/ledger/` and computes a
#' multiplier that scales the base kelly_frac in `leagues.yml`. Pseudo-count
#' Beta-Binomial smoothing starts updating from bet 1.
#'
#' multiplier = (prior_weight * prior_ratio + sum(win)) / (prior_weight + sum(p))
#'
#' Falls back to `prior_ratio` when no settled history exists at any of three
#' points: the ledger directory is absent, no rows are present for the
#' (sport, country) partition, or no rows survive the (sex, settled, non-NA)
#' filter. The return carries WHICH of those happened, because the multiplier
#' alone cannot say: "no settled history at all", "400 settled bets and
#' genuinely well calibrated" and "evidence so bad the raw ratio was clamped up
#' to the floor" all reduce to one number on a real-money staking path. `n`,
#' `raw_ratio`, `clamped` and `basis` are what keep them distinguishable.
#'
#' @param league List with `sport` + `country`.
#' @param sex "male" or "female".
#' @param market Optional character. When supplied, restrict the ledger to
#'   that market before computing the multiplier — implements the K2
#'   invariant's "split by market only at ≥ N settled bets per market"
#'   when the caller has already decided which market deserves a split.
#'   For the operator-facing dispatcher that picks per-market vs aggregate
#'   automatically, see [compute_calibrations()].
#' @param root Data root. Default `here::here("data")`.
#' @param prior_weight Pseudo-count strength — equivalent expected wins of
#'   prior data. Higher = slower adaptation. Default 30 (vs legacy 10) was
#'   chosen to resist single-season noise: with ~300 settled bets per
#'   (league, sex), prior_weight = 30 keeps the multiplier near-1.0 unless
#'   the sample is large or strongly biased; prior_weight = 10 would track
#'   short runs of luck more aggressively.
#' @param prior_ratio Prior calibration ratio. 1.0 = model is well-calibrated.
#'   Default 1.0. Also the multiplier returned in all empty-history paths.
#' @param floor Lower clamp on multiplier. Default 0.5.
#' @param ceiling Upper clamp. Default 1.5.
#' @return A list of the multiplier AND the evidence behind it:
#'   * `multiplier` — numeric in `[floor, ceiling]`, rounded to 3 decimals.
#'     This is the value that scales `kelly_frac`.
#'   * `n` — integer count of settled bets the estimate rests on; `0` on every
#'     empty-history path.
#'   * `raw_ratio` — the unclamped, unrounded Beta-Binomial ratio, or
#'     `NA_real_` when there was no evidence to compute one.
#'   * `clamped` — `TRUE` when `floor`/`ceiling` bit, i.e. `multiplier` is a
#'     boundary rather than what the ledger actually estimated.
#'   * `basis` — one of `"evidence"`, `"no_ledger_dir"`, `"no_partition_rows"`,
#'     `"no_settled_rows"`, `"ledger_read_error"`.
#' @export
compute_calibration <- function(league, sex,
                                market = NULL,
                                root = here::here("data"),
                                prior_weight = 30, prior_ratio = 1.0,
                                floor = 0.5, ceiling = 1.5) {
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$sport), !is.null(league$country))

  # Every no-evidence exit returns the same multiplier a perfectly calibrated
  # long history would. `basis` is the only thing that tells them apart, so it
  # is set on each one rather than left to the caller to infer.
  no_evidence <- function(basis) {
    list(
      multiplier = prior_ratio, n = 0L, raw_ratio = NA_real_,
      clamped = FALSE, basis = basis
    )
  }

  ledger_dir <- file.path(root, "decisions", "ledger")
  if (!dir.exists(ledger_dir)) {
    return(no_evidence("no_ledger_dir"))
  }

  # Distinguish "no data yet" (silent fall-through) from "data exists but
  # unreadable" (signal to the caller via message + `basis`). read_table itself
  # returns an empty tibble for missing-partition cases, so any error here is
  # genuine.
  read_failed <- FALSE
  led <- tryCatch(
    read_table("ledger",
      root = root,
      filter = list(sport = league$sport, country = league$country)
    ),
    error = function(e) {
      read_failed <<- TRUE
      cli::cli_warn(c(
        "compute_calibration: ledger read failed; falling back to prior_ratio",
        "i" = "{conditionMessage(e)}"
      ))
      tibble::tibble()
    }
  )

  if (nrow(led) == 0L) {
    return(no_evidence(if (read_failed) "ledger_read_error" else "no_partition_rows"))
  }

  # Filter to settled bets matching sex with non-NA win + p.
  led <- led[!is.na(led$sex) & led$sex == sex, , drop = FALSE]
  led <- led[!is.na(led$settled) & led$settled, , drop = FALSE]
  led <- led[!is.na(led$win) & !is.na(led$p), , drop = FALSE]
  if (!is.null(market)) {
    led <- led[!is.na(led$market) & led$market == market, , drop = FALSE]
  }

  if (nrow(led) == 0L) {
    return(no_evidence("no_settled_rows"))
  }

  # sum() on a logical vector counts TRUEs; the prior !is.na guard already
  # cleared NAs so the explicit na.rm is redundant but harmless.
  actual_wins <- sum(led$win)
  expected_wins <- sum(led$p)

  raw_ratio <- (prior_weight * prior_ratio + actual_wins) /
    (prior_weight + expected_wins)

  clamped <- raw_ratio < floor || raw_ratio > ceiling
  multiplier <- round(max(floor, min(ceiling, raw_ratio)), 3)

  if (clamped) {
    # A clamp is the ledger's own estimate being overruled: the stake that
    # goes out is the boundary, not what the evidence said. Silently returning
    # the boundary made a 0.31 ratio indistinguishable from a floor-grazing
    # 0.49, and both from a deliberate floor setting -- say it out loud.
    cell <- paste0(
      league$sport, "/", league$country, " ", sex,
      if (is.null(market)) "" else paste0(" [", market, "]")
    )
    cli::cli_alert_warning(
      "compute_calibration: {cell} raw ratio {round(raw_ratio, 3)} clamped to \\
       {multiplier} on n = {nrow(led)} settled bet{?s}."
    )
  }

  list(
    multiplier = multiplier, n = nrow(led), raw_ratio = raw_ratio,
    clamped = clamped, basis = "evidence"
  )
}

#' Resolve K2 — per-market calibration multipliers with aggregate fallback.
#'
#' Returns a named list with an `aggregate` multiplier plus a per-market
#' multiplier for each market that meets the K2 threshold of `k2_min_n`
#' settled bets. The caller looks up each candidate bet's market in the
#' list, falling back to `aggregate` when the per-market entry is absent.
#'
#' Pre-2026-05-15 `decide_league()` applied a single aggregate multiplier
#' across all markets — an active heterogeneity-masking pattern that K2 in
#' `.claude/rules/sports-betting.md` explicitly forbids when sample size
#' allows the split. Empirical decomposition at audit time (2026-05-15)
#' showed `football_iceland / male` aggregate 0.86 but moneyline 0.81 vs
#' total 0.89 — within-cell heterogeneity above the K2 threshold the docs
#' claim to enforce. Audit §C.
#'
#' @param league List with `sport` + `country`.
#' @param sex "male" or "female".
#' @param k2_min_n Minimum settled bets per market before splitting out.
#'   Default `100` per K2 in `sports-betting.md`.
#' @param root Data root. Default `here::here("data")`.
#' @param ... Forwarded to [compute_calibration()] (prior_weight, prior_ratio,
#'   floor, ceiling).
#' @return Named list of bare numeric multipliers, with `aggregate` always
#'   present and per-market keys only where `n >= k2_min_n`. Per-market keys
#'   mirror the `market` column of the recommendations Parquet (`moneyline`,
#'   `spread`, `total`, ...). The per-key evidence records from
#'   [compute_calibration()] (`n`, `raw_ratio`, `clamped`, `basis`) ride along
#'   in the `"evidence"` attribute, so a caller can audit WHY a multiplier is
#'   what it is; the list values themselves stay scalar because `decide_league()`
#'   indexes them per bet with `vapply(..., numeric(1))`.
#' @export
compute_calibrations <- function(league, sex,
                                 k2_min_n = 100L,
                                 root = here::here("data"),
                                 ...) {
  stopifnot(sex %in% c("male", "female"))
  agg <- compute_calibration(league, sex, root = root, ...)
  out <- list(aggregate = agg$multiplier)
  evidence <- list(aggregate = agg)
  # Every exit below is an early return, so attach the evidence in one place
  # rather than at four; the closure reads `out`/`evidence` as they stand.
  finish <- function() {
    attr(out, "evidence") <- evidence
    out
  }

  # Discover which markets cross the K2 threshold for this (sport, country, sex).
  ledger_dir <- file.path(root, "decisions", "ledger")
  if (!dir.exists(ledger_dir)) {
    return(finish())
  }
  led <- tryCatch(
    read_table("ledger",
      root = root,
      filter = list(sport = league$sport, country = league$country)
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(led) == 0L) {
    return(finish())
  }
  led <- led[!is.na(led$sex) & led$sex == sex, , drop = FALSE]
  led <- led[!is.na(led$settled) & led$settled, , drop = FALSE]
  led <- led[!is.na(led$win) & !is.na(led$p), , drop = FALSE]
  if (nrow(led) == 0L) {
    return(finish())
  }

  counts <- table(led$market)
  for (mkt in names(counts)) {
    if (counts[[mkt]] >= k2_min_n) {
      res <- compute_calibration(
        league, sex,
        market = mkt, root = root, ...
      )
      out[[mkt]] <- res$multiplier
      evidence[[mkt]] <- res
    }
  }
  finish()
}

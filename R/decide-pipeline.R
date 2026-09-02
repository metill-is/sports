#' @include decide-odds.R decide-kelly.R decide-portfolio.R decide-calibration.R config.R storage.R
NULL

#' Decide pipeline: beliefs + odds + bankroll -> candidates + recommendations.
#'
#' Chains prepare_odds + kelly_joint + portfolio_optimise + compute_calibration,
#' then writes `candidates` (terminal-state audit log) and `recommendations`
#' (post-filter survivors) Parquet via write_table().
#'
#' The `stage` column on candidates is the bet's terminal classification:
#'   - `kept`: passed all filters; appears in recommendations
#'   - `dropped_low_ev`: kelly_joint zeroed it (ev below ev_threshold)
#'   - `dropped_min_bet`: bet_amount fell below `betting$min_bet`
#'   - `dropped_market_off`: market disabled in `betting$markets` toggles
#'   - `dropped_invalid_input`: odds non-finite/<=1 or posterior p non-finite
#' The pipeline does not emit per-step intermediate stages — the final stage
#' captures the decisive filter.
#'
#' @param league_key Key into load_leagues(). Mutually exclusive with `league`.
#' @param league Pre-loaded league list. Mutually exclusive with `league_key`.
#' @param sex "male" or "female".
#' @param run_date Stamped on every row's run_id partition. Default today.
#' @param root Data root. Default here::here("data").
#' @param bankroll Pre-loaded bankroll list (NULL = load_bankroll()).
#' @param write Write candidates + recommendations? Default TRUE.
#' @param max_age_hours Odds-staleness window forwarded to [prepare_odds()].
#'   `NULL` (default) uses the league's `betting$max_age_hours` (or 48h),
#'   preserving live behaviour. The walk-forward harness passes a large value so
#'   a historical decide (run weeks after its cutoff) can still see the
#'   pre-match odds it pre-sliced into the isolated root.
#' @param return_candidates When `TRUE`, return the FULL candidates tibble (all
#'   stages, with `stage`/`ev`/`kelly_raw`) instead of just kept recommendations.
#'   The walk-forward harness needs every bettable candidate (each carries a
#'   model `p`) for its OOS calibration arm, then keys PnL off `stage == "kept"`.
#'   Default `FALSE` preserves the live return (kept recommendations only).
#' @return Recommendations (kept bets) invisibly, or the full candidates tibble
#'   when `return_candidates = TRUE`.
#' @export
decide_league <- function(league_key = NULL, league = NULL, sex,
                          run_date = Sys.Date(),
                          root = here::here("data"),
                          bankroll = NULL,
                          write = TRUE,
                          max_age_hours = NULL,
                          return_candidates = FALSE) {
  # 1. Resolve league -------------------------------------------------------
  if (is.null(league) == is.null(league_key)) {
    stop("Exactly one of `league_key` or `league` must be supplied",
      call. = FALSE
    )
  }
  if (is.null(league)) {
    leagues <- load_leagues()
    if (!league_key %in% names(leagues)) {
      stop("Unknown league: ", league_key, call. = FALSE)
    }
    league <- leagues[[league_key]]
  }

  # 2. Validate inputs ------------------------------------------------------
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$betting))

  if (is.null(bankroll)) bankroll <- load_bankroll(ledger_root = root)

  betting <- league$betting
  run_id <- as.POSIXct(format(run_date), tz = "UTC")

  # An early exit returns the candidate-shaped empty tibble (with `stage`) when
  # the caller asked for candidates, so the harness's bind_rows stays schema-safe.
  empty_return <- function() {
    if (isTRUE(return_candidates)) empty_candidates() else empty_recommendations()
  }

  # 2b. Betting interlock ---------------------------------------------------
  # D2 (spec 2026-09-02 section 3): a league may be modelled and published
  # without being bet. Refuse here rather than relying on an empty odds store,
  # because "no odds today" and "never bet this league" must not look alike.
  # decide_league() is the single funnel for decide_one(), scripts/04_decide.R,
  # R/backtest-walkforward.R and the replay script, so one guard covers all of
  # them. The placer keeps its own guards: recommendations written before a
  # league was disarmed outlive the config change.
  if (!betting_enabled(league)) {
    cli::cli_alert_info(
      "{league$sport}/{league$country}/{sex}: betting disabled \\
       (betting.enabled: false) -- no candidates or recommendations"
    )
    # Mirrors the two sibling early-exits below. Note this is currently a
    # no-op: decide_write_empty() passes zero-row frames to write_table(),
    # which returns early without creating a partition. Called anyway so this
    # path stays consistent with them if that ever changes.
    if (write) decide_write_empty(league, sex, run_id, root)
    return(empty_return())
  }

  # 3. Read beliefs ---------------------------------------------------------
  beliefs <- tryCatch(
    read_table("beliefs_latest",
      root = root,
      filter = list(
        sport = league$sport, country = league$country,
        sex = sex
      )
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(beliefs) == 0L) {
    warning(
      "decide_league: no beliefs for ",
      league$sport, "/", league$country, "/", sex,
      " \u2014 skipping. Run scripts/fit_all.R first.",
      call. = FALSE
    )
    return(invisible(empty_return()))
  }

  # 4. Odds + market toggles ------------------------------------------------
  odds <- prepare_odds(league, sex,
    end_date = run_date,
    max_age_hours = max_age_hours %||% betting$max_age_hours %||% 48,
    root = root
  )
  if (nrow(odds) == 0L) {
    if (write) decide_write_empty(league, sex, run_id, root)
    return(invisible(empty_return()))
  }

  # Markets toggle: drop bets where markets[[market]] is FALSE
  market_kept <- vapply(odds$market, function(m) isTRUE(betting$markets[[m]]), logical(1))
  odds_on <- odds[market_kept, , drop = FALSE]
  odds_off <- odds[!market_kept, , drop = FALSE]

  if (nrow(odds_on) == 0L) {
    cand_off <- annotate_market_off(odds_off, league, sex, run_id)
    if (write) {
      write_table(cand_off, "candidates", root = root)
      write_table(empty_recommendations(), "recommendations", root = root)
    }
    return(invisible(if (isTRUE(return_candidates)) cand_off else empty_recommendations()))
  }

  # 5. Per-match Kelly packages --------------------------------------------
  kelly_frac_val <- if (is.list(betting$kelly_frac)) {
    betting$kelly_frac[[sex]]
  } else {
    betting$kelly_frac
  }
  if (is.null(kelly_frac_val)) {
    stop("decide_league: no kelly_frac for sex=", sex, call. = FALSE)
  }

  match_groups <- unique(odds_on[, c("match_date", "home_team", "away_team"), drop = FALSE])
  packages <- list()
  candidates_list <- list()
  market_off_rows <- if (nrow(odds_off) > 0L) {
    annotate_market_off(odds_off, league, sex, run_id)
  } else {
    NULL
  }

  for (i in seq_len(nrow(match_groups))) {
    md <- match_groups$match_date[i]
    ht <- match_groups$home_team[i]
    at <- match_groups$away_team[i]

    mb <- beliefs[
      beliefs$match_date == md &
        beliefs$home_team == ht &
        beliefs$away_team == at, ,
      drop = FALSE
    ]
    if (nrow(mb) == 0L) {
      cli::cli_alert_warning(
        "decide: no beliefs for {league$sport}/{league$country}/{sex} {md} {ht}-{at} (skipping)"
      )
      next
    }

    mb_odds <- odds_on[
      odds_on$match_date == md &
        odds_on$home_team == ht &
        odds_on$away_team == at, ,
      drop = FALSE
    ]
    bets_in <- mb_odds[, c("market", "outcome", "line", "odds"), drop = FALSE]

    kj <- kelly_joint(
      mb[, c("draw_id", "home_goals", "away_goals"), drop = FALSE],
      bets_in,
      max_match_stake = betting$max_match_stake %||%
        (bankroll$max_match_stake_default %||% 1.0),
      ev_threshold = betting$ev_threshold %||% 0.0,
      tie_threshold = betting$scoring$tie_threshold %||% 0
    )

    match_key <- paste(md, ht, at, sep = "||")
    packages[[match_key]] <- list(
      match_key = match_key,
      match_pnl = kj$match_pnl,
      match_kelly_sum = kj$match_kelly_sum,
      kelly_frac = kelly_frac_val
    )

    cand <- tibble::tibble(
      run_id = run_id,
      sport = league$sport,
      country = league$country,
      sex = sex,
      match_date = md,
      home_team = ht,
      away_team = at,
      market = kj$bets$market,
      outcome = kj$bets$outcome,
      line = kj$bets$line,
      p = kj$bets$p,
      odds = kj$bets$odds,
      ev = kj$bets$ev,
      kelly_raw = kj$bets$kelly_raw,
      valid_input = kj$diagnostics$valid_input,
      stage = ifelse(kj$bets$kelly_raw > 0, "candidate", "dropped_low_ev")
    )
    candidates_list[[match_key]] <- cand
  }

  if (length(packages) == 0L) {
    if (write) decide_write_empty(league, sex, run_id, root)
    return(invisible(empty_return()))
  }

  # 7. Portfolio scaling ---------------------------------------------------
  port <- portfolio_optimise(packages,
    max_daily = bankroll$daily_budget_frac,
    mode = "optimal"
  )
  lambdas <- port$lambdas

  # 8. Calibration --------------------------------------------------------
  # K2: per-market multiplier when n_settled >= 100 per market, aggregate
  # otherwise. Pre-2026-05-15 the aggregate ran across all markets, masking
  # heterogeneity the rule file claimed to enforce. Audit §C.
  calibs <- compute_calibrations(league, sex, root = root)

  # 9-11. Apply per-bet final kelly + bet_amount + min_bet ----------------
  cands <- dplyr::bind_rows(candidates_list)

  # Build lookup key for lambda (match_key = "date||home||away")
  mk <- paste(cands$match_date, cands$home_team, cands$away_team, sep = "||")
  cands$lambda <- lambdas[mk]

  # Per-bet calibration: per-market when K2 promoted it, aggregate otherwise.
  cands$calib <- vapply(cands$market, function(m) {
    val <- calibs[[m]]
    if (is.null(val)) calibs[["aggregate"]] else val
  }, numeric(1))

  # Restored §7.2 multiplicative chain with K5 ceiling clamp:
  #   shrink_eff = min(kelly_frac × calibration, kelly_ceiling)
  #   kelly      = kelly_raw × portfolio_lambda × shrink_eff
  # The optimiser ran unconstrained at max_match_stake (default 1.0); the
  # post-hoc kelly_frac multiplier is the Browne-finite-horizon hedge against
  # model misspecification. K5 ensures kelly_frac × calibration never exceeds
  # 0.25 even with calibration drift up to ceiling 1.5.
  kelly_ceiling <- bankroll$kelly_ceiling %||% 0.25
  shrink_eff <- pmin(kelly_frac_val * cands$calib, kelly_ceiling)
  cands$kelly <- cands$kelly_raw * cands$lambda * shrink_eff
  cands$bet_amount <- round(cands$kelly * bankroll$current_pool)

  # Stage: dropped_low_ev already set; update remaining.
  # WHY (audit 2026-05-16 follow-up): the filter compares post-round
  # `bet_amount` to `min_bet` because `min_bet = 200` is Lengjan's external
  # hard floor — bets under 200 ISK are rejected at the bookmaker. The
  # rounded `bet_amount` is what we actually send. A raw Kelly of 199.5
  # rounds to 200, which IS Lengjan-acceptable, so keep it; 199.4 rounds
  # to 199, which is not. Do NOT switch to pre-round comparison even
  # though it looks more monotonic — that would drop bets that round up
  # to a valid stake.
  cands$stage <- dplyr::case_when(
    !cands$valid_input ~ "dropped_invalid_input",
    cands$kelly_raw <= 0 ~ "dropped_low_ev",
    cands$bet_amount < (betting$min_bet %||% 0) ~ "dropped_min_bet",
    TRUE ~ "kept"
  )

  # Append any market-off rows
  all_cands <- if (!is.null(market_off_rows)) {
    dplyr::bind_rows(
      cands[, c(
        "run_id", "sport", "country", "sex", "match_date",
        "home_team", "away_team", "market", "outcome", "line",
        "p", "odds", "ev", "kelly_raw", "stage"
      ),
      drop = FALSE
      ],
      market_off_rows
    )
  } else {
    cands[, c(
      "run_id", "sport", "country", "sex", "match_date",
      "home_team", "away_team", "market", "outcome", "line",
      "p", "odds", "ev", "kelly_raw", "stage"
    ),
    drop = FALSE
    ]
  }

  # 12+13. Write tables ---------------------------------------------------
  recs <- cands[cands$stage == "kept", , drop = FALSE]

  if (write) {
    write_table(all_cands, "candidates", root = root)
    rec_out <- recs[, c(
      "run_id", "sport", "country", "sex",
      "match_date", "home_team", "away_team",
      "market", "outcome", "line", "p", "odds",
      "ev", "kelly", "bet_amount"
    ),
    drop = FALSE
    ]
    write_table(rec_out, "recommendations", root = root)
  }

  # 14. Return: full candidates (harness) or kept recommendations (live) -------
  if (isTRUE(return_candidates)) {
    return(invisible(all_cands))
  }
  recs_out <- recs[, c(
    "run_id", "sport", "country", "sex",
    "match_date", "home_team", "away_team",
    "market", "outcome", "line", "p", "odds",
    "ev", "kelly", "bet_amount"
  ),
  drop = FALSE
  ]
  invisible(recs_out)
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
empty_recommendations <- function() {
  tibble::tibble(
    run_id = as.POSIXct(character(), tz = "UTC"),
    sport = character(), country = character(), sex = character(),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), p = numeric(), odds = numeric(),
    ev = numeric(), kelly = numeric(), bet_amount = numeric()
  )
}

#' @keywords internal
#' @noRd
empty_candidates <- function() {
  tibble::tibble(
    run_id = as.POSIXct(character(), tz = "UTC"),
    sport = character(), country = character(), sex = character(),
    match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    market = character(), outcome = character(),
    line = numeric(), p = numeric(), odds = numeric(),
    ev = numeric(), kelly_raw = numeric(),
    stage = character()
  )
}

#' @keywords internal
#' @noRd
decide_write_empty <- function(league, sex, run_id, root) {
  write_table(empty_candidates(), "candidates", root = root)
  write_table(empty_recommendations(), "recommendations", root = root)
}

#' @keywords internal
#' @noRd
annotate_market_off <- function(odds, league, sex, run_id) {
  tibble::tibble(
    run_id = run_id,
    sport = league$sport,
    country = league$country,
    sex = sex,
    match_date = odds$match_date,
    home_team = odds$home_team,
    away_team = odds$away_team,
    market = odds$market,
    outcome = odds$outcome,
    line = odds$line,
    p = NA_real_,
    odds = odds$odds,
    ev = NA_real_,
    kelly_raw = 0,
    stage = "dropped_market_off"
  )
}

#' Decide for a single (league x sex).
#'
#' Takes the static + lengjan + betting slices separately to keep callers
#' from re-loading the full leagues config per call. decide_league() reads
#' its inputs from data/ Parquet.
#'
#' @param static Per-league static slice (sport, country, sexes, data_source).
#' @param lengjan Per-league `lengjan` slice (competitions + team_names).
#' @param betting Per-league `betting` slice.
#' @param sex `"male"` or `"female"`.
#' @param bankroll Output of `load_bankroll()`.
#' @return Integer count of recommendation rows.
#' @export
decide_one <- function(static, lengjan, betting, sex, bankroll) {
  league <- static
  league$lengjan <- lengjan
  league$betting <- betting
  recs <- decide_league(league = league, sex = sex, bankroll = bankroll)
  nrow(recs)
}

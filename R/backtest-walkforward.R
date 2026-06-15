#' Out-of-sample probability scores: Brier and log-loss over frozen as-of `p`.
#'
#' The PRIMARY walk-forward verdict. Rows with `NA` win (bets that never
#' settled within the horizon) are dropped before scoring. `p` is clamped to
#' `[eps, 1-eps]` so a degenerate 0/1 forecast cannot send log-loss to Inf.
#' @param settled Tibble with numeric `p` and logical `win`.
#' @param eps Clamp bound. Default `1e-6`.
#' @return One-row tibble `(n, brier, log_loss)`; `n = 0` -> NA scores.
#' @export
bt_oos_scores <- function(settled, eps = 1e-6) {
  # Scoring all candidates means market-off (p = NA) and invalid-input (p = NaN)
  # rows reach here; they carry no model probability, so drop alongside NA wins.
  d <- settled[!is.na(settled$win) & is.finite(settled$p), , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble::tibble(n = 0L, brier = NA_real_, log_loss = NA_real_))
  }
  p <- pmin(pmax(d$p, eps), 1 - eps)
  y <- as.numeric(d$win)
  tibble::tibble(
    n = nrow(d),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p))
  )
}

#' Drop odds snapshots scraped on or after each match's OWN kickoff day (G3/G4).
#'
#' `prepare_odds` bounds `scraped_at` from below only and takes
#' `slice_max(scraped_at)`, so without an upper bound the decider could select a
#' post-result snapshot. The leak-free boundary is each match's own match day,
#' NOT the training cutoff `d`: Lengjan posts odds ~2 days before kickoff, so an
#' OOS match 5-14 days after `d` has every one of its pre-match snapshots scraped
#' well after `d`. The old `scraped_at <= d + 12h` rule therefore emptied the OOS
#' odds set entirely (the 0-bets bug). A snapshot scraped any day BEFORE the
#' match cannot embed the result; one on or after the match day can, so it is
#' dropped. `slice_max(scraped_at)` downstream then takes the latest survivor =
#' the closing-ish line. Writing the result into the isolated `wf_root` makes the
#' decider structurally unable to see a match-day/post-result snapshot.
#' @param odds Tibble from `read_table("odds")` (has `scraped_at`, `match_date`).
#' @return `odds` restricted to snapshots scraped strictly before their match day.
#' @export
bt_wf_slice_odds <- function(odds) {
  if (nrow(odds) == 0L) {
    return(odds)
  }
  scrape_day <- as.Date(odds$scraped_at, tz = "UTC")
  odds[scrape_day < odds$match_date, , drop = FALSE]
}

#' Large `max_age_hours` so prepare_odds' lower bound never drops a
#' legitimately-old historical snapshot once the upper bound is enforced by
#' [bt_wf_slice_odds()] (G3). ~10 years.
#' @return Numeric hours.
#' @export
bt_wf_max_age_hours <- function() 24 * 365 * 10

#' Restrict OOS candidates to matches STRICTLY after the cutoff, within horizon.
#'
#' G1/G2: neutralises the non-strict `schedules >= d` / `odds match_date >= d`
#' operators even if a cutoff coincides with a match day. A day-`d` match is in
#' the (inclusive) training set, so it must never be a bet.
#' @param candidates Tibble with `match_date`.
#' @param d Training cutoff date.
#' @param horizon_days OOS window length.
#' @return Candidates with `match_date > d & match_date <= d + horizon_days`.
#' @export
bt_wf_filter_oos <- function(candidates, d, horizon_days) {
  if (nrow(candidates) == 0L) {
    return(candidates)
  }
  d <- as.Date(d)
  hi <- d + as.integer(horizon_days)
  candidates[candidates$match_date > d & candidates$match_date <= hi, , drop = FALSE]
}

#' Assert the OOS bet match-set is disjoint from the training match-set (G1).
#'
#' Training = results with `match_date <= d` (what prepare_data trained on).
#' @param oos OOS candidates (`match_date`, `home_team`, `away_team`).
#' @param results Full results store.
#' @param d Training cutoff.
#' @return `TRUE` if disjoint.
#' @export
bt_wf_training_disjoint <- function(oos, results, d) {
  d <- as.Date(d)
  trn <- results[results$match_date <= d, , drop = FALSE]
  tkey <- paste(trn$match_date, trn$home_team, trn$away_team, sep = "\r")
  okey <- paste(oos$match_date, oos$home_team, oos$away_team, sep = "\r")
  length(intersect(tkey, okey)) == 0L
}

#' As-of ledger view: rows whose match resolved strictly before `d` (G5).
#'
#' Feeds calibration so `compute_calibrations` cannot peek at bets that
#' settled after the cutoff. Approximates settle-date by `match_date` (the
#' ledger has no settle timestamp); `match_date < d` is the conservative cut.
#' @param ledger Ledger tibble (or empty/NULL).
#' @param d Cutoff date.
#' @return Settled ledger rows with `match_date < d`.
#' @export
bt_wf_ledger_asof <- function(ledger, d) {
  if (is.null(ledger) || nrow(ledger) == 0L) {
    return(ledger[0, , drop = FALSE])
  }
  d <- as.Date(d)
  keep <- !is.na(ledger$settled) & ledger$settled & ledger$match_date < d
  ledger[keep, , drop = FALSE]
}

#' Drop OOS candidates with no pre-cutoff odds snapshot (G8).
#'
#' A fixture whose only stored date is a post-`d` schedule revision has no
#' pre-cutoff odds and is not a bettable as-of match. Keying on the selection
#' identity sidesteps phantom rescheduled fixtures.
#' @param candidates OOS candidates.
#' @param sliced_odds Output of [bt_wf_slice_odds()].
#' @return Candidates that have a matching pre-cutoff odds snapshot.
#' @export
bt_wf_require_pre_cutoff_odds <- function(candidates, sliced_odds) {
  if (nrow(candidates) == 0L || nrow(sliced_odds) == 0L) {
    return(candidates[0, , drop = FALSE])
  }
  ok <- paste(sliced_odds$match_date, sliced_odds$home_team,
    sliced_odds$away_team, sliced_odds$market, sliced_odds$outcome,
    sliced_odds$line,
    sep = "\r"
  )
  ck <- paste(candidates$match_date, candidates$home_team,
    candidates$away_team, candidates$market, candidates$outcome,
    candidates$line,
    sep = "\r"
  )
  candidates[ck %in% ok, , drop = FALSE]
}

#' Score one walk-forward cutoff inside an isolated tempdir.
#'
#' G6: builds a fresh `wf_root` tempdir, seeds pre-`d` results + a pre-sliced
#' odds store into it; the live root is never written. `decide_fn` defaults to
#' the real fit+decide closure ([bt_wf_default_decide()]); tests inject a fake
#' to exercise the orchestration without Stan. G7: the universe is the decide
#' return, never [bt_load_universe()]. G9: settlement uses window 0.
#' @param sex,d,horizon_days Cutoff parameters.
#' @param results,odds,ledger Pre-loaded stores (`ledger` may be NULL).
#' @param live_root Production data root (read-only here).
#' @param decide_fn Closure `(root, run_date, sex, ledger_asof)` -> candidate
#'   tibble. Default fits as-of then decides.
#' @param tie_threshold Per-(sport,country) push band (football = 0).
#' @return Scored OOS tibble (`cutoff`, `run_id`, candidate cols, `win`).
#' @export
bt_walkforward_cutoff <- function(sex, d, horizon_days,
                                  results, odds, ledger = NULL,
                                  live_root = here::here("data"),
                                  decide_fn = bt_wf_default_decide,
                                  tie_threshold = 0) {
  d <- as.Date(d)
  wf_root <- withr::local_tempdir()

  pre_results <- results[results$match_date <= (d + as.integer(horizon_days)), , drop = FALSE]
  bt_wf_seed_results(pre_results, wf_root)

  sliced <- bt_wf_slice_odds(odds)
  if (nrow(sliced) > 0L) write_table(sliced, "odds", root = wf_root)

  led_asof <- bt_wf_ledger_asof(ledger, d)

  cands <- decide_fn(root = wf_root, run_date = d, sex = sex, ledger_asof = led_asof)
  cands <- bt_wf_filter_oos(cands, d, horizon_days)
  cands <- bt_wf_require_pre_cutoff_odds(cands, sliced)
  if (nrow(cands) == 0L) {
    cands$cutoff <- as.Date(character())
    cands$run_id <- as.POSIXct(character(), tz = "UTC")
    cands$win <- logical()
    cands$kept <- logical()
    return(cands)
  }

  bets <- cands
  bets$sport <- "football"
  bets$country <- "iceland"
  bets$sex <- sex
  bets$odds_placed <- bets$odds
  bets$bet_amount <- 1
  bets$settled <- FALSE
  bets$win <- NA
  bets$pnl <- NA_real_
  settled <- compute_settlement(bets, results,
    match_date_window_days = 0L, tie_threshold = tie_threshold
  )
  cands$win <- settled$win
  # `kept` splits the two arms: calibration (bt_oos_scores) scores all candidates
  # with a finite p; PnL (bt_metrics) is restricted to the placed subset. A fake
  # decide_fn without a `stage` column (the pure tests) -> all rows count as kept.
  cands$kept <- if ("stage" %in% names(cands)) cands$stage == "kept" else TRUE
  cands$cutoff <- d
  cands$run_id <- as.POSIXct(format(d), tz = "UTC")
  cands
}

#' Seed a results store into an isolated tempdir for prepare_data.
#' @noRd
bt_wf_seed_results <- function(results, root) {
  if (nrow(results) == 0L) {
    return(invisible(NULL))
  }
  res_root <- file.path(root, "facts", "results")
  fs::dir_create(res_root, recurse = TRUE)
  arrow::write_dataset(results,
    path = res_root, format = "parquet",
    partitioning = c("sport", "country", "sex", "season"),
    existing_data_behavior = "overwrite"
  )
  invisible(NULL)
}

#' Default decide closure: fit as-of `d` into `wf_root`, then decide (G7).
#'
#' Never called by the pure tests (which inject a fake). The empty `wf_root`
#' has no ledger, so `compute_calibrations` falls back to the neutral K3 prior
#' (calibration disabled, leak-free); `ledger_asof` is accepted for symmetry.
#' @noRd
bt_wf_default_decide <- function(root, run_date, sex, ledger_asof = NULL) {
  # WHY: fit at the production adapt_delta (0.95). Raising it to 0.99 traded an
  # occasional benign divergence-gate trip for ~30x slower fits (this model's
  # geometry saturates max_treedepth at small step sizes -> ~60 min/fit). So
  # keep the production sampler and let bt_walkforward skip any cutoff whose fit
  # trips the gate instead.
  fit_league(
    league_key = "football_iceland", sex = sex,
    fit_date = run_date, end_date = run_date,
    seed = as.integer(format(run_date, "%Y%m%d")),
    schedule_horizon_days = 200L, root = root
  )
  # WHY: a historical decide runs weeks after the cutoff, so prepare_odds' default
  # 48h (vs Sys.time()) lower bound would drop every pre-sliced snapshot. Pass a
  # ~10yr window so only bt_wf_slice_odds' per-match-day upper bound binds.
  # return_candidates = TRUE: the OOS calibration arm scores EVERY bettable
  # candidate (each carries a model p), not just the handful that clear the
  # staking filters; the PnL arm keys off stage == "kept" downstream.
  decide_league(
    league_key = "football_iceland", sex = sex,
    run_date = run_date, root = root, write = FALSE,
    max_age_hours = bt_wf_max_age_hours(),
    return_candidates = TRUE
  )
}

#' Walk-forward out-of-sample validator over a set of cutoff dates.
#'
#' For each `d` in `cutoffs` (sorted ascending), re-fit as-of `d`, decide from
#' pre-`d` odds, and score the OOS matches in `(d, min(d + horizon_days, next
#' cutoff)]` — the window is capped at the next cutoff so every match is scored
#' once, under its freshest as-of fit (no double-counting across overlapping
#' windows). The final cutoff keeps the full `horizon_days` tail. PRIMARY verdict
#' is OOS Brier/log-loss ([bt_oos_scores()]); the secondary PnL arm uses unit
#' stakes and is approximate. football_iceland only (the engine stays general).
#' Each cutoff is a full Stan fit: run detached.
#' @param sex "male" or "female".
#' @param cutoffs Vector of cutoff Dates (strictly pre-round per G1); sorted
#'   ascending internally.
#' @param horizon_days OOS window CAP, and the tail length for the final cutoff.
#'   Default 14. Earlier cutoffs use `min(horizon_days, gap to next cutoff)`.
#' @param results,odds,ledger Pre-loaded stores (NULL ledger -> neutral calib).
#' @param live_root Production data root (read-only).
#' @param decide_fn Injected for tests; default fits+decides.
#' @param tie_threshold Per-(sport,country) push band.
#' @return `list(bets, scores, pnl, skipped)`. `bets` is every scored OOS
#'   candidate with a `kept` flag; `scores` ([bt_oos_scores()]) is the PRIMARY
#'   OOS calibration over ALL candidates with a finite `p` (so n reflects every
#'   bettable prediction, not just placed bets); `pnl` ([bt_metrics()]) is the
#'   secondary unit-stake arm over the `kept` (placed) subset only. `skipped` is
#'   a `(cutoff, error)` tibble of cutoffs whose fit failed (e.g. the Stan
#'   divergence gate) — a single bad fit is recorded and skipped, never aborting
#'   the whole sweep. NB: candidate rows within a match (e.g. the three moneyline
#'   outcomes) are correlated, so the effective n for calibration CIs is below
#'   `scores$n`.
#' @export
bt_walkforward <- function(sex, cutoffs, horizon_days = 14L,
                           results, odds, ledger = NULL,
                           live_root = here::here("data"),
                           decide_fn = bt_wf_default_decide,
                           tie_threshold = 0) {
  cutoffs <- sort(as.Date(cutoffs))
  n_cut <- length(cutoffs)
  scored <- list()
  skipped <- list()
  for (i in seq_len(n_cut)) {
    d <- cutoffs[i]
    # WHY: cap each cutoff's OOS window at the NEXT cutoff so every match is
    # scored once, under its freshest as-of fit. Per-round cutoffs sit closer
    # than horizon_days (gaps ~4-9d vs 14d), so a fixed window would re-score
    # ~every match under 2+ fits, inflating the calibration n ~2x. The last
    # cutoff has no successor and keeps the full horizon_days tail.
    eff_h <- if (i < n_cut) {
      min(as.integer(horizon_days), as.integer(cutoffs[i + 1L] - d))
    } else {
      as.integer(horizon_days)
    }
    res <- tryCatch(
      bt_walkforward_cutoff(
        sex = sex, d = d, horizon_days = eff_h,
        results = results, odds = odds, ledger = ledger,
        live_root = live_root, decide_fn = decide_fn,
        tie_threshold = tie_threshold
      ),
      error = function(e) structure(conditionMessage(e), class = "bt_wf_skip")
    )
    if (inherits(res, "bt_wf_skip")) {
      cli::cli_alert_warning("walk-forward cutoff {format(d)} skipped: {as.character(res)}")
      skipped[[length(skipped) + 1L]] <- tibble::tibble(cutoff = d, error = as.character(res))
    } else {
      scored[[length(scored) + 1L]] <- res
    }
  }
  bets <- dplyr::bind_rows(scored)
  skipped_df <- if (length(skipped) > 0L) {
    dplyr::bind_rows(skipped)
  } else {
    tibble::tibble(cutoff = as.Date(character()), error = character())
  }
  # Secondary PnL arm: only the PLACED bets (stage == "kept"), settled, unit
  # stake. Calibration below scores ALL candidates; PnL scores what we'd bet.
  pnl <- {
    kb <- bets
    if ("kept" %in% names(kb)) kb <- kb[!is.na(kb$kept) & kb$kept, , drop = FALSE]
    kb <- kb[!is.na(kb$win), , drop = FALSE]
    if (nrow(kb) > 0L) {
      kb$stake <- 1
      kb$pnl <- ifelse(kb$win, kb$stake * (kb$odds - 1), -kb$stake)
      bt_metrics(kb)
    } else {
      tibble::tibble()
    }
  }
  list(bets = bets, scores = bt_oos_scores(bets), pnl = pnl, skipped = skipped_df)
}

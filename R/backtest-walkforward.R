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
  d <- settled[!is.na(settled$win), , drop = FALSE]
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

#' Upper-bound the odds snapshots a decision at cutoff `d` may see.
#'
#' G3/G4: `prepare_odds` bounds `scraped_at` from below only and takes
#' `slice_max(scraped_at)`. Pre-slicing here to `scraped_at <= d + 12h` and
#' writing the result into the isolated `wf_root` makes the decider unable to
#' select a closing/post-result snapshot.
#' @param odds Tibble from `read_table("odds")` (has `scraped_at`).
#' @param d Cutoff date.
#' @return `odds` restricted to pre-cutoff snapshots.
#' @export
bt_wf_slice_odds <- function(odds, d) {
  if (nrow(odds) == 0L) {
    return(odds)
  }
  cutoff <- as.POSIXct(format(as.Date(d)), tz = "UTC") + lubridate::dhours(12)
  odds[odds$scraped_at <= cutoff, , drop = FALSE]
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

  sliced <- bt_wf_slice_odds(odds, d)
  if (nrow(sliced) > 0L) write_table(sliced, "odds", root = wf_root)

  led_asof <- bt_wf_ledger_asof(ledger, d)

  cands <- decide_fn(root = wf_root, run_date = d, sex = sex, ledger_asof = led_asof)
  cands <- bt_wf_filter_oos(cands, d, horizon_days)
  cands <- bt_wf_require_pre_cutoff_odds(cands, sliced)
  if (nrow(cands) == 0L) {
    cands$cutoff <- as.Date(character())
    cands$run_id <- as.POSIXct(character(), tz = "UTC")
    cands$win <- logical()
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
  # WHY: walk-forward re-fits at many historical cutoffs hit borderline
  # divergence rates; adapt_delta = 0.99 keeps most under the Stan gate's 1%
  # threshold. bt_walkforward tolerates any cutoff whose fit still trips it.
  fit_league(
    league_key = "football_iceland", sex = sex,
    fit_date = run_date, end_date = run_date,
    seed = as.integer(format(run_date, "%Y%m%d")),
    adapt_delta = 0.99,
    schedule_horizon_days = 200L, root = root
  )
  decide_league(
    league_key = "football_iceland", sex = sex,
    run_date = run_date, root = root, write = FALSE
  )
}

#' Walk-forward out-of-sample validator over a set of cutoff dates.
#'
#' For each `d` in `cutoffs`, re-fit as-of `d`, decide from pre-`d` odds, and
#' score the OOS matches in `(d, d + horizon_days]`. PRIMARY verdict is OOS
#' Brier/log-loss ([bt_oos_scores()]); the secondary PnL arm uses unit stakes
#' and is approximate. football_iceland only (the engine stays general). Each
#' cutoff is a full Stan fit: run detached.
#' @param sex "male" or "female".
#' @param cutoffs Vector of cutoff Dates (strictly pre-round per G1).
#' @param horizon_days OOS window length. Default 14.
#' @param results,odds,ledger Pre-loaded stores (NULL ledger -> neutral calib).
#' @param live_root Production data root (read-only).
#' @param decide_fn Injected for tests; default fits+decides.
#' @param tie_threshold Per-(sport,country) push band.
#' @return `list(bets, scores, pnl, skipped)`. `skipped` is a `(cutoff, error)`
#'   tibble of cutoffs whose fit failed (e.g. the Stan divergence gate) — a
#'   single bad fit is recorded and skipped, never aborting the whole sweep.
#' @export
bt_walkforward <- function(sex, cutoffs, horizon_days = 14L,
                           results, odds, ledger = NULL,
                           live_root = here::here("data"),
                           decide_fn = bt_wf_default_decide,
                           tie_threshold = 0) {
  scored <- list()
  skipped <- list()
  for (d in as.list(as.Date(cutoffs))) {
    res <- tryCatch(
      bt_walkforward_cutoff(
        sex = sex, d = d, horizon_days = horizon_days,
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
  pnl <- if (nrow(bets) > 0L) {
    b <- bets
    b$stake <- 1
    b$pnl <- ifelse(b$win, b$stake * (b$odds - 1), -b$stake)
    bt_metrics(b)
  } else {
    tibble::tibble()
  }
  list(bets = bets, scores = bt_oos_scores(bets), pnl = pnl, skipped = skipped_df)
}

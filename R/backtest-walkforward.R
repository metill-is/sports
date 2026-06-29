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
#'
#' The two sides live in different team-name namespaces: `decide_league` emits
#' candidates in canonical (federation) names because `prepare_odds` runs
#' [normalise_lengjan_team_names()], while `sliced_odds` are the raw pre-decide
#' snapshots still in Lengjan display names. Any cell with a per-sex
#' `lengjan.team_names` map (every football_iceland female team — "FH" vs
#' "FH kv") would otherwise key two disjoint namespaces and drop every
#' candidate. Pass `league` + `sex` to rewrite the odds side with the SAME map
#' before keying; omit them (the default) for cells with no map, where the
#' names already coincide.
#' @param candidates OOS candidates.
#' @param sliced_odds Output of [bt_wf_slice_odds()].
#' @param league League list (as from [load_leagues()]) carrying
#'   `lengjan$team_names`. `NULL` skips name normalisation.
#' @param sex `"male"` or `"female"`. Required (with `league`) to normalise.
#' @return Candidates that have a matching pre-cutoff odds snapshot.
#' @export
bt_wf_require_pre_cutoff_odds <- function(candidates, sliced_odds,
                                          league = NULL, sex = NULL) {
  if (nrow(candidates) == 0L || nrow(sliced_odds) == 0L) {
    return(candidates[0, , drop = FALSE])
  }
  if (!is.null(league) && !is.null(sex)) {
    sliced_odds <- normalise_lengjan_team_names(sliced_odds, league, sex)
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
#' @param league League list (as from [load_leagues()]) used to bridge the
#'   candidate (canonical) vs odds (Lengjan) team-name namespaces in
#'   [bt_wf_require_pre_cutoff_odds()]. `NULL` (default) keys verbatim — correct
#'   only for cells with no `lengjan.team_names` map.
#' @return Scored OOS tibble (`cutoff`, `run_id`, candidate cols, `win`).
#' @export
bt_walkforward_cutoff <- function(sex, d, horizon_days,
                                  results, odds, ledger = NULL,
                                  live_root = here::here("data"),
                                  decide_fn = bt_wf_default_decide,
                                  tie_threshold = 0, league = NULL) {
  d <- as.Date(d)
  wf_root <- withr::local_tempdir()

  pre_results <- results[results$match_date <= (d + as.integer(horizon_days)), , drop = FALSE]
  bt_wf_seed_results(pre_results, wf_root)

  sliced <- bt_wf_slice_odds(odds)
  if (nrow(sliced) > 0L) write_table(sliced, "odds", root = wf_root)

  led_asof <- bt_wf_ledger_asof(ledger, d)

  cands <- decide_fn(root = wf_root, run_date = d, sex = sex, ledger_asof = led_asof)
  cands <- bt_wf_filter_oos(cands, d, horizon_days)
  cands <- bt_wf_require_pre_cutoff_odds(cands, sliced, league = league, sex = sex)
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

#' Empty per-draw beliefs tibble in the `beliefs_latest` schema.
#' @noRd
bt_wf_empty_beliefs <- function() {
  tibble::tibble(
    sport = character(), country = character(), sex = character(),
    fit_date = as.Date(character()), match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    draw_id = integer(), home_goals = numeric(), away_goals = numeric()
  )
}

#' Reconstruct per-draw beliefs from a saved football `predicted_matches` extract.
#'
#' The model's RDS fit is deliberately discarded (size); the durable record of a
#' fit is `beliefs/extracts/.../fit_date=F/predicted_matches.parquet`, a histogram
#' of the posterior draws — one row per `(match, home_goals, away_goals)` with a
#' `count`. `uncount(count)` restores exactly the per-draw `beliefs_latest`
#' `decide_league` consumes (the joint goal distribution, hence every market
#' probability and the cross-market correlations, is preserved). football_iceland
#' only, matching the harness scope.
#' @param source_root Live data root holding `beliefs/extracts/`.
#' @param fit_date The fit's as-of date (== the walk-forward cutoff).
#' @param sex "male" or "female".
#' @return A `beliefs_latest`-schema tibble, or an empty one if no extract exists.
#' @export
bt_wf_beliefs_from_extract <- function(source_root, fit_date, sex) {
  fit_date <- as.Date(fit_date)
  pm_path <- file.path(
    source_root, "beliefs", "extracts", "sport=football", "country=iceland",
    paste0("sex=", sex), paste0("fit_date=", format(fit_date)),
    "predicted_matches.parquet"
  )
  if (!file.exists(pm_path)) {
    return(bt_wf_empty_beliefs())
  }
  pm <- arrow::read_parquet(pm_path)
  if (nrow(pm) == 0L) {
    return(bt_wf_empty_beliefs())
  }
  draws <- pm[rep(seq_len(nrow(pm)), times = pm$count),
    c("match_date", "home_team", "away_team", "home_goals", "away_goals"),
    drop = FALSE
  ]
  draws <- draws |>
    dplyr::group_by(.data$match_date, .data$home_team, .data$away_team) |>
    dplyr::mutate(draw_id = dplyr::row_number()) |>
    dplyr::ungroup()
  tibble::tibble(
    sport = "football", country = "iceland", sex = sex,
    fit_date = fit_date,
    match_date = as.Date(draws$match_date),
    home_team = as.character(draws$home_team),
    away_team = as.character(draws$away_team),
    draw_id = as.integer(draws$draw_id),
    home_goals = as.numeric(draws$home_goals),
    away_goals = as.numeric(draws$away_goals)
  )
}

#' Decide closure that REUSES saved per-fit predictions instead of re-fitting.
#'
#' The drop-in alternative to [bt_wf_default_decide()]: instead of a Stan fit, it
#' loads the saved `predicted_matches` extract for `run_date` from `source_root`,
#' reconstructs the as-of beliefs ([bt_wf_beliefs_from_extract()]), seeds them
#' into the isolated `root`, and decides. No Stan — a season sweep runs in
#' seconds, and it scores the model's ACTUAL published predictions. Pass the
#' walk-forward cutoffs as the extract `fit_date`s so each is exactly leak-free
#' (`end_date == fit_date`).
#' @param source_root Live data root holding the saved extracts (read-only).
#' @return A `decide_fn` closure `(root, run_date, sex, ledger_asof)`.
#' @export
bt_wf_extract_decide <- function(source_root = here::here("data")) {
  function(root, run_date, sex, ledger_asof = NULL) {
    beliefs <- bt_wf_beliefs_from_extract(source_root, run_date, sex)
    if (nrow(beliefs) == 0L) {
      stop("bt_wf_extract_decide: no saved predicted_matches for fit_date=",
        format(as.Date(run_date)),
        call. = FALSE
      )
    }
    write_table(beliefs, "beliefs_latest", root = root)
    decide_league(
      league_key = "football_iceland", sex = sex,
      run_date = run_date, root = root, write = FALSE,
      max_age_hours = bt_wf_max_age_hours(),
      return_candidates = TRUE
    )
  }
}

#' football_iceland league list with the (S,D) Gaussian Stan model.
#'
#' Overrides only `stan_model` so `fit_league(league = …)` fits the (S,D) model
#' through the exact same prepare/fit/extract path as the production BVP.
#' @param stan_model Stan path relative to `Stan/`. Default the (S,D) model.
#' @return The football_iceland league list with `stan_model` replaced.
#' @noRd
bt_wf_sd_league <- function(stan_model = "football_iceland/2d_gaussian_sd.stan") {
  lg <- load_leagues()[["football_iceland"]]
  lg$stan_model <- stan_model
  lg
}

#' Decide closure that fits the (S,D) Gaussian model as-of `d`, then decides.
#'
#' The (S,D) analogue of [bt_wf_default_decide()]: it fits the (S,D) model (not
#' the config's BVP) into the isolated `root` with `end_date == run_date`
#' (leak-free), then decides over the pre-sliced odds. `write_archive = FALSE`
#' skips the football publish extracts (which assume BVP parameters); the
#' `beliefs_latest` write the decider needs is unconditional.
#' @param stan_model Stan path relative to `Stan/`. Default the (S,D) model.
#' @return A `decide_fn` closure `(root, run_date, sex, ledger_asof)`.
#' @export
bt_wf_sd_decide <- function(stan_model = "football_iceland/2d_gaussian_sd.stan") {
  league <- bt_wf_sd_league(stan_model)
  function(root, run_date, sex, ledger_asof = NULL) {
    fit_league(
      league = league, sex = sex,
      fit_date = run_date, end_date = run_date,
      seed = as.integer(format(run_date, "%Y%m%d")),
      schedule_horizon_days = 200L, root = root,
      write_archive = FALSE
    )
    decide_league(
      league_key = "football_iceland", sex = sex,
      run_date = run_date, root = root, write = FALSE,
      max_age_hours = bt_wf_max_age_hours(),
      return_candidates = TRUE
    )
  }
}

#' Select the walk-forward decide closure for a model + mode.
#'
#' `model = "sd"` re-fits the (S,D) model and cannot REUSE (no saved extracts).
#' `model = "bvp"` uses [bt_wf_extract_decide()] under `reuse`, else
#' [bt_wf_default_decide()].
#' @param model "bvp" or "sd".
#' @param reuse Replay saved extracts (bvp only).
#' @param source_root Live data root for the reuse path.
#' @return A `decide_fn` closure.
#' @noRd
wf_select_decide_fn <- function(model = c("bvp", "sd"), reuse = FALSE,
                                source_root = here::here("data")) {
  model <- match.arg(model)
  if (identical(model, "sd")) {
    if (isTRUE(reuse)) {
      stop("--reuse is unavailable for model=sd (no saved (S,D) extracts); use --per-round or --as-of",
        call. = FALSE
      )
    }
    return(bt_wf_sd_decide())
  }
  if (isTRUE(reuse)) bt_wf_extract_decide(source_root) else bt_wf_default_decide
}

#' Walk-forward in REUSE mode over the saved extract fit_dates (no Stan).
#'
#' Thin orchestration shared by the CLI (`--reuse`) and the season report:
#' enumerates the saved `predicted_matches` fit_dates for football_iceland/`sex`
#' (optionally one season), loads `results` + `odds`, and runs [bt_walkforward()]
#' with [bt_wf_extract_decide()]. A whole season scores in seconds.
#' @param sex "male" or "female".
#' @param season Optional integer year; filters the fit_dates to that season.
#' @param horizon_days OOS window cap (tail for the final cutoff). Default 14.
#' @param root Data root holding `beliefs/extracts/` + `facts/`.
#' @return The [bt_walkforward()] list, or `NULL` if the cell has no saved
#'   extracts (e.g. an off-season sex).
#' @export
bt_walkforward_reuse <- function(sex, season = NULL, horizon_days = 14L,
                                 root = here::here("data")) {
  ext_dir <- file.path(
    root, "beliefs", "extracts", "sport=football",
    "country=iceland", paste0("sex=", sex)
  )
  if (!dir.exists(ext_dir)) {
    return(NULL)
  }
  fds <- sort(as.Date(sub("fit_date=", "", list.files(ext_dir))))
  if (!is.null(season)) fds <- fds[format(fds, "%Y") == as.character(season)]
  if (length(fds) == 0L) {
    return(NULL)
  }
  league <- load_leagues()[["football_iceland"]]
  tie_threshold <- league$betting$scoring$tie_threshold %||% 0
  results <- read_table("results",
    root = root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )
  odds <- read_table("odds",
    root = root,
    filter = list(sport = "football", country = "iceland")
  )
  bt_walkforward(
    sex = sex, cutoffs = fds, horizon_days = horizon_days,
    results = results, odds = odds, ledger = NULL, live_root = root,
    decide_fn = bt_wf_extract_decide(root), tie_threshold = tie_threshold,
    league = league
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
#' @param league League list forwarded to [bt_walkforward_cutoff()] to bridge
#'   the candidate vs odds team-name namespaces (needed for any cell with a
#'   `lengjan.team_names` map, e.g. football_iceland female). `NULL` keys
#'   verbatim.
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
                           tie_threshold = 0, league = NULL) {
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
        tie_threshold = tie_threshold, league = league
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

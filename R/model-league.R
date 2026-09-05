#' @include model-prepare.R model-fit.R model-posteriors.R storage.R config.R round-cutoff.R
NULL

# Read an integer env var with sensible fallbacks. `Sys.getenv(name, "1000")`
# treats an unset env var as 1000 but a *set-to-empty* env var as `""`, which
# `as.integer()` converts to NA. nzchar() guards against that.
.env_iter <- function(name, default = 1000L) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) {
    return(as.integer(default))
  }
  as.integer(v)
}

# Same shape but for a double-valued knob (e.g. adapt_delta).
.env_dbl <- function(name, default) {
  v <- Sys.getenv(name, "")
  if (!nzchar(v)) {
    return(as.numeric(default))
  }
  as.numeric(v)
}

#' End-to-end: prepare data, fit Stan, extract posteriors, write beliefs.
#'
#' Supports two call modes:
#'   1. By league_key: `fit_league("basketball_iceland", sex = "male")`
#'      -- looks up the league via `load_leagues()`.
#'   2. By league list: `fit_league(league = <list>, sex = "male")`
#'      -- bypasses `load_leagues()`; used by tests and one-off runs.
#'
#' Writes `data/beliefs/latest/` (snapshot -- overwritten per call) and
#' optionally `data/beliefs/archive/sport=X/country=Y/sex=Z/fit_date=D/`.
#' For football iceland, the legacy `beliefs_archive` write is skipped
#' (Phase 3b, 2026-05-04) because `extract_football_iceland()` already
#' persists the per-fit posterior summaries to `beliefs/extracts/`;
#' `extracts/` is the canonical per-fit accretive store for that league.
#' Pass `force_archive_write = TRUE` to bypass the skip — used by the
#' one-off `scripts/03c_backfill_football_archive_2026_05.R` to fill the
#' 2026-05-04 → 2026-05-25 archive gap.
#'
#' @param league_key Key into `load_leagues()`. Mutually exclusive with `league`.
#' @param league Pre-loaded league list. Mutually exclusive with `league_key`.
#' @param sex "male" or "female".
#' @param fit_date Date stamped on every posterior row. Default today.
#' @param end_date Training cutoff. Default = `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param stan_dir Stan-model root. Default `here::here("Stan")`.
#' @param method Passed to `fit_model()`. "sample" (default), "pathfinder", or "variational".
#' @param iter_warmup,iter_sampling MCMC iteration counts. Passed to `fit_model()`.
#' @param adapt_delta NUTS target acceptance probability. Default reads
#'   `SPORTS_FIT_ADAPT_DELTA` env var, falling back to `0.95`. Raised from
#'   Stan's stock `0.8` after 2026-05-17 — football iceland's funnel-shaped
#'   posterior (Mjólkurbikar blowouts between top-flight and 4th-tier teams)
#'   hit 7% divergent transitions at the default and tripped the diagnostic
#'   gate. Set `SPORTS_FIT_ADAPT_DELTA=0.99` to escalate if 0.95 still fails.
#' @param chains Number of MCMC chains. Passed to `fit_model()`.
#' @param seed Integer seed for reproducibility. NULL = cmdstanr default.
#' @param from_season Optional integer: earliest season to include in training data.
#' @param schedule_horizon_days Days ahead of `end_date` to include from schedule. Default 14.
#' @param write_archive Write `beliefs/archive/` in addition to `beliefs/latest/`? Default TRUE.
#'   Football iceland skips the archive write even when this is TRUE
#'   (the per-fit Parquets under `beliefs/extracts/` are the canonical
#'   per-fit accretive store for that league). Set
#'   `force_archive_write = TRUE` to override.
#' @param force_archive_write Bypass the football-iceland archive skip
#'   introduced in Phase 3b. Default `FALSE`. Only intended for the
#'   one-off 2026-05-04 → 2026-05-25 backfill; daily fits should rely on
#'   `extracts/` instead.
#' @param round_cutoff Optional integer. When supplied (with `season`),
#'   triggers per-round mode: `end_date` is overridden to the date of the
#'   round-N completion (max of each top-division team's Nth match);
#'   `schedule_horizon_days` is widened to cover the rest of the season; the
#'   fit RDS is written to `data/beliefs/fits_by_round/sport=X/.../season=YYYY/round=NN/fit.rds`
#'   instead of `data/beliefs/fits/`; and predictive draws are appended to
#'   `beliefs_by_round` (with `season` and `round_cutoff` columns) instead of
#'   `beliefs_latest` / `beliefs_archive`. Returns `NULL` invisibly if the
#'   round is not yet complete in the data.
#' @param season Integer season year. Required when `round_cutoff` is set.
#' @param top_division Division code identifying the top flight for round
#'   accounting. Default `"BD"`.
#' @return Tibble of beliefs (invisibly).
#' @export
fit_league <- function(league_key = NULL,
                       league = NULL,
                       sex,
                       fit_date = Sys.Date(),
                       end_date = fit_date,
                       root = here::here("data"),
                       stan_dir = here::here("Stan"),
                       method = "sample",
                       # Defaults read from env vars so CI can opt into faster
                       # fits (e.g. SPORTS_FIT_ITER_WARMUP=100) while iterating
                       # on pipeline plumbing without changing call sites or
                       # test code. Empty/unset env vars fall back to the
                       # production-quality 1000 default.
                       iter_warmup = .env_iter("SPORTS_FIT_ITER_WARMUP"),
                       iter_sampling = .env_iter("SPORTS_FIT_ITER_SAMPLING"),
                       adapt_delta = .env_dbl("SPORTS_FIT_ADAPT_DELTA", 0.95),
                       chains = 4L,
                       seed = NULL,
                       from_season = NULL,
                       schedule_horizon_days = 14L,
                       write_archive = TRUE,
                       force_archive_write = FALSE,
                       round_cutoff = NULL,
                       season = NULL,
                       top_division = "BD") {
  if (is.null(league) == is.null(league_key)) {
    stop("Exactly one of `league_key` or `league` must be supplied",
      call. = FALSE
    )
  }
  if (is.null(league)) {
    leagues <- load_leagues()
    if (!league_key %in% names(leagues)) {
      stop("Unknown league: ", league_key,
        " (available: ", paste(names(leagues), collapse = ", "), ")",
        call. = FALSE
      )
    }
    league <- leagues[[league_key]]
  }
  stopifnot(sex %in% c("male", "female"))
  stopifnot(!is.null(league$stan_model))

  by_round_mode <- !is.null(round_cutoff)
  if (by_round_mode) {
    if (is.null(season)) {
      stop("`season` is required when `round_cutoff` is supplied", call. = FALSE)
    }
    results_all <- read_table(
      "results",
      root = root,
      filter = list(sport = league$sport, country = league$country, sex = sex)
    )
    cutoff_date <- compute_round_cutoff_date(
      results_all,
      season = season,
      round_cutoff = round_cutoff,
      top_division = top_division
    )
    if (is.null(cutoff_date)) {
      cli::cli_alert_warning(
        paste0(
          "fit_league({league$sport}/{league$country}/{sex}): round ",
          "{round_cutoff} of season {season} not yet complete -- skipping."
        )
      )
      return(invisible(NULL))
    }
    end_date <- cutoff_date
    schedule_horizon_days <- 200L
    cli::cli_alert_info(
      paste0(
        "fit_league round mode: season {season} round {round_cutoff} ",
        "(cutoff_date = {format(cutoff_date)})"
      )
    )
  }

  stan_path <- file.path(stan_dir, league$stan_model)
  if (!file.exists(stan_path)) {
    stop("Stan model missing: ", stan_path, call. = FALSE)
  }

  prep <- prepare_data(league, sex,
    end_date = end_date, root = root,
    from_season = from_season,
    schedule_horizon_days = schedule_horizon_days
  )

  # Nothing to predict -> refuse BEFORE sampling. On 2026-09-05 a forced
  # basketball fit ran its chains for 100 minutes with N_pred = 0 (the
  # season's first fixture lay beyond the 14-day horizon), then failed in the
  # extractor on "Can't find goals1_pred, goals2_pred". prepare_data() knew
  # that before a single draw. The daily path never reaches this line --
  # fit_skip_reason() pauses a league with no upcoming game -- so it fires on
  # a forced or --league run, where the operator needs a plain answer. Round
  # mode is exempt: a per-round replay legitimately reaches the season's end
  # with nothing left to predict and keeps its historical behaviour.
  if (is.null(round_cutoff) && nrow(prep$pred_d) == 0L) {
    sched <- tryCatch(
      read_table(
        "schedules",
        root = root,
        filter = list(sport = league$sport, country = league$country, sex = sex)
      ),
      error = function(e) NULL
    )
    upcoming <- if (!is.null(sched)) sched$match_date[!is.na(sched$match_date) & sched$match_date > end_date] else as.Date(character())
    first_fixture <- if (length(upcoming)) format(min(upcoming)) else "none scheduled"
    gap <- if (length(upcoming)) as.integer(min(upcoming) - end_date) else NA_integer_
    cli::cli_abort(
      c(
        "fit_league({league$sport}/{league$country}/{sex}): no fixture inside the {schedule_horizon_days}-day horizon after {format(end_date)} -- nothing to predict, refusing to sample.",
        "i" = "First scheduled fixture: {first_fixture}{if (!is.na(gap)) paste0(' (', gap, ' days out)') else ''}.",
        "i" = "Wait until it enters the horizon, or pass a wider schedule_horizon_days."
      ),
      call = NULL
    )
  }

  fit <- fit_model(
    stan_data       = prep$stan_data,
    stan_model_path = stan_path,
    method          = method,
    chains          = chains,
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    adapt_delta     = adapt_delta,
    seed            = seed
  )

  # Persist sampler diagnostics (drift tracking, audit 2026-05-30). Daily
  # MCMC fits only: by_round backfills share a fit_date across rounds and
  # would collide in the (sport,country,sex,fit_date) partition. Best-effort.
  if (identical(method, "sample") && !by_round_mode) {
    persist_fit_diagnostics(
      fit, league,
      sex = sex,
      fit_date = fit_date,
      n_obs = prep$stan_data$N %||% NA_integer_,
      adapt_delta = adapt_delta,
      iter_sampling = iter_sampling,
      chains = chains,
      root = root
    )
  }

  # Plan 6: persist the fit object so publish_one() can read it back.
  # `data/beliefs/latest/` is the canonical Parquet for the long-form draws,
  # but publishers also need team-level Stan parameters via fit$draws(var)
  # which aren't in that schema. Save the fit RDS alongside.
  if (by_round_mode) {
    fits_dir <- file.path(
      root, "beliefs", "fits_by_round",
      paste0("sport=", league$sport),
      paste0("country=", league$country),
      paste0("sex=", sex),
      paste0("season=", season),
      paste0("round=", sprintf("%02d", as.integer(round_cutoff)))
    )
  } else {
    fits_dir <- file.path(
      root, "beliefs", "fits",
      paste0("sport=", league$sport),
      paste0("country=", league$country),
      paste0("sex=", sex)
    )
  }
  dir.create(fits_dir, recursive = TRUE, showWarnings = FALSE)
  # save_object() depends on cmdstanr's underlying CSV temp files. They can
  # be GC'd between fit and save (e.g. cmdstan_fit() output_dir cleanup).
  # Warn-and-continue rather than fail the whole pipeline.
  tryCatch(
    fit$save_object(file = file.path(fits_dir, "fit.rds")),
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to save fit RDS at {fits_dir}: {conditionMessage(e)}"
      )
    }
  )

  beliefs <- extract_posteriors(fit, prep$pred_d,
    league = league, sex = sex,
    fit_date = fit_date
  )

  if (nrow(beliefs) > 0L) {
    if (by_round_mode) {
      beliefs$season <- as.integer(season)
      beliefs$round_cutoff <- as.integer(round_cutoff)
      beliefs <- beliefs[, c(
        "sport", "country", "sex", "season", "round_cutoff",
        "fit_date", "match_date", "home_team", "away_team",
        "draw_id", "home_goals", "away_goals"
      ), drop = FALSE]
      write_table(beliefs, "beliefs_by_round", root = root)
    } else {
      write_table(beliefs, "beliefs_latest", root = root)
      # WHY: football iceland's per-fit archive is now the per-fit Parquets
      # emitted by extract_football_iceland() under beliefs/extracts/ (Phase
      # 1, 2026-05-04) -- the legacy long-form per-draw part-0.parquet write
      # is redundant for that league. Basketball + handball iceland still
      # rely on beliefs_archive even after F6 shipped their own extracts
      # tree, because extracts/ for those sports doesn't carry per-draw
      # state (no cup forward sim). `force_archive_write = TRUE` overrides
      # the football skip for the one-off 2026-05-04 → 2026-05-25 backfill;
      # do not flip its default.
      is_football_iceland <- identical(league$sport, "football") &&
        identical(league$country, "iceland")
      skip_archive <- is_football_iceland && !isTRUE(force_archive_write)
      if (isTRUE(write_archive) && !skip_archive) {
        write_table(beliefs, "beliefs_archive", root = root)
      }
    }
  } else {
    cli::cli_alert_warning(
      "fit_league({league$sport}/{league$country}/{sex}): no predictions -- skipping belief writes"
    )
  }

  # WHY: extraction-layer per-fit persistence. Football iceland writes 9
  # publish-layer summary Parquets per fit (Phase 1, since 2026-05-04);
  # basketball + handball iceland write 5 each (F6 shipped 2026-05-26 — the
  # 4 football-specific extras like cup bracket inputs and round projections
  # are skipped). All three flows let future republish runs render JSONs
  # without the gitignored fit RDS. See Sports/Knowledge/Publish Pipeline/
  # extraction-layer in the Metill vault.
  if (!by_round_mode && isTRUE(write_archive) &&
    identical(league$country, "iceland")) {
    extract_fn <- switch(league$sport,
      football = extract_football_iceland,
      basketball = extract_basketball_iceland,
      handball = extract_handball_iceland,
      NULL
    )
    if (!is.null(extract_fn)) {
      tryCatch(
        extract_fn(
          fit, league,
          sex = sex,
          fit_date = fit_date,
          end_date = end_date,
          root = root,
          prep = prep
        ),
        error = function(e) {
          # Abort, do not warn. The extracts tree is becoming the SOLE input
          # to publish, so a swallowed failure here means the cell silently
          # stops publishing -- the exact invisible breakage this workstream
          # exists to remove, and how B5 (an exp() on an additive parameter)
          # sat unexercised for months.
          #
          # Safe to abort: beliefs_latest / beliefs_archive are already
          # written above (:248-264), so the fit's output survives and only
          # the run goes red.
          cli::cli_abort(
            c(
              "extract_{league$sport}_iceland({sex}) failed.",
              "x" = conditionMessage(e),
              "i" = "Beliefs were written; the extract partition was not, so
                     this cell would publish nothing."
            ),
            call = NULL
          )
        }
      )
    }
  }

  invisible(beliefs)
}

#' Fit a single (league x sex) and return belief row count.
#'
#' Takes the per-league "static" slice (sport, country, stan_model, sexes,
#' data_source) rather than the full leagues config — keeps callers from
#' re-loading the full config per call.
#'
#' @param static Per-league static slice (sport, country, stan_model, sexes, data_source).
#' @param sex `"male"` or `"female"`.
#' @return Integer count of belief rows written.
#' @export
fit_one <- function(static, sex) {
  beliefs <- fit_league(league = static, sex = sex)
  nrow(beliefs)
}

#' Order fit targets so a timeout cuts a publish-only league, never the betting one.
#'
#' `fit.yml` runs every (league, sex) target inside ONE job with a fixed
#' `timeout-minutes` budget. Football alone took 196 min on 2026-08-28 (male
#' 126, female 70) against the then-240-min cap, and the two May 2026 runs that
#' fitted all three sports hit 236 and 240 min (one failed, one cancelled by
#' the timeout). In `config/leagues.yml` order -- basketball, handball,
#' football -- the target a timeout cuts is the LAST one, and that was
#' football: the only league whose posterior the decide layer turns into
#' recommendations that the autoplace agent stakes real money on. A stale
#' publish-only fit is a stale page; a stale betting fit is money.
#'
#' [betting_enabled()] is the data-driven expression of that difference, so no
#' sport name is hardcoded here and a league that is armed later moves up on
#' its own. The sort is stable: within a tier, config order (and each league's
#' declared sex order) is preserved, so a league's rows are never interleaved.
#'
#' @param targets Tibble of `key`/`sex` rows from `resolve_targets()`.
#' @param leagues Leagues list; each `leagues[[key]]` may carry a `betting`
#'   slice. A missing league or slice counts as betting-enabled (the
#'   [betting_enabled()] default), which keeps the conservative tier on top.
#' @return `targets` reordered; identical to the input when every league is in
#'   the same tier.
#' @export
order_fit_targets <- function(targets, leagues) {
  if (nrow(targets) == 0L) {
    return(targets)
  }
  armed <- vapply(
    targets$key,
    function(k) betting_enabled(leagues[[k]]),
    logical(1),
    USE.NAMES = FALSE
  )
  # order() leaves ties in their original order; !armed puts TRUE first.
  targets[order(!armed), , drop = FALSE]
}

#' Fit every target, isolating a per-target abort.
#'
#' The fit loop's skip and failure policy, lifted out of `scripts/03_fit.R` so
#' it is testable without spawning `Rscript` or compiling Stan.
#'
#' WHY THIS EXISTS. `fit_model()` ABORTS on a diagnostics-gate breach, and
#' `fit_skip_reason()`'s own docstring records real off-season basketball
#' R-hat/ESS breaches. A bare loop meant a single marginal 2DT fit taking the
#' gate down killed every target after it in the same run -- and the first live
#' 2DT fits in five months are the highest abort-risk event of the season.
#' Isolating per target means the run still goes red, but every posterior that
#' did fit is written and committed.
#'
#' Two defences, deliberately separate. [order_fit_targets()] decides WHICH
#' target a `fit.yml` timeout cuts (a publish-only one, never the betting
#' league); isolation decides that an abort in one target cannot reach the
#' next or hide behind exit 0. Neither substitutes for the other.
#'
#' Failures are collected, not swallowed: the returned `failed` frame is what
#' the script turns into `quit(status = 1L)`, on ANY failure rather than only on
#' all of them (INT-2).
#'
#' @param targets Tibble of `key`/`sex` rows from `resolve_targets()`.
#' @param leagues Leagues list.
#' @param force,league_named The `--force` flag and whether `--league` named one.
#' @param root Storage root, forwarded to `skip_fn`.
#' @param fit_fn Injectable fitter; defaults to [fit_one()].
#' @param skip_fn Injectable skip rule; defaults to [fit_skip_reason()].
#' @return `list(fitted, skipped, failed = tibble(key, sex, message))`.
#' @export
run_fit_targets <- function(targets, leagues, force, league_named,
                            root = here::here("data"),
                            fit_fn = fit_one,
                            skip_fn = fit_skip_reason) {
  fitted <- 0L
  skipped <- 0L
  failed <- list()

  # A timeout cuts the LAST target; make sure that is never the betting league.
  targets <- order_fit_targets(targets, leagues)

  for (i in seq_len(nrow(targets))) {
    row <- targets[i, ]
    league_def <- leagues[[row$key]]
    static <- league_def[c(
      "sport", "country", "sexes", "active", "stan_model", "data_source"
    )]

    skip <- skip_fn(static, row$sex, force, league_named, root = root)
    if (!is.null(skip)) {
      cli::cli_alert_info("Skipping {row$key} ({row$sex}): {skip}.")
      skipped <- skipped + 1L
      next
    }

    cli::cli_h2("{row$key} ({row$sex})")
    ok <- tryCatch(
      {
        fit_fn(static, row$sex)
        TRUE
      },
      error = function(e) {
        cli::cli_alert_danger(
          "fit failed for {row$key} ({row$sex}): {conditionMessage(e)}"
        )
        failed[[length(failed) + 1L]] <<- tibble::tibble(
          key = row$key, sex = row$sex, message = conditionMessage(e)
        )
        FALSE
      }
    )
    if (isTRUE(ok)) fitted <- fitted + 1L
  }

  list(
    fitted = fitted,
    skipped = skipped,
    failed = if (length(failed) == 0L) {
      tibble::tibble(key = character(), sex = character(), message = character())
    } else {
      dplyr::bind_rows(failed)
    }
  )
}

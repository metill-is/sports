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
          cli::cli_alert_warning(
            "extract_{league$sport}_iceland failed: {conditionMessage(e)}"
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

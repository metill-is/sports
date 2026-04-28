#' @include model-prepare.R model-fit.R model-posteriors.R storage.R config.R
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
#' @param chains Number of MCMC chains. Passed to `fit_model()`.
#' @param seed Integer seed for reproducibility. NULL = cmdstanr default.
#' @param from_season Optional integer: earliest season to include in training data.
#' @param schedule_horizon_days Days ahead of `end_date` to include from schedule. Default 14.
#' @param write_archive Write `beliefs/archive/` in addition to `beliefs/latest/`? Default TRUE.
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
                       chains = 4L,
                       seed = NULL,
                       from_season = NULL,
                       schedule_horizon_days = 14L,
                       write_archive = TRUE) {
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
    seed            = seed
  )

  # Plan 6: persist the fit object so publish_one() can read it back.
  # `data/beliefs/latest/` is the canonical Parquet for the long-form draws,
  # but publishers also need team-level Stan parameters via fit$draws(var)
  # which aren't in that schema. Save the fit RDS alongside.
  fits_dir <- file.path(
    root, "beliefs", "fits",
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex)
  )
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
    write_table(beliefs, "beliefs_latest", root = root)
    if (isTRUE(write_archive)) {
      write_table(beliefs, "beliefs_archive", root = root)
    }
  } else {
    cli::cli_alert_warning(
      "fit_league({league$sport}/{league$country}/{sex}): no predictions -- skipping belief writes"
    )
  }

  invisible(beliefs)
}

#' tar_target wrapper: fit a single (league x sex) and return belief row count.
#'
#' Takes the per-league "static" slice (sport, country, stan_model, sexes,
#' data_source) rather than the full leagues config, so the fit cache is
#' insulated from `lengjan` or `betting` changes -- those don't affect the
#' Stan model or its inputs and shouldn't trigger a 30-90 minute refit.
#' `ingest_dep` is unused at runtime; it only declares the DAG edge to the
#' upstream ingest target.
#'
#' @param static Per-league static slice (output of `league_static_<key>`).
#' @param sex `"male"` or `"female"`.
#' @param ingest_dep Pure DAG-dependency declaration; value is ignored.
#' @return Integer count of belief rows written.
#' @export
fit_one <- function(static, sex, ingest_dep = NULL) {
  beliefs <- fit_league(league = static, sex = sex)
  nrow(beliefs)
}

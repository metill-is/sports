#' @include model-prepare.R model-fit.R model-posteriors.R storage.R config.R
NULL

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
                       iter_warmup = 1000L,
                       iter_sampling = 1000L,
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

  prep <- prepare_data(league, sex,
    end_date = end_date, root = root,
    from_season = from_season,
    schedule_horizon_days = schedule_horizon_days
  )

  stan_path <- file.path(stan_dir, league$stan_model)
  if (!file.exists(stan_path)) {
    stop("Stan model missing: ", stan_path, call. = FALSE)
  }

  fit <- fit_model(
    stan_data       = prep$stan_data,
    stan_model_path = stan_path,
    method          = method,
    chains          = chains,
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    seed            = seed
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

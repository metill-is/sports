#' Step: Fit — run model fitting (and optionally results) for a league
#'
#' Dispatches on league$pipeline to call the right fitting code.
#'
#' @usage
#' box::use(R/pipeline/step_fit[run_fit_step])
#' run_fit_step(league, sex, sports_dir, fit_model = TRUE, generate_results = TRUE)

#' @param league League config list from leagues.yml
#' @param sex "male" or "female"
#' @param sports_dir Absolute path to Sports/ root
#' @param iter_warmup Warmup iterations (from leagues.yml defaults or CLI override)
#' @param iter_sampling Sampling iterations
#' @param fit_model Whether to fit the model (default TRUE)
#' @param generate_results Whether to generate results after fitting (default TRUE)
#' @export
run_fit_step <- function(
  league,
  sex,
  sports_dir,
  iter_warmup = 1000,
  iter_sampling = 1000,
  fit_model = TRUE,
  generate_results = TRUE
) {
  handler <- switch(
    league$pipeline,
    shared        = fit_shared,
    football      = fit_football,
    handball_other = fit_handball_other,
    stop("Unknown pipeline: ", league$pipeline)
  )
  handler(
    league, sex, sports_dir,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    do_fit = fit_model,
    do_results = generate_results
  )
}


# ── Helpers ───────────────────────────────────────────────────────────────────

quiet_here <- function(...) suppressMessages(here::i_am(...))


# ── Pipeline: shared (basketball/handball Iceland) ────────────────────────────
#
# Uses R/config/{sport}_iceland.R config objects + R/shared/ modules.
# These modules use here::here() relative to Sports/ root.

fit_shared <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results) {
  # Load the config module to get the rich config object
  # config_module is relative to Sports/ (e.g., "R/config/basketball_iceland.R")
  config_path <- file.path(sports_dir, league$config_module)
  env <- new.env(parent = globalenv())
  source(config_path, local = env)
  config <- env$get_config()

  end_date <- Sys.Date()

  if (do_fit) {
    box::use(R/shared/model_fitting[fit_model_fn = fit_model])
    fit_model_fn(
      config = config,
      sex = sex,
      end_date = end_date,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling
    )
  }

  if (do_results) {
    box::use(R/shared/get_model_results[generate_model_results])
    suppressMessages(generate_model_results(
      config = config,
      sex = sex,
      end_date = end_date
    ))
  }
}


# ── Pipeline: football (football/{country}) ──────────────────────────────────
#
# Each football league has its own R/common/ with fit_football_model(sex, ...).
# Must run from inside the league directory with here::i_am() set.

fit_football <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results) {
  league_dir <- file.path(sports_dir, league$dir)

  withr::with_dir(league_dir, {
    quiet_here(league$rproj %||% ".here")

    # box::use() resolves from calling file's dir, not cwd — use source() instead
    if (do_fit) {
      env <- new.env(parent = globalenv())
      source(here::here("R", "common", "model_fitting.R"), local = env)
      env$fit_football_model(
        sex = sex,
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling
      )
    }

    if (do_results) {
      env <- new.env(parent = globalenv())
      source(here::here("R", "common", "get_model_results.R"), local = env)
      env$generate_model_results(sex)
    }
  })
}


# ── Pipeline: handball_other (European handball via handball/other/) ──────────
#
# handball/other/ has its own model_fitting.R that takes country + sex args.
# Must run from handball/other/ directory.

fit_handball_other <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results) {
  other_dir <- file.path(sports_dir, "handball", "other")

  withr::with_dir(other_dir, {
    quiet_here("handball_other.Rproj")

    if (do_fit) {
      box::use(R/utils/model_fitting[fit_model_fn = fit_model])
      fit_model_fn(
        country = league$country,
        sex = sex,
        end_date = Sys.Date(),
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling
      )
    }

    if (do_results) {
      box::use(R/utils/get_model_results[generate_model_results])
      generate_model_results(
        country = league$country,
        sex = sex,
        end_date = Sys.Date()
      )
    }
  })
}

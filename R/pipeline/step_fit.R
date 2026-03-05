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
#' @param expected_duration Expected fit duration in seconds (from timing cache), or NULL
#' @export
run_fit_step <- function(
  league,
  sex,
  sports_dir,
  iter_warmup = 1000,
  iter_sampling = 1000,
  fit_model = TRUE,
  generate_results = TRUE,
  generate_plots = TRUE,
  expected_duration = NULL
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
    do_results = generate_results,
    make_plots = generate_plots,
    expected_duration = expected_duration
  )
}


# ── Helpers ───────────────────────────────────────────────────────────────────

quiet_here <- function(...) suppressMessages(here::i_am(...))


# ── Pipeline: shared (basketball/handball Iceland) ────────────────────────────
#
# Uses R/config/{sport}_iceland.R config objects + R/shared/ modules.
# These modules use here::here() relative to Sports/ root.

fit_shared <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results, make_plots = TRUE, expected_duration = NULL) {
  # Load the config module to get the rich config object
  # config_module is relative to Sports/ (e.g., "R/config/basketball_iceland.R")
  config_path <- file.path(sports_dir, league$config_module)
  env <- new.env(parent = globalenv())
  source(config_path, local = env)
  config <- env$get_config()

  end_date <- Sys.Date()

  if (do_fit) {
    env <- new.env(parent = globalenv())
    source(file.path(sports_dir, "R", "shared", "model_fitting.R"), local = env)
    env$fit_model(
      config = config,
      sex = sex,
      end_date = end_date,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      expected_duration = expected_duration
    )
  }

  if (do_results) {
    env <- new.env(parent = globalenv())
    source(file.path(sports_dir, "R", "shared", "get_model_results.R"), local = env)
    suppressMessages(env$generate_model_results(
      config = config,
      sex = sex,
      end_date = end_date,
      make_plots = make_plots
    ))

    # Dual-write to Parquet store
    tryCatch({
      source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)
      csv_path <- file.path(sports_dir, config$sport_dir, "results", sex, "posterior_goals.csv")
      if (file.exists(csv_path)) {
        df <- readr::read_csv(csv_path, show_col_types = FALSE)
        store_predictions(df, league$sport, league$country, sex, sports_dir)
      }
    }, error = function(e) warning("Store write failed: ", e$message))
  }
}


# ── Pipeline: football (football/{country}) ──────────────────────────────────
#
# Each football league has its own R/common/ with fit_football_model(sex, ...).
# Must run from inside the league directory with here::i_am() set.

fit_football <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results, make_plots = TRUE, expected_duration = NULL) {
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
      env$generate_model_results(sex, make_plots = make_plots)

      # Dual-write to Parquet store
      tryCatch({
        source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)
        csv_path <- here::here("results", sex, "posterior_goals.csv")
        if (file.exists(csv_path)) {
          df <- readr::read_csv(csv_path, show_col_types = FALSE)
          store_predictions(df, league$sport, league$country, sex, sports_dir)
        }
      }, error = function(e) warning("Store write failed: ", e$message))
    }
  })
}


# ── Pipeline: handball_other (European handball via handball/other/) ──────────
#
# handball/other/ has its own model_fitting.R that takes country + sex args.
# Must run from handball/other/ directory.

fit_handball_other <- function(league, sex, sports_dir, iter_warmup, iter_sampling, do_fit, do_results, make_plots = TRUE, expected_duration = NULL) {
  other_dir <- file.path(sports_dir, "handball", "other")

  withr::with_dir(other_dir, {
    quiet_here("handball_other.Rproj")

    if (do_fit) {
      env <- new.env(parent = globalenv())
      source(here::here("R", "utils", "model_fitting.R"), local = env)
      env$fit_model(
        country = league$country,
        sex = sex,
        end_date = Sys.Date(),
        iter_warmup = iter_warmup,
        iter_sampling = iter_sampling
      )
    }

    if (do_results) {
      env <- new.env(parent = globalenv())
      source(here::here("R", "utils", "get_model_results.R"), local = env)
      env$generate_model_results(
        country = league$country,
        sex = sex,
        end_date = Sys.Date(),
        make_plots = make_plots
      )

      # Dual-write to Parquet store
      tryCatch({
        source(file.path(sports_dir, "R", "storage", "store.R"), local = TRUE)
        csv_path <- here::here("results", league$country, sex, "posterior_goals.csv")
        if (file.exists(csv_path)) {
          df <- readr::read_csv(csv_path, show_col_types = FALSE)
          store_predictions(df, league$sport, league$country, sex, sports_dir)
        }
      }, error = function(e) warning("Store write failed: ", e$message))
    }
  })
}

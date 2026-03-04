#### Model Fitting Utilities ####
box::use(
  cmdstanr[cmdstan_model],
  here[here],
  metill[theme_metill],
  ggplot2[theme_set],
  R / shared / prep_data[prepare_data]
)

theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

# Detect whether cmdstanr supports progressr-based progress bars (PR #1138)
has_progress_bar <- function(model) {
  "show_progress_bar" %in% names(formals(model$sample))
}

#' Format seconds as human-readable duration
format_expected <- function(secs) {
  if (is.null(secs) || is.na(secs)) return(NULL)
  secs <- round(secs)
  if (secs < 60) return(paste0("~", secs, "s"))
  mins <- secs %/% 60
  remaining <- secs %% 60
  if (remaining == 0) return(paste0("~", mins, "m"))
  paste0("~", mins, "m ", remaining, "s")
}

#' Fit model for specified sport and sex
#'
#' @param config Config list from sport-specific config file
#' @param sex Character string, either "male" or "female"
#' @param end_date Date for filtering data
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Number of warmup iterations (default: 1000)
#' @param iter_sampling Number of sampling iterations (default: 1000)
#' @param init Initial values (default: 0)
#' @param expected_duration Expected duration in seconds (from timing cache),
#'   displayed before fitting starts. NULL to suppress.
#'
#' @export
fit_model <- function(
  config,
  sex = "male",
  end_date = Sys.Date(),
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0,
  expected_duration = NULL
) {
  if (!sex %in% c("male", "female")) {
    stop("Sex must be either 'male' or 'female'")
  }

  # Show cached expected duration
  dur_label <- format_expected(expected_duration)
  if (!is.null(dur_label)) {
    cat(sprintf(" (%s expected)", dur_label))
    utils::flush.console()
  }

  sport_dir <- config$sport_dir
  model_name <- config$stan_model

  # Prepare data (suppress readr column messages)
  stan_data <- suppressMessages(prepare_data(config, sex, end_date))

  # Compile model
  model <- cmdstan_model(
    here(sport_dir, "Stan", model_name)
  )

  # Fit model — use progressr progress bar if available (cmdstanr PR #1138)
  # refresh controls how often Stan emits progress updates; 20 gives ~5% granularity
  if (has_progress_bar(model)) {
    cat("\n")
    results <- model$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      init = init,
      refresh = max(1, (iter_warmup + iter_sampling) %/% 50),
      show_progress_bar = TRUE,
      suppress_iteration_messages = TRUE,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
  } else {
    results <- model$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      init = init,
      refresh = 0,
      show_messages = FALSE,
      show_exceptions = FALSE
    )
  }

  # Save results
  results$save_object(
    file = here(sport_dir, "results", sex, end_date, "fit.rds")
  )
}

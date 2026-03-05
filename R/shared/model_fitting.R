#### Model Fitting Utilities ####

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

#' Fit a Stan model and save the result
#'
#' Pure function: stan_data in, fit.rds out. No config coupling, no prep_data.
#'
#' @param stan_data Pre-computed list for Stan
#' @param stan_model_path Absolute path to .stan file
#' @param output_path Absolute path for fit.rds
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Number of warmup iterations (default: 1000)
#' @param iter_sampling Number of sampling iterations (default: 1000)
#' @param init Initial values (default: 0)
#' @param expected_duration Expected duration in seconds (from timing cache),
#'   displayed before fitting starts. NULL to suppress.
fit_model <- function(
  stan_data,
  stan_model_path,
  output_path,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  init = 0,
  expected_duration = NULL
) {
  # Show cached expected duration
  dur_label <- format_expected(expected_duration)
  if (!is.null(dur_label)) {
    cat(sprintf(" (%s expected)", dur_label))
    utils::flush.console()
  }

  # Compile model
  model <- cmdstanr::cmdstan_model(stan_model_path)

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
      refresh = min(20, max(1, (iter_warmup + iter_sampling) %/% 50)),
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

  # Ensure output directory exists
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Save results
  results$save_object(file = output_path)
}

#' Fit a Stan model and return the result object.
#'
#' Thin wrapper over `cmdstanr::cmdstan_model()$sample()` (or `$pathfinder()`,
#' `$variational()`). Unlike the legacy counterpart, this does **not** write
#' to disk -- callers handle `save_object()`.
#'
#' For approximate methods, `generate_quantities()` is called automatically
#' to produce predictive draws so downstream code (extract_posteriors) works
#' unchanged.
#'
#' @param stan_data Named list matching the model's `data {}` block.
#' @param stan_model_path Absolute path to a `.stan` file.
#' @param method "sample" (default), "pathfinder", or "variational".
#' @param chains Number of MCMC chains (MCMC only).
#' @param parallel_chains Number of chains to run in parallel.
#' @param iter_warmup,iter_sampling Iteration counts (MCMC only).
#' @param num_paths Number of Pathfinder paths.
#' @param draws Number of draws for approximate methods.
#' @param seed Integer seed for reproducibility. NULL = cmdstanr default.
#' @param init Initial values passed to cmdstanr. Default 0 matches legacy.
#' @param show_progress Print cmdstanr progress bar? Default TRUE.
#' @return CmdStanMCMC (sample) or CmdStanGQ (pathfinder / variational).
#' @export
fit_model <- function(stan_data,
                      stan_model_path,
                      method = c("sample", "pathfinder", "variational"),
                      chains = 4L,
                      parallel_chains = chains,
                      iter_warmup = 1000L,
                      iter_sampling = 1000L,
                      num_paths = 4L,
                      draws = 4000L,
                      seed = NULL,
                      init = 0,
                      show_progress = TRUE) {
  method <- tryCatch(
    match.arg(method),
    error = function(e) {
      stop("method must be one of \"sample\", \"pathfinder\", or \"variational\"",
        call. = FALSE
      )
    }
  )

  model <- cmdstanr::cmdstan_model(stan_model_path, quiet = TRUE)

  common_quiet <- list(show_messages = FALSE, show_exceptions = FALSE)

  if (method == "sample") {
    args <- c(list(
      data            = stan_data,
      chains          = chains,
      parallel_chains = parallel_chains,
      iter_warmup     = iter_warmup,
      iter_sampling   = iter_sampling,
      init            = init,
      refresh         = if (show_progress) 100L else 0L
    ), if (!is.null(seed)) list(seed = seed), common_quiet)
    do.call(model$sample, args)
  } else if (method == "pathfinder") {
    args <- c(list(
      data      = stan_data,
      num_paths = num_paths,
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed))
    approx <- do.call(model$pathfinder, args)
    pf_draws <- posterior::as_draws_matrix(approx$draws())
    model$generate_quantities(fitted_params = pf_draws, data = stan_data)
  } else {
    args <- c(list(
      data      = stan_data,
      algorithm = "fullrank",
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed))
    approx <- do.call(model$variational, args)
    model$generate_quantities(fitted_params = approx, data = stan_data)
  }
}

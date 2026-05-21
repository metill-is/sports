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
#' @param adapt_delta Target acceptance probability during warmup (MCMC only).
#'   Default `0.95` (raised from Stan's stock 0.8 after the 2026-05-17 audit:
#'   the football iceland model's funnel-shaped tails — driven by Mjólkurbikar
#'   blowouts between top-flight and 4th-tier teams — produced 7% divergent
#'   transitions at 0.8, tripping `check_stan_diagnostics()`). Higher = smaller
#'   leapfrog stepsize, fewer divergences, slower warmup.
#' @param max_treedepth Maximum NUTS tree depth (MCMC only). Default `10`
#'   (Stan stock).
#' @param num_paths Number of Pathfinder paths.
#' @param draws Number of draws for approximate methods.
#' @param seed Integer seed for reproducibility. NULL = cmdstanr default.
#' @param init Initial values passed to cmdstanr. Default 0 matches legacy.
#' @param show_progress Print cmdstanr progress bar? Default TRUE.
#' @param check_diagnostics If `TRUE` (default), call
#'   [check_stan_diagnostics()] after sampling and abort with a clear
#'   diagnostic on divergent transitions, R-hat, or ESS problems. Set to
#'   `FALSE` only for explicit pre-flight model exploration where you
#'   know the fit will be poor.
#' @param max_divergent_frac Maximum allowed fraction of post-warmup
#'   iterations that diverged. Default `0.01` (1%).
#' @param max_rhat Maximum allowed R-hat on any monitored parameter.
#'   Default `1.05`.
#' @param min_ess_bulk Minimum allowed bulk ESS on any monitored parameter.
#'   Default `100`.
#' @return CmdStanMCMC (sample) or CmdStanGQ (pathfinder / variational).
#' @export
fit_model <- function(stan_data,
                      stan_model_path,
                      method = c("sample", "pathfinder", "variational"),
                      chains = 4L,
                      parallel_chains = chains,
                      iter_warmup = 1000L,
                      iter_sampling = 1000L,
                      adapt_delta = 0.95,
                      max_treedepth = 10L,
                      num_paths = 4L,
                      draws = 4000L,
                      seed = NULL,
                      init = 0,
                      show_progress = TRUE,
                      check_diagnostics = TRUE,
                      max_divergent_frac = 0.01,
                      max_rhat = 1.05,
                      min_ess_bulk = 100) {
  method <- match.arg(method)

  model <- cmdstanr::cmdstan_model(stan_model_path, quiet = TRUE)

  common_quiet <- list(show_messages = FALSE, show_exceptions = FALSE)

  gq_or_explain <- function(fitted_params) {
    tryCatch(
      model$generate_quantities(fitted_params = fitted_params, data = stan_data),
      error = function(e) {
        stop(
          "generate_quantities() failed after ", method, " fit: ",
          conditionMessage(e),
          "\n  Approximate posteriors can place mass on invalid parameter ",
          "regions (e.g. non-PD covariance matrices). Use method = 'sample'.",
          call. = FALSE
        )
      }
    )
  }

  if (method == "sample") {
    args <- c(list(
      data            = stan_data,
      chains          = chains,
      parallel_chains = parallel_chains,
      iter_warmup     = iter_warmup,
      iter_sampling   = iter_sampling,
      adapt_delta     = adapt_delta,
      max_treedepth   = max_treedepth,
      init            = init,
      refresh         = if (show_progress) 100L else 0L
    ), if (!is.null(seed)) list(seed = seed), common_quiet)
    fit <- do.call(model$sample, args)
    if (isTRUE(check_diagnostics)) {
      check_stan_diagnostics(
        fit,
        max_divergent_frac = max_divergent_frac,
        max_rhat           = max_rhat,
        min_ess_bulk       = min_ess_bulk
      )
    }
    fit
  } else if (method == "pathfinder") {
    args <- c(list(
      data      = stan_data,
      num_paths = num_paths,
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed), common_quiet)
    approx <- do.call(model$pathfinder, args)
    # generate_quantities() does not accept CmdStanPathfinder directly;
    # materialise the draws matrix so it's passed via fitted_params.
    pf_draws <- posterior::as_draws_matrix(approx$draws())
    gq_or_explain(pf_draws)
  } else {
    args <- c(list(
      data      = stan_data,
      algorithm = "fullrank",
      draws     = draws,
      init      = init
    ), if (!is.null(seed)) list(seed = seed), common_quiet)
    approx <- do.call(model$variational, args)
    # CmdStanVB is accepted by generate_quantities() directly.
    gq_or_explain(approx)
  }
}

#' Abort if a Stan fit's posterior is unreliable.
#'
#' Pre-2026-05-15 no R-side code inspected the cmdstanr fit's diagnostics
#' after `$sample()` — a fit with hundreds of divergent transitions, or
#' R-hat well above 1.01, or ESS below the rule-of-thumb 100, would flow
#' silently through `extract_posteriors()` into `data/beliefs/latest/` and
#' become live bets via the placer. The 2026-04-12 memory note about
#' approximate-inference crashing left the converged-but-bad full-NUTS
#' failure mode uncovered. Audit 2026-05-15 §I.
#'
#' Defaults:
#' - `max_divergent_frac = 0.01`: more than 1% divergent transitions on the
#'   post-warmup window means HMC is missing typical-set regions of the
#'   posterior. Below the operational acceptance per
#'   `_legacy/sports/CLAUDE.md`'s historical convergence note.
#' - `max_rhat = 1.05`: chains have not mixed; the published posterior is
#'   not converged.
#' - `min_ess_bulk = 100`: too few effective samples for stable summaries.
#'
#' This function intentionally `stop()`s rather than `quit()`s so callers
#' can wrap it in a tryCatch when scoping (e.g. backfill scripts choosing
#' which fits to refit). `scripts/03_fit.R` converts the abort to a
#' workflow-failing exit at the script boundary.
#'
#' @param fit A `CmdStanMCMC` fit returned by `cmdstan_model()$sample()`.
#' @param max_divergent_frac Numeric in `[0, 1]`.
#' @param max_rhat Numeric `>= 1`.
#' @param min_ess_bulk Numeric `>= 0`.
#' @return Invisibly the fit, on success. Stops with a clear diagnostic
#'   on failure.
#' @keywords internal
#' @export
check_stan_diagnostics <- function(fit,
                                   max_divergent_frac = 0.01,
                                   max_rhat = 1.05,
                                   min_ess_bulk = 100) {
  # Divergent transitions: a hard correctness signal — the sampler
  # actively recognised it was wandering outside the typical set.
  diag_summary <- tryCatch(
    fit$diagnostic_summary(quiet = TRUE),
    error = function(e) NULL
  )
  if (!is.null(diag_summary) && !is.null(diag_summary$num_divergent)) {
    num_divergent <- sum(diag_summary$num_divergent, na.rm = TRUE)
    iter_per_chain <- tryCatch(
      fit$metadata()$iter_sampling,
      error = function(e) NA_integer_
    )
    n_chains <- length(diag_summary$num_divergent)
    total_iter <- if (is.numeric(iter_per_chain) && length(iter_per_chain) == 1L) {
      iter_per_chain * n_chains
    } else {
      NA_integer_
    }
    if (is.finite(total_iter) && total_iter > 0L) {
      div_frac <- num_divergent / total_iter
      if (div_frac > max_divergent_frac) {
        stop(
          sprintf(
            paste0(
              "Stan diagnostic gate: %d divergent transitions in %d ",
              "post-warmup iterations (%.2f%%) > %.2f%% threshold. ",
              "Posterior is unreliable; do not promote to beliefs/latest. ",
              "Reparameterise, raise adapt_delta, or inspect the data."
            ),
            num_divergent, total_iter, div_frac * 100, max_divergent_frac * 100
          ),
          call. = FALSE
        )
      }
    }
  }

  # Mixing diagnostics: R-hat and bulk ESS on all monitored parameters.
  # Summary call is heavier than diagnostic_summary() but still seconds.
  summ <- tryCatch(
    fit$summary(NULL, "rhat", "ess_bulk"),
    error = function(e) NULL
  )
  if (!is.null(summ) && nrow(summ) > 0L) {
    bad_rhat <- summ$rhat > max_rhat & !is.na(summ$rhat)
    if (any(bad_rhat)) {
      worst <- summ[which.max(summ$rhat), , drop = FALSE]
      stop(
        sprintf(
          paste0(
            "Stan diagnostic gate: max R-hat %.3f on parameter %s ",
            "exceeds %.2f. Chains have not mixed; the posterior is not ",
            "trustworthy. Increase iter_warmup or reparameterise."
          ),
          worst$rhat, worst$variable, max_rhat
        ),
        call. = FALSE
      )
    }
    bad_ess <- summ$ess_bulk < min_ess_bulk & !is.na(summ$ess_bulk)
    if (any(bad_ess)) {
      worst <- summ[which.min(summ$ess_bulk), , drop = FALSE]
      stop(
        sprintf(
          paste0(
            "Stan diagnostic gate: min bulk ESS %.0f on parameter %s ",
            "below %d. Too few effective samples for stable summaries. ",
            "Increase iter_sampling or reduce model autocorrelation."
          ),
          worst$ess_bulk, worst$variable, as.integer(min_ess_bulk)
        ),
        call. = FALSE
      )
    }
  }

  invisible(fit)
}

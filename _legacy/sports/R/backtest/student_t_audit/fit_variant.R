#' Fit one Stan variant with common audit conventions.
#'
#' Reproducible audit settings: fixed seed, init=0, 4 chains x 1000 warmup +
#' 1000 sampling (matches production). Returns the cmdstanr fit and a
#' summary diagnostic row.

suppressPackageStartupMessages({
  library(cmdstanr)
})

#' @param stan_path Absolute path to .stan file.
#' @param stan_data list passed as `data=` to sample().
#' @param seed Integer seed (default 20260420, same as production swap audits).
#' @param iter_warmup / iter_sampling / chains / parallel_chains: MCMC settings.
#' @param adapt_delta cmdstanr control argument (default 0.8).
#' @param label Human-readable label for logging.
#' @return list(name, fit, wall_secs, diag)
fit_variant <- function(stan_path,
                        stan_data,
                        label = basename(stan_path),
                        seed = 20260420,
                        iter_warmup = 1000,
                        iter_sampling = 1000,
                        chains = 4,
                        parallel_chains = 4,
                        adapt_delta = 0.8,
                        init = 0,
                        refresh = 200) {
  cat(sprintf("\n[fit_variant] %s\n  path: %s\n", label, stan_path))
  mod <- cmdstan_model(stan_path)

  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data,
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    refresh = refresh,
    seed = seed,
    init = init,
    adapt_delta = adapt_delta,
    # Silence per-iteration leapfrog rejection spam. The Student-t
    # direct_SD model emits frequent "Sigma not symmetric" warnings
    # during warmup as HMC probes the Sigma-overflow boundary; they
    # are informational (rejected proposals, no effect on posterior)
    # but flooding stdout slowed the 2026-04-20 first attempt to a
    # crawl. Real divergences are still tracked via diagnostic_summary.
    show_messages = FALSE,
    show_exceptions = FALSE
  )
  wall_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat(sprintf("[fit_variant] %s: wall %.1f min\n", label, wall_secs / 60))

  list(
    name = label,
    stan_path = stan_path,
    fit = fit,
    wall_secs = wall_secs,
    seed = seed,
    settings = list(
      chains = chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      init = init
    )
  )
}

#' Produce a diagnostic snapshot (Rhat / ESS / divergences / E-BFMI / treedepth).
#' Uses only `fit$summary()` and `fit$diagnostic_summary()` — no full
#' posterior scan beyond min/max queries.
collect_diagnostics <- function(result) {
  fit <- result$fit
  diag <- fit$diagnostic_summary(quiet = TRUE)
  summ <- fit$summary()
  summ_finite <- summ[is.finite(summ$rhat), , drop = FALSE]

  list(
    wall_min = result$wall_secs / 60,
    chains = length(diag$num_divergent),
    iter_sampling = fit$metadata()$iter_sampling,
    total_transitions = length(diag$num_divergent) * fit$metadata()$iter_sampling,
    divergences = sum(diag$num_divergent),
    div_rate = sum(diag$num_divergent) /
      (length(diag$num_divergent) * fit$metadata()$iter_sampling),
    max_treedepth_hits = sum(diag$num_max_treedepth),
    ebfmi = diag$ebfmi,
    max_rhat = max(summ_finite$rhat),
    max_rhat_var = summ_finite$variable[which.max(summ_finite$rhat)],
    min_ess_bulk = min(summ_finite$ess_bulk, na.rm = TRUE),
    min_ess_bulk_var = summ_finite$variable[which.min(summ_finite$ess_bulk)],
    min_ess_tail = min(summ_finite$ess_tail, na.rm = TRUE),
    min_ess_tail_var = summ_finite$variable[which.min(summ_finite$ess_tail)],
    n_params = nrow(summ_finite)
  )
}

#' Pretty-print a diagnostic list to a connection (default stdout).
#' Used by compare.R to build compare_*.txt artefacts.
print_diagnostics <- function(d, label, con = stdout()) {
  w <- function(...) cat(sprintf(...), file = con)
  w("  label:                %s\n", label)
  w("  wall:                 %.1f min\n", d$wall_min)
  w(
    "  chains x iter:        %d x %d (total %d transitions)\n",
    d$chains, d$iter_sampling, d$total_transitions
  )
  w("  divergences:          %d (%.2f%%)\n", d$divergences, 100 * d$div_rate)
  w("  max_treedepth hits:   %d\n", d$max_treedepth_hits)
  w(
    "  E-BFMI per chain:     %s\n",
    paste(sprintf("%.3f", d$ebfmi), collapse = ", ")
  )
  w("  max Rhat:             %.4f  (on %s)\n", d$max_rhat, d$max_rhat_var)
  w("  min ESS_bulk:         %.0f  (on %s)\n", d$min_ess_bulk, d$min_ess_bulk_var)
  w("  min ESS_tail:         %.0f  (on %s)\n", d$min_ess_tail, d$min_ess_tail_var)
  w("  n_params:             %d\n", d$n_params)
}

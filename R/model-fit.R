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
#'   diagnostic on divergent transitions, R-hat, or ESS problems — or on a
#'   fit whose diagnostics could not be read at all. Set to
#'   `FALSE` only for explicit pre-flight model exploration where you
#'   know the fit will be poor.
#' @param max_divergent_frac Maximum allowed fraction of post-warmup
#'   iterations that diverged. Default `0.01` (1%).
#' @param max_rhat Maximum allowed R-hat on any monitored parameter.
#'   Default `1.05`.
#' @param min_ess_bulk Minimum allowed bulk ESS on any monitored parameter.
#'   Default `100`.
#' @param max_treedepth_frac Maximum fraction of post-warmup iterations
#'   allowed to saturate `max_treedepth`. Default `0.20`.
#' @param min_ebfmi Minimum E-BFMI (energy) across chains. Default `0.2`.
#' @param min_ess_tail Minimum tail ESS on any monitored parameter.
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
                      min_ess_bulk = 100,
                      max_treedepth_frac = 0.20,
                      min_ebfmi = 0.2,
                      min_ess_tail = 100) {
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
        min_ess_bulk       = min_ess_bulk,
        max_treedepth_frac = max_treedepth_frac,
        min_ebfmi          = min_ebfmi,
        min_ess_tail       = min_ess_tail
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

#' Extract sampler diagnostics from a cmdstanr fit into a flat metric list.
#'
#' Defensive by construction: every cmdstanr accessor is wrapped so a partial
#' fit, a legacy cmdstanr version, or a stubbed test fit yields `NA` for the
#' missing metric rather than erroring. Gates downstream skip any `NA` metric
#' — but since 2026-09-20 [evaluate_stan_diagnostics()] reports *which* gates
#' it had to skip, so a wholesale accessor failure (every metric `NA`, every
#' gate skipped) can no longer be mistaken for a clean fit.
#'
#' @keywords internal
#' @noRd
extract_stan_metrics <- function(fit) {
  ds <- tryCatch(fit$diagnostic_summary(quiet = TRUE), error = function(e) NULL)
  iter_per_chain <- tryCatch(fit$metadata()$iter_sampling, error = function(e) NA_integer_)

  ds_field <- function(field) {
    if (!is.null(ds) && !is.null(ds[[field]])) ds[[field]] else NULL
  }
  ndiv_vec <- ds_field("num_divergent")
  ntd_vec <- ds_field("num_max_treedepth")
  ebfmi_vec <- ds_field("ebfmi")

  n_chains <- if (!is.null(ndiv_vec)) length(ndiv_vec) else NA_integer_
  total_iter <- if (is.numeric(iter_per_chain) && length(iter_per_chain) == 1L &&
    !is.na(iter_per_chain) && !is.na(n_chains)) {
    as.integer(iter_per_chain) * n_chains
  } else {
    NA_integer_
  }

  frac_of <- function(count_vec) {
    if (is.null(count_vec) || !is.finite(total_iter) || total_iter <= 0L) {
      return(NA_real_)
    }
    sum(count_vec, na.rm = TRUE) / total_iter
  }
  safe_min <- function(x) if (is.null(x) || all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  safe_max <- function(x) if (is.null(x) || all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

  summ <- tryCatch(
    fit$summary(NULL, "rhat", "ess_bulk", "ess_tail"),
    error = function(e) NULL
  )
  col <- function(nm) {
    if (!is.null(summ) && nm %in% names(summ) && nrow(summ) > 0L) summ[[nm]] else NULL
  }
  worst <- function(nm, which_fn) {
    v <- col(nm)
    if (is.null(v)) {
      return(NA_character_)
    }
    idx <- which_fn(v)
    if (length(idx) == 0L) NA_character_ else summ$variable[idx]
  }

  list(
    n_divergent = if (!is.null(ndiv_vec)) sum(ndiv_vec, na.rm = TRUE) else NA_integer_,
    total_iter = total_iter,
    n_chains = n_chains,
    div_frac = frac_of(ndiv_vec),
    n_max_treedepth = if (!is.null(ntd_vec)) sum(ntd_vec, na.rm = TRUE) else NA_integer_,
    treedepth_frac = frac_of(ntd_vec),
    min_ebfmi = safe_min(ebfmi_vec),
    max_rhat = safe_max(col("rhat")),
    min_ess_bulk = safe_min(col("ess_bulk")),
    min_ess_tail = safe_min(col("ess_tail")),
    worst_rhat_var = worst("rhat", which.max),
    worst_ess_bulk_var = worst("ess_bulk", which.min),
    worst_ess_tail_var = worst("ess_tail", which.min)
  )
}

#' Evaluate extracted Stan metrics against gate thresholds.
#'
#' Pure: takes the metric list from [extract_stan_metrics()] and returns a
#' character vector of gate-failure messages (empty when all gates pass). Each
#' gate is skipped when its metric is `NA`. The divergence / R-hat / bulk-ESS
#' messages are preserved verbatim from the pre-2026-05-30 gate so existing
#' callers and tests see identical text.
#'
#' The returned vector carries two attributes recording gate *coverage*:
#' `gates_evaluated` and `gates_skipped`, each a character vector of gate
#' names. They are attributes rather than a list return so every existing
#' caller (`length(msgs)`, `paste(msgs, collapse = ...)`) keeps working
#' unchanged. Callers that care whether the fit was actually checked — as
#' opposed to merely not flagged — must read `gates_evaluated`: an empty
#' message vector with zero evaluated gates is an unchecked fit, not a clean
#' one.
#'
#' @keywords internal
#' @noRd
evaluate_stan_diagnostics <- function(metrics,
                                      max_divergent_frac = 0.01,
                                      max_rhat = 1.05,
                                      min_ess_bulk = 100,
                                      max_treedepth_frac = 0.20,
                                      min_ebfmi = 0.2,
                                      min_ess_tail = 100) {
  m <- metrics
  msgs <- character(0)

  # Gate-coverage bookkeeping. A gate whose metric is NA never runs, and until
  # 2026-09-20 that silence was indistinguishable from a pass: when a cmdstanr
  # accessor threw inside extract_stan_metrics() *every* metric degraded to NA,
  # so every gate skipped and the empty message vector was read upstream as
  # "clean fit" — a fail-OPEN gate on the path that feeds beliefs/latest and
  # the placer. Recording which gates ran lets the caller tell an unchecked
  # fit from a checked one; the skipping itself stays deliberate, so a partial
  # accessor failure still lets the gates that *can* run, run.
  evaluated <- character(0)
  skipped <- character(0)

  # `breached` and `msg` are promises forced only once the metric is known to
  # be non-NA, so a skipped gate never compares against NA nor sprintf()s one
  # into a message. `if (breached)` rather than `isTRUE(breached)` on purpose:
  # a non-NA metric against an NA threshold must still error loudly, exactly
  # as the pre-2026-09-20 `!is.na(x) && x > thr` form did.
  gate <- function(name, value, breached, msg) {
    if (is.na(value)) {
      skipped <<- c(skipped, name)
      return(invisible(NULL))
    }
    evaluated <<- c(evaluated, name)
    if (breached) msgs <<- c(msgs, msg)
    invisible(NULL)
  }

  gate("divergences", m$div_frac, m$div_frac > max_divergent_frac, sprintf(
    paste0(
      "Stan diagnostic gate: %d divergent transitions in %d ",
      "post-warmup iterations (%.2f%%) > %.2f%% threshold. ",
      "Posterior is unreliable; do not promote to beliefs/latest. ",
      "Reparameterise, raise adapt_delta, or inspect the data."
    ),
    m$n_divergent, m$total_iter, m$div_frac * 100, max_divergent_frac * 100
  ))
  gate("R-hat", m$max_rhat, m$max_rhat > max_rhat, sprintf(
    paste0(
      "Stan diagnostic gate: max R-hat %.3f on parameter %s ",
      "exceeds %.2f. Chains have not mixed; the posterior is not ",
      "trustworthy. Increase iter_warmup or reparameterise."
    ),
    m$max_rhat, m$worst_rhat_var, max_rhat
  ))
  gate("bulk ESS", m$min_ess_bulk, m$min_ess_bulk < min_ess_bulk, sprintf(
    paste0(
      "Stan diagnostic gate: min bulk ESS %.0f on parameter %s ",
      "below %d. Too few effective samples for stable summaries. ",
      "Increase iter_sampling or reduce model autocorrelation."
    ),
    m$min_ess_bulk, m$worst_ess_bulk_var, as.integer(min_ess_bulk)
  ))
  gate("treedepth", m$treedepth_frac, m$treedepth_frac > max_treedepth_frac, sprintf(
    paste0(
      "Stan diagnostic gate: %d of %d post-warmup iterations (%.2f%%) ",
      "saturated max treedepth, above the %.2f%% threshold. NUTS is ",
      "hitting the depth limit; raise max_treedepth or reparameterise."
    ),
    m$n_max_treedepth, m$total_iter, m$treedepth_frac * 100, max_treedepth_frac * 100
  ))
  gate("E-BFMI", m$min_ebfmi, m$min_ebfmi < min_ebfmi, sprintf(
    paste0(
      "Stan diagnostic gate: min E-BFMI %.3f below %.2f. Low energy ",
      "fraction of missing information; momentum resampling is ",
      "inefficient. Reparameterise the scale/variance parameters."
    ),
    m$min_ebfmi, min_ebfmi
  ))
  gate("tail ESS", m$min_ess_tail, m$min_ess_tail < min_ess_tail, sprintf(
    paste0(
      "Stan diagnostic gate: min tail ESS %.0f on parameter %s below ",
      "%d. Tail quantiles are unreliable \u2014 the spread/total stakes ",
      "depend on them. Increase iter_sampling."
    ),
    m$min_ess_tail, m$worst_ess_tail_var, as.integer(min_ess_tail)
  ))

  structure(msgs, gates_evaluated = evaluated, gates_skipped = skipped)
}

#' Abort if a Stan fit's posterior is unreliable.
#'
#' Pre-2026-05-15 no R-side code inspected the cmdstanr fit's diagnostics
#' after `$sample()` — a fit with hundreds of divergent transitions, or
#' R-hat well above 1.01, or ESS below the rule-of-thumb 100, would flow
#' silently through `extract_posteriors()` into `data/beliefs/latest/` and
#' become live bets via the placer. The 2026-04-12 memory note about
#' approximate-inference crashing left the converged-but-bad full-NUTS
#' failure mode uncovered. Audit 2026-05-15 §I. 2026-05-30: also gates
#' treedepth saturation, E-BFMI, and tail ESS, and returns the metrics.
#' 2026-09-20: the gate was itself fail-open — a cmdstanr accessor failure
#' NA'd every metric, every gate skipped, and this function reported success
#' on a fit nothing had inspected. It now warns whenever a gate had to be
#' skipped, and aborts when *no* gate could be evaluated at all.
#'
#' @param fit A `CmdStanMCMC` fit returned by `cmdstan_model()$sample()`.
#' @param max_divergent_frac Numeric in `[0, 1]`. Default `0.01`.
#' @param max_rhat Numeric `>= 1`. Default `1.05`.
#' @param min_ess_bulk Numeric `>= 0`. Default `100`.
#' @param max_treedepth_frac Maximum fraction of post-warmup iterations
#'   allowed to saturate `max_treedepth`. Default `0.20`.
#' @param min_ebfmi Minimum E-BFMI (energy fraction) across chains.
#'   Default `0.2`.
#' @param min_ess_tail Minimum tail ESS on any monitored parameter.
#'   Default `100`; tail quantiles drive the spread/total stakes.
#' @return Invisibly a named list of extracted diagnostic metrics
#'   (`div_frac`, `treedepth_frac`, `min_ebfmi`, `max_rhat`,
#'   `min_ess_bulk`, `min_ess_tail`, ...), on success. Stops with a clear
#'   diagnostic on failure — either a breached threshold, or no evaluable
#'   gate at all. A partial skip (some metrics `NA`) warns and returns.
#' @keywords internal
#' @export
check_stan_diagnostics <- function(fit,
                                   max_divergent_frac = 0.01,
                                   max_rhat = 1.05,
                                   min_ess_bulk = 100,
                                   max_treedepth_frac = 0.20,
                                   min_ebfmi = 0.2,
                                   min_ess_tail = 100) {
  metrics <- extract_stan_metrics(fit)
  msgs <- evaluate_stan_diagnostics(
    metrics,
    max_divergent_frac = max_divergent_frac,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    max_treedepth_frac = max_treedepth_frac,
    min_ebfmi = min_ebfmi,
    min_ess_tail = min_ess_tail
  )
  evaluated <- attr(msgs, "gates_evaluated")
  skipped <- attr(msgs, "gates_skipped")

  # A skipped gate is not a passed gate. Fit time is the only moment anyone is
  # watching, so name the skips here: the persisted diagnostics row records the
  # counts, but nobody opens that store until something has already gone wrong.
  # Only for a *partial* skip — an all-skipped fit aborts below, which names
  # the same gates, and calling that "partially checked" would understate it.
  if (length(skipped) > 0L && length(evaluated) > 0L) {
    n_gates <- length(evaluated) + length(skipped)
    cli::cli_alert_warning(
      paste0(
        "Stan diagnostic gate: {length(skipped)} of {n_gates} gates could ",
        "not be evaluated (metric was NA): {.val {skipped}}. The fit is ",
        "only partially checked."
      )
    )
  }

  # Zero evaluable gates is indistinguishable from an unchecked fit, and an
  # unchecked posterior is precisely what this gate exists to keep out of
  # beliefs/latest and the placer. Route it down the same abort path a breach
  # takes rather than returning success on an all-NA metric list.
  if (length(evaluated) == 0L) {
    msgs <- c(msgs, sprintf(
      paste0(
        "Stan diagnostic gate: no gate could be evaluated \u2014 every ",
        "diagnostic metric was NA (gates skipped: %s). The fit is ",
        "UNCHECKED, not clean: cmdstanr's $diagnostic_summary() and ",
        "$summary() accessors both failed. Do not promote to ",
        "beliefs/latest."
      ),
      paste(skipped, collapse = ", ")
    ))
  }
  if (length(msgs) > 0L) {
    stop(paste(msgs, collapse = "\n"), call. = FALSE)
  }
  invisible(metrics)
}

#' Persist one row of Stan sampler diagnostics for a completed fit.
#'
#' Writes a single row to the `fit_diagnostics` store
#' (`data/beliefs/diagnostics/sport=*/country=*/sex=*/fit_date=*/`) so that
#' convergence drift *below* the abort gate — divergences creeping from 3 to 80
#' while still under 1%, or R-hat drifting toward 1.05 — becomes observable over
#' time. Best-effort: a write failure warns rather than aborting the fit.
#'
#' `gates_evaluated` / `gates_skipped` (added 2026-09-20) count how many of the
#' six thresholds could actually be checked, so a `passed = TRUE` row can never
#' again mean "nothing was checked". They are written ahead of being added to
#' `schemas()$fit_diagnostics`; `read_table()` opens this store with
#' `unify_schemas = TRUE`, so partitions written before today null-fill.
#'
#' @keywords internal
#' @noRd
persist_fit_diagnostics <- function(fit, league, sex, fit_date,
                                    n_obs = NA_integer_,
                                    adapt_delta = NA_real_,
                                    iter_sampling = NA_integer_,
                                    chains = NA_integer_,
                                    root = here::here("data")) {
  m <- extract_stan_metrics(fit)
  msgs <- evaluate_stan_diagnostics(m)
  n_evaluated <- length(attr(msgs, "gates_evaluated"))
  n_skipped <- length(attr(msgs, "gates_skipped"))
  # `passed` must mean "the gates ran and none breached", never "no gate ran".
  # An all-NA metric row previously landed here as passed = TRUE, i.e. the
  # drift-tracking store recording a clean bill of health for a fit nothing
  # had inspected — the same fail-open hole as the abort gate itself.
  passed <- length(msgs) == 0L && n_evaluated > 0L
  row <- tibble::tibble(
    sport = league$sport,
    country = league$country,
    sex = sex,
    fit_date = as.Date(fit_date),
    n_obs = as.integer(n_obs %||% NA_integer_),
    n_divergent = as.integer(m$n_divergent),
    total_iter = as.integer(m$total_iter),
    div_frac = as.numeric(m$div_frac),
    n_max_treedepth = as.integer(m$n_max_treedepth),
    treedepth_frac = as.numeric(m$treedepth_frac),
    min_ebfmi = as.numeric(m$min_ebfmi),
    max_rhat = as.numeric(m$max_rhat),
    min_ess_bulk = as.numeric(m$min_ess_bulk),
    min_ess_tail = as.numeric(m$min_ess_tail),
    adapt_delta = as.numeric(adapt_delta),
    iter_sampling = as.integer(iter_sampling),
    chains = as.integer(chains),
    gates_evaluated = as.integer(n_evaluated),
    gates_skipped = as.integer(n_skipped),
    passed = passed
  )
  tryCatch(
    write_table(row, "fit_diagnostics", root = root),
    error = function(e) {
      cli::cli_alert_warning(
        "Failed to persist fit diagnostics: {conditionMessage(e)}"
      )
    }
  )
  invisible(row)
}

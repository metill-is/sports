#' Compare two or more fits via PSIS-LOO + diagnostic-snapshot table.
#'
#' Convention: each element of `fits` is a list with
#'   - name       (character, short label)
#'   - fit        (cmdstanr fit object)
#'   - wall_secs  (numeric, seconds)
#'   - diag       (list from collect_diagnostics)
#' If `fit` is missing (e.g. loaded from rds) we load it with save_object.
#'
#' Outputs:
#'   - loo object per fit (written into each list in-place)
#'   - loo_compare table
#'   - concatenated compare_*.txt artefact

suppressPackageStartupMessages({
  library(loo)
})

#' Run loo on each fit's log_lik draws and attach a `loo` list element.
#' Uses cores = min(4, parallel::detectCores()). Caches in the results list.
add_loo <- function(fits, cores = min(4L, parallel::detectCores())) {
  for (i in seq_along(fits)) {
    cat(sprintf("\n[loo] %s\n", fits[[i]]$name))
    ll <- fits[[i]]$fit$draws("log_lik", format = "matrix")
    r_eff <- relative_eff(exp(ll), chain_id = rep(seq_len(fits[[i]]$fit$num_chains()),
      each = fits[[i]]$fit$metadata()$iter_sampling
    ))
    l <- loo(ll, r_eff = r_eff, cores = cores)
    fits[[i]]$loo <- l
    print(l)
  }
  fits
}

#' Run loo_compare; returns a matrix with elpd_diff + SE.
loo_compare_fits <- function(fits) {
  loo_list <- lapply(fits, function(x) x$loo)
  names(loo_list) <- vapply(fits, function(x) x$name, character(1))
  loo_compare(loo_list)
}

#' Summarise posterior-mean of a set of shared parameters across fits.
#' Tolerant of variables missing from some fits - emits NA rows for those.
#' Returns a data.frame with (variable, mean_<name1>, sd_<name1>, ...).
shared_param_summary <- function(fits, variables) {
  out <- NULL
  for (i in seq_along(fits)) {
    all_vars <- fits[[i]]$fit$metadata()$variables
    stripped <- sub("\\[.*$", "", all_vars)
    present <- vapply(variables, function(v) {
      v %in% all_vars || sub("\\[.*$", "", v) %in% stripped
    }, logical(1))
    vars_here <- variables[present]
    if (length(vars_here) == 0) {
      s2 <- data.frame(
        variable = variables,
        mean = NA_real_, sd = NA_real_,
        ess_bulk = NA_real_, rhat = NA_real_
      )
    } else {
      s <- fits[[i]]$fit$summary(variables = vars_here)
      s2 <- data.frame(
        variable = s$variable,
        mean = s$mean,
        sd = s$sd,
        ess_bulk = s$ess_bulk,
        rhat = s$rhat
      )
      missing_vars <- setdiff(variables, s$variable)
      if (length(missing_vars)) {
        s2 <- rbind(s2, data.frame(
          variable = missing_vars,
          mean = NA_real_, sd = NA_real_,
          ess_bulk = NA_real_, rhat = NA_real_
        ))
      }
    }
    names(s2)[-1] <- paste0(names(s2)[-1], "_", fits[[i]]$name)
    if (is.null(out)) out <- s2 else out <- merge(out, s2, by = "variable", all = TRUE)
  }
  out
}

#' Per-fit summary of parameters unique to that model.
per_fit_param_summary <- function(fit, variables) {
  all_vars <- fit$metadata()$variables
  stripped <- sub("\\[.*$", "", all_vars)
  present <- vapply(variables, function(v) {
    v %in% all_vars || sub("\\[.*$", "", v) %in% stripped
  }, logical(1))
  vars_here <- variables[present]
  if (length(vars_here) == 0) {
    return(NULL)
  }
  s <- fit$summary(variables = vars_here)
  data.frame(
    variable = s$variable,
    mean = s$mean,
    sd = s$sd,
    ess_bulk = s$ess_bulk,
    rhat = s$rhat
  )
}

#' Write a comparison report file.
write_compare_report <- function(fits, compare_mat,
                                 shared_params_df = NULL,
                                 extra = NULL,
                                 out_path,
                                 title = "Comparison report") {
  con <- file(out_path, open = "wt")
  on.exit(close(con))
  w <- function(...) cat(sprintf(...), file = con)

  w("%s\n", title)
  w("Date: %s\n", format(Sys.Date()))
  w("Commit: %s\n", tryCatch(
    system("git rev-parse --short HEAD", intern = TRUE),
    error = function(e) "(unknown)"
  ))
  w("%s\n\n", strrep("=", 72))

  for (f in fits) {
    w("=== %s ===\n", f$name)
    w("  stan_path:            %s\n", f$stan_path %||% "(unknown)")
    w("  seed:                 %s\n", as.character(f$seed %||% "?"))
    cat("", file = con)
    source_env <- environment(print_diagnostics)
    print_diagnostics(f$diag, f$name, con = con)
    w("\n")
  }

  w("=== loo_compare ===\n")
  capture.output(print(compare_mat), file = con, append = TRUE)
  w("\n\n")

  if (!is.null(shared_params_df)) {
    w("=== Shared parameter posteriors ===\n")
    capture.output(
      print(shared_params_df, row.names = FALSE, digits = 4),
      file = con, append = TRUE
    )
    w("\n\n")
  }

  if (!is.null(extra)) {
    w("=== Additional metrics ===\n")
    if (is.character(extra)) {
      w("%s\n", extra)
    } else {
      capture.output(print(extra), file = con, append = TRUE)
    }
    w("\n")
  }

  invisible(out_path)
}

# small utility
`%||%` <- function(x, y) if (is.null(x)) y else x

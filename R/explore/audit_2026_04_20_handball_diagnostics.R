#!/usr/bin/env Rscript
# Handball Iceland male — diagnostic deep-dive on the production-length fits.
#
# Loads handball/iceland/results/male/fit_{tier1,scalarsigma}.rds and
# investigates the ~1% divergence rate + max Rhat 1.0094 (tier1) /
# 1.0115 (scalarsig) flagged in compare_2026_04_20.txt.
#
# Specifically:
#   1. Top-20 Rhat tail per variant (which parameter drives max Rhat?)
#   2. E-BFMI per chain (funnel signature)
#   3. Divergence localisation: where (in z_sigma_off / scale_sigma_off space)
#      do divergent transitions sit?
#   4. ν / ρ marginals — eliminate likelihood-side suspects
#
# Output: handball/iceland/results/male/diagnostics_2026_04_20.txt

suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(dplyr)
  library(here)
})

stopifnot(basename(getwd()) %in% c("Sports", "Sports-audit"))

results_dir <- here("handball", "iceland", "results", "male")
out_path <- file.path(results_dir, "diagnostics_2026_04_20.txt")

cat("--- Loading fits ---\n")
fit_tier1 <- readRDS(file.path(results_dir, "fit_tier1.rds"))
fit_scalar <- readRDS(file.path(results_dir, "fit_scalarsigma.rds"))

inspect <- function(name, fit) {
  cat(sprintf("\n##### %s #####\n", name))

  cat("\n--- Top 20 Rhat ---\n")
  s <- fit$summary()
  rhat_top <- s |>
    arrange(desc(rhat)) |>
    head(20) |>
    select(variable, mean, sd, rhat, ess_bulk, ess_tail)
  print(rhat_top, n = 20)

  cat("\n--- Bottom 20 ESS_bulk ---\n")
  ess_low <- s |>
    arrange(ess_bulk) |>
    head(20) |>
    select(variable, mean, sd, rhat, ess_bulk, ess_tail)
  print(ess_low, n = 20)

  cat("\n--- Sampler diagnostics by chain ---\n")
  diag <- fit$sampler_diagnostics(format = "draws_df")
  per_chain <- diag |>
    posterior::as_draws_df() |>
    group_by(.chain) |>
    summarise(
      divergent = sum(divergent__),
      max_treedepth = sum(treedepth__ >= 10),
      mean_accept = mean(accept_stat__),
      mean_step = mean(stepsize__),
      mean_leapfrog = mean(n_leapfrog__),
      .groups = "drop"
    )
  print(per_chain)

  cat("\n--- E-BFMI by chain ---\n")
  ebfmi <- diag |>
    posterior::as_draws_df() |>
    group_by(.chain) |>
    summarise(
      ebfmi = {
        e <- energy__
        de <- diff(e)
        var(de) / var(e)
      },
      .groups = "drop"
    )
  print(ebfmi)

  cat("\n--- nu, rho posteriors ---\n")
  for (v in c("nu", "rho")) {
    d <- as.numeric(fit$draws(variables = v, format = "draws_matrix"))
    cat(sprintf(
      "  %-3s  mean %8.4f  sd %.4f  q[5,50,95] %8.4f / %8.4f / %8.4f\n",
      v, mean(d), sd(d), quantile(d, 0.05), quantile(d, 0.5), quantile(d, 0.95)
    ))
  }

  cat("\n--- scale_sigma_off, scale_sigma_def posteriors ---\n")
  for (v in c("scale_sigma_off", "scale_sigma_def", "mean_sigma_off", "mean_sigma_def")) {
    d <- as.numeric(fit$draws(variables = v, format = "draws_matrix"))
    cat(sprintf(
      "  %-18s  mean %8.4f  sd %.4f  q[5,50,95] %8.4f / %8.4f / %8.4f\n",
      v, mean(d), sd(d), quantile(d, 0.05), quantile(d, 0.5), quantile(d, 0.95)
    ))
  }

  cat("\n--- Divergence localisation in (scale_sigma_off, scale_sigma_def) ---\n")
  draws_df <- fit$draws(
    variables = c("scale_sigma_off", "scale_sigma_def", "nu", "rho"),
    format = "draws_df"
  )
  np <- diag |> posterior::as_draws_df()
  joined <- draws_df |>
    inner_join(
      np |> select(.iteration, .chain, divergent__),
      by = c(".iteration", ".chain")
    )
  div <- joined |> filter(divergent__ == 1)
  ok <- joined |> filter(divergent__ == 0)
  if (nrow(div) > 0) {
    cat(sprintf("  N divergent: %d, N ok: %d\n", nrow(div), nrow(ok)))
    summarise_pair <- function(label, x) {
      cat(sprintf(
        "  %-18s  ok mean %.4f sd %.4f  |  div mean %.4f sd %.4f  |  ratio of means %.3f\n",
        label,
        mean(ok[[x]]), sd(ok[[x]]),
        mean(div[[x]]), sd(div[[x]]),
        mean(div[[x]]) / mean(ok[[x]])
      ))
    }
    summarise_pair("scale_sigma_off", "scale_sigma_off")
    summarise_pair("scale_sigma_def", "scale_sigma_def")
    summarise_pair("nu", "nu")
    summarise_pair("rho", "rho")
  } else {
    cat("  (no divergences in this fit)\n")
  }

  invisible(NULL)
}

sink(out_path, split = TRUE)
cat("# Handball Iceland male — diagnostic deep-dive\n")
cat(sprintf("# Generated %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("# Loaded from fit_tier1.rds + fit_scalarsigma.rds\n")
inspect("tier1 (per-team sigma)", fit_tier1)
inspect("scalarsig (scalar sigma)", fit_scalar)
sink()

cat(sprintf("\nDiagnostics written to %s\n", out_path))

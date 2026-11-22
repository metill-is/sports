#!/usr/bin/env Rscript
# Fit the Stan reference of the national-team model before one tournament and
# compare it with the INLA fit of the same model (strengths at the forecast
# origin, hyperparameters, tournament scores).
#
# Usage:
#   Rscript scripts/nt/stan_reference.R <results.csv> <out_dir> <key> \
#     [damp=0|1] [inla_fit=<fit rds from evaluate_tournaments.R>]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
csv_path <- args[[1]]
out_dir <- args[[2]]
key <- args[[3]]
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
damp <- opt("damp", "0") == "1"
inla_path <- opt("inla_fit", "")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

results <- nt_read_results(csv_path)
spec <- nt_tournaments()
spec <- spec[spec$key == key, ]
tm <- nt_tournament_matches(results, key)
venue <- as.integer(!tm$neutral)
prep <- nt_prepare(results, spec$start)
sd <- nt_stan_data(prep, tm$home_team, tm$away_team, venue, damp_friendly = damp)
cat(sprintf("K=%d T=%d N=%d N_pred=%d damp=%d\n", sd$K, sd$T, sd$N, sd$N_pred, sd$damp_friendly))

mod <- cmdstanr::cmdstan_model(here::here("Stan", "football_national", "nt_v2.stan"), quiet = TRUE)
t0 <- Sys.time()
fit <- mod$sample(
  data = sd, chains = 4L, parallel_chains = 4L,
  iter_warmup = 750L, iter_sampling = 750L,
  adapt_delta = 0.9, seed = 20261115L, init = 0.1, refresh = 250L,
  show_messages = FALSE, show_exceptions = FALSE
)
wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

scalars <- c("mu", "home_for", "home_against", "sigma_alpha", "sigma_u", "sigma_delta")
if (damp) scalars <- c(scalars, "kappa_friendly")
summ <- fit$summary(c(scalars, "theta_now"))
diag <- fit$diagnostic_summary(quiet = TRUE)
sampler <- posterior::as_draws_df(fit$sampler_diagnostics())
cat(sprintf(
  "DIAG max_rhat=%.4f min_ess_bulk=%.0f min_ess_tail=%.0f divergences=%d max_treedepth_hits=%d min_ebfmi=%.3f mean_leapfrog=%.0f wall_min=%.1f\n",
  max(summ$rhat, na.rm = TRUE), min(summ$ess_bulk, na.rm = TRUE), min(summ$ess_tail, na.rm = TRUE),
  sum(diag$num_divergent), sum(diag$num_max_treedepth), min(diag$ebfmi),
  mean(sampler$n_leapfrog__), wall_min
))
print(as.data.frame(summ[summ$variable %in% scalars, c("variable", "mean", "sd", "q5", "q95", "rhat", "ess_bulk")]), digits = 4)

stan_draws <- nt_stan_draws(fit, prep)
grid <- nt_scoreline_probs(nt_match_rates(stan_draws, tm$home_team, tm$away_team, venue))
scores <- nt_evaluate_grid(grid, tm$home_90, tm$away_90)

out <- list(
  key = key, damp = damp, summary = summ, diagnostics = diag,
  mean_leapfrog = mean(sampler$n_leapfrog__), wall_min = wall_min,
  theta_mean = colMeans(stan_draws$draws$theta), theta_sd = apply(stan_draws$draws$theta, 2, stats::sd),
  delta_mean = colMeans(stan_draws$draws$delta), scores = scores, teams = prep$teams,
  step_months = prep$step_months
)

if (nzchar(inla_path) && file.exists(inla_path)) {
  inla_fit <- readRDS(inla_path)
  i_mean <- colMeans(inla_fit$draws$theta)
  i_sd <- apply(inla_fit$draws$theta, 2, stats::sd)
  centre <- function(x) x - mean(x)
  i_grid <- nt_scoreline_probs(nt_match_rates(inla_fit, tm$home_team, tm$away_team, venue))
  i_probs <- nt_outcome_probs(i_grid)
  out$inla <- list(
    theta_mean = i_mean, theta_sd = i_sd, hyper = inla_fit$hyper, fixed = inla_fit$fixed,
    cor_theta = stats::cor(i_mean, out$theta_mean),
    max_abs_theta = max(abs(centre(i_mean) - centre(out$theta_mean))),
    sd_ratio = stats::median(i_sd / out$theta_sd),
    max_abs_prob = max(abs(as.matrix(i_probs[, 1:3]) - as.matrix(scores[, 1:3]))),
    mean_abs_prob = mean(abs(as.matrix(i_probs[, 1:3]) - as.matrix(scores[, 1:3])))
  )
  cat(sprintf(
    "INLA vs Stan: cor(theta mean) %.4f, max |centred theta diff| %.3f, median sd ratio INLA/Stan %.3f, 1X2 prob diff mean %.4f max %.4f\n",
    out$inla$cor_theta, out$inla$max_abs_theta, out$inla$sd_ratio,
    out$inla$mean_abs_prob, out$inla$max_abs_prob
  ))
  print(inla_fit$hyper)
}
cat(sprintf(
  "Stan scores on %s: log1x2 %.4f rps %.4f scoreline %.4f\n",
  key, mean(scores$log_1x2), mean(scores$rps), mean(scores$log_score)
))
saveRDS(out, file.path(out_dir, sprintf("stan_reference_%s_%s.rds", key, if (damp) "damped" else "base")))
saveRDS(stan_draws, file.path(out_dir, sprintf("stan_draws_%s_%s.rds", key, if (damp) "damped" else "base")))

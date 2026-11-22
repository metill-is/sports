# Fit one ladder rung for one cell with production sampler settings; save timing,
# sampler diagnostics, R-hat / ESS for every sampled parameter, the draws needed for
# held-out scoring, and PSIS-LOO.
# Usage: Rscript docs/reports/2026-11-15-hb-bb-v2/run_fit.R <cell> <model> <cutoff>
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
suppressMessages({
  library(cmdstanr)
  library(posterior)
})
# WHY: CI pins CmdStan 2.38.0 for the production fit, and the football pilot was timed
# on it; every rung is timed on the same build.
suppressMessages(set_cmdstan_path("/Users/brynjolfurjonsson/.cmdstan/cmdstan-2.38.0"))
args <- commandArgs(trailingOnly = TRUE)
cell <- args[[1]]
model <- args[[2]]
cutoff <- args[[3]]
ci <- CELLS[[cell]]
sc <- SPORT_SCALE[[ci$sport]]
d <- readRDS(file.path(OUT, "out", sprintf("data_%s_%s.rds", cell, cutoff)))

pc_rate <- function(u, alpha = 0.01) -log(alpha) / u
slot_data <- function(extra = list()) {
  c(d$slots, list(
    prior_level = c(sc$score_mean, sc$score_mean_sd),
    prior_score_sd = sc$score_sd_prior,
    prior_ha_mean_sd = sc$hfa_sd, prior_ha_dev_sd = sc$u_ha
  ), extra)
}
v2_stan_data <- function(which, within_mode = 1L) {
  v <- d$v2
  base <- v[c(
    "K", "NS", "TS", "W", "ND", "N", "N_pred", "home", "away", "season", "div", "wk",
    "ts_home", "ts_away", "home_pred", "away_pred", "season_pred", "div_pred", "wk_pred",
    "ts_home_pred", "ts_away_pred"
  )]
  base <- c(base, list(within_mode = within_mode, rho_week = 0.98))
  rate_w <- pc_rate(if (within_mode == 2L) v2_priors(ci$sport)$u_w_ar1 else sc$u_w)
  if (which == "margin") {
    c(base, list(
      y = v$y_home - v$y_away, rate_sigma = pc_rate(sc$u_sigma_m), rate_b = pc_rate(sc$u_b),
      rate_u = pc_rate(sc$u_u), rate_w = rate_w, rate_ha = pc_rate(sc$u_ha),
      rho_mu = log(1.7 / 0.3), rho_sd = 0.75, hfa_sd = sc$hfa_sd
    ))
  } else {
    c(base, list(
      y = v$y_home + v$y_away, rate_sigma = pc_rate(sc$u_sigma_t), rate_b = pc_rate(sc$u_b),
      rate_u = pc_rate(sc$u_u), rate_w = rate_w, rate_season = pc_rate(sc$u_season),
      rho_mu = log(1.7 / 0.3), rho_sd = 0.75, level_mean = sc$total_mean, level_sd = sc$total_mean_sd
    ))
  }
}

league <- load_leagues()[[ci$league]]
registry <- list(
  r0_prod = list(file = file.path("Stan", league$stan_model), data = function() d$prod),
  r1_ragged = list(file = file.path(EXP_DIR, "stan", paste0(ci$sport, "_r1_ragged.stan")), data = slot_data),
  r2_common = list(file = file.path(EXP_DIR, "stan", "r2_common.stan"), data = slot_data),
  r3_pooled = list(file = file.path(EXP_DIR, "stan", "r3_pooled.stan"), data = slot_data),
  r4_ident = list(file = file.path(EXP_DIR, "stan", "r4_ident.stan"), data = function() slot_data(list(use_t = 1L))),
  r5_gauss = list(file = file.path(EXP_DIR, "stan", "r4_ident.stan"), data = function() slot_data(list(use_t = 0L))),
  v2_margin = list(file = file.path(EXP_DIR, "stan", "v2_margin.stan"), data = function() v2_stan_data("margin", 1L)),
  v2_total = list(file = file.path(EXP_DIR, "stan", "v2_total.stan"), data = function() v2_stan_data("total", 1L)),
  v2s_margin = list(file = file.path(EXP_DIR, "stan", "v2_margin.stan"), data = function() v2_stan_data("margin", 0L)),
  v2s_total = list(file = file.path(EXP_DIR, "stan", "v2_total.stan"), data = function() v2_stan_data("total", 0L)),
  v2a_margin = list(file = file.path(EXP_DIR, "stan", "v2_margin.stan"), data = function() v2_stan_data("margin", 2L)),
  v2a_total = list(file = file.path(EXP_DIR, "stan", "v2_total.stan"), data = function() v2_stan_data("total", 2L))
)
reg <- registry[[model]]
stopifnot(!is.null(reg), file.exists(reg$file))

exe_dir <- file.path(OUT, "stan", "exe")
dir.create(exe_dir, showWarnings = FALSE, recursive = TRUE)
# WHY: rungs share Stan files and #include fragments, and the two sports' production
# files share a name, so the executable is keyed on the hash of the source plus includes.
inc_dir <- normalizePath(file.path(EXP_DIR, "stan"))
inc <- list.files(inc_dir, pattern = "^_.*[.]stan$", full.names = TRUE)
key <- substr(digest::digest(unname(tools::md5sum(c(reg$file, inc)))), 1, 8)
src <- file.path(exe_dir, sprintf("%s_%s.stan", tools::file_path_sans_ext(basename(reg$file)), key))
if (!file.exists(src)) file.copy(reg$file, src)
mod <- cmdstan_model(src, dir = exe_dir, include_paths = inc_dir, quiet = TRUE)
stan_data <- reg$data()
csv_dir <- file.path(OUT, "out", sprintf("csv_%s_%s", cell, model))
dir.create(csv_dir, showWarnings = FALSE)

# What else was competing for the CPU when this fit started: wall-clock is only
# comparable between fits that ran under the same conditions.
machine <- list(
  started = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  loadavg = tryCatch(system("sysctl -n vm.loadavg", intern = TRUE), error = function(e) NA_character_),
  slots = tryCatch(
    system("/Users/brynjolfurjonsson/.claude-reviews/machine-slots/slot.sh status", intern = TRUE),
    error = function(e) NA_character_
  )
)
recover <- length(args) >= 4 && args[[4]] == "recover"
if (recover) {
  # WHY: re-read the chain CSVs of a fit whose post-processing did not finish.
  files <- list.files(csv_dir, pattern = "[.]csv$", full.names = TRUE)
  stopifnot(length(files) == 4)
  fit <- as_cmdstan_fit(files)
  # A fifth argument says the fit ran inside the machine limiter: then the slowest
  # chain's elapsed time stands in for the wall time. Without it no time is reported.
  in_slot <- length(args) >= 5
  wall <- if (in_slot) max(fit$time()$chains$total) else NA_real_
  machine$started <- "recovered from chain CSVs; start conditions not recorded"
  machine$slots <- if (in_slot) args[[5]] else NA_character_
} else {
  stale <- file.remove(list.files(csv_dir, full.names = TRUE))
  t0 <- Sys.time()
  fit <- mod$sample(
    data = stan_data, chains = 4, parallel_chains = 4, iter_warmup = 1000, iter_sampling = 1000,
    adapt_delta = 0.95, max_treedepth = 10, init = 0, seed = 20261115, refresh = 500,
    output_dir = csv_dir, show_messages = FALSE, show_exceptions = FALSE
  )
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
}
tm <- fit$time()
ds <- fit$diagnostic_summary(quiet = TRUE)
sp <- fit$sampler_diagnostics(format = "df")

sizes <- fit$metadata()$stan_variable_sizes
n_of <- function(v) prod(sizes[[v]])
sampled <- names(mod$variables()$parameters)
# WHY: a rung can switch a block off (the static v2 form has no within-season terms),
# which leaves zero-length parameters; they are absent from the output's variable list.
sampled <- intersect(sampled, names(sizes))
summ <- summarise_draws(fit$draws(variables = sampled), "mean", "sd", "rhat", "ess_bulk", "ess_tail")
summ <- as.data.frame(summ)

small <- sampled[vapply(sampled, n_of, numeric(1)) <= 2 * max(stan_data$K, 1)]
extra <- intersect(
  c(
    "cur_offense_away", "cur_defense_away", "mean_goals", "sigma_team", "sigma_off", "sigma_def",
    "home_advantage_off", "home_advantage_def", "att_last", "def_last", "loc_m", "sc_m", "loc_t",
    "sc_t", "nu_out", "mu_pred", "rho", "b", "u", "ha", "level", "sigma_w"
  ),
  names(sizes)
)
dr <- fit$draws(variables = unique(c("lp__", small, extra)))
hyper <- summarise_draws(
  fit$draws(variables = small), "mean", "sd",
  ~ quantile(.x, c(0.05, 0.5, 0.95)), "rhat", "ess_bulk", "ess_tail"
)

ll <- fit$draws("log_lik", format = "matrix")
loo_res <- loo::loo(ll, r_eff = loo::relative_eff(exp(ll), chain_id = rep(1:4, each = 1000)), cores = 2)

worst <- function(col, f) summ[order(f(summ[[col]])), c("variable", "rhat", "ess_bulk", "ess_tail")][1:10, ]
res <- list(
  cell = cell, model = model, cutoff = cutoff, wall_secs = wall, time = tm, machine = machine,
  total_leapfrog = sum(sp$n_leapfrog__),
  n_sampled = nrow(summ), n_params = sizes[sampled],
  n_divergent = sum(ds$num_divergent), n_max_treedepth = sum(ds$num_max_treedepth), ebfmi = ds$ebfmi,
  mean_treedepth = mean(sp$treedepth__), mean_leapfrog = mean(sp$n_leapfrog__),
  stepsize = tapply(sp$stepsize__, sp$.chain, mean),
  max_rhat = max(summ$rhat, na.rm = TRUE), min_ess_bulk = min(summ$ess_bulk, na.rm = TRUE),
  min_ess_tail = min(summ$ess_tail, na.rm = TRUE),
  worst_ess = worst("ess_bulk", identity), worst_rhat = worst("rhat", function(x) -x),
  hyper = as.data.frame(hyper), loo = loo_res$estimates,
  loo_pointwise = loo_res$pointwise[, "elpd_loo"], pareto_k_bad = sum(loo_res$diagnostics$pareto_k > 0.7)
)
saveRDS(res, file.path(OUT, "out", sprintf("res_%s_%s.rds", cell, model)))
saveRDS(as_draws_matrix(dr), file.path(OUT, "out", sprintf("draws_%s_%s.rds", cell, model)))
ok <- file.remove(list.files(csv_dir, full.names = TRUE))
unlink(csv_dir, recursive = TRUE)
cat(sprintf(
  "DONE %s %s: wall %.0fs, %d sampled params, div %d, maxtd %d, leapfrog %.0f, max rhat %.3f, min ess_bulk %.0f (%s), elpd_loo %.1f (bad k %d)\n",
  cell, model, wall, res$n_sampled, res$n_divergent, res$n_max_treedepth, res$mean_leapfrog,
  res$max_rhat, res$min_ess_bulk, res$worst_ess$variable[1],
  loo_res$estimates["elpd_loo", "Estimate"], res$pareto_k_bad
))

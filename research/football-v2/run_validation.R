# Like-for-like engine validation: fit ONE spec with Stan (NUTS) and with
# INLA (full integration, empirical Bayes) at one cutoff.
#
#   Rscript research/football-v2/run_validation.R <sex> <cutoff> <horizon_end> <engine> [variant] [tag]
#   engine: stan | inla; variant: a name from variants.R (default "base");
#   tag names the output files (default "calendar", the reference run).
# Run from the repo root (load_leagues() resolves config through here::here()).
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
source("research/football-v2/variants.R")
fv2_source()

args <- commandArgs(trailingOnly = TRUE)
sex <- args[[1]]
cutoff <- as.Date(args[[2]])
horizon_end <- as.Date(args[[3]])
engine <- args[[4]]
variant <- if (length(args) >= 5L) args[[5]] else "base"
tag <- if (length(args) >= 6L) args[[6]] else "calendar"

out <- fv2_out_dir()
league <- load_leagues()[["football_iceland"]]
res <- fv2_results(sex)
sp <- fv2_split(res, league, cutoff, horizon_end = horizon_end)
spec <- do.call(fv2_build, c(
  list(sp$train, sp$pred, divisions = fv2_division_table(res, fv2_rank_order(league))),
  fv2_variants()[[variant]]
))
stem <- file.path(out, sprintf("val_%s_%s_%s", tag, sex, cutoff))
saveRDS(list(spec = spec, train = sp$train, pred = sp$pred), paste0(stem, "_spec.rds"))
cat(sprintf(
  "%s %s cutoff %s: K=%d N=%d S=%d (%d fixture-only), holdout=%d\n",
  tag, sex, cutoff, spec$K, spec$N, spec$S, sum(!spec$slots$has_obs), nrow(sp$pred)
))

report <- function(f) {
  pr <- fv2_predict(f$draws, spec)
  sc <- fv2_score(pr$lam_home, pr$lam_away, sp$pred$home_score, sp$pred$away_score)
  cat(sprintf(
    "  %s: scoreline LS %.4f, 1X2 log loss %.4f, mean p(home) %.3f\n",
    f$engine, mean(sc$ls_score), -mean(sc$ls_1x2), mean(sc$p_home)
  ))
}

if (engine == "inla") {
  for (strategy in c("eb", "auto")) {
    f <- fv2_fit_inla(spec, int_strategy = strategy, n_samp = 4000L, threads = "2:1")
    cat(sprintf(
      "INLA %s: fit %.1fs, sampling %.1fs, configs %d, mode status %s\n",
      strategy, f$t_fit, f$t_samp, f$n_config, f$mode_status
    ))
    print(round(f$hyper_summary, 4))
    report(f)
    saveRDS(f, sprintf("%s_inla-%s.rds", stem, strategy))
  }
} else {
  csv_dir <- paste0(stem, "_csv")
  dir.create(csv_dir, showWarnings = FALSE)
  exe_dir <- file.path(dirname(out), "exe")
  f <- fv2_fit_stan(spec, fv2_stan_file(spec, "research/football-v2/stan"), exe_dir, csv_dir)
  d <- f$diagnostics
  cat(sprintf(
    "Stan: wall %.0fs, divergences %d, max treedepth hits %d, leapfrogs/iter %.0f, state max Rhat %.3f, state min ESS %.0f\n",
    d$wall_secs, d$n_divergent, d$n_max_treedepth, d$mean_leapfrog, d$state_max_rhat, d$state_min_ess
  ))
  print(f$summary, digits = 3)
  report(f)
  saveRDS(f, paste0(stem, "_stan.rds"))
  unlink(csv_dir, recursive = TRUE)
}
cat("DONE\n")

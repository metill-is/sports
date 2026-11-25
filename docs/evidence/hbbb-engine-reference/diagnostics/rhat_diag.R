# Which NUTS parameters keep the HB/BB reference's Rhat above 1.01, and do
# longer chains or a higher adapt_delta bring it under 1.02?
# Usage: Rscript rhat_diag.R <sport> <sex> <end_date> <iter_sampling> <adapt_delta> <out.rds>
options(width = 160)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
a <- commandArgs(trailingOnly = TRUE)
sport <- a[1]; sex <- a[2]; end_date <- as.Date(a[3]); iters <- as.integer(a[4]); ad <- as.numeric(a[5]); out <- a[6]
league <- load_leagues()[[paste0(sport, "_iceland")]]
opts <- hbbb_v2_options(sport, sex)
prep <- prepare_data(league, sex, end_date = end_date, root = here::here("data"))
spec <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = here::here("data"), opts = opts)
if (spec$v$N_pred == 0L) {
  sched <- as.data.frame(read_table("schedules", root = here::here("data"), filter = list(sport = sport, country = "iceland", sex = sex)))
  pairs <- hbbb_v2_break_fixtures(spec$train, sched, end_date, opts$division_map)
  spec <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = here::here("data"), opts = opts, pred = pairs)
}
stan_dir <- do.call(here::here, as.list(HBBB_REFERENCE_STAN_DIR))
exe_dir <- file.path(tempdir(), "exe"); dir.create(exe_dir)
res <- list()
for (resp in c("margin", "total")) {
  mod <- cmdstanr::cmdstan_model(file.path(stan_dir, sprintf("v2_%s.stan", resp)), dir = exe_dir, include_paths = stan_dir, quiet = TRUE)
  t0 <- Sys.time()
  fit <- mod$sample(data = hbbb_v2_stan_data(spec, resp, v2_priors(sport)), chains = 4, parallel_chains = 4,
    iter_warmup = 1000, iter_sampling = iters, adapt_delta = ad, max_treedepth = 10, init = 0,
    seed = as.integer(format(end_date, "%Y%m%d")), refresh = 0, show_messages = FALSE, show_exceptions = FALSE)
  wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  hyper <- c("sigma", "sigma_b", "sigma_u", "theta_rho", if (!identical(spec$opts$within, "none")) "sigma_w",
             if (resp == "margin") c("sigma_ha", "hfa") else c("sigma_season", "alpha"))
  sm <- posterior::summarise_draws(fit$draws(c(hyper, "b", "u", "mu_pred")), "mean", "sd", "rhat", "ess_bulk", "ess_tail")
  dg <- fit$diagnostic_summary(quiet = TRUE)
  sm <- as.data.frame(sm)
  top <- sm[order(-sm$rhat), ][1:12, ]
  cat("\n==", sport, sex, resp, "iters", iters, "adapt_delta", ad, "wall", round(wall), "s; divergent", sum(dg$num_divergent),
      "treedepth", sum(dg$num_max_treedepth), "\n")
  cat("max rhat: hyper", round(max(sm$rhat[sm$variable %in% c(hyper, grep("^alpha|^sigma_w", sm$variable, value = TRUE))]), 4),
      " b/u", round(max(sm$rhat[grepl("^(b|u)\\[", sm$variable)]), 4),
      " mu_pred", round(max(sm$rhat[grepl("^mu_pred", sm$variable)]), 4), "\n")
  print(top, digits = 4)
  res[[resp]] <- list(summary = sm, divergent = sum(dg$num_divergent), wall = wall)
}
saveRDS(res, out)

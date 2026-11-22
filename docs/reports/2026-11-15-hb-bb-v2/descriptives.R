# Mismatch diagnostics from one in-sample fit per cell (static v2, plug-in INLA, all
# played matches up to the newest result): what does the data ask of the likelihood?
# Residuals are in-sample, so variances are slightly optimistic; the calibration tables
# in the report use the walk-forward predictions instead.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/descriptives.R")' [defaults]
#   "defaults" fits with R-INLA's own setting of its online optimisation instead of the
#   fitter's (reproducible = FALSE), as the runs measured in the report were made.
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
reproducible <- !identical(commandArgs(trailingOnly = TRUE)[1], "defaults")
out <- list()
for (cell in names(CELLS)) {
  ci <- CELLS[[cell]]
  league <- load_leagues()[[ci$league]]
  res_all <- cell_results(cell)
  train <- as.data.frame(suppressMessages(model_training_results(res_all, league, end_date = max(res_all$match_date))))
  teams <- sort(unique(c(train$home_team, train$away_team)))
  v <- build_v2_data(train, train[0, ], teams,
    season_start = season_starts(res_all),
    n_weeks = season_weeks(res_all), division_map = DIVISION_MAP[[ci$sport]]
  )
  pr <- v2_priors(ci$sport)
  fm <- fit_v2_inla(v, "margin", pr, within = "none", int_strategy = "eb", cpo = TRUE, reproducible = reproducible)
  ft <- fit_v2_inla(v, "total", pr, within = "none", int_strategy = "eb", cpo = TRUE, reproducible = reproducible)
  d <- train[, c("match_date", "season", "division", "round", "home_team", "away_team", "home_score", "away_score")]
  d$margin <- d$home_score - d$away_score
  d$total <- d$home_score + d$away_score
  d$mu_m <- fm$fitted$mean
  d$mu_t <- ft$fitted$mean
  d$sd_m <- sqrt(fm$fitted$sd^2 + fm$sigma2)
  d$sd_t <- sqrt(ft$fitted$sd^2 + ft$sigma2)
  d$r_m <- d$margin - d$mu_m
  d$r_t <- d$total - d$mu_t
  d$cell <- cell
  out[[cell]] <- list(
    d = d, hyper_m = fm$hyper_sd, hyper_t = ft$hyper_sd, hfa = fm$fixed, alpha = ft$fixed,
    sigma_m = sqrt(fm$sigma2), sigma_t = sqrt(ft$sigma2), divisions = v$divisions,
    status_m = fm$mode_status, status_t = ft$mode_status
  )
  cat(sprintf(
    "\n================ %s: N=%d, hfa %.2f (sd %.2f), raw mean margin %.2f, sigma_m %.2f, sigma_t %.2f, cor(r_m, r_t) %.3f\n",
    cell, nrow(d), fm$fixed$mean[1], fm$fixed$sd[1], mean(d$margin), sqrt(fm$sigma2), sqrt(ft$sigma2), cor(d$r_m, d$r_t)
  ))
  print(fm$hyper_sd[, c("parameter", "q05", "q50", "q95")], digits = 3)
  print(ft$hyper_sd[, c("parameter", "q05", "q50", "q95")], digits = 3)
  # By division: raw margin, HFA, residual sds, residual correlation, implied score variances.
  by_div <- do.call(rbind, lapply(split(d, d$division), function(x) {
    data.frame(
      division = x$division[1], n = nrow(x), raw_margin = mean(x$margin), sd_margin = sd(x$margin),
      mean_total = mean(x$total), sd_total = sd(x$total), home_win = mean(x$margin > 0), draw = mean(x$margin == 0),
      resid_m = mean(x$r_m), rsd_m = sd(x$r_m), rsd_t = sd(x$r_t), cor_mt = cor(x$r_m, x$r_t),
      var_home = var((x$r_t + x$r_m) / 2), var_away = var((x$r_t - x$r_m) / 2),
      kurt_m = mean((x$r_m / sd(x$r_m))^4), kurt_t = mean((x$r_t / sd(x$r_t))^4),
      z3_m = mean(abs(x$r_m / sd(x$r_m)) > 3), z3_t = mean(abs(x$r_t / sd(x$r_t)) > 3)
    )
  }))
  print(by_div, digits = 3, row.names = FALSE)
  # Dispersion of each side's score around its fitted mean, against Poisson and a binomial
  # with n attacks (handball literature: 50 to 60 attacks at about 50 % efficiency).
  mu_h <- (d$mu_t + d$mu_m) / 2
  mu_a <- (d$mu_t - d$mu_m) / 2
  rh <- d$home_score - mu_h
  ra <- d$away_score - mu_a
  disp <- do.call(rbind, lapply(split(seq_len(nrow(d)), d$division), function(ix) {
    data.frame(
      division = d$division[ix][1], mean_score = mean(c(mu_h[ix], mu_a[ix])),
      var_score = mean(c(rh[ix]^2, ra[ix]^2)), pearson_poisson = mean(c(rh[ix]^2 / mu_h[ix], ra[ix]^2 / mu_a[ix])),
      binom_n55 = mean(c(rh[ix]^2 / (mu_h[ix] * (1 - mu_h[ix] / 55)), ra[ix]^2 / (mu_a[ix] * (1 - mu_a[ix] / 55)))),
      cor_home_away = cor(rh[ix], ra[ix])
    )
  }))
  cat("score dispersion by division:\n")
  print(disp, digits = 3, row.names = FALSE)
  out[[cell]]$by_div <- by_div
  out[[cell]]$disp <- disp
  # Residuals against the predicted margin (blowouts) and the predicted total.
  bm <- cut(abs(d$mu_m), quantile(abs(d$mu_m), 0:5 / 5), include.lowest = TRUE)
  cat("margin residual by |predicted margin| quintile: signed residual toward the favourite, sd\n")
  print(do.call(rbind, lapply(split(d, bm), function(x) {
    data.frame(
      n = nrow(x), abs_pred = mean(abs(x$mu_m)), resid_to_fav = mean(sign(x$mu_m) * x$r_m), rsd_m = sd(x$r_m), rsd_t = sd(x$r_t)
    )
  })), digits = 3)
  bt <- cut(d$mu_t, quantile(d$mu_t, 0:5 / 5), include.lowest = TRUE)
  cat("total residual by predicted-total quintile:\n")
  print(do.call(rbind, lapply(split(d, bt), function(x) {
    data.frame(
      n = nrow(x), pred_total = mean(x$mu_t), resid_t = mean(x$r_t), rsd_t = sd(x$r_t), rsd_m = sd(x$r_m)
    )
  })), digits = 3)
  # Level scores and their neighbours against the rounded Gaussian.
  ks <- -4:4
  obs <- vapply(ks, function(k) mean(d$margin == k), numeric(1))
  prd <- vapply(ks, function(k) mean(v2_integer_pmf(rep(k, nrow(d)), d$mu_m, d$sd_m)), numeric(1))
  cat("margin -4..4: observed share, rounded-Gaussian share, ratio\n")
  print(rbind(k = ks, observed = round(obs, 4), predicted = round(prd, 4), ratio = round(obs / prd, 2)))
  p0 <- v2_integer_pmf(rep(0, nrow(d)), d$mu_m, d$sd_m)
  if (any(d$margin == 0)) {
    zs <- v2_fit_zero_shift(p0, d$margin == 0)
    cat(sprintf("level scores: observed %.4f, rounded Gaussian %.4f, logit shift %.3f (se %.3f)\n", mean(d$margin == 0), mean(p0), zs[["shift"]], zs[["se"]]))
    cb <- cut(abs(d$mu_m), c(0, 1, 2, 4, 7, Inf), include.lowest = TRUE)
    print(do.call(rbind, lapply(split(data.frame(p0 = p0, tie = d$margin == 0), cb), function(x) {
      data.frame(
        n = nrow(x), predicted = mean(x$p0), observed = mean(x$tie), ties = sum(x$tie)
      )
    })), digits = 3)
  }
}
saveRDS(out, file.path(OUT, "out", "descriptives.rds"))

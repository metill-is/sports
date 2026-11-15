# Like-for-like check of the R-INLA approximation against NUTS for one identical model:
# v2 with the within-season AR(1), margin and total, same data, same priors.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/compare_validation.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))
cutoff <- "2026-01-31"
qs <- function(x) c(mean = mean(x), sd = stats::sd(x), stats::quantile(x, c(0.05, 0.5, 0.95), names = FALSE))
out <- list()
for (cell in names(CELLS)) {
  fi <- file.path(OUT, "out", sprintf("inla_validate_%s.rds", cell))
  if (!file.exists(fi)) next
  iv <- readRDS(fi)
  d <- readRDS(file.path(OUT, "out", sprintf("data_%s_%s.rds", cell, cutoff)))
  v <- d$v2
  for (resp in c("margin", "total")) {
    fs <- file.path(OUT, "out", sprintf("draws_%s_v2a_%s.rds", cell, resp))
    if (!file.exists(fs)) next
    dr <- readRDS(fs)
    res <- readRDS(file.path(OUT, "out", sprintf("res_%s_v2a_%s.rds", cell, resp)))
    full <- iv[[paste(resp, "ar1 auto")]]
    eb <- iv[[paste(resp, "ar1 eb")]]
    # Hyperparameters, on the sd / correlation scale.
    stan_h <- rbind(
      `sd: the Gaussian observations` = qs(pick_scalar(dr, "sigma")), `sd: u_s` = qs(pick_scalar(dr, "sigma_u")),
      `rho: u_s` = qs(pick_scalar(dr, "rho")), `sd: b` = qs(pick_scalar(dr, "sigma_b")), `sd: w_t` = qs(pick_scalar(dr, "sigma_w[1]"))
    )
    if (resp == "margin") {
      stan_h <- rbind(stan_h, `sd: ha` = qs(pick_scalar(dr, "sigma_ha")), hfa = qs(pick_scalar(dr, "hfa")))
    } else {
      stan_h <- rbind(stan_h, `sd: s_s` = qs(pick_scalar(dr, "sigma_season")))
      for (i in seq_len(v$ND)) stan_h <- rbind(stan_h, qs(pick_scalar(dr, sprintf("alpha[%d]", i))))
      rownames(stan_h)[nrow(stan_h) - v$ND + seq_len(v$ND)] <- paste0("alpha", seq_len(v$ND))
    }
    ih <- full$hyper_sd
    inla_h <- as.matrix(ih[, c("mean", "sd", "q05", "q50", "q95")])
    rownames(inla_h) <- ih$parameter
    fx <- full$fixed
    fx_rows <- cbind(mean = fx$mean, sd = fx$sd, q05 = fx$mean - 1.645 * fx$sd, q50 = fx$`0.5quant`, q95 = fx$mean + 1.645 * fx$sd)
    rownames(fx_rows) <- rownames(fx)
    inla_h <- rbind(inla_h, fx_rows)
    common <- intersect(rownames(stan_h), rownames(inla_h))
    hyper <- data.frame(parameter = common, stan_mean = stan_h[common, 1], inla_mean = inla_h[common, 1],
                        stan_sd = stan_h[common, 2], inla_sd = inla_h[common, 2], stan_q05 = stan_h[common, 3],
                        inla_q05 = inla_h[common, 3], stan_q50 = stan_h[common, 4], inla_q50 = inla_h[common, 4],
                        stan_q95 = stan_h[common, 5], inla_q95 = inla_h[common, 5])
    hyper$diff_in_sd <- (hyper$inla_mean - hyper$stan_mean) / hyper$stan_sd
    # States: club level b (K) and season level u (K x NS; Stan stores it column-major,
    # R-INLA team by team).
    b_stan <- pick_cols(dr, "b", v$K)
    u_names <- as.vector(outer(seq_len(v$K), seq_len(v$NS), function(k, s) sprintf("u[%d,%d]", k, s)))
    u_stan <- matrix(as.numeric(unclass(dr[, u_names])), nrow(dr))
    u_inla_mean <- as.vector(matrix(full$random$u_s$mean, nrow = v$K, byrow = TRUE))
    u_inla_sd <- as.vector(matrix(full$random$u_s$sd, nrow = v$K, byrow = TRUE))
    state_stan_mean <- c(colMeans(b_stan), colMeans(u_stan))
    state_stan_sd <- c(apply(b_stan, 2, stats::sd), apply(u_stan, 2, stats::sd))
    state_inla_mean <- c(full$random$b$mean, u_inla_mean)
    state_inla_sd <- c(full$random$b$sd, u_inla_sd)
    # Held-out predictions.
    n <- nrow(d$hold)
    mu_stan <- pick_cols(dr, "mu_pred", n)
    y <- if (resp == "margin") d$hold$home_score - d$hold$away_score else d$hold$home_score + d$hold$away_score
    ls_stan <- log(v2_integer_pmf(y, mu_stan, pick_scalar(dr, "sigma")))
    ls_full <- log(v2_integer_pmf(y, full$samples$loc, full$samples$sigma))
    ls_eb <- log(v2_integer_pmf(y, eb$pred$mean, sqrt(eb$pred$sd^2 + eb$sigma2)))
    ls_plug <- log(v2_integer_pmf(y, full$pred$mean, sqrt(full$pred$sd^2 + full$sigma2)))
    o <- list(
      cell = cell, response = resp, hyper = hyper,
      state = c(n = length(state_stan_mean), cor_mean = stats::cor(state_stan_mean, state_inla_mean),
                max_abs_mean_diff_in_sd = max(abs(state_inla_mean - state_stan_mean) / state_stan_sd),
                median_sd_ratio = stats::median(state_inla_sd / state_stan_sd), min_sd_ratio = min(state_inla_sd / state_stan_sd),
                max_sd_ratio = max(state_inla_sd / state_stan_sd)),
      pred = c(n = n, cor_mean = stats::cor(colMeans(mu_stan), full$pred$mean),
               max_abs_mean_diff = max(abs(colMeans(mu_stan) - full$pred$mean)),
               max_abs_mean_diff_in_sd = max(abs(colMeans(mu_stan) - full$pred$mean) / apply(mu_stan, 2, stats::sd)),
               median_sd_ratio = stats::median(full$pred$sd / apply(mu_stan, 2, stats::sd)),
               max_abs_mean_diff_eb = max(abs(colMeans(mu_stan) - eb$pred$mean)),
               median_sd_ratio_eb = stats::median(eb$pred$sd / apply(mu_stan, 2, stats::sd))),
      log_score = c(stan = mean(ls_stan), inla_full_samples = mean(ls_full), inla_full_plugin = mean(ls_plug), inla_eb = mean(ls_eb),
                    max_abs_diff_full = max(abs(ls_full - ls_stan)), max_abs_diff_eb = max(abs(ls_eb - ls_stan))),
      loo = c(stan_elpd_loo = res$loo["elpd_loo", "Estimate"], inla_sum_log_cpo = sum(log(full$cpo)), inla_eb_sum_log_cpo = sum(log(eb$cpo))),
      stan = c(divergent = res$n_divergent, leapfrog = res$mean_leapfrog, min_ess = res$min_ess_bulk, max_rhat = res$max_rhat,
               wall = res$wall_secs, n_sampled = res$n_sampled),
      inla = c(secs_full = full$secs, secs_eb = eb$secs, hyper_points = full$n_hyper_points),
      stan_machine = res$machine, inla_machine = iv$machine
    )
    if (resp == "margin") {
      ties <- CELLS[[cell]]$sport == "handball"
      ps <- v2_outcome_probs(mu_stan, pick_scalar(dr, "sigma"), ties = ties)
      pf <- v2_outcome_probs(full$samples$loc, full$samples$sigma, ties = ties)
      pe <- v2_outcome_probs(eb$pred$mean, sqrt(eb$pred$sd^2 + eb$sigma2), ties = ties)
      o$outcome <- c(max_abs_p_home_diff_full = max(abs(ps$p_home - pf$p_home)), max_abs_p_home_diff_eb = max(abs(ps$p_home - pe$p_home)),
                     max_abs_p_draw_diff_full = max(abs(ps$p_draw - pf$p_draw)), max_abs_p_draw_diff_eb = max(abs(ps$p_draw - pe$p_draw)))
    }
    out[[paste(cell, resp)]] <- o
    cat(sprintf("\n===== %s %s: Stan (%d sampled, %d divergences, leapfrog %.0f, min ESS %.0f, R-hat %.3f) vs R-INLA full integration (%d hyper points, %.1f s) and plug-in (%.1f s)\n",
                cell, resp, res$n_sampled, res$n_divergent, res$mean_leapfrog, res$min_ess_bulk, res$max_rhat, full$n_hyper_points, full$secs, eb$secs))
    print(format(hyper[, -1], digits = 3), quote = FALSE)
    cat(sprintf("states (%d): cor of posterior means %.5f, largest mean difference %.3f posterior sd, sd ratio INLA/Stan median %.3f (range %.3f to %.3f)\n",
                o$state[["n"]], o$state[["cor_mean"]], o$state[["max_abs_mean_diff_in_sd"]], o$state[["median_sd_ratio"]], o$state[["min_sd_ratio"]], o$state[["max_sd_ratio"]]))
    cat(sprintf("held-out linear predictor (%d matches): cor %.5f, largest mean difference %.3f (%.3f sd), sd ratio median %.3f | plug-in: largest mean difference %.3f, sd ratio %.3f\n",
                n, o$pred[["cor_mean"]], o$pred[["max_abs_mean_diff"]], o$pred[["max_abs_mean_diff_in_sd"]], o$pred[["median_sd_ratio"]], o$pred[["max_abs_mean_diff_eb"]], o$pred[["median_sd_ratio_eb"]]))
    cat(sprintf("held-out integer log score: Stan %.4f | INLA full (samples) %.4f | INLA full (Gaussian plug-in) %.4f | INLA plug-in hyperparameters %.4f; largest per-match difference %.4f / %.4f\n",
                o$log_score[["stan"]], o$log_score[["inla_full_samples"]], o$log_score[["inla_full_plugin"]], o$log_score[["inla_eb"]], o$log_score[["max_abs_diff_full"]], o$log_score[["max_abs_diff_eb"]]))
    cat(sprintf("leave-one-out: Stan PSIS elpd %.1f | INLA sum log CPO %.1f (plug-in %.1f)\n", o$loo[[1]], o$loo[[2]], o$loo[[3]]))
    if (!is.null(o$outcome)) cat(sprintf("largest difference in p(home): %.4f (full), %.4f (plug-in); in p(draw): %.4f, %.4f\n", o$outcome[[1]], o$outcome[[2]], o$outcome[[3]], o$outcome[[4]]))
  }
}
saveRDS(out, file.path(OUT, "out", "compare_validation.rds"))

# Ladder tables: one row per rung per cell, from the saved fits at the fixed cutoff.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/ladder_summary.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))
cutoff <- "2026-01-31"
stan_rungs <- c("r0_prod", "r1_ragged", "r2_common", "r3_pooled", "r4_ident", "r5_gauss")
out_path <- function(...) file.path(OUT, "out", sprintf(...))
rows <- list()
scores <- list()
for (cell in names(CELLS)) {
  ci <- CELLS[[cell]]
  d <- readRDS(out_path("data_%s_%s.rds", cell, cutoff))
  k <- d$prod$K
  nh <- tabulate(d$train$home_nr, k)
  w_ha <- function(m) as.numeric(m %*% nh) / sum(nh)
  n_train <- nrow(d$train)
  add <- function(label, engine, sc, ha, extra) {
    scores[[paste(cell, label)]] <<- sc
    rows[[length(rows) + 1L]] <<- cbind(
      data.frame(cell = cell, rung = label, engine = engine), extra,
      data.frame(ha_mean = mean(ha), ha_sd = stats::sd(ha), hold_n = nrow(sc), ls_margin = mean(sc$ls_margin),
                 ls_total = mean(sc$ls_total), ll_wdl = mean(sc$ll_wdl))
    )
  }
  stan_extra <- function(res, n_fits = 1L) {
    data.frame(
      n_sampled = res$n_sampled, divergent = res$n_divergent, leapfrog = res$mean_leapfrog,
      grad_evals = if (is.null(res$total_leapfrog)) res$mean_leapfrog * 4000 else res$total_leapfrog,
      max_rhat = res$max_rhat, min_ess = res$min_ess_bulk, worst = res$worst_ess$variable[1],
      wall_secs = res$wall_secs, elpd = res$loo["elpd_loo", "Estimate"], bad_k = res$pareto_k_bad,
      slots = paste(sub(" pid=.*", "", res$machine$slots), collapse = " | ")
    )
  }
  for (m in stan_rungs) {
    f <- out_path("res_%s_%s.rds", cell, m)
    if (!file.exists(f)) next
    res <- readRDS(f)
    dr <- readRDS(out_path("draws_%s_%s.rds", cell, m))
    pp <- if (m == "r0_prod") pred_params_prod(dr, d, ci$sport) else pred_params_lean(dr, d)
    sc <- score_holdout(pp, d$hold, ci$sport)
    ha <- w_ha(pick_cols(dr, "home_advantage_off", k) + pick_cols(dr, "home_advantage_def", k))
    if (m %in% c("r4_ident", "r5_gauss")) ha <- ha + pick_scalar(dr, "hfa")
    add(m, "Stan", sc, ha, stan_extra(res))
  }
  for (v in c("v2s", "v2a", "v2")) {
    fm <- out_path("res_%s_%s_margin.rds", cell, v)
    ft <- out_path("res_%s_%s_total.rds", cell, v)
    if (!file.exists(fm) || !file.exists(ft)) next
    rm_ <- readRDS(fm)
    rt_ <- readRDS(ft)
    dm <- readRDS(out_path("draws_%s_%s_margin.rds", cell, v))
    dt <- readRDS(out_path("draws_%s_%s_total.rds", cell, v))
    sc <- score_holdout(pred_params_v2(dm, dt, d), d$hold, ci$sport)
    ha <- pick_scalar(dm, "hfa") + w_ha(pick_cols(dm, "ha", k))
    ex <- stan_extra(rm_)
    ex$n_sampled <- rm_$n_sampled + rt_$n_sampled
    ex$divergent <- rm_$n_divergent + rt_$n_divergent
    ex$leapfrog <- rm_$mean_leapfrog + rt_$mean_leapfrog
    ex$grad_evals <- ex$grad_evals + if (is.null(rt_$total_leapfrog)) rt_$mean_leapfrog * 4000 else rt_$total_leapfrog
    ex$max_rhat <- max(rm_$max_rhat, rt_$max_rhat)
    if (rt_$min_ess_bulk < rm_$min_ess_bulk) {
      ex$min_ess <- rt_$min_ess_bulk
      ex$worst <- paste0("total:", rt_$worst_ess$variable[1])
    } else {
      ex$worst <- paste0("margin:", ex$worst)
    }
    ex$wall_secs <- rm_$wall_secs + rt_$wall_secs
    # WHY log 2: (margin, total) is a linear map of (home, away) with determinant 2, so
    # the joint density of the two scores is the product of the two densities times 2.
    ex$elpd <- rm_$loo["elpd_loo", "Estimate"] + rt_$loo["elpd_loo", "Estimate"] + n_train * log(2)
    ex$bad_k <- rm_$pareto_k_bad + rt_$pareto_k_bad
    add(paste0(v, " (margin + total)"), "Stan", sc, ha, ex)
  }
  fi <- out_path("inla_validate_%s.rds", cell)
  if (file.exists(fi)) {
    iv <- readRDS(fi)
    for (within in c("none", "ar1")) {
      for (strategy in c("auto", "eb")) {
        im <- iv[[paste("margin", within, strategy)]]
        it <- iv[[paste("total", within, strategy)]]
        if (is.null(im) || is.null(it)) next
        pp <- if (strategy == "auto") {
          list(loc_m = im$samples$loc, sc_m = im$samples$sigma, loc_t = it$samples$loc, sc_t = it$samples$sigma, nu = Inf)
        } else {
          list(loc_m = im$pred$mean, sc_m = sqrt(im$pred$sd^2 + im$sigma2), loc_t = it$pred$mean,
               sc_t = sqrt(it$pred$sd^2 + it$sigma2), nu = Inf)
        }
        sc <- score_holdout(pp, d$hold, ci$sport)
        ha_team <- sum(nh * im$random$ha$mean) / sum(nh)
        ha <- stats::rnorm(4000, im$fixed$mean[1] + ha_team, im$fixed$sd[1])
        ex <- data.frame(
          n_sampled = NA_integer_, divergent = NA_integer_, leapfrog = NA_real_, grad_evals = NA_real_,
          max_rhat = NA_real_, min_ess = NA_real_, worst = sprintf("%d + %d hyper points", im$n_hyper_points, it$n_hyper_points),
          wall_secs = im$secs + it$secs, elpd = sum(log(im$cpo)) + sum(log(it$cpo)) + n_train * log(2), bad_k = NA_integer_,
          slots = paste(sub(" pid=.*", "", iv$machine$slots), collapse = " | ")
        )
        add(sprintf("v2 %s, INLA %s", if (within == "none") "static" else "+ within-season AR(1)",
                    if (strategy == "auto") "full" else "plug-in"), "INLA", sc, ha, ex)
      }
    }
  }
}
tab <- do.call(rbind, rows)
saveRDS(list(tab = tab, scores = scores), out_path("ladder.rds"))
options(width = 250)
for (cell in unique(tab$cell)) {
  x <- tab[tab$cell == cell, ]
  cat(sprintf("\n===== %s (hold-out n = %d)\n", cell, x$hold_n[1]))
  show <- x[, c("rung", "n_sampled", "divergent", "leapfrog", "grad_evals", "max_rhat", "min_ess", "worst", "wall_secs", "elpd",
                "ha_mean", "ha_sd", "ls_margin", "ls_total", "ll_wdl")]
  num <- vapply(show, is.numeric, logical(1))
  show[num] <- lapply(show[num], function(z) signif(z, 5))
  print(show, row.names = FALSE)
  base <- scores[[paste(cell, "r0_prod")]]
  if (is.null(base)) next
  for (r in setdiff(x$rung, "r0_prod")) {
    s <- scores[[paste(cell, r)]]
    cat(sprintf("  %-38s vs production: margin LS %+.4f (se %.4f), total LS %+.4f (se %.4f), W/D/L %+.4f (se %.4f)\n", r,
                paired(s$ls_margin, base$ls_margin)[1], paired(s$ls_margin, base$ls_margin)[2],
                paired(s$ls_total, base$ls_total)[1], paired(s$ls_total, base$ls_total)[2],
                paired(s$ll_wdl, base$ll_wdl)[1], paired(s$ll_wdl, base$ll_wdl)[2]))
  }
}

# Does the production fit show the football model's defects? Reads the saved draws of
# the r0 (production) fit per cell.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/diag_prod.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))
cutoff <- "2026-01-31"
q <- function(x) stats::quantile(x, c(0.05, 0.5, 0.95), names = FALSE)
fmt <- function(x) sprintf("%.3f [%.3f, %.3f]", x[2], x[1], x[3])
out <- list()
for (cell in names(CELLS)) {
  f <- file.path(OUT, "out", sprintf("draws_%s_r0_prod.rds", cell))
  if (!file.exists(f)) next
  ci <- CELLS[[cell]]
  d <- readRDS(file.path(OUT, "out", sprintf("data_%s_%s.rds", cell, cutoff)))
  dr <- readRDS(f)
  res <- readRDS(file.path(OUT, "out", sprintf("res_%s_r0_prod.rds", cell)))
  tr <- d$train
  k <- d$prod$K
  ns <- d$prod$N_seasons
  ho <- pick_cols(dr, "home_advantage_off", k)
  hd <- pick_cols(dr, "home_advantage_def", k)
  nh <- tabulate(tr$home_nr, k)
  ha_w <- as.numeric((ho + hd) %*% nh) / sum(nh)
  raw <- mean(tr$home_score - tr$away_score)
  raw_se <- stats::sd(tr$home_score - tr$away_score) / sqrt(nrow(tr))
  by_div <- do.call(rbind, lapply(split(tr, tr$division), function(x) {
    w <- tabulate(x$home_nr, k)
    hw <- as.numeric((ho + hd) %*% w) / sum(w)
    data.frame(division = x$division[1], n = nrow(x), raw_margin = mean(x$home_score - x$away_score),
               raw_se = stats::sd(x$home_score - x$away_score) / sqrt(nrow(x)), prod_ha = mean(hw), prod_ha_sd = stats::sd(hw))
  }))
  level <- pick_scalar(dr, sprintf("mean_goals[%d]", ns))
  so <- pick_cols(dr, "sigma_off", k)
  sdf <- pick_cols(dr, "sigma_def", k)
  med_so <- apply(so, 2, stats::median)
  med_sd <- apply(sdf, 2, stats::median)
  rel_width <- function(m) stats::median(apply(m, 2, function(x) diff(stats::quantile(x, c(0.05, 0.95)))) / apply(m, 2, stats::median))
  o <- list(
    cell = cell, n = nrow(tr), k = k, n_rounds = d$prod$N_rounds,
    prior_only_share = 1 - 2 * nrow(tr) / (k * d$prod$N_rounds),
    n_sampled = res$n_sampled, n_divergent = res$n_divergent, leapfrog = res$mean_leapfrog,
    max_rhat = res$max_rhat, min_ess_bulk = res$min_ess_bulk, worst_ess = res$worst_ess, wall = res$wall_secs,
    ha_weighted = c(mean = mean(ha_w), sd = stats::sd(ha_w)), raw_margin = c(mean = raw, se = raw_se), by_div = by_div,
    ha_off_team_means = c(mean = mean(colMeans(ho)), sd_across = stats::sd(colMeans(ho)), within_sd = stats::median(apply(ho, 2, stats::sd))),
    ha_def_team_means = c(mean = mean(colMeans(hd)), sd_across = stats::sd(colMeans(hd)), within_sd = stats::median(apply(hd, 2, stats::sd))),
    cor_level_ha_off = stats::cor(level, rowMeans(ho)), cor_level_ha_def = stats::cor(level, rowMeans(hd)),
    cor_level_ha_diff = stats::cor(level, rowMeans(ho) - rowMeans(hd)),
    level = c(mean = mean(level), sd = stats::sd(level)),
    ha_split = c(sd_off_minus_def = stats::sd(rowMeans(ho) - rowMeans(hd)), sd_off_plus_def = stats::sd(rowMeans(ho) + rowMeans(hd))),
    scale_sigma_off = q(pick_scalar(dr, "scale_sigma_off")), scale_sigma_def = q(pick_scalar(dr, "scale_sigma_def")),
    scale_prior = stats::qexp(c(0.05, 0.5, 0.95), 2),
    sigma_off_ratio = max(med_so) / min(med_so), sigma_def_ratio = max(med_sd) / min(med_sd),
    sigma_off_rel_width = rel_width(so), sigma_def_rel_width = rel_width(sdf),
    nu = q(pick_scalar(dr, "nu")), nu_prior = stats::qgamma(c(0.05, 0.5, 0.95), 3, 0.15)
  )
  if (ci$sport == "handball") {
    st <- pick_cols(dr, "sigma_team", k)
    med_st <- apply(st, 2, stats::median)
    o$scale_sigma_team <- q(pick_scalar(dr, "scale_sigma_team"))
    o$sigma_team_ratio <- max(med_st) / min(med_st)
    o$sigma_team_rel_width <- rel_width(st)
    o$sigma_team_median <- stats::median(med_st)
    o$rho <- q(pick_scalar(dr, "rho"))
  } else {
    o$sigma <- q(pick_scalar(dr, "sigma"))
    o$rho_at_zero <- q(2 * plogis(pick_scalar(dr, "alpha_rho")) - 1)
    o$rho_betas <- rbind(beta = q(pick_scalar(dr, "beta_rho")), beta2 = q(pick_scalar(dr, "beta2_rho")), beta3 = q(pick_scalar(dr, "beta3_rho")))
  }
  out[[cell]] <- o
  cat(sprintf("\n===== %s production fit: N=%d, K=%d, N_rounds=%d, sampled params %d, prior-only innovation share %.3f\n",
              cell, o$n, k, o$n_rounds, o$n_sampled, o$prior_only_share))
  cat(sprintf("sampler: divergences %d, leapfrog/iter %.0f, max R-hat %.3f, min bulk ESS %.0f\n", o$n_divergent, o$leapfrog, o$max_rhat, o$min_ess_bulk))
  print(o$worst_ess[1:5, ], row.names = FALSE, digits = 4)
  cat(sprintf("home advantage: production home-match-weighted total %.2f (sd %.2f) vs raw home-minus-away margin %.2f (se %.2f) -> ratio %.2f\n",
              o$ha_weighted[1], o$ha_weighted[2], raw, raw_se, o$ha_weighted[1] / raw))
  print(by_div, row.names = FALSE, digits = 3)
  cat(sprintf("unpooled HA_off: mean of team means %.2f, sd across teams %.2f, typical within-team posterior sd %.2f | HA_def: %.2f, %.2f, %.2f\n",
              o$ha_off_team_means[1], o$ha_off_team_means[2], o$ha_off_team_means[3], o$ha_def_team_means[1], o$ha_def_team_means[2], o$ha_def_team_means[3]))
  cat(sprintf("season level (last season): mean %.2f, posterior sd %.2f; cor(level, mean HA_off) %.3f, cor(level, mean HA_def) %.3f, cor(level, HA_off - HA_def) %.3f\n",
              o$level[1], o$level[2], o$cor_level_ha_off, o$cor_level_ha_def, o$cor_level_ha_diff))
  cat(sprintf("posterior sd of mean(HA_off) - mean(HA_def) %.3f vs of mean(HA_off) + mean(HA_def) %.3f\n", o$ha_split[1], o$ha_split[2]))
  cat(sprintf("volatility hierarchy scale: off %s, def %s; prior exponential(2): %s\n", fmt(o$scale_sigma_off), fmt(o$scale_sigma_def), fmt(o$scale_prior)))
  cat(sprintf("per-team RW sd medians: max/min ratio off %.2f, def %.2f; typical 90%% interval width / median: off %.2f, def %.2f\n",
              o$sigma_off_ratio, o$sigma_def_ratio, o$sigma_off_rel_width, o$sigma_def_rel_width))
  cat(sprintf("Student-t nu: %s; prior gamma(3, 0.15): %s\n", fmt(o$nu), fmt(o$nu_prior)))
  if (ci$sport == "handball") {
    cat(sprintf("per-team score sd: hierarchy scale %s, team medians max/min %.2f, typical 90%% width / median %.2f, median %.2f; rho %s\n",
                fmt(o$scale_sigma_team), o$sigma_team_ratio, o$sigma_team_rel_width, o$sigma_team_median, fmt(o$rho)))
  } else {
    cat(sprintf("score sd %s; rho at zero strength gap %s\n", fmt(o$sigma), fmt(o$rho_at_zero)))
    print(round(o$rho_betas, 4))
  }
}
saveRDS(out, file.path(OUT, "out", "diag_prod.rds"))

# Compare the Stan and INLA fits of the one specification (run_validation.R):
# hyperparameters, every state, and match-by-match predictive probabilities.
#
#   Rscript research/football-v2/compare_validation.R <tag> <cutoff>
options(width = 170)
source("research/football-v2/spec.R")
source("research/football-v2/score.R")
args <- commandArgs(trailingOnly = TRUE)
tag <- if (length(args) >= 1L) args[[1]] else "calendar"
cutoff <- if (length(args) >= 2L) args[[2]] else "2026-07-31"
out <- Sys.getenv("FV2_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/out")

q3 <- function(x) stats::quantile(x, c(0.05, 0.5, 0.95), names = FALSE)
batch_se <- function(p_by_batch) apply(p_by_batch, 1L, stats::sd) / sqrt(ncol(p_by_batch))

predict_scores <- function(f, spec, pred, batches) {
  pr <- fv2_predict(f$draws, spec)
  sc <- fv2_score(pr$lam_home, pr$lam_away, pred$home_score, pred$away_score)
  by_batch <- vapply(sort(unique(batches)), function(b) {
    i <- batches == b
    s <- fv2_score(pr$lam_home[, i, drop = FALSE], pr$lam_away[, i, drop = FALSE], pred$home_score, pred$away_score)
    c(s$p_home, s$p_draw, s$p_away)
  }, numeric(3L * nrow(pred)))
  list(sc = sc, mcse = matrix(batch_se(by_batch), ncol = 3L, dimnames = list(NULL, c("home", "draw", "away"))))
}

res <- list()
for (sex in c("female", "male")) {
  stem <- file.path(out, sprintf("val_%s_%s_%s", tag, sex, cutoff))
  if (!file.exists(paste0(stem, "_stan.rds"))) next
  sp <- readRDS(paste0(stem, "_spec.rds"))
  spec <- sp$spec
  st <- readRDS(paste0(stem, "_stan.rds"))
  full <- readRDS(paste0(stem, "_inla-auto.rds"))
  eb <- readRDS(paste0(stem, "_inla-eb.rds"))
  cat(sprintf("\n================ %s (%s, cutoff %s): K=%d, N=%d, S=%d, holdout=%d\n", sex, tag, cutoff, spec$K, spec$N, spec$S, nrow(sp$pred)))

  hn <- colnames(st$draws$hyper)
  hyper <- do.call(rbind, lapply(hn, function(h) {
    s <- st$draws$hyper[, h]
    data.frame(
      sex = sex, parameter = h,
      stan_mean = mean(s), stan_q5 = q3(s)[1], stan_q50 = q3(s)[2], stan_q95 = q3(s)[3],
      inla_mean = full$hyper_summary[h, "mean"], inla_q5 = full$hyper_summary[h, "q5"],
      inla_q50 = full$hyper_summary[h, "q50"], inla_q95 = full$hyper_summary[h, "q95"],
      eb_mode = eb$draws$hyper[1L, h]
    )
  }))
  fixed <- do.call(rbind, lapply(c("c_home", "c_away", "home_adv"), function(v) {
    get <- function(d) if (v == "home_adv") d$c_home - d$c_away else d[[v]]
    data.frame(
      sex = sex, parameter = v,
      stan_mean = mean(get(st$draws)), stan_sd = stats::sd(get(st$draws)),
      inla_mean = mean(get(full$draws)), inla_sd = stats::sd(get(full$draws)),
      eb_mean = mean(get(eb$draws)), eb_sd = stats::sd(get(eb$draws))
    )
  }))
  print(format(hyper, digits = 3), row.names = FALSE)
  print(format(fixed, digits = 3), row.names = FALSE)

  # WHY: INLA's own marginal summaries exist per latent field. For attack and
  # defence walks those are the states; for strength plus style the states
  # are sums of fields, so they are summarised from INLA's posterior samples.
  inla_state <- function(f, nm, inla_nm) {
    if (spec$opts$param == "ad") {
      return(f$state_summary[[inla_nm]])
    }
    data.frame(mean = colMeans(f$draws[[nm]]), sd = apply(f$draws[[nm]], 2L, stats::sd))
  }
  state_tab <- function(nm, inla_nm) {
    s_mean <- colMeans(st$draws[[nm]])
    s_sd <- apply(st$draws[[nm]], 2L, stats::sd)
    one <- function(f, label) {
      m <- inla_state(f, nm, inla_nm)
      data.frame(
        sex = sex, component = nm, engine = label, slot = seq_along(s_mean),
        stan_mean = s_mean, stan_sd = s_sd, mean = m$mean, sd = m$sd
      )
    }
    rbind(one(full, "inla-full"), one(eb, "inla-eb"))
  }
  states <- rbind(state_tab("att", "f1"), state_tab("def", "f2"))
  states$has_obs <- spec$slots$has_obs[states$slot]
  states$z <- (states$mean - states$stan_mean) / states$stan_sd
  states$sd_ratio <- states$sd / states$stan_sd
  state_summ <- do.call(rbind, lapply(split(states, list(states$engine, states$component, states$has_obs)), function(d) {
    data.frame(
      sex = sex, engine = d$engine[1], component = d$component[1], has_obs = d$has_obs[1], n = nrow(d),
      max_abs_dmean = max(abs(d$mean - d$stan_mean)), rms_z = sqrt(mean(d$z^2)), max_abs_z = max(abs(d$z)),
      sd_ratio_q05 = stats::quantile(d$sd_ratio, 0.05), sd_ratio_med = stats::median(d$sd_ratio),
      sd_ratio_q95 = stats::quantile(d$sd_ratio, 0.95)
    )
  }))
  ess <- 1000
  cat(sprintf("state comparison (z = difference in posterior mean / Stan posterior sd; Stan MCSE of a state mean is about %.3f sd):\n", 1 / sqrt(ess)))
  print(format(state_summ, digits = 3), row.names = FALSE)

  p_st <- predict_scores(st, spec, sp$pred, st$chain_id)
  p_full <- predict_scores(full, spec, sp$pred, rep(1:4, length.out = length(full$draws$c_home)))
  p_eb <- predict_scores(eb, spec, sp$pred, rep(1:4, length.out = length(eb$draws$c_home)))
  cols <- c("p_home", "p_draw", "p_away")
  pm <- function(a, b) as.matrix(a$sc[cols]) - as.matrix(b$sc[cols])
  pred_summ <- do.call(rbind, lapply(list(c("inla-full", "p_full"), c("inla-eb", "p_eb")), function(x) {
    d <- pm(get(x[2]), p_st)
    dl <- get(x[2])$sc$ls_score - p_st$sc$ls_score
    d1 <- get(x[2])$sc$ls_1x2 - p_st$sc$ls_1x2
    data.frame(
      sex = sex, engine = x[1], n = nrow(d),
      max_abs_dp = max(abs(d)), mean_abs_dp = mean(abs(d)), rms_dp = sqrt(mean(d^2)),
      stan_mcse_mean = mean(p_st$mcse), stan_mcse_max = max(p_st$mcse),
      d_scoreline = mean(dl), se_scoreline = stats::sd(dl) / sqrt(length(dl)),
      d_1x2 = mean(d1), se_1x2 = stats::sd(d1) / sqrt(length(d1))
    )
  }))
  # Fixtures with a team that changed division at the latest season break:
  # the hardest forecasts, because that team's state has just taken a jump.
  steps <- spec$steps
  latest <- max(spec$slots$season)
  moved <- unique(steps$team[steps$move != 0 & spec$slots$season[steps$slot] == latest])
  with_mover <- spec$pred$home_team %in% moved | spec$pred$away_team %in% moved
  subset_summ <- do.call(rbind, lapply(list(c("inla-full", "p_full"), c("inla-eb", "p_eb")), function(x) {
    d <- abs(pm(get(x[2]), p_st))
    one <- function(rows, label) {
      data.frame(
        sex = sex, engine = x[1], subset = label, n = sum(rows),
        mean_abs_dp = if (any(rows)) mean(d[rows, ]) else NA_real_,
        max_abs_dp = if (any(rows)) max(d[rows, ]) else NA_real_
      )
    }
    rbind(one(with_mover, "with a promoted or relegated team"), one(!with_mover, "other fixtures"))
  }))
  print(format(subset_summ, digits = 3), row.names = FALSE)
  scores <- data.frame(
    sex = sex, engine = c("stan-nuts", "inla-full", "inla-eb"),
    scoreline = c(mean(p_st$sc$ls_score), mean(p_full$sc$ls_score), mean(p_eb$sc$ls_score)),
    logloss_1x2 = -c(mean(p_st$sc$ls_1x2), mean(p_full$sc$ls_1x2), mean(p_eb$sc$ls_1x2)),
    p_home = c(mean(p_st$sc$p_home), mean(p_full$sc$p_home), mean(p_eb$sc$p_home))
  )
  print(format(pred_summ, digits = 3), row.names = FALSE)
  print(format(scores, digits = 5), row.names = FALSE)

  matches <- cbind(
    sp$pred[c("match_date", "home_team", "away_team", "home_score", "away_score", "division")],
    stan = p_st$sc[cols], full = p_full$sc[cols], eb = p_eb$sc[cols],
    stan_mcse = p_st$mcse
  )
  d <- st$diagnostics
  timing <- data.frame(
    sex = sex, engine = c("stan-nuts", "inla-full", "inla-eb"),
    fit_secs = c(d$wall_secs, full$t_fit, eb$t_fit), sample_secs = c(0, full$t_samp, eb$t_samp),
    n_draws = c(length(st$draws$c_home), length(full$draws$c_home), length(eb$draws$c_home)),
    divergences = c(d$n_divergent, NA, NA), leapfrog = c(d$mean_leapfrog, NA, NA),
    max_rhat = c(d$state_max_rhat, NA, NA), min_ess = c(d$state_min_ess, NA, NA)
  )
  print(format(timing, digits = 4), row.names = FALSE)

  n_app <- tabulate(c(spec$obs$home_team, spec$obs$away_team), spec$K)
  lvl <- colMeans(st$draws$att[, spec$last_obs_slot] + st$draws$def[, spec$last_obs_slot])
  ord <- order(lvl)
  show <- unique(c(ord[length(ord)], ord[round(length(ord) / 2)], ord[2L], which.min(n_app)))
  traj <- do.call(rbind, lapply(show, function(k) {
    idx <- spec$slots$slot[spec$slots$team == k]
    do.call(rbind, lapply(c("att", "def"), function(nm) {
      inla_nm <- if (nm == "att") "f1" else "f2"
      s <- st$draws[[nm]][, idx, drop = FALSE]
      m <- inla_state(full, nm, inla_nm)[idx, ]
      e <- inla_state(eb, nm, inla_nm)[idx, ]
      data.frame(
        sex = sex, team = spec$teams[k], component = nm, week = spec$slots$week[idx],
        has_obs = spec$slots$has_obs[idx],
        stan_mean = colMeans(s), stan_sd = apply(s, 2L, stats::sd),
        inla_mean = m$mean, inla_sd = m$sd, eb_mean = e$mean, eb_sd = e$sd
      )
    }))
  }))
  res[[sex]] <- list(
    hyper = hyper, fixed = fixed, state_summ = state_summ, pred_summ = pred_summ, scores = scores,
    subset_summ = subset_summ,
    matches = matches, timing = timing, traj = traj,
    states = states[c("sex", "component", "engine", "slot", "has_obs", "stan_mean", "stan_sd", "mean", "sd")],
    dims = data.frame(sex = sex, K = spec$K, N = spec$N, S = spec$S, holdout = nrow(sp$pred))
  )
}
saveRDS(res, file.path(out, sprintf("validation_summary_%s_%s.rds", tag, cutoff)))
cat("saved\n")

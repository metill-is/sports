# Collect the compact results the report reads, from the experiment outputs
# (which are large and live outside the repository) into one small file next
# to the report source.
#
#   Rscript research/football-v2/export_report_data.R
# Run from the repo root after compare_validation.R and wf_summarise.R.
options(width = 160)
out <- Sys.getenv("FV2_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/out")
dest <- file.path("docs", "reports", "2026-11-15-football-v2")
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

read_validation <- function(tag) {
  f <- file.path(out, sprintf("validation_summary_%s_2026-07-31.rds", tag))
  if (!file.exists(f)) {
    return(NULL)
  }
  v <- readRDS(f)
  bind <- function(nm) do.call(rbind, lapply(v, `[[`, nm))
  states <- bind("states")
  states$z <- (states$mean - states$stan_mean) / states$stan_sd
  states$sd_ratio <- states$sd / states$stan_sd
  keep <- stats::ave(seq_len(nrow(states)), states$sex, states$engine, FUN = function(i) {
    set.seed(1)
    i %in% sample(i, min(length(i), 1500L))
  }) == 1
  timing <- bind("timing")
  list(
    hyper = bind("hyper"), fixed = bind("fixed"), state_summ = bind("state_summ"),
    pred_summ = bind("pred_summ"), scores = bind("scores"), matches = bind("matches"),
    matches_sex = rep(names(v), vapply(v, function(x) nrow(x$matches), integer(1))),
    sampler = timing[c("sex", "engine", "n_draws", "divergences", "leapfrog", "max_rhat", "min_ess")],
    traj = bind("traj"), dims = bind("dims"),
    states_sample = states[keep, c("sex", "component", "engine", "has_obs", "stan_mean", "mean", "z", "sd_ratio")]
  )
}

ha_files <- list.files(out, pattern = "^ha_ha_.*\\.rds$", full.names = TRUE)
ha <- do.call(rbind, lapply(ha_files, function(f) {
  x <- readRDS(f)
  data.frame(
    sex = x$sex, variant = x$variant, n = x$n, teams = x$K,
    raw_log_ratio = x$raw_log_ratio,
    total_ha = x$total_ha[["mean"]], total_ha_sd = x$total_ha[["sd"]],
    total_ha_q5 = x$total_ha[["5%"]], total_ha_q95 = x$total_ha[["95%"]],
    team_ha_sd_off = stats::median(x$team_ha_off_sd), team_ha_sd_def = stats::median(x$team_ha_def_sd),
    divergences = x$n_divergent, leapfrog = x$mean_leapfrog,
    holdout_n = x$holdout[["n"]], holdout_p_home = x$holdout[["p_home"]],
    holdout_obs_home = x$holdout[["obs_home"]],
    holdout_logloss = x$holdout[["logloss_1x2"]], holdout_scoreline = x$holdout[["scoreline"]]
  )
}))

wf_file <- file.path(out, "wf_summary.rds")
wf <- if (file.exists(wf_file)) readRDS(wf_file) else NULL
if (!is.null(wf)) {
  wf$pred <- wf$pred[wf$pred$variant %in% c(wf$ref, "base"), ]
  wf$hyper <- wf$hyper[wf$hyper$variant %in% c(wf$ref, "base"), ]
  keep <- c("sex", "name", "variant", "season", "cutoff", "value", "reestimated")
  wf$hyper <- wf$hyper[wf$hyper$name %in% grep("^(tau|sigma|kappa|omega|delta|c_)", unique(wf$hyper$name), value = TRUE), keep]
  wf$priced_detail <- wf$priced_detail[c("sex", "match_date", "division", "phase", "ls_model", "ls_line", "ls_prod", "p_home", "phome_line", "phome_prod", "home_score", "away_score")]
  wf$rounds_detail <- wf$rounds_detail[c("sex", "match_date", "division", "phase", "ls_model", "ls_prodround", "p_home", "phome_prodround", "home_score", "away_score")]
}

read_if <- function(name) {
  f <- file.path(out, name)
  if (file.exists(f)) readRDS(f) else NULL
}
q3 <- function(x) stats::quantile(x, c(0.05, 0.5, 0.95), names = FALSE)

# Stan fits that estimate the reversion coefficient: tag "rhoest" for attack
# and defence walks, "rhoestsq" for strength with a fixed style.
rho <- do.call(rbind, lapply(c("rhoest", "rhoestsq"), function(tag) {
  do.call(rbind, lapply(c("female", "male"), function(sex) {
    f <- read_if(sprintf("val_%s_%s_2026-07-31_stan.rds", tag, sex))
    if (is.null(f)) {
      return(NULL)
    }
    h <- f$draws$hyper
    d <- f$diagnostics
    data.frame(
      form = if (tag == "rhoest") "ad" else "sq", sex = sex, parameter = colnames(h),
      mean = colMeans(h), q5 = apply(h, 2L, function(x) q3(x)[1]),
      q50 = apply(h, 2L, function(x) q3(x)[2]), q95 = apply(h, 2L, function(x) q3(x)[3]),
      divergences = d$n_divergent, leapfrog = d$mean_leapfrog, max_rhat = d$state_max_rhat,
      row.names = NULL
    )
  }))
}))

dry <- lapply(list.files(out, pattern = "^dryrun_.*\\.rds$"), read_if)

# Weekly against four-weekly hyperparameter re-estimation, same matches.
h1 <- read_if("wf_base_female_2026_h1.rds")
h4 <- read_if("wf_base_female_2026.rds")
hyper_check <- NULL
if (!is.null(h1) && !is.null(h4)) {
  a <- h1$pred
  b <- h4$pred[match(paste(a$match_date, a$home_team, a$away_team), paste(h4$pred$match_date, h4$pred$home_team, h4$pred$away_team)), ]
  hyper_check <- data.frame(
    n = nrow(a), mean_abs_dp = mean(abs(a$p_home - b$p_home)), max_abs_dp = max(abs(a$p_home - b$p_home)),
    d_1x2 = mean(a$ls_1x2 - b$ls_1x2), se_1x2 = stats::sd(a$ls_1x2 - b$ls_1x2) / sqrt(nrow(a)),
    d_scoreline = mean(a$ls_score - b$ls_score), se_scoreline = stats::sd(a$ls_score - b$ls_score) / sqrt(nrow(a))
  )
}

res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  validation = read_validation("calendar"),
  validation_final = read_validation("final"),
  ha = ha, wf = wf,
  rho = rho, timing = read_if("engine_timing.rds"), dryrun = dry, hyper_check = hyper_check
)
saveRDS(res, file.path(dest, "results.rds"), compress = "xz")
cat(sprintf("wrote %s (%.0f KB)\n", file.path(dest, "results.rds"), file.info(file.path(dest, "results.rds"))$size / 1024))

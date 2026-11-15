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
  keep <- c("sex", "name", "variant", "season", "cutoff", "value", "reestimated")
  wf$hyper <- wf$hyper[wf$hyper$name %in% grep("^(tau|sigma|kappa|omega|delta|c_)", unique(wf$hyper$name), value = TRUE), keep]
  wf$priced_detail <- wf$priced_detail[c("sex", "match_date", "division", "phase", "ls_model", "ls_line", "ls_prod", "p_home", "phome_line", "phome_prod", "home_score", "away_score")]
  wf$rounds_detail <- wf$rounds_detail[c("sex", "match_date", "division", "phase", "ls_model", "ls_prodround", "p_home", "phome_prodround", "home_score", "away_score")]
}

extra_file <- file.path(out, "report_extra.rds")
res <- list(
  generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
  validation = read_validation("calendar"),
  validation_final = read_validation("final"),
  ha = ha, wf = wf,
  extra = if (file.exists(extra_file)) readRDS(extra_file) else NULL
)
saveRDS(res, file.path(dest, "results.rds"), compress = "xz")
cat(sprintf("wrote %s (%.0f KB)\n", file.path(dest, "results.rds"), file.info(file.path(dest, "results.rds"))$size / 1024))

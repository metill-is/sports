# Is one fit reproducible from a cold start? Refits the margin model of two handball men
# cutoffs without a warm start and keeps the internal mode and the predictions. Run it
# several times in separate R processes; wf_status.R compares the runs with each other and
# with the walk-forward, which reached the same cutoffs through a chain of warm starts.
# With "noopt" as second argument R-INLA's online internal optimisation is switched off for
# the fit (fit_v2_inla(reproducible = TRUE), the fitter's default since this test); without
# it the fit runs with R-INLA's own setting, to see whether that option is what makes
# separate processes disagree.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/cold_start_check.R")' <run number> [noopt]
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
args <- commandArgs(trailingOnly = TRUE)
run <- args[[1]]
noopt <- length(args) >= 2 && args[[2]] == "noopt"
cell <- "hb_m"
ci <- CELLS[[cell]]
league <- load_leagues()[[ci$league]]
res_all <- cell_results(cell)
played <- played_only(res_all, ci$sport)
out <- list()
for (cu in c("2021-09-15", "2022-10-19")) {
  cutoff <- as.Date(cu)
  train <- as.data.frame(suppressMessages(model_training_results(res_all, league, end_date = cutoff)))
  teams <- sort(unique(c(train$home_team, train$away_team)))
  pred <- played[played$match_date > cutoff & played$match_date <= cutoff + 7 &
    played$home_team %in% teams & played$away_team %in% teams, , drop = FALSE]
  v <- build_v2_data(train, pred, teams,
    season_start = season_starts(res_all), n_weeks = season_weeks(res_all), division_map = DIVISION_MAP[[ci$sport]]
  )
  f <- fit_v2_inla(v, "margin", v2_priors(ci$sport), within = "none", int_strategy = "eb", reproducible = noopt)
  out[[cu]] <- list(
    theta = f$theta, mode_status = f$mode_status,
    pred = data.frame(match_date = pred$match_date, home_team = pred$home_team, away_team = pred$away_team, mean_m = f$pred$mean)
  )
  cat(sprintf("cutoff %s: mode status %d, theta %s\n", cu, f$mode_status, paste(sprintf("%.6f", f$theta), collapse = " ")))
}
saveRDS(out, file.path(OUT, "out", sprintf("cold_start_%s%s.rds", if (noopt) "noopt_" else "", run)))

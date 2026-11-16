#!/usr/bin/env Rscript
# The out-of-sample draw correction of the report (its draw-correction table),
# computed from the raw experiment files so it can be checked without rendering.
# For each tournament: estimate one draw inflation from the base model's
# walk-forward forecasts of competitive matches played before the tournament
# started (target tournaments excluded), then apply it to the base model's
# tournament forecasts.
#
# Usage: Rscript scripts/nt/draw_correction.R <experiment out dir> [out.rds]
#   reads  <out dir>/wf/base/walk_forward.rds and <out dir>/eval/eval_<key>.rds

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 150)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- args[[1]]

wf <- readRDS(file.path(out_dir, "wf", "base", "walk_forward.rds"))
panel <- do.call(rbind, lapply(wf, function(x) {
  if (is.null(x$matches)) {
    return(NULL)
  }
  data.frame(
    match_date = x$matches$match_date,
    friendly = x$matches$tournament == "Friendly",
    in_target = x$matches$in_target,
    home_score = x$matches$home_score, away_score = x$matches$away_score,
    p_home = x$scores$base$p_home, p_draw = x$scores$base$p_draw,
    p_away = x$scores$base$p_away
  )
}))
panel <- panel[!panel$friendly & !panel$in_target & !is.na(panel$p_draw), ]

spec <- nt_tournaments()
rows <- list()
diff_log <- diff_rps <- numeric()
for (i in seq_len(nrow(spec))) {
  ev <- readRDS(file.path(out_dir, "eval", paste0("eval_", spec$key[i], ".rds")))
  base <- ev$scores$base
  m <- ev$matches
  hist <- panel[panel$match_date < spec$start[i], ]
  phi <- nt_fit_draw_inflation(
    hist$p_home, hist$p_draw, hist$p_away, hist$home_score, hist$away_score
  )
  adj <- nt_inflate_draws(base$p_home, base$p_draw, base$p_away, phi)
  sc <- nt_score_outcomes(adj$p_home, adj$p_draw, adj$p_away, m$home_90, m$away_90)
  rows[[i]] <- data.frame(
    tournament = spec$label[i], panel_matches = nrow(hist), phi = phi,
    draws_expected = sum(base$p_draw), draws_corrected = sum(adj$p_draw),
    draws_observed = sum(m$home_90 == m$away_90),
    log_1x2 = mean(base$log_1x2), log_1x2_corrected = mean(sc$log_1x2)
  )
  diff_log <- c(diff_log, sc$log_1x2 - base$log_1x2)
  diff_rps <- c(diff_rps, sc$rps - base$rps)
}
tab <- do.call(rbind, rows)
print(tab, digits = 4)
pooled <- rbind(
  data.frame(score = "log_1x2", nt_paired_diff(diff_log, 0 * diff_log)),
  data.frame(score = "rps", nt_paired_diff(diff_rps, 0 * diff_rps))
)
print(pooled[, c("score", "diff", "se", "lower", "upper", "n")], digits = 4)
if (length(args) >= 2L) {
  saveRDS(list(table = tab, pooled = pooled), args[[2]])
}

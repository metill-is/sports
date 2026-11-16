# How much do two runs of the SAME engine differ on held-out match
# probabilities? The engine comparison was run twice (before and after the
# review fixes). Where the corrected reversion groups do not reach a fit, the
# two NUTS runs are two runs of one model, so their difference is NUTS's own
# Monte Carlo noise: the yardstick for reading INLA against NUTS. The same
# goes for the held-out log scores: the difference between two runs of one
# engine is what the draws alone contribute to a score difference.
#
#   Rscript research/football-v2/engine_noise.R <old_out> <new_out>
# Writes engine_noise.rds to `new_out`. Run from the repo root.
options(width = 200)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
old_out <- args[[1]]
new_out <- args[[2]]
scan <- readRDS(file.path(old_out, "group_change_scan.rds"))$validation

probs <- function(dir, tag, sex, cutoff, engine) {
  s <- readRDS(file.path(dir, sprintf("val_%s_%s_%s_spec.rds", tag, sex, cutoff)))
  f <- readRDS(file.path(dir, sprintf("val_%s_%s_%s_%s.rds", tag, sex, cutoff, engine)))
  p <- fv2_predict(f$draws, s$spec)
  sc <- fv2_score(p$lam_home, p$lam_away, s$pred$home_score, s$pred$away_score)
  list(p = cbind(sc$p_home, sc$p_draw, sc$p_away), ls_score = sc$ls_score, ls_1x2 = sc$ls_1x2)
}
settings <- data.frame(tag = c("finallvl", "early26", "early22"), cutoff = c("2026-07-31", "2026-05-03", "2022-05-08"))
rows <- list()
for (i in seq_len(nrow(settings))) {
  for (sex in c("female", "male")) {
    tag <- settings$tag[i]
    cutoff <- settings$cutoff[i]
    pair <- function(a, b) {
      d <- abs(a$p - b$p)
      c(mean = mean(d), max = max(d))
    }
    # Mean held-out log score of run `a` minus run `b`, same matches.
    score <- function(a, b, metric) mean(a[[metric]] - b[[metric]])
    nuts_old <- probs(old_out, tag, sex, cutoff, "stan")
    nuts_new <- probs(new_out, tag, sex, cutoff, "stan")
    inla_old <- probs(old_out, tag, sex, cutoff, "inla-auto")
    inla_new <- probs(new_out, tag, sex, cutoff, "inla-auto")
    g <- scan[scan$sex == sex & scan$cutoff == cutoff, ]
    rows[[length(rows) + 1L]] <- data.frame(
      tag = tag, cutoff = cutoff, sex = sex, matches = nrow(nuts_new$p), breaks = g$breaks, breaks_changed = g$differing,
      nuts_rerun_mean = pair(nuts_new, nuts_old)[["mean"]], nuts_rerun_max = pair(nuts_new, nuts_old)[["max"]],
      inla_rerun_mean = pair(inla_new, inla_old)[["mean"]], inla_rerun_max = pair(inla_new, inla_old)[["max"]],
      before_mean = pair(inla_old, nuts_old)[["mean"]], before_max = pair(inla_old, nuts_old)[["max"]],
      after_mean = pair(inla_new, nuts_new)[["mean"]], after_max = pair(inla_new, nuts_new)[["max"]],
      nuts_rerun_scoreline = score(nuts_new, nuts_old, "ls_score"), nuts_rerun_1x2 = score(nuts_new, nuts_old, "ls_1x2"),
      inla_rerun_scoreline = score(inla_new, inla_old, "ls_score"), inla_rerun_1x2 = score(inla_new, inla_old, "ls_1x2"),
      before_scoreline = score(inla_old, nuts_old, "ls_score"), after_scoreline = score(inla_new, nuts_new, "ls_score"),
      before_1x2 = score(inla_old, nuts_old, "ls_1x2"), after_1x2 = score(inla_new, nuts_new, "ls_1x2")
    )
  }
}
noise <- do.call(rbind, rows)
saveRDS(noise, file.path(new_out, "engine_noise.rds"))
print(format(noise, digits = 3), row.names = FALSE)

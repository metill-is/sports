# Leak-free weekly walk-forward of one variant over one season.
#
#   Rscript research/football-v2/run_wf.R <sex> <season> <variant> [threads] [n_samp]
#
# For every calendar week of the season with a match, the model is fitted on
# results up to the Sunday before (production's own training filter) and
# predicts that week's matches. States are refitted at every cutoff (INLA,
# empirical Bayes); hyperparameters are re-estimated every `hyper_every`-th
# cutoff (default 4, warm-started) and held at their last estimate in
# between, so they too only ever see results before the cutoff.
#   [n_samp] [hyper_every] are optional 5th and 6th arguments.
# Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
source("research/football-v2/variants.R")
fv2_source()

args <- commandArgs(trailingOnly = TRUE)
sex <- args[[1]]
season <- as.integer(args[[2]])
variant <- args[[3]]
threads <- if (length(args) >= 4L) args[[4]] else "1:1"
n_samp <- if (length(args) >= 5L) as.integer(args[[5]]) else 500L
hyper_every <- if (length(args) >= 6L) as.integer(args[[6]]) else 4L
suffix <- if (hyper_every == 4L) "" else sprintf("_h%d", hyper_every)
opts <- fv2_variants()[[variant]]
stopifnot(!is.null(opts))

out_file <- file.path(fv2_out_dir(), sprintf("wf_%s_%s_%d%s.rds", variant, sex, season, suffix))
league <- load_leagues()[["football_iceland"]]
res <- fv2_results(sex)
divisions <- fv2_division_table(res, fv2_rank_order(league))
res$phase <- fv2_phase(res)
weeks <- sort(unique(fv2_week(res$match_date[res$season == season])))

pred <- hyper <- list()
theta <- NULL
since_estimate <- 0L
for (w in weeks) {
  cutoff <- FV2_EPOCH + 7L * w - 1L
  sp <- fv2_split(res, league, cutoff, horizon_days = 7L)
  if (!nrow(sp$pred)) next
  spec <- do.call(fv2_build, c(list(sp$train, sp$pred, divisions = divisions), opts))
  fit <- function(init, fixed = FALSE) {
    fv2_fit_inla(spec,
      int_strategy = "eb", n_samp = n_samp, threads = threads,
      theta_init = init, theta_fixed = fixed
    )
  }
  n_theta <- length(unlist(fv2_hyper_names(spec))) + 2L + 2L * isTRUE(opts$div_offsets) +
    (identical(opts$param, "sq") && identical(opts$style, "static"))
  if (length(theta) != n_theta) theta <- NULL
  reestimate <- is.null(theta) || since_estimate >= hyper_every
  f <- tryCatch(fit(theta, fixed = !reestimate), error = function(e) NULL)
  if (is.null(f) || !identical(as.integer(f$mode_status), 0L)) {
    reestimate <- TRUE
    f2 <- tryCatch(fit(NULL), error = function(e) {
      cat("FAIL", as.character(cutoff), conditionMessage(e), "\n")
      NULL
    })
    if (!is.null(f2)) f <- f2
  }
  if (is.null(f)) next
  since_estimate <- if (reestimate) 1L else since_estimate + 1L
  theta <- f$theta_mode
  pr <- fv2_predict(f$draws, spec)
  sc <- fv2_score(pr$lam_home, pr$lam_away, sp$pred$home_score, sp$pred$away_score)
  keep <- c("match_date", "home_team", "away_team", "home_score", "away_score", "division", "round", "season", "phase")
  pred[[length(pred) + 1L]] <- cbind(
    sex = sex, variant = variant, sp$pred[keep], cutoff = cutoff, n_train = nrow(sp$train), sc
  )
  hyper[[length(hyper) + 1L]] <- data.frame(
    sex = sex, variant = variant, season = season, cutoff = cutoff,
    n_train = nrow(sp$train), n_pred = nrow(sp$pred), skipped = sp$skipped,
    S = spec$S, t_fit = f$t_fit, t_samp = f$t_samp, mode_status = f$mode_status,
    reestimated = reestimate,
    name = c(colnames(f$draws$hyper), rownames(f$fixed)),
    value = c(f$draws$hyper[1L, ], f$fixed[, "mean"])
  )
  cat(sprintf(
    "%s %s %d %s: n_train=%d predicted %d (skipped %d), fit %.1fs + %.1fs\n",
    variant, sex, season, cutoff, nrow(sp$train), nrow(sp$pred), sp$skipped, f$t_fit, f$t_samp
  ))
}
pred <- do.call(rbind, pred)
hyper <- do.call(rbind, hyper)
saveRDS(list(pred = pred, hyper = hyper), out_file)
cat(sprintf(
  "WF %s %s %d DONE: %d matches; scoreline LS %.4f, 1X2 log loss %.4f\n",
  variant, sex, season, nrow(pred), mean(pred$ls_score), -mean(pred$ls_1x2)
))

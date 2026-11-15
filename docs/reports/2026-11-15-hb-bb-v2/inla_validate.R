# R-INLA fits of every v2 variant at the fixed cutoff, for the like-for-like comparison
# with the Stan reference (compare_validation.R) and for the last rungs of the ladder.
# Per cell: margin and total x within-season form (none, ar1) x hyperparameter
# integration (full, plug-in). Keeps hyperparameter and state summaries, the held-out
# predictions, joint posterior samples and leave-one-out densities.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/inla_validate.R")' [cells]
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
args <- commandArgs(trailingOnly = TRUE)
cells <- if (length(args)) strsplit(args[[1]], ",")[[1]] else names(CELLS)
cutoff <- "2026-01-31"
slots <- tryCatch(system("/Users/brynjolfurjonsson/.claude-reviews/machine-slots/slot.sh status", intern = TRUE),
                  error = function(e) NA_character_)
for (cell in cells) {
  d <- readRDS(file.path(OUT, "out", sprintf("data_%s_%s.rds", cell, cutoff)))
  pr <- v2_priors(CELLS[[cell]]$sport)
  res <- list(machine = list(started = format(Sys.time()), slots = slots,
                             loadavg = system("sysctl -n vm.loadavg", intern = TRUE)))
  for (resp in c("margin", "total")) {
    for (within in c("none", "ar1")) {
      for (strategy in c("eb", "auto")) {
        f <- fit_v2_inla(d$v2, resp, pr, within = within, int_strategy = strategy,
                         n_samples = if (strategy == "auto") 2000L else 0L, cpo = TRUE)
        f$n_hyper_points <- nrow(f$fit$joint.hyper)
        f$cpu <- f$fit$cpu.used
        f$fit <- NULL
        res[[paste(resp, within, strategy)]] <- f
        cat(sprintf("%s %s within=%s %s: %.1f s, sum log cpo %.1f, sigma %.3f\n", cell, resp, within, strategy, f$secs,
                    sum(log(f$cpo)), sqrt(f$sigma2)))
      }
    }
  }
  saveRDS(res, file.path(OUT, "out", sprintf("inla_validate_%s.rds", cell)))
}

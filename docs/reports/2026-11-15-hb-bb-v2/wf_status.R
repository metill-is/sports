# Two questions about the R-INLA fits behind the report.
# 1. Did any fit end with a failed search for the hyperparameter mode? Every script stores
#    the status since the fitter reads it; this collects them.
# 2. How far apart are two complete runs of the walk-forward? The first run (the one the
#    report was first written from, kept under out/first-run/) and the second (out/) used
#    the same code path for the fits and the same data.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/wf_status.R")'
options(width = 200)
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
rd <- function(...) readRDS(file.path(OUT, "out", ...))
key <- function(x) paste(x$match_date, x$home_team, x$away_team)
se <- function(v) stats::sd(v) / sqrt(length(v))
runs <- c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg", "bb_m_ar1_reg", "bb_f_ar1_reg", "bb_m_none", "bb_f_none")

wf <- do.call(rbind, lapply(runs, function(k) {
  b <- rd(sprintf("wf_%s.rds", k))
  a <- rd("first-run", sprintf("wf_%s.rds", k))
  i <- match(key(a$pred), key(b$pred))
  dm <- abs(a$pred$mean_m - b$pred$mean_m[i])
  dt <- abs(a$pred$mean_t - b$pred$mean_t[i])
  hy <- grep("|", names(b$cut), fixed = TRUE, value = TRUE)
  j <- match(format(a$cut$cutoff), format(b$cut$cutoff))
  rel <- abs(as.matrix(a$cut[, hy]) - as.matrix(b$cut[j, hy])) / pmax(abs(as.matrix(b$cut[j, hy])), 0.05)
  data.frame(
    run = k, cutoffs = nrow(b$cut), fits = 2L * nrow(b$cut),
    status_not_zero = sum(!(b$cut$status_m %in% 0L)) + sum(!(b$cut$status_t %in% 0L)),
    matches = nrow(b$pred), in_both = sum(!is.na(i)),
    median_abs_diff_margin = stats::median(dm, na.rm = TRUE), max_abs_diff_margin = max(dm, na.rm = TRUE),
    share_margin_over_half = mean(dm > 0.5, na.rm = TRUE),
    median_abs_diff_total = stats::median(dt, na.rm = TRUE), max_abs_diff_total = max(dt, na.rm = TRUE),
    cutoffs_hyper_differs_25pct = sum(apply(rel, 1, max) > 0.25, na.rm = TRUE)
  )
}))
cat("Walk-forward: mode status of the second run, and distance between the two runs\n")
print(format(wf, digits = 3), row.names = FALSE)

# The same scores from each run's predictions, scored by the same script.
s2 <- rd("wf_scores.rds")
s1 <- rd("first-run", "wf_scores.rds")
tags <- c(hb_m = "hb_m none", hb_f = "hb_f none", bb_m = "bb_m none_reg", bb_f = "bb_f none_reg")
cols <- c(margin = "lsm_raw", total = "lst_raw", result = "ll_wdl_raw")
scores <- do.call(rbind, lapply(names(tags), function(cl) {
  a <- s1[[tags[[cl]]]]$scores
  b <- s2[[tags[[cl]]]]$scores
  i <- match(key(a), key(b))
  do.call(rbind, lapply(names(cols), function(nm) {
    d <- b[[cols[[nm]]]][i] - a[[cols[[nm]]]]
    data.frame(
      cell = cl, score = nm, n = sum(!is.na(d)), first_run = mean(a[[cols[[nm]]]]), second_run = mean(b[[cols[[nm]]]]),
      diff = mean(d, na.rm = TRUE), diff_se = se(d[!is.na(d)]), max_abs_match_diff = max(abs(d), na.rm = TRUE)
    )
  }))
}))
cat("\nMean out-of-sample scores from each run (second minus first, paired by match)\n")
print(format(scores, digits = 4), row.names = FALSE)

# Mode status of the fits outside the walk-forward.
ds <- rd("descriptives.rds")
other <- do.call(rbind, c(
  lapply(names(ds), function(cl) data.frame(source = "descriptives.rds", fit = paste(cl, c("margin", "total")), status = c(ds[[cl]]$status_m, ds[[cl]]$status_t))),
  lapply(names(tags), function(cl) {
    iv <- rd(sprintf("inla_validate_%s.rds", cl))
    fits <- setdiff(names(iv), "machine")
    data.frame(source = sprintf("inla_validate_%s.rds", cl), fit = fits, status = vapply(fits, function(f) iv[[f]]$mode_status, 1L))
  }),
  list({
    wr <- rd("within_rho.rds")
    data.frame(source = "within_rho.rds", fit = paste(wr$cell, wr$rho_week), status = wr$mode_status)
  })
))
rownames(other) <- NULL
cat(sprintf("\nFits outside the walk-forward: %d, of which mode status not 0: %d\n", nrow(other), sum(!(other$status %in% 0L))))
cat(sprintf("Walk-forward fits: %d, of which mode status not 0: %d\n", sum(wf$fits), sum(wf$status_not_zero)))
# Cold starts: the same fit in separate R processes (cold_start_check.R), against each other
# and against the walk-forward's warm-started fit of the same cutoff.
cold_files <- sort(Sys.glob(file.path(OUT, "out", "cold_start_*.rds")))
cold <- NULL
if (length(cold_files) > 1L) {
  runs_c <- lapply(cold_files, readRDS)
  wfp <- rd("wf_hb_m_none.rds")$pred
  wfp1 <- rd("first-run", "wf_hb_m_none.rds")$pred
  cold <- do.call(rbind, lapply(names(runs_c[[1]]), function(cu) {
    p1 <- runs_c[[1]][[cu]]$pred
    same <- vapply(runs_c, function(r) identical(r[[cu]]$pred$mean_m, p1$mean_m), TRUE)
    data.frame(
      cutoff = cu, processes = length(runs_c), bit_identical = sum(same),
      max_abs_diff_from_second_run = max(abs(p1$mean_m - wfp$mean_m[match(key(p1), key(wfp))])),
      max_abs_diff_from_first_run = max(abs(p1$mean_m - wfp1$mean_m[match(key(p1), key(wfp1))]))
    )
  }))
  cat("\nCold-start fits of two handball men cutoffs in separate processes\n")
  print(format(cold, digits = 3), row.names = FALSE)
}
saveRDS(list(wf = wf, scores = scores, other = other, cold = cold), file.path(OUT, "out", "wf_status.rds"))

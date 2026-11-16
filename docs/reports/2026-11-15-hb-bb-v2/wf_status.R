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
# A walk-forward whose failed fits were refitted keeps the original status beside the new one,
# and the failed fit's predictions beside the refit's.
first_status <- function(cut, r) {
  x <- cut[[paste0("status_", r, "_first")]]
  if (is.null(x)) cut[[paste0("status_", r)]] else x
}
refit_rows <- function(pred) {
  hit <- rep(FALSE, nrow(pred))
  for (cn in c("mean_m_failed_fit", "mean_t_failed_fit")) if (!is.null(pred[[cn]])) hit <- hit | !is.na(pred[[cn]])
  hit
}
refit_change <- function(pred) {
  d <- rep(0, nrow(pred))
  for (r in c("m", "t")) {
    old <- pred[[paste0("mean_", r, "_failed_fit")]]
    if (!is.null(old)) d <- pmax(d, abs(pred[[paste0("mean_", r)]] - old), na.rm = TRUE)
  }
  d[refit_rows(pred)]
}

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
    # As the run was made, and after the failed fits were repeated from a cold start.
    status_not_zero = sum(!(first_status(b$cut, "m") %in% 0L)) + sum(!(first_status(b$cut, "t") %in% 0L)),
    status_not_zero_after_refit = sum(!(b$cut$status_m %in% 0L)) + sum(!(b$cut$status_t %in% 0L)),
    refit_matches = sum(refit_rows(b$pred)), refit_max_change = if (any(refit_rows(b$pred))) max(refit_change(b$pred)) else 0,
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
cat(sprintf("Walk-forward fits: %d, of which mode status not 0: %d as run, %d after the cold refit of those\n", sum(wf$fits), sum(wf$status_not_zero),
            sum(wf$status_not_zero_after_refit)))
# Cold starts: the same fit in separate R processes (cold_start_check.R), with R-INLA's
# defaults and with its online internal optimisation switched off; against each other and
# against the walk-forward's warm-started fit of the same cutoff.
wfp <- rd("wf_hb_m_none.rds")$pred
wfp1 <- rd("first-run", "wf_hb_m_none.rds")$pred
cold_set <- function(pattern, label) {
  files <- sort(Sys.glob(file.path(OUT, "out", pattern)))
  if (length(files) < 2L) {
    return(NULL)
  }
  rr <- lapply(files, readRDS)
  do.call(rbind, lapply(names(rr[[1]]), function(cu) {
    p1 <- rr[[1]][[cu]]$pred
    m <- vapply(rr, function(r) r[[cu]]$pred$mean_m, numeric(nrow(p1)))
    grp <- match(apply(m, 2, function(v) paste(format(v, digits = 17), collapse = " ")), unique(apply(m, 2, function(v) paste(format(v, digits = 17), collapse = " "))))
    modal <- as.integer(names(which.max(table(grp))))
    ref <- m[, which(grp == modal)[1]]
    data.frame(
      setting = label, cutoff = cu, processes = ncol(m), distinct_results = max(grp), most_frequent = sum(grp == modal),
      max_abs_diff_between_processes = max(abs(m - ref)),
      max_abs_diff_from_second_run = max(abs(ref - wfp$mean_m[match(key(p1), key(wfp))])),
      max_abs_diff_from_first_run = max(abs(ref - wfp1$mean_m[match(key(p1), key(wfp1))])),
      status_not_zero = sum(vapply(rr, function(r) !(r[[cu]]$mode_status %in% 0L), TRUE))
    )
  }))
}
cold <- rbind(cold_set("cold_start_[0-9]*.rds", "R-INLA defaults"), cold_set("cold_start_noopt_*.rds", "internal.opt = FALSE"))
if (!is.null(cold)) {
  cat("\nCold-start fits of two handball men cutoffs in separate processes\n")
  print(format(cold, digits = 3), row.names = FALSE)
}
# Two whole walk-forwards with the online optimisation off, if they were run (noopt-a, noopt-b).
noopt <- NULL
fa <- file.path(OUT, "noopt-a", "out", "wf_hb_m_none.rds")
fb <- file.path(OUT, "noopt-b", "out", "wf_hb_m_none.rds")
if (file.exists(fa) && file.exists(fb)) {
  a <- readRDS(fa)$pred
  b <- readRDS(fb)$pred
  i <- match(key(a), key(b))
  j <- match(key(a), key(wfp))
  cols <- c("mean_m", "sd_m", "sig2_m", "mean_t", "sd_t", "sig2_t")
  same <- Reduce(`&`, lapply(cols, function(cn) a[[cn]] == b[[cn]][i]))
  noopt <- data.frame(
    run = "hb_m_none", matches = nrow(a), identical_rows = sum(same),
    max_abs_diff_margin = max(abs(a$mean_m - b$mean_m[i])), max_abs_diff_total = max(abs(a$mean_t - b$mean_t[i])),
    max_abs_diff_margin_from_default_run = max(abs(a$mean_m - wfp$mean_m[j]), na.rm = TRUE),
    median_abs_diff_margin_from_default_run = stats::median(abs(a$mean_m - wfp$mean_m[j]), na.rm = TRUE),
    # WHY a ratio: both sets of fits were timed on the shared machine, so neither time means
    # much alone. The ratio of the medians over the same cutoffs is what the option costs.
    fit_seconds_ratio_to_default = local({
      ca <- readRDS(fa)$cut
      cb <- rd("wf_hb_m_none.rds")$cut
      k <- match(format(ca$cutoff), format(cb$cutoff))
      stats::median(ca$secs_m + ca$secs_t) / stats::median((cb$secs_m + cb$secs_t)[k], na.rm = TRUE)
    })
  )
  cat("\nTwo walk-forwards of handball men with internal.opt = FALSE, against each other and against the second run\n")
  print(format(noopt, digits = 3), row.names = FALSE)
}
saveRDS(list(wf = wf, scores = scores, other = other, cold = cold, noopt = noopt), file.path(OUT, "out", "wf_status.rds"))

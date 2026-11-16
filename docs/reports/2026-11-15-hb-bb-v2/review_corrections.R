# Old value beside new for every number the review of 16 November changed.
# Three states of the result files:
#   published  out/at-84b9e6e5b/  the results the report at 84b9e6e5b was built from
#   corrected  out/first-run/     the same walk-forward predictions, scored by the corrected scripts
#   final      out/               the second run of the walk-forward (mode status recorded), corrected scripts
# So "published -> corrected" is the effect of a fix and "corrected -> final" the effect of
# running the fits again.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/review_corrections.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
state <- list(
  published = function(f) readRDS(file.path(OUT, "out", "at-84b9e6e5b", f)),
  corrected = function(f) readRDS(file.path(OUT, "out", "first-run", f)),
  final = function(f) readRDS(file.path(OUT, "out", f))
)
pm <- function(d) sprintf("%+.4f (se %.4f)", mean(d), stats::sd(d) / sqrt(length(d)))
rows <- list()
add_row <- function(finding, what, f) {
  v <- vapply(names(state), function(s) tryCatch(f(state[[s]], s), error = function(e) paste("n/a:", conditionMessage(e))), "")
  rows[[length(rows) + 1L]] <<- data.frame(finding = finding, what = what, published = v[["published"]], corrected = v[["corrected"]], final = v[["final"]])
}
tags <- c(hb_m = "hb_m none", hb_f = "hb_f none", bb_m = "bb_m none_reg", bb_f = "bb_f none_reg")
lab <- c(hb_m = "handball men", hb_f = "handball women", bb_m = "basketball men", bb_f = "basketball women")

# Finding 1 (and 13 for the women's variant): v2 minus production's stored forecast, margin.
for (cl in c("bb_m", "bb_f")) {
  add_row("1", sprintf("v2 minus production's stored forecast, final-margin log score, %s", lab[[cl]]), function(get, s) {
    x <- get("wf_compare.rds")[[cl]]$cmp
    pm(x$lsm_raw - x$ls_margin)
  })
}
add_row("1", "the same, basketball pooled", function(get, s) {
  x <- get("wf_compare.rds")
  pm(c(x$bb_m$cmp$lsm_raw - x$bb_m$cmp$ls_margin, x$bb_f$cmp$lsm_raw - x$bb_f$cmp$ls_margin))
})
add_row("1", "for reference, handball pooled (not affected by the fix)", function(get, s) {
  x <- get("wf_compare.rds")
  pm(c(x$hb_m$cmp$lsm_raw - x$hb_m$cmp$ls_margin, x$hb_f$cmp$lsm_raw - x$hb_f$cmp$ls_margin))
})

# Finding 10: regulation + overtime against the final-score model, final margin.
for (cl in c("bb_m", "bb_f")) {
  add_row("10", sprintf("regulation + overtime minus the final-score model, final-margin log score, %s", lab[[cl]]), function(get, s) {
    j <- get("wf_scores.rds")[[paste(cl, "reg_vs_final")]]
    v <- if (s == "published") "shift_div" else REG_SHIFT[[cl]]
    pm(j[[paste0("lsm_final_", v)]] - j$lsm_raw.final)
  })
}

# The same defect at the fixed cutoff (not listed by the review): every rung of the ladder
# scored a basketball final margin with mass on a level score. All rungs alike, so the
# levels move and the differences between rungs hardly do.
for (cl in c("bb_m", "bb_f")) {
  add_row("1, 10 (ladder)", sprintf("ladder, held-out margin log score of production, %s", lab[[cl]]), function(get, s) {
    sprintf("%.4f", mean(get("ladder.rds")$scores[[paste(cl, "r0_prod")]]$ls_margin))
  })
  add_row("1, 10 (ladder)", sprintf("ladder, v2 static (R-INLA, full) minus production, held-out margin, %s", lab[[cl]]), function(get, s) {
    l <- get("ladder.rds")$scores
    pm(l[[paste(cl, "v2 static, INLA full")]]$ls_margin - l[[paste(cl, "r0_prod")]]$ls_margin)
  })
}

# Finding 11: season phase of the unfinished season.
for (cl in names(tags)) {
  add_row("11", sprintf("matches labelled last 6 weeks (of which from the unfinished season), %s", lab[[cl]]), function(get, s) {
    d <- get("wf_scores.rds")[[tags[[cl]]]]$scores
    late <- d$phase == "last 6 weeks"
    sprintf("%d (%d)", sum(late), sum(late & d$season == max(d$season)))
  })
  add_row("11", sprintf("sd of the standardised margin residual in the last 6 weeks, %s", lab[[cl]]), function(get, s) {
    d <- get("wf_scores.rds")[[tags[[cl]]]]$scores
    d <- d[d$phase == "last 6 weeks", ]
    sprintf("%.3f", stats::sd((d$margin_fit - d$mean_m) / d$s_m))
  })
}

# Finding 12: whose forecast is the older one.
add_row("12", "matches where v2's forecast is the older one / production's / same age", function(get, s) {
  if (s == "published") {
    return("stated as: v2 always the older")
  }
  x <- get("wf_compare.rds")
  a <- do.call(rbind, lapply(names(tags), function(cl) x[[cl]]$cmp[, c("lead_days", "lead_days_v2")]))
  sprintf("%d / %d / %d of %d", sum(a$lead_days < a$lead_days_v2), sum(a$lead_days > a$lead_days_v2), sum(a$lead_days == a$lead_days_v2), nrow(a))
})

# Finding 13: the basketball variant in the Lengjan table.
for (cl in c("bb_m", "bb_f")) {
  add_row("13", sprintf("v2 regulation 1X2 log likelihood on the Lengjan-priced matches, %s", lab[[cl]]), function(get, s) {
    m <- get("wf_compare.rds")[[cl]]$market
    sprintf("%.4f (n %d)", mean(m$ll_v2), nrow(m))
  })
}

# Finding 19: predictor agreement with Stan, full integration and plug-in.
add_row("19", "largest difference from Stan in a held-out match's expected value: full / plug-in", function(get, s) {
  v <- get("compare_validation.rds")
  full <- max(vapply(v, function(o) o$pred[["max_abs_mean_diff"]], 1))
  eb <- max(vapply(v, function(o) o$pred[["max_abs_mean_diff_eb"]], 1))
  if (s == "published") sprintf("%.2f (plug-in %.2f not shown)", full, eb) else sprintf("%.2f / %.2f", full, eb)
})

# Finding 21: an odds ratio printed as a ratio of rates.
add_row("21", "level games in men's Bonusdeild against the smooth curve: odds ratio / rate ratio the shift implies / observed", function(get, s) {
  w <- get("wf_scores.rds")[["bb_m none_reg"]]
  d <- w$scores[w$scores$division == "BD", ]
  sh <- w$pars[[paste(max(w$scores$season), "shift_div")]][["BD"]]
  rr <- mean(stats::plogis(stats::qlogis(d$p_draw) + sh)) / mean(d$p_draw)
  obs <- mean(d$margin_fit == 0) / mean(d$p_draw)
  if (s == "published") sprintf("%.1f printed as the rate ratio", exp(sh)) else sprintf("%.1f / %.1f / %.1f", exp(sh), rr, obs)
})
add_row("21", "handball men, more draws than the band: odds ratio / per cent more draws", function(get, s) {
  d <- get("descriptives.rds")$hb_m$d
  p0 <- v2_integer_pmf(rep(0, nrow(d)), d$mu_m, d$sd_m)
  sh <- v2_fit_zero_shift(p0, d$margin == 0)[["shift"]]
  rr <- mean(stats::plogis(stats::qlogis(p0) + sh)) / mean(p0)
  if (s == "published") sprintf("%.0f %% (from the odds ratio %.2f)", 100 * (exp(sh) - 1), exp(sh)) else sprintf("%.2f / %.0f %%", exp(sh), 100 * (rr - 1))
})

# Finding 23: the variance form that grows with the expected total.
for (cl in names(tags)) {
  add_row("23", sprintf("variance by expected total minus plain, total log score, %s", lab[[cl]]), function(get, s) {
    d <- get("wf_scores.rds")[[tags[[cl]]]]$scores
    pm(d$lst_by_mismatch - d$lst_raw)
  })
}

# Finding 24: calibration shown in the "final result" rows.
for (cl in c("bb_m", "bb_f")) {
  add_row("24", sprintf("final-result row, sd of standardised residual margin / total; 80 %% coverage margin / total, %s", lab[[cl]]), function(get, s) {
    d <- get("wf_scores.rds")[[tags[[cl]]]]$scores
    if (s == "published") {
      zm <- (d$margin_fit - d$mean_m) / d$s_m
      zt <- (d$total_fit - d$mean_t) / d$s_t
    } else {
      zm <- d[[paste0("zm_final_", REG_SHIFT[[cl]])]]
      zt <- d[[paste0("zt_final_", REG_SHIFT[[cl]])]]
    }
    sprintf(
      "%.2f / %.2f; %.0f %% / %.0f %%%s", stats::sd(zm), stats::sd(zt), 100 * mean(abs(zm) < stats::qnorm(0.9)), 100 * mean(abs(zt) < stats::qnorm(0.9)),
      if (s == "published") " (the regulation-time values)" else ""
    )
  })
}

# Finding 7: the second run. The rows of the report's walk-forward table, from each state:
# handball on the plain rounded margin; basketball regulation time with the cell's tie
# shift for the margin and for the 1X2 alike.
for (cl in names(tags)) {
  handball <- cl %in% c("hb_m", "hb_f")
  what <- if (handball) {
    sprintf("walk-forward margin / total / W-D-L score, %s", lab[[cl]])
  } else {
    sprintf("walk-forward regulation-time margin / total / 1X2 score with the tie shift (%s), %s", if (REG_SHIFT[[cl]] == "shift_div") "per division" else "one shift", lab[[cl]])
  }
  add_row("7 (second run)", what, function(get, s) {
    d <- get("wf_scores.rds")[[tags[[cl]]]]$scores
    m <- if (handball) "lsm_raw" else paste0("lsm_", REG_SHIFT[[cl]])
    w <- if (handball) "ll_wdl_raw" else paste0("ll_wdl_", REG_SHIFT[[cl]])
    sprintf("%.4f / %.4f / %.4f (n %d)", mean(d[[m]]), mean(d$lst_raw), mean(d[[w]]), nrow(d))
  })
}
for (g in list(c("hb_m", "hb_f"), c("bb_m", "bb_f"))) {
  add_row("7 (second run)", sprintf("v2 minus production's stored forecast, %s pooled: margin / total / result", sub(" men", "", lab[[g[1]]])), function(get, s) {
    x <- get("wf_compare.rds")
    pool <- function(a, b) pm(unlist(lapply(g, function(cl) x[[cl]]$cmp[[a]] - x[[cl]]$cmp[[b]])))
    paste(pool("lsm_raw", "ls_margin"), pool("lst_raw", "ls_total"), pool("ll_wdl_raw", "ll_wdl"), sep = " / ")
  })
}

# Finding 7: what moved when the fixed-cutoff R-INLA fits were run again. The Stan side
# of these comparisons is one stored fit; "corrected" still has the first run's R-INLA fits.
add_row("7 (fixed-cutoff fits)", "R-INLA against Stan: largest difference in a state's posterior mean, in posterior sds", function(get, s) {
  sprintf("%.3f", max(vapply(get("compare_validation.rds"), function(o) o$state[["max_abs_mean_diff_in_sd"]], 1)))
})
add_row("7 (fixed-cutoff fits)", "R-INLA against Stan: largest hyperparameter difference, in posterior sds", function(get, s) {
  sprintf("%.2f", max(vapply(get("compare_validation.rds"), function(o) max(abs(o$hyper$diff_in_sd)), 1)))
})
add_row("7 (fixed-cutoff fits)", "R-INLA against Stan: held-out log scores agree to (full integration)", function(get, s) {
  sprintf("%.4f", max(vapply(get("compare_validation.rds"), function(o) abs(o$log_score[["stan"]] - o$log_score[["inla_full_samples"]]), 1)))
})
add_row("7 (fixed-cutoff fits)", "R-INLA, basketball men total model with the AR(1): medians of sd b / sd s_s / sd u_s", function(get, s) {
  h <- get("compare_validation.rds")[["bb_m total"]]$hyper
  g <- function(p) h$inla_q50[h$parameter == p]
  if (is.null(h$inla_q50)) {
    g <- function(p) h$inla_mean[h$parameter == p]
  }
  sprintf("%.2f / %.2f / %.2f", g("sd: b"), g("sd: s_s"), g("sd: u_s"))
})
add_row("7 (fixed-cutoff fits)", "ladder, basketball men, v2 + AR(1) R-INLA full: leave-one-out density / held-out total log score", function(get, s) {
  t <- get("ladder.rds")$tab
  r <- t[t$cell == "bb_m" & t$rung == "v2 + within-season AR(1), INLA full", ]
  sprintf("%.1f / %.4f", r$elpd, r$ls_total)
})

tab <- do.call(rbind, rows)
print(tab, right = FALSE, row.names = FALSE)
saveRDS(tab, file.path(OUT, "out", "review_corrections.rds"))

# Recompute every headline number of the report straight from the result files, using
# only the file and the fields the report's appendix names for it. Nothing here goes
# through the report builder, so it is an independent read of the same files.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/check_headlines.R")'
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
rd <- function(f) readRDS(file.path(OUT, "out", f))
se <- function(v) stats::sd(v) / sqrt(length(v))
say <- function(label, ...) cat(sprintf("%-58s %s\n", label, sprintf(...)))
cells <- c("hb_m", "hb_f", "bb_m", "bb_f")
# The tie treatment proposed for regulation-time basketball (common.R: REG_SHIFT).
reg_shift <- c(bb_m = "shift_div", bb_f = "shift")

cat("== diag_prod.rds: [[cell]]$ha_weighted, $raw_margin, $nu, $n_sampled\n")
dg <- rd("diag_prod.rds")
for (cl in cells) {
  say(
    paste(cl, "implied HA / raw margin / ratio"), "%.2f / %.2f / %.2f", dg[[cl]]$ha_weighted[["mean"]], dg[[cl]]$raw_margin[["mean"]],
    dg[[cl]]$ha_weighted[["mean"]] / dg[[cl]]$raw_margin[["mean"]]
  )
  say(paste(cl, "Student-t nu median; sampled parameters"), "%.1f; %d", dg[[cl]]$nu[2], dg[[cl]]$n_sampled)
}

cat("\n== res_<cell>_r0_prod.rds: $wall_secs, $n_divergent, $mean_leapfrog\n")
for (cl in cells) {
  r <- rd(sprintf("res_%s_r0_prod.rds", cl))
  say(paste(cl, "wall minutes / divergences / leapfrog"), "%.1f / %d / %.0f", r$wall_secs / 60, r$n_divergent, r$mean_leapfrog)
}

cat("\n== descriptives.rds: [[cell]]$hfa (identified home advantage, all data)\n")
ds <- rd("descriptives.rds")
for (cl in cells) say(paste(cl, "v2 home advantage (sd)"), "%.2f (%.2f)", ds[[cl]]$hfa$mean[1], ds[[cl]]$hfa$sd[1])
cat("\n== descriptives.rds: [[cell]]$disp$pearson_poisson (score variance / mean given strengths, by division)\n")
for (cl in cells) {
  say(paste(cl, "variance / mean by division"), "%s", paste(sprintf("%s %.2f", ds[[cl]]$disp$division, ds[[cl]]$disp$pearson_poisson), collapse = ", "))
}

cat("\n== repro_descriptives.rds: $fits (n_runs, n_modal, max_abs_fitted_diff), $hfa, $stored_is_modal\n")
rp <- rd("repro_descriptives.rds")
say("runs; fits; bit-identical with the most frequent result", "%d; %d; %d", length(rp$runs), sum(rp$fits$n_runs), sum(rp$fits$n_modal))
for (i in which(rp$fits$n_distinct > 1)) {
  say(
    paste(rp$fits$cell[i], rp$fits$response[i], "odd run; largest fitted diff; hyper median"), "%s; %.3f; %s %.3f vs %.3f", rp$fits$odd_runs[i],
    rp$fits$max_abs_fitted_diff[i], rp$fits$hyper_param[i], rp$fits$hyper_q50_other[i], rp$fits$hyper_q50_modal[i]
  )
}
say("hb_m home advantage min / max / most frequent", "%.4f / %.4f / %.4f", rp$hfa$hfa_min[1], rp$hfa$hfa_max[1], rp$hfa$hfa_modal[1])
say("stored descriptives.rds is the most frequent result for every fit", "%s", rp$stored_is_modal)

cat("\n== ladder.rds: $tab (cell, rung, n_sampled, leapfrog, elpd, ha_mean), $scores[[\"<cell> <rung>\"]]\n")
ld <- rd("ladder.rds")
tab <- ld$tab
for (cl in c("hb_m", "bb_m")) {
  x <- tab[tab$cell == cl & tab$engine == "Stan", c("rung", "n_sampled", "leapfrog", "elpd", "ha_mean")]
  print(format(x, digits = 6), row.names = FALSE)
}
pair <- function(cl, a, b, col) {
  d <- ld$scores[[paste(cl, a)]][[col]] - ld$scores[[paste(cl, b)]][[col]]
  sprintf("%+.4f (se %.4f)", mean(d), se(d))
}
say("bb_f v2 static INLA - production, held-out W/L", "%s", pair("bb_f", "v2 static, INLA full", "r0_prod", "ll_wdl"))
say("bb_f v2 + AR(1) INLA - production, held-out W/L", "%s", pair("bb_f", "v2 + within-season AR(1), INLA full", "r0_prod", "ll_wdl"))
say("hb_m r3 - r2, held-out margin", "%s", pair("hb_m", "r3_pooled", "r2_common", "ls_margin"))
say(
  "hb_m v2 static (Stan) - production: margin / total", "%s / %s", pair("hb_m", "v2s (margin + total)", "r0_prod", "ls_margin"),
  pair("hb_m", "v2s (margin + total)", "r0_prod", "ls_total")
)

cat("\n== compare_validation.rds: [[\"<cell> <response>\"]]$log_score, $pred, $state, $hyper\n")
cv <- rd("compare_validation.rds")
for (nm in names(cv)) {
  o <- cv[[nm]]
  say(
    nm, "log score Stan %.4f INLA %.4f | largest predictor diff %.3f | largest state diff %.3f sd | largest hyper diff %.2f sd",
    o$log_score[["stan"]], o$log_score[["inla_full_samples"]], o$pred[["max_abs_mean_diff"]], o$state[["max_abs_mean_diff_in_sd"]],
    max(abs(o$hyper$diff_in_sd))
  )
}

cat("\n== wf_<cell>_<tag>.rds: $pred (one row per predicted match), $cut$secs_m + $cut$secs_t\n")
secs <- unlist(lapply(c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), function(k) with(rd(sprintf("wf_%s.rds", k))$cut, secs_m + secs_t)))
say("weekly cutoffs fitted; median seconds for both fits", "%d; %.1f", length(secs), stats::median(secs))

cat("\n== wf_scores.rds: [[\"<cell> <tag>\"]]$scores, $within_j\n")
wf <- rd("wf_scores.rds")
sc <- list(hb_m = wf[["hb_m none"]]$scores, hb_f = wf[["hb_f none"]]$scores, bb_m = wf[["bb_m none_reg"]]$scores, bb_f = wf[["bb_f none_reg"]]$scores)
say("scored matches per cell; total", "%s; %d", paste(vapply(sc, nrow, 1L), collapse = " + "), sum(vapply(sc, nrow, 1L)))
dif <- function(x, a, b) sprintf("%+.4f (se %.4f)", mean(x[[a]] - x[[b]]), se(x[[a]] - x[[b]]))
for (cl in c("hb_m", "hb_f")) {
  say(paste(cl, "margin / total / W-D-L log score"), "%.4f / %.4f / %.4f", mean(sc[[cl]]$lsm_raw), mean(sc[[cl]]$lst_raw), mean(sc[[cl]]$ll_wdl_raw))
  say(paste(cl, "level scores observed / rounded"), "%.4f / %.4f", mean(sc[[cl]]$margin_fit == 0), mean(sc[[cl]]$p_draw))
  say(paste(cl, "possession variance - free, margin"), "%s", dif(sc[[cl]], "lsm_possession", "lsm_raw"))
  say(paste(cl, "draw shift - none, W/D/L"), "%s", dif(sc[[cl]], "ll_wdl_shift", "ll_wdl_raw"))
}
for (cl in c("bb_m", "bb_f")) {
  x <- sc[[cl]]
  say(paste(cl, "level after regulation observed / rounded"), "%.4f / %.4f", mean(x$margin_fit == 0), mean(x$p_draw))
  say(paste(cl, "tie shift by division - none, regulation 1X2"), "%s", dif(x, "ll_wdl_shift_div", "ll_wdl_raw"))
  say(paste(cl, "Student-t - Gaussian, regulation margin"), "%s", dif(x, "lsm_student", "lsm_raw"))
  say(paste(cl, "Student-t - Gaussian, regulation 1X2 / final"), "%s / %s", dif(x, "ll_wdl_student", "ll_wdl_shift_div"), dif(x, "ll_final_student", "ll_final_shift_div"))
  say(paste(cl, "final-result log likelihood (regulation + OT)"), "%.4f", mean(x[[paste0("ll_final_", reg_shift[[cl]])]]))
  zf <- x[[paste0("zm_final_", reg_shift[[cl]])]]
  say(paste(cl, "final-margin predictive: sd of standardised residual / 80 % coverage"), "%.3f / %.3f", stats::sd(zf), mean(abs(zf) < stats::qnorm(0.9)))
}
ot <- rbind(sc$bb_m[sc$bb_m$margin_fit == 0, c("mean_m", "margin")], sc$bb_f[sc$bb_f$margin_fit == 0, c("mean_m", "margin")])
say("overtime games; favourite won", "%d; %d", nrow(ot), sum(ifelse(ot$mean_m > 0, ot$margin > 0, ot$margin < 0)))
bd <- sc$bb_m[sc$bb_m$division == "BD", ]
say("bb_m BD level after regulation / rounded (ratio)", "%.4f / %.4f (%.1f)", mean(bd$margin_fit == 0), mean(bd$p_draw), mean(bd$margin_fit == 0) / mean(bd$p_draw))
wj <- do.call(rbind, lapply(c("bb_m_reg", "bb_f_reg"), function(k) wf$within_j[[k]]))
w <- function(j, a) sprintf("%+.4f (se %.4f)", mean(j[[paste0(a, ".ar1")]] - j[[paste0(a, ".static")]]), se(j[[paste0(a, ".ar1")]] - j[[paste0(a, ".static")]]))
say("basketball AR(1) - static, n; margin / total / 1X2", "%d; %s / %s / %s", nrow(wj), w(wj, "lsm_raw"), w(wj, "lst_raw"), w(wj, "ll_wdl_raw"))
w1 <- wj[wj$division == "1D", ]
say("... 1. deild only", "%d; %s / %s / %s", nrow(w1), w(w1, "lsm_raw"), w(w1, "lst_raw"), w(w1, "ll_wdl_raw"))
for (cl in c("bb_m", "bb_f")) {
  j <- wf[[paste(cl, "reg_vs_final")]]
  say(
    paste(cl, "regulation + OT - final-score model: result / margin"), "%s / %s", dif(j, paste0("ll_final_", reg_shift[[cl]]), "ll_wdl_raw.final"),
    dif(j, paste0("lsm_final_", reg_shift[[cl]]), "lsm_raw.final")
  )
}

for (cl in cells) {
  d <- sc[[cl]]
  late <- d$phase == "last 6 weeks"
  say(paste(cl, "last 6 weeks: matches (from the unfinished season); sd of standardised margin residual"), "%d (%d); %.3f", sum(late),
      sum(late & d$season == max(d$season)), stats::sd(((d$margin_fit - d$mean_m) / d$s_m)[late]))
}

cat("\n== wf_status.rds: $wf (fits, status_not_zero, retried, distance from out/second-run/), $scores, $other\n")
ws <- rd("wf_status.rds")
say("walk-forward fits; repeated from a cold start; still failed", "%d; %s; %d", sum(ws$wf$fits), format(sum(ws$wf$retried, na.rm = TRUE)), sum(ws$wf$status_not_zero))
say("predictions left out of the scores for a failed fit", "%d", sum(unlist(lapply(wf[grep(" (none|none_reg|ar1|ar1_reg)$", names(wf))], function(x) x$failed_fit_rows_left_out)), na.rm = TRUE))
say("other fits; with mode status not 0", "%d; %d", nrow(ws$other), sum(!(ws$other$status %in% 0L)))
main <- ws$wf[ws$wf$run %in% c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), ]
say("reported run against the second run, expected margin: median diff range; largest", "%.4f to %.4f; %.2f", min(main$median_abs_diff_margin),
    max(main$median_abs_diff_margin), max(main$max_abs_diff_margin))
say("... cutoffs with a hyperparameter median differing > 25 %", "%d of %d", sum(main$cutoffs_hyper_differs_25pct), sum(main$cutoffs))
say("... largest change in a cell's mean score", "%.4f", max(abs(ws$scores$diff)))
ws2 <- rd("second-run/wf_status.rds")
m2 <- ws2$wf[ws2$wf$run %in% c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), ]
say("second run: walk-forward fits; mode status not 0", "%d; %d", sum(ws2$wf$fits), sum(ws2$wf$status_not_zero))
say("first run against second run, expected margin: largest; cutoffs > 25 %; score", "%.2f; %d of %d; %.4f", max(m2$max_abs_diff_margin),
    sum(m2$cutoffs_hyper_differs_25pct), sum(m2$cutoffs), max(abs(ws2$scores$diff)))
for (i in seq_len(nrow(ws2$cold))) {
  say(paste("cold start,", ws2$cold$setting[i], ws2$cold$cutoff[i]), "%d of %d processes give the most frequent result", ws2$cold$most_frequent[i], ws2$cold$processes[i])
}
say("two test walk-forwards with the option off: identical predictions", "%d of %d", ws2$noopt$identical_rows, ws2$noopt$matches)

cat("\n== run_identity.rds: $files (numbers, identical, max_abs_diff): the reported run against its repeat\n")
ri <- rd("run_identity.rds")$files
say("result files compared; numbers; identical files", "%d; %d; %d", nrow(ri), sum(ri$numbers), sum(ri$identical))
if (any(!ri$identical)) print(ri[!ri$identical, ], row.names = FALSE)
r1 <- rd("run_identity_first.rds")$files
say("first comparison, before the draws were seeded: files; identical files", "%d; %d", nrow(r1), sum(r1$identical))
ru <- rd("run_identity_first.rds")$differing
is_draw <- grepl("[$]samples[$](loc|sigma)$", ru$component)
sg <- ru[grepl("[$]samples[$]sigma$", ru$component), ]
say("  components that differed; of them posterior draws; fits; share of a fit's draws that differed", "%d; %d; %d; %.1f%% to %.1f%%", nrow(ru), sum(is_draw), nrow(sg),
    100 * min(sg$differ / sg$values), 100 * max(sg$differ / sg$values))
say("  largest difference in a drawn value, handball / basketball", "%.2f / %.2f", max(ru$max_abs_diff[grepl("_hb_", ru$file)]), max(ru$max_abs_diff[grepl("_bb_", ru$file)]))
rv <- rd("run_identity_vs_second.rds")$differing
rv <- rv[!grepl("^wf_", rv$file) & !grepl("[$]samples[$](loc|sigma)$", rv$component), ]
cold_moved <- unique(paste(rv$file, sub("^[$]([^$]+)[$].*", "\\1", rv$component)))
say("fits outside the walk-forward whose summaries differ, second run against here", "%d of %d%s", length(cold_moved), nrow(ws$other),
    if (length(cold_moved)) paste0(": ", paste(cold_moved, collapse = "; ")) else "")
rs <- rd("run_identity_seeding.rds")
say("fixed-cutoff fits before against after the seeding: components that differ; of them posterior draws; other numbers compared", "%d; %d; %d", nrow(rs$differing),
    sum(grepl("[$]samples[$](loc|sigma)$", rs$differing$component)), sum(rs$files$numbers) - sum(rs$differing$values))

cat("\n== wf_compare.rds: [[cell]]$cmp (production: ls_margin, ls_total, ll_wdl; v2: lsm_raw, lst_raw, ll_wdl_raw), $market\n")
wc <- rd("wf_compare.rds")
pool <- function(cl) do.call(rbind, lapply(cl, function(k) wc[[k]]$cmp[, c("lsm_raw", "ls_margin", "lst_raw", "ls_total", "ll_wdl_raw", "ll_wdl", "mean_m.prod", "mean_m.v2", "margin")]))
for (sp in list(c("hb_m", "hb_f"), c("bb_m", "bb_f"))) {
  j <- pool(sp)
  say(
    paste(paste(sp, collapse = " + "), "v2 - production: n; margin / total / result"), "%d; %s / %s / %s", nrow(j), dif(j, "lsm_raw", "ls_margin"),
    dif(j, "lst_raw", "ls_total"), dif(j, "ll_wdl_raw", "ll_wdl")
  )
  say("  mean expected margin production / v2 / observed", "%.2f / %.2f / %.2f", mean(j$mean_m.prod), mean(j$mean_m.v2), mean(j$margin))
}
say("bb_f v2 - production, result, stored forecasts", "%s (n %d)", dif(wc$bb_f$cmp, "ll_wdl_raw", "ll_wdl"), nrow(wc$bb_f$cmp))
m <- wc$hb_m$market
say("hb_m Lengjan: n; market / v2 1X2 log likelihood", "%d; %.4f / %.4f", nrow(m), mean(m$ll_market), mean(m$ll_v2))
age <- do.call(rbind, lapply(cells, function(k) wc[[k]]$cmp[, c("lead_days", "lead_days_v2")]))
say("forecast age: v2 older / production older / same, of n", "%d / %d / %d of %d", sum(age$lead_days < age$lead_days_v2), sum(age$lead_days > age$lead_days_v2),
    sum(age$lead_days == age$lead_days_v2), nrow(age))

cat("\n== ../kki/periods_matched.rds: one row per stored basketball game (ok, overtime, sex, division)\n")
per <- readRDS(file.path(OUT, "kki", "periods_matched.rds"))
say("stored games; with a usable regulation score", "%d; %d (%.1f %%)", nrow(per), sum(per$ok), 100 * mean(per$ok))
for (k in split(per[per$ok, ], paste(per$sex[per$ok], per$division[per$ok]))) {
  say(paste(k$sex[1], k$division[1], "overtime games / games (share)"), "%d / %d (%.1f %%)", sum(k$overtime %in% TRUE), nrow(k), 100 * mean(k$overtime %in% TRUE))
}

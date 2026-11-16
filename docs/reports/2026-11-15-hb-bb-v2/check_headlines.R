# Recompute every headline number of the report straight from the result files, using
# only the file and the fields the report's appendix names for it. Nothing here goes
# through the report builder, so it is an independent read of the same files.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/check_headlines.R")'
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
rd <- function(f) readRDS(file.path(OUT, "out", f))
se <- function(v) stats::sd(v) / sqrt(length(v))
say <- function(label, ...) cat(sprintf("%-58s %s\n", label, sprintf(...)))
cells <- c("hb_m", "hb_f", "bb_m", "bb_f")

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
  say(paste(cl, "final-result log likelihood (regulation + OT)"), "%.4f", mean(x$ll_final_shift_div))
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
  say(paste(cl, "regulation + OT - final-score model: result / margin"), "%s / %s", dif(j, "ll_final_shift_div", "ll_wdl_raw.final"), dif(j, "lsm_final_shift_div", "lsm_raw.final"))
}

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

cat("\n== ../kki/periods_matched.rds: one row per stored basketball game (ok, overtime, sex, division)\n")
per <- readRDS(file.path(OUT, "kki", "periods_matched.rds"))
say("stored games; with a usable regulation score", "%d; %d (%.1f %%)", nrow(per), sum(per$ok), 100 * mean(per$ok))
for (k in split(per[per$ok, ], paste(per$sex[per$ok], per$division[per$ok]))) {
  say(paste(k$sex[1], k$division[1], "overtime games / games (share)"), "%d / %d (%.1f %%)", sum(k$overtime %in% TRUE), nrow(k), 100 * mean(k$overtime %in% TRUE))
}

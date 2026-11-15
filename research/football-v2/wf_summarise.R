# Summarise the walk-forward: scores per variant, paired differences against
# a reference by sex, season and season phase, and the 2026 benchmarks
# (production's stored forecasts, the Lengjan line).
#
#   Rscript research/football-v2/wf_summarise.R [ref] [variant ...]
# Run from the repo root.
options(width = 200)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args) >= 1L) args[[1]] else "base"
only <- if (length(args) >= 2L) c(ref, args[-1]) else NULL
out <- fv2_out_dir()

wf <- fv2_read_wf(out, only)
pred <- wf$pred
variants <- unique(pred$variant)
complete <- names(which(table(unique(pred[c("variant", "sex", "season")])$variant) == 10L))
cat("variants found:", paste(variants, collapse = ", "), "\ncomplete (10 sex-seasons):", paste(complete, collapse = ", "), "\n")
pred <- pred[pred$variant %in% complete, , drop = FALSE]
pred$all <- "all"

show <- function(x, digits = 4) print(format(x, digits = digits), row.names = FALSE)
cat("\n== scores by variant\n")
scores <- fv2_score_table(pred, "variant")
show(scores)
cat("\n== scores by variant and sex\n")
scores_sex <- fv2_score_table(pred, c("variant", "sex"))
show(scores_sex)

cmp <- function(metric) {
  rbind(
    cbind(split = "all", fv2_compare(pred, ref, NULL, metric)),
    cbind(split = "sex", fv2_compare(pred, ref, "sex", metric)),
    cbind(split = "season", fv2_compare(pred, ref, "season", metric)),
    cbind(split = "phase", fv2_compare(pred, ref, "phase", metric)),
    cbind(split = "sex x phase", fv2_compare(pred, ref, c("sex", "phase"), metric))
  )
}
paired <- NULL
if (length(setdiff(complete, ref))) {
  paired <- rbind(cmp("ls_1x2"), cmp("ls_score"))
  cat(sprintf("\n== paired differences against '%s' (positive = better than %s)\n", ref, ref))
  show(paired[paired$split %in% c("all", "sex", "phase"), ])
}

cat("\n== reference by sex, season and phase\n")
base <- pred[pred$variant == ref, , drop = FALSE]
by_season <- fv2_score_table(base, c("sex", "season"))
by_phase <- fv2_score_table(base, c("sex", "phase"))
show(by_season)
show(by_phase)

coverage <- stats::aggregate(
  cbind(n_pred, skipped) ~ sex + season,
  unique(wf$hyper[wf$hyper$variant == ref, c("sex", "season", "cutoff", "n_pred", "skipped")]), sum
)
cat("\n== fixtures predicted and skipped (a team with no training match)\n")
show(coverage)

priced <- fv2_priced_matches()
rounds <- rbind(fv2_round_history(fv2_results("male"), "male"), fv2_round_history(fv2_results("female"), "female"))
bench <- function(ext, sources, label) {
  rows <- list()
  detail <- list()
  for (v in complete) {
    d <- fv2_benchmark(pred, ext, sources, v)
    detail[[v]] <- d
    for (g in c("all", "male", "female")) {
      k <- if (g == "all") rep(TRUE, nrow(d)) else d$sex == g
      for (s in sources) {
        p <- fv2_paired((d$ls_model - d[[paste0("ls_", s)]])[k])
        rows[[length(rows) + 1L]] <- data.frame(
          set = label, variant = v, group = g, against = s, n = p$n,
          logloss_model = -mean(d$ls_model[k]), logloss_other = -mean(d[[paste0("ls_", s)]][k]),
          diff = p$mean, se = p$se, lo = p$lo, hi = p$hi,
          se_week = fv2_paired((d$ls_model - d[[paste0("ls_", s)]])[k], paste(d$sex, d$cutoff)[k])$se,
          phome_model = mean(d$p_home[k]), phome_other = mean(d[[paste0("phome_", s)]][k]),
          obs_home = mean((d$home_score > d$away_score)[k])
        )
      }
    }
  }
  list(table = do.call(rbind, rows), detail = detail)
}
b_priced <- bench(priced, c("line", "linepow", "prod"), "priced 2026")
b_rounds <- bench(rounds, "prodround", "pre-round 2026")
cat("\n== 2026 priced matches: model against the Lengjan line (proportional, power) and production's last stored forecast\n")
cat("   (diff > 0: the model has the better log score)\n")
show(b_priced$table)
cat("\n== 2026 published divisions: model against production's stored pre-round forecast\n")
show(b_rounds$table)

d <- b_priced$detail[[ref]]
pl <- fv2_paired(d$ls_prod - d$ls_line)
cat(sprintf(
  "\nproduction against the line on the same %d matches: %+.4f (se %.4f); overround mean %.3f\n",
  pl$n, pl$mean, pl$se, mean(priced$overround)
))

hyper <- wf$hyper[wf$hyper$variant %in% complete, ]
timing <- stats::aggregate(
  cbind(t_fit, t_samp) ~ variant + sex + reestimated,
  unique(hyper[c("variant", "sex", "season", "cutoff", "reestimated", "t_fit", "t_samp")]), mean
)

saveRDS(
  list(
    ref = ref, scores = scores, scores_sex = scores_sex, paired = paired,
    by_season = by_season, by_phase = by_phase, coverage = coverage,
    priced = b_priced$table, rounds = b_rounds$table,
    priced_detail = b_priced$detail[[ref]], rounds_detail = b_rounds$detail[[ref]],
    prod_vs_line = pl, hyper = hyper, timing = timing,
    pred = pred[c(
      "sex", "variant", "season", "phase", "division", "match_date", "cutoff", "key",
      "home_score", "away_score", "ls_score", "ls_1x2", "rps", "p_home", "p_draw", "p_away"
    )]
  ),
  file.path(out, "wf_summary.rds")
)
cat("\nsaved", file.path(out, "wf_summary.rds"), "\n")

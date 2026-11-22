# Does the possession-informed variance of one match depend on other matches' predictions?
# It should not: the shape is fitted on the training seasons and applied match by match.
# Usage (repository root): Rscript docs/reports/2026-11-15-hb-bb-v2/evidence/check_possession_bound.R
for (e in parse("docs/reports/2026-11-15-hb-bb-v2/wf_score.R", keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("shapes"))) shapes <- eval(e[[3]])
}
f <- shapes$possession$f
par <- c(log(30), log(16))
one <- data.frame(mean_m = 0, mean_t = 50, sd_m = 2, sd_t = 3)
two <- rbind(one, data.frame(mean_m = 0, mean_t = 80, sd_m = 2, sd_t = 3))
alone <- f(par, one, NULL)$s_m
with_other <- f(par, two, NULL)$s_m[1]
cat(sprintf("first match's margin sd alone %.6f, beside a high-scoring match %.6f: %s\n", alone, with_other,
            if (isTRUE(all.equal(alone, with_other))) "the same" else "DIFFERENT"))

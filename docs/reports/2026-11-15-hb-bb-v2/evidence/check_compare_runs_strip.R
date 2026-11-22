# Does compare_runs.R's strip() keep what it does not mean to drop? Two results that differ
# only in an attribute (a class, a factor's levels) must still compare as different.
# Usage (repository root): Rscript docs/reports/2026-11-15-hb-bb-v2/evidence/check_compare_runs_strip.R
D <- "docs/reports/2026-11-15-hb-bb-v2"
not_results <- c("secs", "secs_m", "secs_t", "cpu", "machine")
for (e in parse(file.path(D, "compare_runs.R"), keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("strip"))) strip <- eval(e[[3]])
}
a <- list(fit = list(est = structure(1:3, class = "status_codes"), secs = 1), secs = 2)
b <- list(fit = list(est = 1:3, secs = 9), secs = 3)
c1 <- list(fit = list(div = factor(c("OD", "1D"), levels = c("OD", "1D"))))
c2 <- list(fit = list(div = factor(c("OD", "1D"), levels = c("1D", "OD"))))
cat(sprintf("class-only difference seen: %s\n", !identical(strip(a), strip(b))))
l1 <- list(fit = structure(list(x = 1), class = "foo"))
l2 <- list(fit = list(x = 1))
d1 <- list(fit = structure(data.frame(x = 1:2, secs = 1), status = 2L))
d2 <- list(fit = data.frame(x = 1:2, secs = 1))
cat(sprintf("class on a list seen: %s\n", !identical(strip(l1), strip(l2))))
cat(sprintf("attribute on a data frame seen: %s\n", !identical(strip(d1), strip(d2))))
cat(sprintf("factor-level-only difference seen: %s\n", !identical(strip(c1), strip(c2))))
cat(sprintf("timings dropped at depth: %s\n", identical(strip(list(fit = list(est = 1:3, secs = 1), secs = 2)), strip(list(fit = list(est = 1:3, secs = 5), secs = 7)))))

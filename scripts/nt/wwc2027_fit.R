#!/usr/bin/env Rscript
# Women's national-team fit for the 2027 Women's World Cup play-offs: v2 (INLA)
# on the martj42 women's results file, every played match before the cutoff
# (default 1 December 2026, the first leg of play-off round 2), trained on
# 90-minute scores where nt_extra_time_matches("women") lists them. One thread,
# R-INLA's internal optimisation off and seeded draws, so the fit repeats.
#
# Usage:
#   Rscript scripts/nt/wwc2027_fit.R <womens results.csv> <out.rds> [cutoff=2026-12-01] [draws=4000]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
cutoff <- as.Date(opt("cutoff", "2026-12-01"))
n_draws <- as.integer(opt("draws", "4000"))

results <- nt_read_results(args[[1]])
prep <- nt_prepare(results, cutoff, ninety = nt_extra_time_matches("women"))
fit <- nt_fit_inla(prep, n_draws = n_draws, threads = "1:1", seed = 1L, reproducible = TRUE)

saveRDS(
  list(
    fit = fit, cutoff = cutoff, n_corrected = prep$n_corrected,
    last_match = max(prep$matches$match_date), n_matches = nrow(prep$matches),
    source = args[[1]], md5 = unname(tools::md5sum(args[[1]]))
  ),
  args[[2]]
)
cat(sprintf(
  "cutoff %s: %d matches to %s, %d teams, %d corrected to 90 minutes; fit %.0f s, draws %.0f s\n",
  cutoff, nrow(prep$matches), max(prep$matches$match_date), length(prep$teams),
  prep$n_corrected, fit$seconds[["fit"]], fit$seconds[["draws"]]
))
print(fit$hyper)
print(fit$fixed)
top <- sort(colMeans(fit$draws$theta), decreasing = TRUE)
print(round(head(top, 25), 3))

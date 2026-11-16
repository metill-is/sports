# Sensitivity of the within-season AR(1) to its fixed weekly correlation, basketball margin
# model at the fixed cutoff: leave-one-out density and held-out margin log score.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/within_rho.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
rows <- list()
for (cell in c("bb_m", "bb_f")) {
  d <- readRDS(file.path(OUT, "out", sprintf("data_%s_2026-01-31.rds", cell)))
  pr <- v2_priors("basketball")
  y <- d$hold$home_score - d$hold$away_score
  for (rho in c(0.90, 0.95, 0.98)) {
    f <- fit_v2_inla(d$v2, "margin", pr, within = "ar1", rho_week = rho, int_strategy = "eb", cpo = TRUE)
    h <- f$hyper_sd
    rows[[length(rows) + 1L]] <- data.frame(
      cell = cell, rho_week = rho, loo = sum(log(f$cpo)),
      ls_margin = mean(log(v2_integer_pmf(y, f$pred$mean, sqrt(f$pred$sd^2 + f$sigma2), ties = FALSE))),
      sd_w = h$q50[h$parameter == "sd: w_t"], sd_u = h$q50[h$parameter == "sd: u_s"], secs = f$secs,
      mode_status = f$mode_status
    )
  }
}
tab <- do.call(rbind, rows)
print(tab, digits = 5, row.names = FALSE)
saveRDS(tab, file.path(OUT, "out", "within_rho.rds"))

#!/usr/bin/env Rscript
# scripts/0Nd_dashboard.R --
# Build the model-quality dashboard: compute every backtest diagnostic into the
# JSON contract under data/dashboard/, then (optionally) render the Quarto
# dashboard at docs/dashboard/experiment.qmd.
#
# Strictly READ-ONLY on the money path -- recomputes from committed Parquet
# (results / odds / predicted_matches extracts) and the REUSE-mode walk-forward
# (no Stan). Never writes the ledger; test-dashboard-ci-isolation.R enforces it.
#
# Usage:
#   Rscript scripts/0Nd_dashboard.R                 # export JSON + render
#   Rscript scripts/0Nd_dashboard.R --no-render     # export JSON only
#   Rscript scripts/0Nd_dashboard.R --season 2026

invisible(Sys.setlocale("LC_ALL", "is_IS.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
season <- {
  i <- which(args == "--season")
  if (length(i) == 1L && length(args) >= i + 1L) as.integer(args[i + 1L]) else 2026L
}
do_render <- !("--no-render" %in% args)

cli::cli_h1("Model-quality dashboard")
cli::cli_alert_info("Computing diagnostics (REUSE walk-forward, both sexes) ...")
contract <- dashboard_export(season = season)
cli::cli_alert_success(
  "Wrote {length(contract)} JSON tables to {.path data/dashboard/} (n_devig = {contract$meta$n_devig})."
)

qmd <- here::here("docs", "dashboard", "experiment.qmd")
if (do_render && file.exists(qmd)) {
  cli::cli_alert_info("Rendering {.path docs/dashboard/experiment.qmd} ...")
  quarto::quarto_render(qmd, quiet = TRUE)
  cli::cli_alert_success("Rendered docs/dashboard/experiment.html")
} else if (do_render) {
  cli::cli_alert_warning("No experiment.qmd yet -- exported JSON only.")
}

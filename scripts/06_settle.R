#!/usr/bin/env Rscript
# scripts/06_settle.R --
# Settle resolvable rows in the canonical Parquet ledger by joining
# unsettled bets against data/facts/results/ and filling
# settled / win / pnl. Already-settled rows are never touched (L4).
#
# Usage:
#   Rscript scripts/06_settle.R

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

cli::cli_h1("Settle ledger")
n <- settle_ledger(root = here::here("data"))
if (n == 0L) {
  cli::cli_alert_info("No newly resolvable bets.")
} else {
  cli::cli_alert_success("Newly settled: {n} bet(s).")
}

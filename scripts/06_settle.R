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

  # Settle is the second writer to data/decisions/ledger/ (placer is the
  # first). Same L1/L4 invariants apply: the canonical record of real-world
  # state must never sit uncommitted -- a later git reset would silently
  # destroy the settlement flips. See commit 121710d.
  cli::cli_h2("Committing ledger to git")
  commit_msg <- sprintf(
    "data(ledger): settle run %s -- %d bet(s) settled",
    format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
    n
  )
  res <- commit_ledger_changes(here::here(), commit_msg)
  switch(res$status,
    committed = cli::cli_alert_success("Ledger committed: {res$message}"),
    nothing_to_commit = cli::cli_alert_info(
      "No ledger changes to commit (already clean)."
    ),
    failed = {
      cli::cli_alert_danger("Ledger commit failed.")
      cli::cli_alert_danger("git output:")
      writeLines(res$output, stderr())
      cli::cli_alert_danger(paste(
        "Settlement landed but the ledger is NOT committed.",
        "Commit data/decisions/ledger/ MANUALLY before doing anything else."
      ))
      quit(save = "no", status = 1L)
    }
  )
}

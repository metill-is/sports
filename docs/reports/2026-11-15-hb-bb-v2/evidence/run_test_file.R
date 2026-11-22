# Run one test file with R-INLA required, and print one row per test and a total.
# Usage (repository root, through the machine limiter):
# Sports sessions share one heavy slot: start it only when `slot.sh status` shows no sports-* owner.
#   LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 slot.sh run sports-hbbb -- \
#     Rscript docs/reports/2026-11-15-hb-bb-v2/evidence/run_test_file.R tests/testthat/<file>.R
options(width = 200)
args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(SPORTS_REQUIRE_INLA = "1")
cat(sprintf("HEAD %s (working tree %s)\n", system("git rev-parse --short HEAD", intern = TRUE),
            if (length(system("git status --short -- R tests", intern = TRUE))) "changed" else "clean"))
devtools::load_all(".", quiet = TRUE)
r <- as.data.frame(testthat::test_file(args[[1]], reporter = "check", stop_on_failure = FALSE))
print(r[, c("test", "nb", "failed", "skipped", "error", "warning")], row.names = FALSE)
cat(sprintf("TOTAL expectations %d | failed %d | errors %d | skipped %d\n", sum(r$nb), sum(r$failed), sum(r$error), sum(r$skipped)))

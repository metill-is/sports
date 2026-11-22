#!/bin/bash
# The package's full test suite on this worktree, with R-INLA required (UTF-8, as CLAUDE.md
# prescribes). Prints the head, one count line and the failing tests.
# Usage, through the machine limiter:
# Sports sessions share one heavy slot: start it only when `slot.sh status` shows no sports-* owner.
#   slot.sh run sports-hbbb -- bash docs/reports/2026-11-15-hb-bb-v2/evidence/run_full_suite.sh
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" || exit 1
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 SPORTS_REQUIRE_INLA=1 OMP_NUM_THREADS=1
echo "HEAD $(git rev-parse --short HEAD) $(date '+%Y-%m-%d %H:%M')$(git diff --quiet HEAD -- R tests || echo ' (R or tests changed in the working tree)')"
Rscript -e 'r <- as.data.frame(devtools::test(reporter = "summary", stop_on_failure = FALSE)); cat(sprintf("\nFILES %d | TESTS %d | PASSED %d | FAILED %d | ERRORS %d | SKIPPED %d | WARNINGS %d\n", length(unique(r$file)), nrow(r), sum(r$passed), sum(r$failed), sum(r$error), sum(r$skipped), sum(r$warning))); bad <- r[r$failed > 0 | r$error, c("file", "test")]; if (nrow(bad)) print(bad, row.names = FALSE)'
echo "DONE $(date '+%Y-%m-%d %H:%M')"

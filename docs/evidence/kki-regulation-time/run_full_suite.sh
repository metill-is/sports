#!/bin/bash
# The package's full test suite on this worktree (UTF-8, as CLAUDE.md prescribes).
# Usage, through the one heavy sports slot: bash docs/evidence/kki-regulation-time/run_full_suite.sh
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" || exit 1
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 OMP_NUM_THREADS=1
echo "HEAD $(git rev-parse --short HEAD) $(date '+%Y-%m-%d %H:%M')"
Rscript -e 'r <- as.data.frame(devtools::test(reporter = "summary", stop_on_failure = FALSE)); cat(sprintf("\nFILES %d | TESTS %d | PASSED %d | FAILED %d | ERRORS %d | SKIPPED %d | WARNINGS %d\n", length(unique(r$file)), nrow(r), sum(r$passed), sum(r$failed), sum(r$error), sum(r$skipped), sum(r$warning))); bad <- r[r$failed > 0 | r$error, c("file", "test")]; if (nrow(bad)) print(bad, row.names = FALSE)'
echo "DONE $(date '+%Y-%m-%d %H:%M')"

#!/bin/bash
# The five test files the review of 22 November touched, one log each with a suffix.
# Usage (repository root, one heavy sports slot): bash run_review_tests.sh <suffix>
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" || exit 1
E=docs/reports/2026-11-15-hb-bb-v2/evidence
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 OMP_NUM_THREADS=1
for f in inla predict fit-rules; do
  Rscript "$E/run_test_file.R" "tests/testthat/test-v2-margin-total-$f.R" > "$E/review-$1-$f.log" 2>&1
  echo "$f: $(grep '^TOTAL' "$E/review-$1-$f.log")"
done
Rscript "$E/run_test_file.R" tests/testthat/test-ingest-kki-periods.R > "$E/review-$1-kki.log" 2>&1
echo "kki: $(grep '^TOTAL' "$E/review-$1-kki.log")"

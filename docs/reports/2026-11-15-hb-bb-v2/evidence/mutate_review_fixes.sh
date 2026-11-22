#!/bin/bash
# GREEN for the five test files the review of 22 November touched, then one mutation per
# test-gap test: each mutation is applied to the package code, the test it targets is run
# alone, and the file is put back (checked against a checksum taken before).
# Usage (repository root, one heavy sports slot): bash mutate_review_fixes.sh
cd "$(git -C "$(dirname "$0")" rev-parse --show-toplevel)" || exit 1
E=docs/reports/2026-11-15-hb-bb-v2/evidence
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 OMP_NUM_THREADS=1 SPORTS_REQUIRE_INLA=1
bash "$E/run_review_tests.sh" green
one() {  # one <log> <test file> <test description>
  Rscript -e 'a <- commandArgs(TRUE); devtools::load_all(".", quiet = TRUE); r <- as.data.frame(testthat::test_file(a[1], desc = a[2], reporter = "check", stop_on_failure = FALSE)); cat(sprintf("TOTAL expectations %d | failed %d | errors %d\n", sum(r$nb), sum(r$failed), sum(r$error)))' "$2" "$3" > "$1" 2>&1
}
mutate() {  # mutate <name> <file> <old> <new> <test file> <test description>
  cp "$2" "$E/.keep"; sum=$(shasum "$2" | cut -d" " -f1)
  python3 - "$2" "$3" "$4" <<'PY'
import sys; p, old, new = sys.argv[1:4]; s = open(p).read(); assert s.count(old) == 1, old
open(p, "w").write(s.replace(old, new))
PY
  one "$E/mutation-$1.log" "$5" "$6"
  cp "$E/.keep" "$2"; rm "$E/.keep"
  [ "$(shasum "$2" | cut -d" " -f1)" = "$sum" ] || { echo "NOT RESTORED: $2"; exit 1; }
  echo "mutation $1: $(grep '^TOTAL' "$E/mutation-$1.log")"
}
I=R/v2-margin-total-inla.R; TI=tests/testthat/test-v2-margin-total-inla.R
mutate draws-of-training-rows $I 'pred_rows <- idx[-seq_len(n_train)]' 'pred_rows <- idx[seq_len(v$N_pred)]' $TI "the posterior draws are of the prediction rows and of the observation sd"
mutate sampler-threads-from-fit $I 'seed = seed, num.threads = "1:1"' 'seed = seed, num.threads = num_threads' $TI "drawing after a fit on more than one thread does not warn"
mutate warm-start-dropped $I 'control.mode = if (is.null(theta_start)) list() else list(theta = theta_start, restart = TRUE),' 'control.mode = list(),' $TI "a warm start reaches R-INLA's mode search, and a cold fit starts without one"
mutate cpo-all-rows $I 'cpo = if (cpo) fit$cpo$cpo[idx[seq_len(n_train)]] else NULL,' 'cpo = if (cpo) fit$cpo$cpo[idx] else NULL,' $TI "leave-one-out densities come back for the training rows, and only for them"
mutate club-level-ignored $I '  if (club_level) {' '  if (TRUE) {' $TI "without the club level the fit has no club term and no club sd"
mutate rho-on-internal-scale $I 'f <- function(x) tanh(x / 2)' 'f <- identity' $TI "hyperparameters are reported on the sd and correlation scale"
mutate parser-not-bytewise R/ingest-kki-periods.R 'm <- regmatches(x, regexec(pattern, x, perl = TRUE, useBytes = TRUE))[[1]]' 'm <- regmatches(x, regexec(pattern, x, perl = TRUE))[[1]]' tests/testthat/test-ingest-kki-periods.R "team names in ISO-8859-1 bytes do not break the parse"
git diff --quiet HEAD -- R/ingest-kki-periods.R && echo "parser: unchanged from HEAD, as it should be"

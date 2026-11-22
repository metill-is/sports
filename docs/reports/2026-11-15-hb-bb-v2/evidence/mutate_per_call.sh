#!/bin/bash
# Three mutations of the per-call internal.opt in fit_v2_inla(); each is applied, the fitter's
# test file is run, and the fitter is put back. Run from the repository root, inside one
# limiter slot:  slot.sh run sports-hbbb -- bash docs/reports/2026-11-15-hb-bb-v2/evidence/mutate_per_call.sh
set -u
E=docs/reports/2026-11-15-hb-bb-v2/evidence
F=R/v2-margin-total-inla.R
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 OMP_NUM_THREADS=1
cp "$F" "$E/.fitter.keep"
run() { Rscript "$E/run_test_file.R" tests/testthat/test-v2-margin-total-inla.R > "$E/mutation-$1.log" 2>&1; echo "mutation $1: $(grep '^TOTAL' "$E/mutation-$1.log")"; cp "$E/.fitter.keep" "$F"; }
python3 - "$F" <<'PY'
import sys; p = sys.argv[1]; s = open(p).read()
old = "      if (reproducible) list(internal.opt = FALSE)\n"; assert s.count(old) == 1
open(p, "w").write(s.replace(old, "      NULL\n"))
PY
run A-option-not-passed
python3 - "$F" <<'PY'
import sys; p = sys.argv[1]; s = open(p).read()
old = "      if (reproducible) list(internal.opt = FALSE)\n"; assert s.count(old) == 1
open(p, "w").write(s.replace(old, "      list(internal.opt = FALSE)\n"))
PY
run B-passed-even-when-not-asked
python3 - "$F" <<'PY'
import sys; p = sys.argv[1]; s = open(p).read()
old = "  is_margin <- response == \"margin\"\n"; assert s.count(old) == 1
open(p, "w").write(s.replace(old, "  if (reproducible) INLA::inla.setOption(internal.opt = FALSE)\n" + old))
PY
run C-session-option-also-switched
rm "$E/.fitter.keep"
git diff --quiet HEAD -- "$F" && echo "fitter: unchanged from HEAD (unexpected)" || echo "fitter: restored to the working-tree fix"

#!/bin/bash
# Four mutations of the regulation-time rules; each is applied, its test file run, and the
# file restored (checked against a checksum). Usage (repository root): bash docs/evidence/kki-regulation-time/mutate.sh
set -u
E=docs/evidence/kki-regulation-time
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
mutate() {  # name file old new testfile
  cp "$2" "$E/.keep"; sum=$(shasum "$2" | cut -d" " -f1)
  python3 - "$2" "$3" "$4" <<'PY'
import sys; p, old, new = sys.argv[1:4]; s = open(p).read(); assert s.count(old) == 1, old
open(p, "w").write(s.replace(old, new))
PY
  Rscript -e 'a <- commandArgs(TRUE); devtools::load_all(".", quiet = TRUE); r <- as.data.frame(testthat::test_file(a[1], reporter = "silent", stop_on_failure = FALSE)); cat(sprintf("expectations %d | failed %d | errors %d\n", sum(r$nb), sum(r$failed), sum(r$error)))' "$5" > "$E/mutation-$1.log" 2>&1
  cp "$E/.keep" "$2"; rm "$E/.keep"
  [ "$(shasum "$2" | cut -d" " -f1)" = "$sum" ] || { echo "NOT RESTORED: $2"; exit 1; }
  echo "mutation $1: $(tail -1 "$E/mutation-$1.log")"
}
mutate no-carry-over R/storage.R 'carry_enrichment(bound[!dup, , drop = FALSE], bound[dup, , drop = FALSE], table, nat_key)' 'bound[!dup, , drop = FALSE]' tests/testthat/test-storage-upsert.R
mutate carry-over-ignores-final R/storage.R '    if (col %in% names(kept)) same_final <- same_final &' '    if (FALSE) same_final <- same_final &' tests/testthat/test-storage-upsert.R
mutate card-final-not-checked R/ingest-kki-regulation.R "    if (!isTRUE(p\$ok) || !identical(c(p\$home_final, p\$away_final), as.integer(c(results\$home_score[i], results\$away_score[i])))) {" "    if (!isTRUE(p\$ok)) {" tests/testthat/test-ingest-kki-regulation.R
mutate no-overtime-rule R/storage-validate.R '  no_ot_but_changed <- both & hr != ar & (hf != hr | af != ar)' '  no_ot_but_changed <- both & FALSE' tests/testthat/test-storage-validate.R

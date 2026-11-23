#!/bin/bash
# Mutations for the review's test-gap tests (22 November 2026, PR #111): each is applied,
# its test file run, and the file restored (checked against a checksum).
# Usage (repository root): bash docs/evidence/kki-regulation-time/mutate_review.sh
set -u
E=docs/evidence/kki-regulation-time
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
mutate() {  # name file old new testfile
  cp "$2" "$E/.keep"; sum=$(shasum "$2" | cut -d" " -f1)
  python3 - "$2" "$3" "$4" <<'PY'
import sys; p, old, new = sys.argv[1:4]; s = open(p).read(); assert s.count(old) == 1, old
open(p, "w").write(s.replace(old, new))
PY
  Rscript -e 'a <- commandArgs(TRUE); devtools::load_all(".", quiet = TRUE); r <- suppressMessages(as.data.frame(testthat::test_file(a[1], reporter = "silent", stop_on_failure = FALSE))); cat(sprintf("expectations %d | failed %d | errors %d | failing: %s\n", sum(r$nb), sum(r$failed), sum(r$error), paste(r$test[r$failed > 0 | r$error], collapse = " / ")))' "$5" > "$E/mutation-review-$1.log" 2>&1
  cp "$E/.keep" "$2"; rm "$E/.keep"
  [ "$(shasum "$2" | cut -d" " -f1)" = "$sum" ] || { echo "NOT RESTORED: $2"; exit 1; }
  echo "mutation $1: $(tail -1 "$E/mutation-review-$1.log")"
}
T=tests/testthat/test-ingest-kki-regulation.R
mutate stored-copy-ignores-final R/ingest-kki-regulation.R '    same[same] <- results$home_score[same] == stored$home_score[at[same]] &' '    same[same] <- TRUE | results$home_score[same] == stored$home_score[at[same]] &' $T
mutate failures-never-reset R/ingest-kki-regulation.R '    in_a_row <- 0L
    p <- parse_baskethotel_game_periods(text)' '    p <- parse_baskethotel_game_periods(text)' $T
mutate wrapper-does-nothing R/ingest-kki-basketball.R '      kki_add_regulation_scores(results, sex, root = root)' '      results' $T

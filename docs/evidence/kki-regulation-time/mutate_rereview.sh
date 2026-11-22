#!/bin/bash
# Mutations for the re-review of #111 (22 November 2026): each is applied, its test file
# run, and the file restored (checked against a checksum).
# Usage (repository root): bash docs/evidence/kki-regulation-time/mutate_rereview.sh
set -u
E=docs/evidence/kki-regulation-time
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
mutate() {  # name file old new testfile
  cp "$2" "$E/.keep"; sum=$(shasum "$2" | cut -d" " -f1)
  python3 - "$2" "$3" "$4" <<'PY'
import sys; p, old, new = sys.argv[1:4]; s = open(p).read(); assert s.count(old) == 1, old
open(p, "w").write(s.replace(old, new))
PY
  Rscript -e 'a <- commandArgs(TRUE); devtools::load_all(".", quiet = TRUE); r <- suppressMessages(suppressWarnings(as.data.frame(testthat::test_file(a[1], reporter = "silent", stop_on_failure = FALSE)))); cat(sprintf("expectations %d | failed %d | errors %d | failing: %s\n", sum(r$nb), sum(r$failed), sum(r$error), paste(r$test[r$failed > 0 | r$error], collapse = " / ")))' "$5" > "$E/mutation-rereview-$1.log" 2>&1
  cp "$E/.keep" "$2"; rm "$E/.keep"
  [ "$(shasum "$2" | cut -d" " -f1)" = "$sum" ] || { echo "NOT RESTORED: $2"; exit 1; }
  echo "mutation $1: $(tail -1 "$E/mutation-rereview-$1.log")"
}
F=R/ingest-kki-regulation.R; T=tests/testthat/test-ingest-kki-regulation.R
mutate budget-checked-after-request $F '    if (as.numeric(difftime(clock(), started, units = "secs")) >= time_budget) {
      stopped <- sprintf("at the time budget of %s s", format(time_budget))
      break
    }
    i <- todo[k]
    if (k > 1L) pause()
    asked <- asked + 1L
    text <- tryCatch(fetch_card(results$game_id[i], season_id[i], sex), error = function(e) NULL)' '    i <- todo[k]
    if (k > 1L) pause()
    asked <- asked + 1L
    text <- tryCatch(fetch_card(results$game_id[i], season_id[i], sex), error = function(e) NULL)
    if (as.numeric(difftime(clock(), started, units = "secs")) >= time_budget) {
      stopped <- sprintf("at the time budget of %s s", format(time_budget))
      break
    }' $T
mutate no-rotation $F '  if (offset > 0L) todo <- c(todo[-seq_len(offset)], todo[seq_len(offset)])' '  invisible(offset)' $T
mutate retry-restored $F '    httr2::req_timeout(20L)
}' '    httr2::req_retry(max_tries = 2L, max_seconds = 10) |>
    httr2::req_timeout(20L)
}' $T

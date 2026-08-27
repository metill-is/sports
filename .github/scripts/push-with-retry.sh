#!/usr/bin/env bash
#
# Push the current commit to origin/main, surviving a concurrent sibling push.
#
# WHY THIS EXISTS
# ---------------
# Seven workflows in this repo commit generated data to main: fit,
# decide-publish, scrape-odds, scrape-results, healthcheck, discover-leagues
# and republish. Each declares its OWN `concurrency` group, which serialises a
# workflow against itself but does nothing across workflows -- so any two of
# them can be in flight at once, and several routinely are (on 2026-08-25 a
# stan fit pushed at 08:39, an odds scrape at 08:40 and a decide+publish at
# 08:44).
#
# The previous `git pull --rebase origin main && git push` had no retry, so it
# lost two ways:
#
#   1. Ref-lock race -- the rebase succeeds, then origin advances in the
#      milliseconds before the push:
#        ! [remote rejected] main -> main (cannot lock ref 'refs/heads/main':
#          is at d2f50f8 but expected 4f9776b)
#      That killed a ~2 h stan fit (run 32698702043, 2026-08-24).
#
#   2. Content conflict -- two writers regenerate the same files, and the
#      rebase stops with conflicts (97 of them in run 32827816691, 2026-08-25,
#      and the same signature on 2026-08-10 and 2026-08-18).
#
# Both failures discard the entire run's output even though the work itself
# was fine. Retrying the fetch/rebase/push cycle fixes (1) outright, and
# --prefer-ours fixes (2) for the workflows that fully regenerate their output.
#
# This mirrors the loop already proven in world-cup.yml, which hit the same
# race on 2026-06-30; it is factored out here so seven call sites cannot drift.
#
# USAGE
#   .github/scripts/push-with-retry.sh                 # plain rebase
#   .github/scripts/push-with-retry.sh --prefer-ours   # keep our side on conflict
#
# --prefer-ours passes `-X theirs` to `git rebase`. That reads backwards but is
# correct: rebase replays OUR commit onto the fetched head, so during the
# replay "theirs" IS our side. Only pass it where every path the job writes is
# fully regenerated from the freshest inputs, making our output authoritative.
#
# CAVEAT for --prefer-ours: a file that ACCUMULATES rather than being recomputed
# (data/beliefs/round_predictions_history/**/round_predictions_history.json is
# an accumulator, as is the WC prediction_log.json) can lose a sibling run's
# appended record for the one round where the two overlapped. That is a strictly
# smaller loss than the old behaviour, which threw away the whole run, and the
# next run re-accumulates. Weigh it before adding the flag to a new caller.

set -euo pipefail

PREFER_OURS=0
case "${1:-}" in
  --prefer-ours) PREFER_OURS=1 ;;
  "") ;;
  *) echo "::error::push-with-retry.sh: unknown argument '$1'"; exit 2 ;;
esac

ATTEMPTS=5

for attempt in $(seq 1 "$ATTEMPTS"); do
  git fetch origin main

  if [ "$PREFER_OURS" -eq 1 ]; then
    rebase_ok=0
    git rebase -X theirs FETCH_HEAD && rebase_ok=1 || true
  else
    rebase_ok=0
    git rebase FETCH_HEAD && rebase_ok=1 || true
  fi

  if [ "$rebase_ok" -ne 1 ]; then
    # A conflict here is not a race we can retry our way out of: either two
    # writers genuinely disagree about a path, or the job left unstaged changes
    # (see test-wc-workflow-staging.R for that failure mode). Surface it rather
    # than looping on a broken tree.
    echo "::error::Rebase onto origin/main failed on attempt ${attempt}. Working tree state:"
    git status --porcelain || true
    git rebase --abort || true
    exit 1
  fi

  if git push; then
    echo "Pushed to origin/main on attempt ${attempt}."
    exit 0
  fi

  echo "::warning::Push rejected (origin advanced); retrying (attempt ${attempt}/${ATTEMPTS})."
  sleep $((attempt * 3))
done

echo "::error::Could not push to origin/main after ${ATTEMPTS} attempts."
exit 1

#!/usr/bin/env bash
# SessionStart health banner.
#
# Prints a one-line pipeline-health status from the committed snapshot so a
# maintainer returning after time away sees the state immediately. Read-only,
# fast (no R load — just a jq read of data/health/status.json), and never fails
# the session. Refresh the underlying snapshot with /pipeline-doctor.
set -uo pipefail

root="${CLAUDE_PROJECT_DIR:-.}"
status="${root}/data/health/status.json"

[ -f "$status" ] || exit 0
command -v jq >/dev/null 2>&1 || exit 0

overall=$(jq -r '.overall // "?"' "$status" 2>/dev/null || echo "?")
nfail=$(jq -r '.n_fail // 0' "$status" 2>/dev/null || echo 0)
nwarn=$(jq -r '.n_warn // 0' "$status" 2>/dev/null || echo 0)
gen=$(jq -r '.generated_at // "?"' "$status" 2>/dev/null || echo "?")

if [ "$overall" != "OK" ] && [ "$overall" != "?" ]; then
  echo "Pipeline health: ${overall} (${nfail} fail, ${nwarn} warn) as of ${gen}. Run /pipeline-doctor for detail."
else
  echo "Pipeline health: ${overall} as of ${gen}."
fi
exit 0

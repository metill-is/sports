#!/usr/bin/env bash
# scripts/wc/refresh_now.sh --
# Manual on-demand World Cup refresh for when martj42 lags. Inject known scores
# via data/wc/manual_results.csv, then re-fit, re-forecast, publish to
# metill-is/sports, and trigger the metill-platform pull. Runs from anywhere.
#
# Usage:
#   scripts/wc/refresh_now.sh --list-missing  # show fixtures to fill, then edit the CSV
#   scripts/wc/refresh_now.sh                 # full run; pauses to confirm before publish
#   scripts/wc/refresh_now.sh --yes           # full run; no confirm prompt
#   scripts/wc/refresh_now.sh --no-push       # run + preview only (no commit/push/trigger)
#   scripts/wc/refresh_now.sh --no-pull       # skip the pre-run git pull --rebase
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$REPO"

LIST_MISSING=0; ASSUME_YES=0; DO_PUSH=1; DO_PULL=1
for arg in "$@"; do
  case "$arg" in
    --list-missing) LIST_MISSING=1 ;;
    --yes)          ASSUME_YES=1 ;;
    --no-push)      DO_PUSH=0 ;;
    --no-pull)      DO_PULL=0 ;;
    *) echo "unknown flag: $arg" >&2; exit 2 ;;
  esac
done

if [[ "$LIST_MISSING" -eq 1 ]]; then
  exec Rscript scripts/wc/list_missing.R
fi

if [[ "$DO_PULL" -eq 1 ]]; then
  echo "==> git pull --rebase origin main"
  git pull --rebase origin main
fi

echo "==> ingest (martj42 + manual overlay)"
Rscript scripts/wc/ingest.R
echo "==> fit (Stan, ~46 min)"
Rscript scripts/wc/fit.R
echo "==> forecast + publish JSON"
Rscript scripts/wc/forecast.R

echo
echo "Preview: open $REPO/data/wc/forecast.html and check the champion table above."

if [[ "$DO_PUSH" -eq 0 ]]; then
  echo "--no-push: stopping before commit. JSON is in data/publish/world_cup/karla/."
  exit 0
fi

if [[ "$ASSUME_YES" -ne 1 ]]; then
  read -r -p "Publish to metill-is/sports and trigger the platform pull? [y/N] " reply
  case "$reply" in
    [yY]|[yY][eE][sS]) ;;
    *) echo "Aborted; nothing pushed."; exit 0 ;;
  esac
fi

echo "==> commit + push"
git add \
  data/publish/world_cup \
  data/facts/results/sport=football/country=world \
  data/facts/schedules/sport=football/country=world \
  data/wc/manual_results.csv
if git diff --cached --quiet; then
  echo "No changes to publish (forecast output identical). Nothing pushed."
  exit 0
fi
git commit -m "data(wc): manual refresh $(date -u +%Y-%m-%dT%H:%MZ) — martj42 lag"
if [[ "$DO_PULL" -eq 1 ]]; then
  git pull --rebase origin main
fi
git push

echo "==> trigger metill-platform pull"
gh workflow run pull-sports-data.yml -R metill-is/metill-platform

echo
echo "Done. Watch the platform pull + deploy:"
echo "  gh run list -R metill-is/metill-platform --workflow=pull-sports-data.yml"

#!/bin/bash
# Run descriptives.R several times in separate R processes, each into its own output
# folder, so repro_descriptives.R can measure how far R-INLA's plug-in fits move between
# runs on identical inputs. Run from the repository root; single core, about a minute a run.
# Usage: docs/reports/2026-11-15-hb-bb-v2/repro_descriptives.sh [number of runs, default 4] [defaults]
#   The report's six runs were made with R-INLA's defaults: four by this script and two
#   earlier runs of descriptives.R filed beside them by hand. Pass "defaults" to fit that
#   way again; without it the fitter switches the online optimisation off and the runs agree.
set -u
OUT="${HBBB_OUT:-/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2}"
N="${1:-4}"
MODE="${2:-}"
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
for i in $(seq 1 "$N"); do
  mkdir -p "$OUT/repro/run_$i/out"
  HBBB_OUT="$OUT/repro/run_$i" Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/descriptives.R")' $MODE > "$OUT/repro/run_$i.log" 2>&1
  echo "run $i exit $?"
done

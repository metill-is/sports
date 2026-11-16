#!/bin/sh
# Run walk-forward jobs in parallel, detached-friendly.
#   sh research/football-v2/run_jobs.sh <workers> <variant> [<variant> ...]
# One job per (variant, sex, season); logs go next to the outputs.
# Run from the repo root.
workers="$1"; shift
log_dir="${FV2_LOGS:-/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/logs}"
mkdir -p "$log_dir"
export LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8
for variant in "$@"; do
  for season in 2026 2025 2024 2023 2022; do
    for sex in male female; do
      echo "$sex $season $variant"
    done
  done
done | xargs -P "$workers" -L 1 sh -c \
  'Rscript research/football-v2/run_wf.R "$0" "$1" "$2" 1:1 > "'"$log_dir"'/wf_$2_$0_$1.log" 2>&1'

#!/bin/bash
# Populate livesport-data from existing Sports/ data
# Run once to seed the repo with historical + current data.
#
# Usage: bash scripts/populate_from_sports.sh

set -euo pipefail

SPORTS_DIR="${HOME}/Metill/Sports"
DATA_DIR="$(cd "$(dirname "$0")/.." && pwd)/data"

echo "Populating from ${SPORTS_DIR} → ${DATA_DIR}"

# ── Football ──────────────────────────────────────────────────────────────────

for country in england italy spain norway; do
  src="${SPORTS_DIR}/football/${country}/data/male"
  if [ ! -d "$src" ]; then
    echo "SKIP football/${country}: no data dir"
    continue
  fi

  echo "Football ${country}..."
  # Find all league dirs (exclude hidden, aggregate files)
  for league_dir in "${src}"/*/; do
    [ -d "$league_dir" ] || continue
    league=$(basename "$league_dir")

    dst="${DATA_DIR}/soccer/${country}/male/${league}"
    mkdir -p "$dst"

    # Copy current season results (latest year dir)
    latest_year=$(ls -d "${league_dir}"/[0-9]* 2>/dev/null | sort -n | tail -1 || true)
    if [ -n "$latest_year" ] && [ -f "${latest_year}/results.csv" ]; then
      cp "${latest_year}/results.csv" "${dst}/results.csv"
      echo "  ${league}: results ($(basename "$latest_year"))"
    fi

    # Copy schedule
    if [ -f "${league_dir}/schedule.csv" ]; then
      cp "${league_dir}/schedule.csv" "${dst}/schedule.csv"
      echo "  ${league}: schedule"
    fi
  done
done

# ── Handball ──────────────────────────────────────────────────────────────────

HANDBALL_COUNTRIES="austria czech-republic denmark finland france germany hungary norway poland portugal spain sweden"

for country in $HANDBALL_COUNTRIES; do
  # Data lives at handball/{country}/data/ (may be symlinked from other/)
  src="${SPORTS_DIR}/handball/${country}/data"
  if [ ! -d "$src" ]; then
    echo "SKIP handball/${country}: no data dir"
    continue
  fi

  echo "Handball ${country}..."
  for sex_dir in "${src}"/*/; do
    [ -d "$sex_dir" ] || continue
    sex=$(basename "$sex_dir")
    # Skip non-sex dirs (e.g., last_updated.json)
    [[ "$sex" == "male" || "$sex" == "female" ]] || continue

    for div_dir in "${sex_dir}"/div*/; do
      [ -d "$div_dir" ] || continue
      div=$(basename "$div_dir")

      dst="${DATA_DIR}/handball/${country}/${sex}/${div}"
      mkdir -p "$dst"

      # Copy current season results (latest year)
      latest_year=$(ls -d "${div_dir}"/[0-9]* 2>/dev/null | sort -n | tail -1 || true)
      if [ -n "$latest_year" ] && [ -f "${latest_year}/results.csv" ]; then
        cp "${latest_year}/results.csv" "${dst}/results.csv"
        echo "  ${country}/${sex}/${div}: results ($(basename "$latest_year"))"
      fi

      # Copy schedule
      if [ -f "${div_dir}/schedule.csv" ]; then
        cp "${div_dir}/schedule.csv" "${dst}/schedule.csv"
        echo "  ${country}/${sex}/${div}: schedule"
      fi
    done
  done
done

echo ""
echo "Done. Files populated:"
find "$DATA_DIR" -name "*.csv" | wc -l
echo "CSV files total"

#!/bin/bash
# Prototype, step 2 of the period-score ingest: fetch Baskethotel's game-view widget
# (widget id 400) once per game id in <OUT>/kki/games.tsv and keep the raw response.
# One request per game, 0.35 s apart, resumable (existing files are skipped).
# Usage: kki_fetch_periods.sh <OUT>
OUT=${1:-/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2}
API=a0d07178160bf749eb6e5e761fc623fe42e2bb57
mkdir -p "$OUT/kki/raw"
n=0
tail -n +2 "$OUT/kki/games.tsv" | cut -f1,2,5 | while IFS=$'\t' read -r gid sex sid; do
  f="$OUT/kki/raw/$gid.js"
  if [ -s "$f" ]; then continue; fi
  if [ "$sex" = "male" ]; then lid=190; else lid=189; fi
  curl -s -g -m 40 --retry 2 -A "sports-pipeline-research" -o "$f" \
    "https://widgets.baskethotel.com/widget-service/show?api=$API&lang=is&nnav=1&nav_object=0&flash=0&request[0][container]=view4&request[0][widget]=400&request[0][param][game_id]=$gid&request[0][param][season_id]=$sid&request[0][param][league_id]=$lid"
  n=$((n + 1))
  if [ $((n % 100)) -eq 0 ]; then echo "$(date '+%H:%M:%S') fetched $n"; fi
  sleep 0.35
done
echo "DONE $(date '+%H:%M:%S'): $(ls "$OUT/kki/raw" | wc -l) files"

#!/usr/bin/env bash
# PreCompact hook: outputs key context to help compaction preserve important details.
# This output is injected before the summariser runs, so critical info survives.

cat <<'EOF'
=== Sports Workspace Context (preserve across compaction) ===
- Repo: /Users/brynjolfurjonsson/sports/ (single git repo, monorepo as of Plan 6)
- _legacy/{sports,lengjan-odds,livesport-data,lengjan-bets}/ holds pre-cutover history
- CLAUDE.md at repo root is authoritative
- Memory: ~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md
- Obsidian vault: ~/Obsidian/Metill/
- Things 3 area: "Metill.is" (ID: 4WyyavEFjCPunRi9iD5tKe)
- R conventions: here::here(), theme_metill(), Icelandic locale via Sys.setlocale()

=== Sports Pipeline (preserve across compaction) ===
- Entry points: scripts/0N_*.R (one per layer; freshness predicates skip when nothing to do)
  00_active_competitions.R / 01_ingest_results.R / 02_scrape_odds.R /
  03_fit.R / 04_decide.R / 05_publish.R / 06_settle.R
- Common flags: --league KEY, --sex male|female, --force (bypass freshness guard)
- League registry: config/leagues.yml (Iceland active; non-Iceland paused for autumn restart)
- Skills: /bet (recommendations), /sports-update (full pipeline), /add-league (new league), /place-bets (placer), /sync-main (cron-collision sync), /wrap-up-session (end-of-session checklist)
- CI: .github/workflows/{scrape-results,scrape-odds,fit,decide-publish}.yml chained via workflow_run
- Rules: sports-betting.md (betting), git-hygiene.md (cron-collision sync), r-conventions.md, stan-conventions.md
EOF

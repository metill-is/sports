#!/usr/bin/env bash
# PreCompact hook: outputs key context to help compaction preserve important details.
# This output is injected before the summariser runs, so critical info survives.

cat <<'EOF'
=== Sports Workspace Context (preserve across compaction) ===
- Workspace: /Users/brynjolfurjonsson/sports/ (NOT a git repo itself)
- Sub-projects: Sports/, lengjan-odds/, livesport-data/, lengjan-bets/
- Each sub-project has its own CLAUDE.md — always read before making changes
- Memory: ~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md
- Obsidian vault: ~/Obsidian/Metill/
- Things 3 area: "Metill.is" (ID: 4WyyavEFjCPunRi9iD5tKe)
- All R code: box::use(), here::here(), theme_metill(), Icelandic locale

=== Sports Pipeline (preserve across compaction) ===
- Unified CLI: cd Sports && Rscript run.R {selector} --step {steps}
  Selectors: --sport, --country, --league, --all, --active, --stale, --due
  Steps: data, fit, results, bet (comma-separated)
  Overrides: --sex, --iter, --dry-run, --method (approximate inference disabled, use sample), --no-plots, --sync
- League registry: Sports/config/leagues.yml (18 leagues; 3 active Icelandic, 15 paused as of 2026-04-10)
- Betting config: Sports/{sport}/{country}/config/bets.yml
- Skills: /bet (run bets), /sports-update (full pipeline), /add-league (new league)
- Rules: sports-pipeline.md (pipeline arch), sports-betting.md (betting), sports-per-sport.md (data sources)
- Key dirs: R/pipeline/ (dispatchers), R/bets/ (betting modules), R/schedule/ (scan.R)
EOF

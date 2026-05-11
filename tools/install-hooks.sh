#!/usr/bin/env bash
# Point git at the tracked hooks under tools/git-hooks/.
# Run once per clone. Reversible with:  git config --unset core.hooksPath
#
# After running:
#   - every `git commit` in this repo fires the pre-commit hook
#   - the hook refuses to commit if data/decisions/ledger/ has unstaged
#     or untracked changes (see commit 121710d for the motivating
#     incident, .claude/rules/git-hygiene.md for the rule)

set -eu

cd "$(dirname "$0")/.."
git config core.hooksPath tools/git-hooks
chmod +x tools/git-hooks/*

current="$(git config --get core.hooksPath)"
echo "[install-hooks] core.hooksPath = $current"
echo "[install-hooks] Hooks active:"
for h in tools/git-hooks/*; do
  echo "  - $(basename "$h")"
done

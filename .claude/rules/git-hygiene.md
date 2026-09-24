# Git Hygiene (Sports Repo)

This repo runs hot — seven scheduled GitHub workflows commit to `main` throughout the day
(`scrape-results`, `scrape-odds`, `fit`, `decide-publish`, `healthcheck`,
`discover-leagues`, `republish`; `world-cup` is dispatch-only). A working session that takes hours typically sees ~10
upstream commits land while you work. The patterns below keep local state in
sync without losing anything.

## Highest-priority data: the ledger

`data/decisions/ledger/` is the **only** path in this repo where local writes
encode real-world state — each row means money was committed on Lengjan
(L1 invariant in [`sports-betting.md`](./sports-betting.md)). It is the
canonical record and cannot be reconstructed from anything else in the
codebase. The generic "data is ephemeral" rule below does **not** apply
here; the ledger always commits.

Three enforcement layers are active, covering different failure modes:

1. **Script-layer auto-commit** (`R/commit-ledger.R::commit_ledger_changes`).
   `scripts/place_bets.R`, `scripts/06_settle.R`, and `scripts/auto_place.R`
   all call it after a run (it is a path-restricted no-op when the ledger is
   clean). The commit is path-restricted
   (`git commit -- data/decisions/ledger/`) so unrelated working-tree WIP
   is never swept in. On failure the wrappers `quit(status = 1L)` with a
   loud warning rather than continuing.
   `tests/testthat/test-script-ledger-commit.R` enforces the call in all
   three wrappers (auto_place.R was missing it when it shipped — the
   2026-06-10 orphaned-rows incident).

2. **Pre-sync rescue** (`R/auto-place.R::sync_recs`). Before the unattended
   placer's stash → pull → pop dance, any uncommitted ledger rows (a run
   that died between the parquet write and its commit) are committed via
   `commit_ledger_changes()`. Money rows are never carried through a stash,
   and a dirty ledger can never wedge the 2-hourly sync. `sync_recs` also
   refuses to run off `main` or during an in-progress rebase/merge (so the
   agent never rewrites a checked-out feature branch or disturbs manual
   conflict resolution), pops only the stash entry it itself created (a
   pre-existing user stash — which may carry a stale ledger parquet — is
   left alone), and aborts the rebase a conflicted pull started. The
   script-layer commit in `scripts/auto_place.R` is likewise skipped on
   `disabled`/`locked` runs, so the kill switch keeps the agent fully inert
   during interactive ledger maintenance.

3. **Pre-commit hook** (`tools/git-hooks/pre-commit`). Refuses any commit
   while `data/decisions/ledger/` has unstaged or untracked changes. This
   catches the "next commit on something else silently leaves the ledger
   behind, a later reset destroys it" pattern. One-time activation per
   clone: `bash tools/install-hooks.sh`. **Check it is live:**
   `git config --get core.hooksPath` must print `tools/git-hooks`. On
   2026-09-24 it read the absolute `.git/hooks` (no `pre-commit` there),
   which silently disabled the hook; the writer is unknown and
   `metill-platform`/`esbvaktin` carried the same value. The SessionStart
   health banner now warns when it drifts.

`place_bets()` and `settle_ledger()` themselves never touch git — only the
script wrappers and `sync_recs()` do. Library callers and the test suite are
unaffected.

Reference incident: commit 121710d (`data(ledger): restore 6 football
iceland bets lost in 2026-05-08 git reset`). The placer wrote the rows
correctly; the loss was at the git layer.

## Working-tree rule of thumb (everything except the ledger)

Anything else in `data/` is **either** committed by cron **or** locally
generated and short-lived (e.g. a manual decide run's new
`run_date=YYYY-MM-DD/` partition). Treat that data state as ephemeral:
either commit it immediately or accept that it will collide with cron and
need stashing during the next sync.

Source code, config, tests, docs are the opposite — never leave them as
untracked WIP across sessions; commit to a branch even if you're not pushing.

## The cron-collision sync pattern

When `git pull --rebase` reports `error: untracked working tree files would be
overwritten by checkout` or refuses because of unstaged changes, do **not**
move data files aside manually. The clean pattern:

```bash
git stash push -u -m "<sensible message describing the WIP>"
git pull --rebase origin main
git stash pop
```

Three things this exploits:
1. `-u` stashes untracked files too, including the data dirs that block checkout.
2. `pull --rebase` fast-forwards through cron commits and detects any local
   commits whose patches are already on origin (e.g. a PR you previously merged
   under a different SHA). Those are silently skipped — local main becomes a
   clean superset of origin/main.
3. `stash pop` reapplies your WIP. If a stashed untracked file's path now has a
   tracked file (because cron committed something there), the pop **keeps the
   stash** rather than discarding the data. Your working tree gets origin/main's
   canonical version; your stash entry remains for inspection.

When pop conflicts, decide per file:
- **Text file conflict** → resolve markers manually, `git add`, then drop or
  keep the stash.
- **Binary file conflict** (parquet, etc.) → `git checkout --ours <path>` to
  keep your working tree's version, `--theirs` to take the stash's. Then
  `git add` to mark resolved.

## Stash discipline

After every sync, `git stash list`. A stash that wasn't auto-dropped means a
conflict happened — investigate and either resolve or drop. Stashes silently
accumulate over weeks if ignored; today the repo had 6.

A stash's content typically degrades over time as the surrounding code on main
evolves: features it once carried get reimplemented and shipped through other
PRs, and what's left is just stylistic deltas. Before dropping a long-lived
stash, `git stash show -p stash@{N}` and grep its file list — if every text
delta has an equivalent on main (search by symbol or roxygen tag), the stash
is subsumed and safe to drop. Binary parquets older than the most recent cron
commit at the same path are always stale.

## Before pushing to main

The seven scheduled CI workflows auto-commit to `main` constantly (metill-platform's
`pull-sports-data` only *reads* this repo — it commits to its own), so `main`
almost always moves under you between sessions. A
plain `git push` will be rejected as non-fast-forward (or, worse, you'll race a
cron commit). Always re-sync first:

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports rebase origin/main
git -C /Users/brynjolfurjonsson/sports push
```

This is for the direct-push case only. The PR path ([`docs/runbooks/git-main-branch.md`](../../docs/runbooks/git-main-branch.md)) stays preferred, but
**not with `--auto`**: GitHub only enables auto-merge on a base branch whose
rules carry a requirement (checks or reviews), and `protect-main` deliberately
has none, so `gh pr merge --auto` fails with `Pull request Branch does not have
required protected branch rules` (observed on PR #78, 2026-09-05).

## Branch protection and PR vs direct push

`main` carries the `protect-main` ruleset (force-push and deletion only; never add `pull_request` or `required_status_checks` rules, which would halt the bots' direct pushes). Ruleset details, the escape hatch and the PR-vs-direct-push guidance: [`docs/runbooks/git-main-branch.md`](../../docs/runbooks/git-main-branch.md).

## When this rule is wrong

If you encounter a git friction that this rule doesn't cover, append a short
note to `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md`
under "Pipeline Gotchas", or run `/self-reflect` at session end to propose
updates (the `learner` agent was retired into that skill). Patterns that recur across sessions belong here.

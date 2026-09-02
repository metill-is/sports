# Git Hygiene (Sports Repo)

This repo runs hot — five GitHub workflows commit to `main` throughout the day
(`scrape-results`, `scrape-odds`, `fit`, `decide-publish`, plus chained
`workflow_run`s). A working session that takes hours typically sees ~10
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
   clone: `bash tools/install-hooks.sh`.

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

The five CI workflows auto-commit to `main` constantly (metill-platform's
`pull-sports-data` only *reads* this repo — it commits to its own), so `main`
almost always moves under you between sessions. A
plain `git push` will be rejected as non-fast-forward (or, worse, you'll race a
cron commit). Always re-sync first:

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin
git -C /Users/brynjolfurjonsson/sports rebase origin/main
git -C /Users/brynjolfurjonsson/sports push
```

This is for the direct-push case only. The PR path below stays preferred —
`gh pr merge --rebase --auto --delete-branch` still works because the repo now
has auto-merge enabled, so keep the `--auto` recommendation.

## Branch protection (`protect-main` ruleset)

As of 2026-06-22 `main` is guarded by a GitHub **repository ruleset** named
`protect-main` (ID `17981831`, enforcement `active`). It enforces exactly two
rules and nothing else:

| Rule | Effect |
|---|---|
| `non_fast_forward` | Rejects **force pushes** — no one can rewrite `main`'s history on origin. |
| `deletion` | Rejects **deletion** of the `main` branch. |

**Why only these two.** The ledger (`data/decisions/ledger/`) is the canonical,
irrecoverable record of real money committed on Lengjan, and this repo has a
prior git-layer data loss — commit `121710d` restored 6 football bets wiped by a
local `2026-05-08 git reset`. Force-push/deletion protection on origin is cheap
defence-in-depth: an accidental local `git push --force` can no longer propagate
a history rewrite to the canonical remote.

**Why nothing more — do NOT add `pull_request` or `required_status_checks`
rules.** All nine workflows (`scrape-results`, `scrape-odds`, `fit`,
`decide-publish`, `healthcheck`, `discover-leagues`, `republish`, `world-cup`,
plus metill-platform's cron) push **directly** to `main` as
`github-actions[bot]`, dozens of times a day. Requiring a PR or status checks
before merging would reject those direct pushes and **halt the entire
auto-commit pipeline**. The disjoint-path design (each workflow `git add`s only
its own paths, then `pull --rebase` → plain `git push`) is the coordination
mechanism — branch protection must not duplicate or block it.

**Operational impact: zero.** No workflow force-pushes — every one ends in
`git pull --rebase origin main && git push` (a fast-forward), and the local
launchd autoplace agent commits the ledger but never pushes. Fast-forward
pushes are unaffected by both rules. The sync pattern above is unchanged.

**Escape hatch — a legitimate history rewrite** (e.g. emergency ledger surgery
that can't be expressed as a forward commit): `current_user_can_bypass` is
`never` and `bypass_actors` is empty, so even the repo owner is blocked by
default. Temporarily disable, do the surgery, re-enable:

```bash
gh api repos/metill-is/sports/rulesets/17981831 -X PUT -f enforcement=disabled
# ... force-push / rewrite ...
gh api repos/metill-is/sports/rulesets/17981831 -X PUT -f enforcement=active
```

Inspect: `gh api repos/metill-is/sports/rulesets/17981831`. Web UI: repo
**Settings → Rules → Rulesets**. To recreate from scratch if ever deleted:

```bash
gh api repos/metill-is/sports/rulesets -X POST --input - <<'JSON'
{ "name": "protect-main", "target": "branch", "enforcement": "active",
  "conditions": { "ref_name": { "include": ["refs/heads/main"], "exclude": [] } },
  "rules": [ { "type": "non_fast_forward" }, { "type": "deletion" } ] }
JSON
```

## Bash-tool gotcha: cwd persists, prefer `git -C <abs-path>`

The Bash tool persists `cd` across calls within a session. A successful `cd
.claude/worktrees/foo` inside one call means subsequent calls run from
`/Users/.../sports/.claude/worktrees/foo`, not the original cwd. This silently
mis-routes git commands to a different worktree.

For any git operation that should target a specific repo or worktree, use
explicit paths:

```bash
git -C /Users/brynjolfurjonsson/sports status              # never just `git status`
git -C /Users/brynjolfurjonsson/sports diff path/to/file   # or with paths
```

`git -C` makes the working directory explicit and unaffected by prior `cd`s.

## PR vs direct push

This repo allows direct pushes to `main` (no required status checks — only the
force-push/deletion guards from the `protect-main` ruleset above, which
fast-forward pushes never trip). But the PR-and-auto-merge pattern is preferred
because:
- A push needs a clean local working tree (or stash dance) every time. A PR
  branch can be created without disturbing the working tree on main.
- `gh pr merge --rebase --auto --delete-branch` works even when no checks are
  required — auto-merge fires immediately if checks pass (or instantly if none
  exist), and the branch is cleaned up server-side.
- The PR view gives a reviewable diff before commit lands on main.

Use direct push only for trivial single-line fixes when the working tree is
already clean.

## When this rule is wrong

If you encounter a git friction that this rule doesn't cover, append a short
note to `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md`
under "Pipeline Gotchas", or run `/self-reflect` at session end to propose
updates (the `learner` agent was retired into that skill). Patterns that recur across sessions belong here.

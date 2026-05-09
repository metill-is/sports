---
name: wrap-up-session
description: Use at the end of a work session to verify local main is in sync with origin/main, no leaked stashes/branches/worktrees, and any session WIP is either committed or consciously preserved. Closes the loop on git hygiene.
---

# /wrap-up-session — End-of-session consolidation checklist

This repo accumulates clutter quickly because cron commits race against local
work. Run this skill before closing a session to reach a known-clean state.

## 1. Branch + worktree alignment

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin --prune
git -C /Users/brynjolfurjonsson/sports worktree list
git -C /Users/brynjolfurjonsson/sports branch -vv
```

Expected: one worktree (the main checkout), one local branch (`main`), tracking
`origin/main` with no `[ahead N]` or `[behind N]` markers (pure tracking
arrow). Anything else is residue.

If `main` is behind: invoke `/sync-main`.
If `main` is ahead: investigate. Commits ahead are usually duplicates of
something already on origin (will be skipped on next rebase as
"previously applied"), but verify with
`git log --oneline origin/main..HEAD` first.

## 2. Stash inventory

```bash
git -C /Users/brynjolfurjonsson/sports stash list
```

For each stash, decide:
- **Mine, transient (e.g. "WIP pre-sync 2026-XX-XX")**: drop after confirming
  pop succeeded earlier in the session.
- **Older, content already on main**: `git stash show --stat stash@{N}` then
  spot-check that each text-file delta has an equivalent on main (grep for
  symbols, roxygen tags). If subsumed, drop.
- **Older, contains unique unmerged work**: extract via PR (see
  `.claude/rules/git-hygiene.md` for the cherry-pick-from-stash pattern), then
  drop.

The goal is `git stash list` returns empty.

## 3. Local-only branches

```bash
git -C /Users/brynjolfurjonsson/sports branch | grep -v '^\* main$'
```

For each branch:
- If it has no unique commits vs main and no unique uncommitted work:
  `git branch -D <name>`.
- If it has unique commits worth keeping: PR them or accept that the branch is
  a permanent local archive. Note in MEMORY.md if the latter.
- If the branch has uncommitted WIP, `git stash` it on the branch first or
  commit a "WIP: <description>" snapshot before deleting.

## 4. Working-tree triage

```bash
git -C /Users/brynjolfurjonsson/sports status -s
```

Categorise each entry:
- **Source code WIP** (R/, scripts/, tests/, .claude/rules/, CLAUDE.md, etc.) →
  commit to a branch before ending the session, even if not pushed. Untracked
  source code that disappears in a `rm -rf .` is real risk.
- **Local pipeline data** (modified data/decisions/ledger/, untracked data/
  partitions for today's date) → safe to leave as long as you understand it
  will be stashed at next sync. Cron will refresh via its own commits.
- **Generated docs** (man/*.Rd) → if accompanying a source change, commit
  together; if orphaned, run `Rscript -e 'devtools::document()'` to confirm
  they're current and either commit or stash.

## 5. Open PRs

```bash
gh pr list --state open
```

For each open PR: either merge (if CI green) or note as deliberately pending.
Stale open PRs are noise.

## 6. Self-update check

If during the session you encountered a friction not covered by existing
rules/skills, propose an update before ending:

- Recurring git issues → append to `.claude/rules/git-hygiene.md`
- Recurring pipeline issues → update `CLAUDE.md` or `.claude/rules/sports-betting.md`
- Operational patterns → add a memory note at
  `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/` and link from
  `MEMORY.md`

For a deeper sweep across recent transcripts: invoke the `learner` agent
(`Agent(subagent_type: "learner", ...)`) to review session patterns and
propose CLAUDE.md / skill updates.

## Done state

After running through all six steps you should see:
- 1 worktree, 1 branch, both tracking origin/main exactly
- 0 stashes
- Working tree contains only categorised, conscious WIP
- 0 stale open PRs
- Any new patterns captured in rules / memory

## Reference

- Sync mechanics: `/sync-main`
- Git pattern catalogue: `.claude/rules/git-hygiene.md`
- Project memory index: `~/.claude/projects/-Users-brynjolfurjonsson-sports/memory/MEMORY.md`

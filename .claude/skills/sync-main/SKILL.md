---
name: sync-main
description: Use when local main has diverged from origin/main due to cron data commits, or when `git pull --rebase` is blocked by dirty working tree. Performs the stash → pull --rebase → pop pattern with conflict guidance.
---

# /sync-main — Bring local main in sync with origin/main

This repo's CI commits to `main` 5+ times a day (cron-driven `scrape-*`, `fit`,
`decide-publish` workflows). Local working trees drift quickly. This skill
runs the canonical sync pattern.

## Pre-flight

```bash
git -C /Users/brynjolfurjonsson/sports fetch origin --prune
git -C /Users/brynjolfurjonsson/sports status -s
git -C /Users/brynjolfurjonsson/sports log --oneline HEAD..origin/main | wc -l
```

If working tree is clean and you're behind: just `git pull --rebase origin main`
and stop. The rest of this skill is for the dirty case.

## Sync (dirty working tree)

```bash
git -C /Users/brynjolfurjonsson/sports stash push -u -m "sync-main on $(date +%F): WIP"
git -C /Users/brynjolfurjonsson/sports pull --rebase origin main
git -C /Users/brynjolfurjonsson/sports stash pop
```

Expected outcomes (in order of likelihood):

1. **Clean pop**: stash auto-drops, working tree matches pre-sync state. Done.
2. **Pop kept**: a stashed file's path now has a tracked file on origin/main
   (typical for `data/decisions/candidates/.../run_date=*/`,
   `data/facts/odds/.../scraped_date=*/`). The stash is preserved at
   `stash@{0}` for inspection. Working tree has origin/main's canonical
   version. Usually safe to `git stash drop stash@{0}` after confirming the
   stashed content was just stale local pipeline outputs.
3. **Conflict**: text or binary file conflict on a path you do care about.
   See `.claude/rules/git-hygiene.md` § "When pop conflicts".

## Verification

```bash
git -C /Users/brynjolfurjonsson/sports log --oneline HEAD..origin/main   # should be empty
git -C /Users/brynjolfurjonsson/sports log --oneline origin/main..HEAD   # should be empty
git -C /Users/brynjolfurjonsson/sports stash list                        # check what survived
```

If both `log` queries are empty and stash list looks expected, sync is done.

## When this is the wrong tool

- Working tree has uncommitted source code (not pipeline data) → commit to a
  branch first, then sync.
- You want to discard local commits in favour of origin/main → that's
  destructive; do not handle here. Confirm with the user before any
  `git reset --hard`.
- Sync is needed inside a worktree → `cd` to the main worktree first or use
  `git -C <main-worktree-path>`. The cwd-persistence gotcha applies.

## Reference

- Full pattern + edge cases: `.claude/rules/git-hygiene.md`
- Session wrap-up checklist: `/wrap-up-session`

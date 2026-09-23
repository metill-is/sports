# Runbook: `main` branch protection and PR vs direct push

Moved verbatim from `.claude/rules/git-hygiene.md` on 2026-09-23. "Above" below means that rule's
cron-collision sync pattern and its `--auto` note (section "Before pushing to main").

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

## PR vs direct push

This repo allows direct pushes to `main` (no required status checks — only the
force-push/deletion guards from the `protect-main` ruleset above, which
fast-forward pushes never trip). But the PR-and-auto-merge pattern is preferred
because:
- A push needs a clean local working tree (or stash dance) every time. A PR
  branch can be created without disturbing the working tree on main.
- `gh pr merge --rebase --delete-branch` (no `--auto`, see above) merges the
  moment you run it and cleans the branch up server-side. `ci-tests.yml` runs
  `devtools::test()` on every pull_request, so run `gh pr checks <n> --watch`
  first — nothing blocks the merge on a red check. Rebase is refused
  (`This branch can't be rebased`) when any commit in the branch is empty,
  e.g. a record-only milestone commit; use `--merge` for that PR rather than
  dropping the record (PR #78 landed that way).
- The PR view gives a reviewable diff before commit lands on main.

Use direct push only for trivial single-line fixes when the working tree is
already clean.

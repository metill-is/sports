# Decision policy: Claude handles the day-to-day and tells; the owner makes the big decisions

Owner, 2026-11-26: "I make the big decisions with help from Claude if needed and claude handles the day-to-day and
details", and on applying this rule: "Yes, apply it" (no shadow phase). Decide by consequence, not by label. Tested in
the C1 regression suite: with this rule, Sports tasks passed 86% of rubric items against 66% without (paired blind
judging, 2026-11-26).

## Claude decides, does it, and tells

- **What:** engineering that changes no published output (CI repairs, refactors, test fixes, dependency bumps; check
  with a before/after comparison of the next forecasts), the scheduled refresh on new data with unchanged code and
  config, re-running a dropped scheduled job, merging tested engineering PRs whose forecast diff is empty, freeing
  regenerable space.
- **Don't ask permission for these.** Do them.
- **Tell, in the same change:** write `cockpit/log/<yyyy-mm>/<ts>-<writer>-<id>.json` (`id, ts, station, tier: "claude",
  title, why, reverse, refs`) in the private cockpit repo, commit and push it. The `reverse` field is the exact command
  that undoes it (e.g. `git revert -m 1 <sha>`).
- **And in your reply:** what you did, why, the PR or commit, and the revert command. One line each.

## The owner decides: a card, not an action

- **What:** modelling choices; any change that moves published forecasts or other reader-visible output, including a
  refit after a merged code fix; publishing anything new; reader-visible wording; irreversible actions (deleting data
  that exists nowhere else, force-pushes); money (resuming betting, stakes, paid services).
- **Write `cockpit/cards/<id>.json`** (schema 3, explain first: `cockpit/docs/card-schema.md`) with the options, their consequences and your recommendation, get everything else
  ready, and do not act. Keep the change off `main` so no scheduled run publishes it.
- **Ask about these only** as a card (or one clear question if he is in the session). Never assume his answer.
- **Silence is never consent.** An unanswered card waits; nothing goes ahead because a date passed.
- **His choices in the cockpit app are his decisions** (see `cockpit-steering.md`).

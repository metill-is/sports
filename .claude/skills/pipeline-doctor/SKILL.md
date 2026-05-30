---
name: pipeline-doctor
description: Use when checking whether the data + model pipeline is healthy, when returning to the project after time away, or when a workflow looks stuck. Runs the read-only health snapshot and triages any breach.
argument-hint: "[--refresh]"
---

# /pipeline-doctor — One-screen pipeline health + triage

Strictly **read-only on the money path** — never places bets, never writes the
ledger. It composes `pipeline_health()` (fit/odds freshness, persisted Stan
diagnostic drift, orphaned bets, bankroll/drawdown) into a single status table
and helps triage anything that is `WARN` or `FAIL`.

## 1. Refresh the snapshot

```bash
cd /Users/brynjolfurjonsson/sports && Rscript scripts/07_healthcheck.R
```

This prints the per-check table, lists the breaches, and writes
`data/health/status.json` (overall status `OK` < `WARN` < `FAIL`, plus `PAUSED`
for an off-season cell). It always exits 0 — it is observability, not a gate.

## 2. Add operational context

```bash
# Recent data commits — did the crons actually run?
git -C /Users/brynjolfurjonsson/sports log --oneline -8 -- data/

# Recent CI runs and their conclusions (needs gh + a GitHub remote)
gh run list --repo metill-is/sports --limit 12
```

## 3. Triage each breach

For every `WARN`/`FAIL` row, map the `check` to its runbook in
`docs/runbooks/` and follow the symptom → diagnose → fix → verify steps:

| check | likely cause | runbook |
|---|---|---|
| `odds_freshness` FAIL | Lengjan scrape failing or off-season | `docs/runbooks/stale-odds.md` |
| `fit_freshness` FAIL/WARN | `fit.yml` failing, or results not moving | `docs/runbooks/failed-fit.md` |
| `divergence_drift` / `rhat_drift` WARN | model degrading under the abort gate | `docs/runbooks/failed-fit.md` |
| `orphaned_bets` WARN | a bet whose match never settled | `docs/runbooks/orphaned-bet.md` |
| `bankroll` FAIL | realised PnL ran the pool to/under zero | `docs/runbooks/orphaned-bet.md` |
| `check_error` | a health sub-check itself errored | inspect the message; usually a missing/corrupt partition |

Confirm-intent before "fixing": a `PAUSED` cell (basketball/handball off-season)
and the schedule fail-open are **intentional** — surface them, do not treat
them as faults.

## Why a skill (and a SessionStart banner), not a hot-path hook

Health is checked **on demand** (this skill), as a **twice-daily cron**
(`healthcheck.yml`), and via a cheap **SessionStart banner** that reads the
committed `status.json`. A `PostToolUse`-after-fit hook was deliberately
rejected: the Stan abort gate already blocks a bad fit synchronously, so such a
hook would add always-on cost for no marginal signal.

# Runbooks

Short, action-oriented responses to the pipeline's known failure modes. Each
runbook is keyed to a `pipeline_health()` check (run `/pipeline-doctor` or
`Rscript scripts/07_healthcheck.R` to see the current status), and follows the
same shape: **symptom -> diagnose -> fix -> verify**.

| Health check / symptom | Runbook |
|---|---|
| `odds_freshness` FAIL, stale football data on fly.metill.is | [stale-odds.md](stale-odds.md) |
| `fit_freshness` FAIL/WARN, `divergence_drift` / `rhat_drift` WARN | [failed-fit.md](failed-fit.md) |
| `orphaned_bets` WARN, `bankroll` FAIL | [orphaned-bet.md](orphaned-bet.md) |
| `decide-publish.yml` red on "Value validation" / "Schema validation" | [schema-abort.md](schema-abort.md) |
| fly.metill.is stale despite fresh `data/publish/` JSONs | [metill-platform-desync.md](metill-platform-desync.md) |

## First principles

- **Confirm-intent before "fixing".** A `PAUSED` cell (basketball/handball
  off-season) and the schedule fail-open are intentional, not faults. Several
  workflow behaviours look like bugs but are by design (the decide-publish
  dual-parent trigger; the kelly_frac operational cut). Surface, don't auto-fix.
- **Never mutate the ledger by hand.** `data/decisions/ledger/` is the canonical
  money record (L1-L4). Bet parameters are frozen at write time; settlement only
  flips `settled` / `win` / `pnl`. See [orphaned-bet.md](orphaned-bet.md).
- The health check is **read-only**. Acting on a breach is a separate, deliberate
  step.

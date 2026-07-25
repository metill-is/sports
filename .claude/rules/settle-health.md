---
paths:
  - "R/settle.R"
  - "R/health.R"
  - "R/storage-validate.R"
  - "R/decide-kelly.R"
  - "scripts/06_settle.R"
  - "scripts/07_healthcheck.R"
  - ".github/workflows/healthcheck.yml"
---

# Settle + health layers

## Settle layer

`R/settle.R` exports `compute_settlement(bets, results, match_date_window_days)`
and `settle_ledger(root, match_date_window_days)`. Joins ledger rows where
`settled = FALSE` against `data/facts/results/` on
`(sport, country, sex, match_date, home_team, away_team)`, computes `win` + `pnl`
per market with strict-inequality boundaries (matching
`decide-kelly.R::build_return_matrix` so calibration stays self-consistent with
the EV used at placement). Already-settled rows are immutable (L4).

**Ordering constraint:** run `scripts/06_settle.R` before `04_decide.R` so
`current_pool = initial_pool + Σ(settled.pnl)` reflects realised PnL.

**Reschedule fallback (default `match_date_window_days = 3`)** — bets the strict
join leaves unsettled get a second-pass lookup against results within the window,
keyed on `(sport, country, sex, home_team, away_team)`. The fallback only fires
when the window contains exactly one candidate result; ambiguous pairings (e.g. a
cup tie + league leg within three days) stay unsettled. This handles ledger
orphans created when Lengjan reschedules a fixture after placement — the placer
freezes `match_date` at the original kick-off (L3/L4 spirit) and the federation
results scraper writes the played match at the new date. The ledger row's
`match_date` is never mutated; only `settled` / `win` / `pnl` flip, preserving L4.

**Local-only by design** — both placer and settle write to
`data/decisions/ledger/`, and `arrow::write_parquet` is read-then-write (not
atomic), so adding a CI-host writer would race concurrent local placer runs and
risk Parquet corruption. Promoting to a workflow would require atomic upsert
semantics or a coordination mechanism.

## Health & monitoring (2026-05-30)

`R/health.R::pipeline_health(root, now)` is a **read-only** snapshot composing
fit/odds freshness, persisted Stan-diagnostic drift
(`data/beliefs/diagnostics/`, written per fit by `fit_league`), orphaned-bet,
**placement-capture-rate** (recommendations for now-played matches that never
reached the ledger — the forensic review found ~45% capture), and
bankroll/drawdown checks into a `{check, scope, status, value, threshold}` tibble
(`OK` < `WARN` < `FAIL`, plus `PAUSED` for off-season cells via
`has_upcoming_games`). Thresholds are named constants in the function.

`scripts/07_healthcheck.R` writes `data/health/status.json` + prints a summary;
`healthcheck.yml` runs it twice daily and fails the run on `overall == FAIL` so
GitHub's failure email fires (the alert channel — enable "Actions: failure"
notifications in GitHub). The `/pipeline-doctor` skill + a SessionStart banner
surface it interactively. All read-only on the ledger (CI-safe against the local
placer); `tests/testthat/test-healthcheck-ci-isolation.R` enforces it. Triage
playbooks: `docs/runbooks/`.

**Two write-boundary guards complement the snapshot:** `validate_values()`
(`R/storage-validate.R`, wired into `write_table`) rejects impossible scores /
`odds <= 1` / out-of-range `p`; `validate_bet_inputs()` (`R/decide-kelly.R`)
quarantines a non-finite `p` or `odds <= 1` into a loud `dropped_invalid_input`
candidate stage before stake sizing. The Stan gate (`check_stan_diagnostics`)
covers treedepth / E-BFMI / tail-ESS and returns its metrics for persistence.

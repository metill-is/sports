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
**eleven** checks into a `{check, scope, status, value, threshold}` tibble
(`OK` < `WARN` < `FAIL`, plus `PAUSED` for off-season cells via
`has_upcoming_games`): fit/odds freshness, persisted Stan-diagnostic drift
(`data/beliefs/diagnostics/`, written per fit by `fit_league`), orphaned-bet,
**placement-capture-rate** (recommendations for now-played matches that never
reached the ledger — the forensic review found ~45% capture), placement health,
bankroll/drawdown, discovery, and the three publish-side checks added
2026-09-04. Thresholds are named constants in the function.

### The publish-side checks (2026-09-04)

Until these landed, NOTHING in `pipeline_health()` read `data/publish/` — which
is how basketball and handball published nothing at all from the Plan-7 cutover
to 2026-09 while every composed check stayed green (B4).

- **`check_publish_freshness`** (`R/health-publish.R`) — one row per (league,
  sex, division) in `publish_divisions` for each active league. FAIL on a
  missing cell, a missing `meta.json`, a `generated_at` older than
  `publish_max_age_hours` (36), or a MISSING expected artefact; WARN on an
  UNEXPECTED extra one (asymmetric on purpose: a missing artefact 404s on the
  platform, an extra one is leftover output from an older shape); PAUSED when
  the cell has no upcoming fixture. **An in-season, active cell with no publish
  output is FAIL, never PAUSED** — that state IS B4, and a check that calls it
  PAUSED re-hides exactly what it was built to find. When there is also no
  extract partition the value says so, because that names the cause rather than
  the symptom.
- **`check_season_resolution`** (`R/health-season.R`) — FAIL per unresolvable
  league division, WARN per federation-deferred cup/playoffs gap, one OK row per
  clean federation. It is what distinguishes "the season is genuinely over" from
  "the scraper went blind in October": identical in the results table, different
  only in whether the federation season id resolved. Consumes Plan A's
  `hsi_unresolved_seasons()` / `kki_unresolved_seasons()`, both pure
  registry + cache lookups — **they must stay network-free**, or the healthcheck
  workflow starts making HTTP calls.
- **`check_publish_format_agreement`** (`R/health-publish.R`) — WARN only, never
  FAIL, when a published `n_rounds` disagrees with the division's configured
  `expected_meetings`. The value carries BOTH numbers side by side. Skips cups,
  cells with no publish output, pre-v2 metas and divisions with nothing
  configured to compare against.

**HONEST LIMIT, and it constrains the design.** The alert channel is a GitHub
workflow-failure email. That is signal, not a pager: `healthcheck.yml` runs
twice daily and fails the run on `overall == FAIL`; there is no push
notification, no escalation and no on-call. A FAIL is noticed within roughly
twelve hours if the maintainer reads mail, and not at all if they do not.
Because the channel is that low-bandwidth, **a check that is permanently WARN
is worse than no check** — which is why `check_season_resolution` scopes FAIL to
the league divisions and leaves HSÍ's federation-deferred cup and playoffs at
WARN, and why a false FAIL must be adjudicated (is the branch behind `main`?)
rather than silenced with a threshold.

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

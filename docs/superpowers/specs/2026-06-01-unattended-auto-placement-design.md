# Unattended low-footprint auto-placement -- design

- **Date:** 2026-06-01
- **Status:** Draft (design approved in brainstorming; pending spec review)
- **Topic:** Raise placement capture toward its structural ceiling with zero
  routine operator effort, while preserving every money-path safety invariant
  and minimising the programmatic footprint visible to Lengjan.

## Motivation

Placement capture sits at ~45% (36/80 distinct recommended legs in the 21-day
window, 2026-06-01 health snapshot). The forensic review and the 2026-06-01
decomposition agree the gap is **operational, not structural**: the CI decide
layer generates recommendations correctly ~3x/day, but the placer is local-only
and **manual**, so whole-day zeros appear when the operator simply does not run
it (e.g. 2026-05-23 placed 0 of 10, 05-31 placed 2 of 9 -- not one-by-one
rejections but days the placer never ran).

The capture metric also *overstates* the true leak: it counts legitimate
non-bets (P4 EV-reject, delisted lower-division fixtures Lengjan never priced)
as misses alongside genuine not-run days. But the operational component is real,
dominant, and fixable -- and is what this design targets.

**Goal:** place +EV bets before kickoff without ongoing operator effort, while
(a) keeping the money-path invariants intact and (b) keeping the authenticated
Lengjan footprint proportionate to actual betting activity.

## Constraints (hard)

1. **Money path is local-only.** `R/placer-*.R` must never run on CI
   (`tests/testthat/test-placer-ci-isolation.R`). Real money + Lengjan
   credentials (`LENGJAN_*` from `.Renviron`) + Chromote browser automation
   require the operator's Mac. Cloud automation of placement is impossible by
   invariant.
2. **No kickoff-time.** The federation results scraper does not capture kickoff
   time (schema migration deferred), so the placer cannot time itself to an
   individual match; it must run on a clock and rely on P1 idempotency to make
   repeated runs free.
3. **ToS / detection risk is real and irreducible.** Automated betting
   plausibly breaches Lengjan terms; worst case is account closure / voided
   balance. This design reduces the *obvious* programmatic signals through
   restraint. It deliberately does NOT implement fingerprint spoofing,
   behavioural mimicry, or proxy rotation to defeat bot-detection -- that is
   disproportionate for a single personal account and out of scope. Restraint
   is not ToS-compliance. Operator-accepted as a known risk.
4. **Mac-availability dependency.** No placement happens while the Mac is asleep
   or offline; launchd coalesces missed runs and catches up on wake.

## Goals / non-goals

**Goals**

- Zero routine operator effort to place +EV bets before kickoff.
- Authenticated Lengjan sessions that scale with real betting activity, not a
  fixed programmatic heartbeat.
- Unattended failures are *visible* through the existing health layer.
- Every existing financial cap and P1-P4 rule preserved, plus a true daily
  stake cap that holds across multiple sessions in a day.
- An instant, low-ceremony kill switch.

**Non-goals**

- Anti-detection beyond proportionate restraint (no fingerprint/behavioural
  spoofing, no proxies).
- Kickoff-time-aware scheduling (blocked upstream).
- Any CI involvement in placement.
- Changing the decide / stake-sizing logic.

## Architecture

### 1. `scripts/auto_place.R` (new orchestration wrapper)

The single entry point the scheduler invokes. Deterministic sequence, wrapped so
failures are recorded rather than lost. The wrapper adds *only* the unattended
concerns; the core placement stays in `place_bets()`.

1. **Jitter** -- `Sys.sleep(runif(1, 0, 1200))` (0-20 min) so wall-clock timing
   is irregular rather than clockwork.
2. **Kill-switch** -- if the sentinel file `data/AUTO_PLACE_DISABLED` exists ->
   record `disabled`, exit 0. The operator's instant off-switch, no `launchctl`
   needed.
3. **Daytime guard** -- if outside the daytime window (~09:00-22:00 local,
   intentionally an hour wider than the last scheduled trigger so jitter and
   wake-catch-up still land inside) -> exit 0 (defends against a wake-catch-up
   firing at 04:00).
4. **Lock** -- acquire an atomic lockfile `data/.auto_place.lock` (PID + mtime).
   If a live lock is held -> record `locked`, exit 0 (no overlapping browser
   sessions / ledger races). A stale lock whose PID is dead is reclaimed.
5. **Sync recs** -- stash-safe `git pull --rebase` (the `git-hygiene` pattern)
   so it acts on CI's latest `recommendations`, not a drifted local tree. On
   pull conflict -> record `sync_failed`, exit 1 (surfaced by health).
6. **Gate (no Lengjan contact)** -- run the `preview_bets` path, which reads
   only local `recommendations` + `ledger`. If 0 pending placeable bets ->
   record `nothing_pending`, exit 0. **Lengjan is never contacted on a quiet
   day.**
7. **Daily-cap check** -- if `sum(today's ledger bet_amount)` already meets/
   exceeds `daily_budget` -> record `daily_cap_reached`, exit 0. Otherwise cap
   the pending set so cumulative day stake stays within `daily_budget`.
8. **Place** -- `place_bets(dry_run = FALSE, interactive = FALSE,
   headless = FALSE, pace = TRUE)`; places all pending in one visible,
   human-paced session.
9. **Record + commit** -- write the run outcome to the placement-status store;
   the existing L1 ledger-commit logic in `place_bets.R` commits the ledger and
   already aborts loudly if money was placed but the commit fails.
10. **Release lock.**

### 2. launchd agent -- `~/Library/LaunchAgents/is.metill.sports.autoplace.plist`

- Triggers `auto_place.R` via `Rscript` roughly every 2h across the daytime
  window (either `StartInterval` 7200s with the script's daytime guard, or
  several `StartCalendarInterval` entries spanning ~09:00-21:00).
- `RunAtLoad` true, so a wake mid-window triggers a catch-up.
- Pins `WorkingDirectory` to the repo and sets `PATH` / R location; the launchd
  minimal-env gotcha means the job must reach the operator environment so R
  loads `.Renviron` (`LENGJAN_*`).
- stdout/stderr -> `~/Library/Logs/sports-autoplace.log`.
- Installed/removed via a small `tools/install-autoplace.sh` wrapping
  `launchctl bootstrap | bootout`.

### 3. Placement-status store + `placement_health` check

- `auto_place.R` writes each run as `{run_at, status, n_pending, n_placed,
  error}` where `status` is one of `placed`, `nothing_pending`, `ev_rejected`,
  `disabled`, `locked`, `sync_failed`, `daily_cap_reached`, `failed:<reason>`.
  Store: `data/health/placement_status.json` (committed).
- A new `check_placement_health(root, now, th)` in `R/health.R`, composed into
  `pipeline_health()`. A run "completed healthily" if its status is any of
  `placed`, `nothing_pending`, `ev_rejected`, `daily_cap_reached` -- the wrapper
  ran and either placed or *correctly declined*. Failure statuses are
  `failed:*` and `sync_failed`. The check keys on **operational health, not on
  residual pending count**: a recommendation that legitimately keeps
  EV-rejecting at live odds stays forever "pending" in preview, and must NOT
  raise an alarm -- otherwise this recreates the leak-vs-legitimate conflation
  that already weakens `capture_rate`.
  - **FAIL** -- the last run was a failure status, OR no healthy run has
    completed within the hard staleness threshold while upcoming matches have
    pending bets.
  - **WARN** -- the last healthy run is older than the soft threshold while
    pending bets exist (placement falling behind, not yet broken).
  - **OK** -- the last run was a failure-free completion, or nothing is pending.
  - **PAUSED** -- off-season (no upcoming games), consistent with existing
    checks.
- Surfaces through `/pipeline-doctor`, the SessionStart banner, and
  `healthcheck.yml` (committed status -> failure email on FAIL). Read-only on
  the ledger.

### 4. Human-paced placement

- A `pace = TRUE` option threads `runif`-based jittered delays between Lengjan
  actions in `R/placer-navigate.R` / `R/placer-place.R`, and runs the browser
  non-headless. Proportionate restraint only -- no behavioural spoofing.

## Data flow

```
CI decide-publish (3x/day) --commit--> recommendations (origin/main)
        |
auto_place.R: git pull --rebase --> local recommendations + ledger
        |
   preview gate (LOCAL ONLY -- no Lengjan contact)
        |  pending == 0 --------------> status: nothing_pending  (Lengjan untouched)
        |  pending >= 1
        v
   daily-cap ok? -- no --> status: daily_cap_reached
        | yes
        v
   place_bets(live, no-confirm, visible, paced) --> Lengjan (one session)
        |
        +--> ledger (append + commit)            [L1]
        +--> placement_status.json (commit) --> placement_health
                                                   --> /pipeline-doctor + banner + email
```

## Error handling / robustness

- Everything in `auto_place.R` runs inside `tryCatch`; any error -> status
  `failed:<reason>`, lock released, exit 1. Never silent -- health surfaces it.
- **Money-placed-but-uncommitted** keeps the existing loud abort + manual-fix
  message in `place_bets.R`.
- **Overlap** prevented by the lockfile; **stale lock** reclaimed via PID
  liveness check.
- **Mac asleep** -> missed launchd runs coalesce; `RunAtLoad` + the daytime
  guard make wake-catch-up safe.
- **Sync conflict** -> `sync_failed`, no placement attempted.

## Safety rails

| Rail | Mechanism | New? |
|---|---|---|
| P1 idempotency (no double-place) | `dedup_against_ledger()` | existing |
| P4 EV-reject | `place_bet()` | existing |
| Per-match cap 12.5% | `max_match_stake x kelly_ceiling` | existing |
| Per-day cap 5% (per decide-run) | `daily_budget_frac` | existing |
| **True daily cap across sessions** | wrapper sums today's ledger + pending vs `daily_budget` | **new** |
| Kill switch | `data/AUTO_PLACE_DISABLED` sentinel | **new** |
| Overlap lock | `data/.auto_place.lock` | **new** |
| Unattended-failure visibility | `placement_health` | **new** |

## Testing

- Pure-function unit tests (testthat 3): gate decision (pending count ->
  place/skip), daily-cap math, kill-switch detection, lock acquire/reclaim,
  status-record writer, and `check_placement_health` across the status x
  pending x staleness matrix.
- **CI-isolation:** extend `test-placer-ci-isolation.R` to also fail the build
  if any `.github/workflows/*.yml` references `auto_place`, `autoplace`, or the
  plist name. Same money-path invariant -- this must never run on CI.
- `place_bets()` stays dry-run testable; browser / money paths and launchd are
  not unit-tested.
- **Manual acceptance:** (1) dry-run `auto_place.R` exercising the gate + status
  with no placement; (2) one supervised live run; (3) enable the agent.

## Risks (stated plainly)

1. **ToS / account risk** -- automated betting may breach Lengjan terms; worst
   case is account closure / voided balance. Restraint reduces obvious signals,
   not the underlying breach. Operator-accepted.
2. **Mac-availability** -- no bets while the Mac is off all day (acceptable: no
   betting happening then anyway).
3. **Daily-cap-across-sessions** -- must verify whether the decide layer already
   prevents cumulative >5%/day; the wrapper cap is the backstop regardless.
4. **Capture ceiling** -- gating + clock cadence will not reach 100%; a match
   whose odds post and which kicks off entirely between two runs is still
   missed (mitigated by the ~2h cadence; fully solved only by kickoff-time,
   which is out of scope).

## Open items to resolve in the implementation plan

- Confirm `preview_bets` / `place_bets` expose a clean pending-count and a
  `pace` / visible-browser option, or add them.
- Confirm the `daily_budget` derivation available to the wrapper cap
  (`daily_budget_frac x current_pool`, floored by `daily_budget_min_isk`).
- Finalise the status-store shape (`data/health/placement_status.json` vs a
  Parquet row) and its commit cadence.
- launchd environment specifics on this Mac (R path, env sourcing).

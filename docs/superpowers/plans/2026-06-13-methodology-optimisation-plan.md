# Implementation plan — betting methodology optimisation

> Companion to `docs/superpowers/specs/2026-06-13-methodology-optimisation-design.md`.
> Read that spec first; this plan is the phased build order with concrete files, steps, and per-phase verification gates.
> Standing principle: **machinery first (validation + scaffolds, all default-OFF), live experiment second, tuning gated on accrued evidence last.** Nothing in Phases 1–3 changes live stakes. Every component ships behaviourally inert and is enabled only by an explicit, evidence-gated, tracked decision.

## Guardrails for every phase

- **Read-only on the money path.** No phase writes `data/decisions/ledger/`, runs the placer (`R/placer-*.R`, `scripts/place_bets.R`, `scripts/auto_place.R`), or is wired into any CI workflow.
- **Schema before config.** `config/leagues.schema.json`'s `betting` block is `additionalProperties:false`; any new YAML key must be added to the schema first or `load_leagues()` rejects it across the whole pipeline.
- **Default-OFF, byte-identical when off.** With every new flag off, the stake chain at `decide-pipeline.R:223-226` and the candidates/recommendations/ledger schemas are unchanged. Add a regression test that asserts this.
- **Verify, don't assert.** Each phase gate is a command whose output must be confirmed (`devtools::test()`, a targeted `test_file`, or a CLI run), not a claim that it "should" pass.
- Detached for long runs: `nohup Rscript ... > /tmp/x.log 2>&1 & disown`.

---

## Phase 0 — Documentation + invariant clarifications (no code behaviour change)

Lock the conceptual guardrails before any scaffold exists, so a future maintainer cannot reintroduce the traps this design avoids.

**Steps**

1. `.claude/rules/sports-betting.md` — under the K-invariants, add: "**kelly_frac stays per-(league,sex); market heterogeneity lives ONLY on the calibration axis. Per-market kelly_frac is forbidden — it double-counts the K2 axis.**" Tie the 7d roadmap entry ("hierarchical kelly_frac pooling") to this: the pooling is realised on the **calibration** axis, not kelly_frac.
2. `.claude/rules/model-decide.md` — document (a) the K1-vs-recalibration mutual exclusion ("one replaces the other, never both; enforced at runtime"); (b) that an active recal cell sizes magnitude off the uncalibrated posterior (half-application); (c) that an active recal cell's `candidates.p` / ledger `p` is the recalibrated value.
3. `.claude/rules/backtest.md` — document the recalibrated-vs-raw `candidates.p` provenance and the leak boundary for the walk-forward harness (`match_date < as_of`, pre-slice odds).
4. Record the VERIFIED live K2 state (football male: aggregate 0.883, moneyline 0.868, total 0.911) in the spec so the "not a no-op" correction is on the record.

**Gate:** `Rscript -e 'devtools::test()'` green (docs-only, no behaviour change); manual read-through confirming the three rule files state the forbidden patterns explicitly.

---

## Phase 1 — Validation harness machinery (read-only, no scaffolds yet)

Build the instrument that will later judge any change. Independent of Components A/B.

**Files (new):** `R/backtest-walkforward.R`, `R/clv.R`, `scripts/0Nb_walkforward.R`, `tests/testthat/test-backtest-walkforward.R`, `tests/testthat/test-clv.R`.
**Files (edit):** `R/health.R` (+ thresholds), `.claude/rules/backtest.md`.

**Steps**

1. `bt_walkforward(sex, cutoffs, horizon_days=14L, candidate=NULL, root)` per spec §5.1: isolated `wf_root` tempdir fit, **pre-slice odds to `scraped_at <= cutoff` before `prepare_odds`**, `ledger_asof(d)` for K1, `decide_league(write=FALSE)`, settle via `compute_settlement()` with the per-(sport,country) `tie_threshold`. Football-only hard stop (mirror `replay.R`).
2. `0Nb_walkforward.R` CLI (`--sex --season --per-round --horizon --candidate`), output `data/backtest/walkforward/` (gitignored). Document detached invocation.
3. `R/clv.R`: `closing_odds(root)` keyed on match identity + market + outcome + line (NOT sex); `attach_clv(bets)` with signed log-CLV, NA-close dropped not zeroed. **Mark these forward-capture-oriented** — do not present a historical-ledger CLV mean as primary (10.7% coverage).
4. `R/health.R`: add `check_clv_drift()` (active once forward capture exists), `check_experiment_progress()` (informational), and `check_calib_market_spread()` — each `safe()`-wrapped, thresholds as inline named constants in `health_thresholds()`.

**Tests / gate**

- `test-backtest-walkforward.R`: an explicit regression test that the harness **pre-slices odds to `scraped_at <= cutoff` before `prepare_odds`** (the single load-bearing leak guard; `prepare_odds` cannot enforce it). A leak-free smoke test on a small cutoff set.
- `test-clv.R`: NA-close dropped not zeroed; key excludes sex; per-sex name normalisation applied (or capture-at-settle path exercised).
- `Rscript -e "devtools::test()"` green; one real `0Nb_walkforward.R` run on a 2-cutoff window completes and writes to `data/backtest/walkforward/`.

---

## Phase 2 — Calibration shrinkage scaffold (Component A, default OFF)

**Files (edit):** `R/decide-calibration.R`, `R/decide-pipeline.R` (dispatch only), `config/leagues.schema.json`, `config/leagues.yml`, `tests/testthat/test-decide-calibration.R`, `.claude/rules/sports-betting.md`.

**Steps**

1. `compute_calibrations_hier()` per spec §3.2–3.4 with the **anchor = existing per-market K1/K2 estimate** (NOT the aggregate), `tau_mult_cap = 1.07`, `min_n_unlock = 30`, ≥2-eligible-market gate, explicit `[0.5,1.5]` clamp, **loud fallback** (`cli_alert` + `nrow(led)>0` assertion when dispatch requested). Same return contract as `compute_calibrations()`. Pass the already-read ledger through to avoid a double scan.
2. `decide-pipeline.R:201` dispatch: hierarchical when `betting$calibration$hierarchical` is TRUE, else today's path. Lines `:211-226` unchanged.
3. Schema: add `calibration` object under `betting.properties` (`additionalProperties:false`). YAML: `betting.calibration: { hierarchical: false, min_n_unlock: 30, tau_mult_cap: 1.07 }` per cell, shipped OFF.

**Tests / gate**

- **Anchor-equivalence (the corrected no-op):** with `tau2_mom == 0`, `compute_calibrations_hier()` returns **byte-identical** multipliers to `compute_calibrations()` on the **live** football-male fixture — i.e. preserves moneyline 0.868 and total 0.911, does **not** collapse to 0.883.
- `<2` eligible markets ⇒ anchors returned unchanged.
- `[0.5,1.5]` clamp asserted (K4); `B_m ∈ [0,1]` monotone in `n_m`; `tau_mult_cap` binds under huge synthetic scatter.
- **Daily-budget invariant test (required, §3.6):** `Σ(kept bet_amount) ≤ daily_budget_frac × current_pool` under a fixture with one market's `calib=1.5` and another's `=0.5`. If it can fail, route the per-market multiplier into `eff_stakes` and re-test.
- Dispatch: `hierarchical=FALSE`/absent ⇒ `decide_league()` calls `compute_calibrations()` (verified via spy), stake chain byte-identical.
- Leak-free: reads only settled ledger rows.
- `Rscript -e "devtools::test()"` green; `Rscript scripts/04_decide.R --league football_iceland --sex male` produces identical recommendations with the flag off vs on-at-current-data (proves the scaffold is inert today).

---

## Phase 3 — Recalibration scaffold (Component B, default OFF)

**Files (new):** `R/decide-recalibration.R`, `tests/testthat/test-decide-recalibration.R`.
**Files (edit):** `R/decide-kelly.R`, `R/decide-pipeline.R`, `R/storage-schemas.R`, `R/storage.R`, `R/health.R`, `config/leagues.schema.json`, `config/leagues.yml`, `.claude/rules/{model-decide,sports-betting,backtest}.md`.

**Steps**

1. `R/decide-recalibration.R`: `recalibrate_p()` (exact identity fast path), `recal_fit_set()` (leak guards: `match_date < as_of`, settled-only, frozen-p; **candidates branch applies the boundary to candidate rows explicitly**), `fit_penalised()` (ridge-to-`centre`, coefficient sanity bounds `b>0`, separable-input test), `fit_recalibration()` (pooled shrink-to-identity, per-market shrink-to-pooled at `n≥100`), `recal_identity()`, `recal_active()`, `renormalise_exhaustive_outcomes()` (caps 2-outcome groups too), `recalibration_gate()`.
2. `decide-kelly.R`: `kelly_joint()` gains `recal = recal_identity()`; after `:231` apply `recalibrate_p` → `renormalise_exhaustive_outcomes` → existing `assert_outcome_prob_coherent` (:232) on recal `p`; `ev`/`keep` use recal `p`; `kelly_raw` (:248) unchanged.
3. `decide-pipeline.R`: fit `recal` once before the per-match loop; pass into `kelly_joint()` at `:147`; **runtime mutual-exclusion assertion** — abort if `recal_active()` AND `compute_calibrations()` returns a non-unit applied multiplier; force `cands$calib<-1` when active; append the audit row.
4. `R/storage-schemas.R` + `R/storage.R`: `recalibration` accretive store, partitioned `c(sport,country,sex,run_date)`. Decide the **ledger-p provenance** choice (raw-p column vs K1-retired-for-active-cell) and wire the audit store / harness accordingly.
5. `R/health.R`: optional `recalibration_status` row reading the audit store.
6. Schema FIRST: `recalibration` under `betting.properties`; YAML `betting.recalibration: { enabled: false }` per cell.

**Tests / gate**

- Identity no-op at `(0,1)`; leak guard drops `match_date >= as_of` (both fit sources); ridge pulls toward identity at low n; per-market partial-pool toward pooled.
- **Coherence prerequisite (§4.6):** `renormalise` keeps full 1X2 and **2-outcome** groups at sum ≤ 1; a `b`-far-from-1 fixture on a home+away-only group does **not** trip `assert_outcome_prob_coherent`. (Hard prerequisite — enabling a cell can otherwise abort the live decider.)
- `cands$calib == 1` whenever `recal_active()` — **hard CI test**; the runtime mutual-exclusion assertion fires on the double-correction fixture.
- Gate FAILs on current-shaped data (sign-flip + materiality + slope-touches-1); hysteresis needs 2 consecutive passes.
- `Rscript -e "devtools::test()"` green; `04_decide.R` byte-identical recommendations with recalibration off (default).

---

## Phase 4 — Paper shadow / A-B experiment harness + SPRT gate (read-only, off-CI)

**Files (new):** `R/experiment.R`, `R/experiment-gate.R`, `config/experiments.yml`, `tests/testthat/test-experiment-ci-isolation.R`, `tests/testthat/test-experiment-gate.R`, `docs/reports/2026-walkforward-clv.qmd`.
**Files (edit):** `R/storage-schemas.R` + `R/storage.R` (if `data/experiments/` persisted via `write_table`; partition `approach,sport,country,run_date`), `scripts/04_decide.R` (local env-gated experiment write — the call lives in `decide_one`/`decide_league` or a new gated branch, NOT a fixed line number), `scripts/06_settle.R` (local CLV forward-capture + experiment-arm settle).

**Steps**

1. `run_experiment_arm()` per spec §5.3: one-factor perturbation, `write=FALSE`, isolated `data/experiments/approach=*/...`; pre-registered `protocol.json`; hard-abort if two calibration axes are active at once.
2. `experiment-gate.R`: Normal-Normal posterior + Wald SPRT on **paired** CLV; verdict {accumulate, graduate, abandon(sticky), continue}; multi-gate graduation (CLV up AND OOS log-loss not worse AND capture_rate≥0.7); operator-action only.
3. **Forward CLV capture** wired into the LOCAL `06_settle.R` (where sex + canonical names are in scope), writing to `data/clv/` (NOT under `data/decisions/`), derived-on-read from the immutable ledger.
4. `config/experiments.yml` approach registry (status sticky on abandon).
5. `docs/reports/2026-walkforward-clv.qmd` — render to verify it builds.

**Tests / gate**

- `test-experiment-ci-isolation.R` (required, §5.6): no CI workflow writes under `data/`; placer/autoplace never read `data/experiments/`; `data/experiments/` and `data/clv/` are NOT prefix-matches of any ledger read glob or `compute_settlement()` input; CLV store written only from local `06_settle.R`.
- `test-experiment-gate.R`: SPRT verdict transitions; `accumulate` below `n_min`; abandon is sticky; graduation requires all three sub-gates.
- `quarto render docs/reports/2026-walkforward-clv.qmd` succeeds.
- `Rscript -e "devtools::test()"` green.

---

## Phase 5 — LIVE experiment (the few-thousand-ISK spend) — gated, opt-in

Only after Phases 1–4 are merged and forward CLV capture has begun accruing.

**Steps**

1. Pre-register a pooled-CLV paper shadow arm for football_iceland male (the highest-volume cell): write `protocol.json` with `primary_metric=clv`, `n_min=30`, `edge_min=0.01`, `alpha`, `beta`, `axis_owner`.
2. Run the control decide as today; locally (env-gated) also run the shadow arm each decide. **No live stake changes** — the shadow arm is paper-only; the few-thousand-ISK budget is the ordinary live betting that produces the paired control CLV, not a separate stake.
3. Watch `check_clv_drift` + `check_experiment_progress` health rows and the SPRT verdict each settle.

**Gate (stopping rules tied to the power analysis):**

- The SPRT graduate/abandon boundaries (`A=(1-β)/α`, `B=β/(1-α)`) on paired CLV are the stopping rule; expect a pooled-CLV verdict in ~50–150 paired bets (weeks at ~15–25/week).
- Per-market verdicts are explicitly **not** in scope at this volume (spread CLV ~year+ to 100 paired obs; kept-universe ROI multiple seasons to decades).
- No knob graduates to live config without: SPRT graduate AND OOS log-loss not worse AND capture_rate ≥ 0.7, confirmed on both candidate and kept universes where applicable, with the K1/K2 double-count + **stake-delta** checklist completed by hand.

---

## Phase 6 — Tuning (gated on accrued evidence) — likely deferred for seasons

No work here until a Phase-5 arm graduates. When one does:

1. Flip the corresponding `enabled`/`hierarchical` flag for the graduated cell only.
2. For a recalibration flip: run the §4.6 coherence stress test on that cell's recent partial-outcome odds as a hard prerequisite; confirm the runtime mutual-exclusion assertion is in place; record the live stake-delta.
3. For a calibration-shrinkage flip: confirm the cross-market calib-spread health row is green and `tau2_mom` has been repeatedly non-zero across refits.
4. Re-baseline via `0Nb_walkforward.R` and the report; record the decision in the spec's history.

---

## Verification summary (per phase, runnable)

| Phase | Primary verification command(s) |
|---|---|
| 0 | `Rscript -e 'devtools::test()'`; rule-file read-through |
| 1 | `Rscript -e 'devtools::test()'`; one `0Nb_walkforward.R` 2-cutoff run |
| 2 | `Rscript -e 'devtools::test()'`; anchor-equivalence + daily-budget tests; off-vs-on identical recs at current data |
| 3 | `Rscript -e 'devtools::test()'`; coherence + mutual-exclusion + gate-FAILs-today tests; off=byte-identical recs |
| 4 | `Rscript -e 'devtools::test()'`; CI-isolation + SPRT tests; `quarto render` |
| 5 | SPRT verdict + health rows over weeks; multi-gate graduation checklist |
| 6 | walk-forward re-baseline; per-cell flip with coherence/budget/spread prerequisites |


## Revised priorities — the per-sport axis (added 2026-06-14)

> Cross-sport reassessment (2026-06-14) added spec §8. Handball Iceland is the prime
> per-sport differentiation candidate but BLOCKED on two gates: ~98% of its realised
> profit was placed on foreign EUR books (EpicBet/CoolBet), not Lengjan, and no forward
> Lengjan validation exists (the candidates store post-dates the late-April pause).
> The bettable male cell is +7.5% all-time (not significant) and -31.8% in the 2026
> Lengjan era. Do NOT raise handball kelly_frac on the +11.1% headline.

- P1 (NEW, high-value) — Per-sport kelly_frac/calibration differentiation is the leading optimisation path, NOT per-market-within-football. The cross-sport ROI spread (handball +11.1% / basketball +1.2% / football +0.8%, verified on the live ledger) is the largest interpretable heterogeneity in the system, and the knob (kelly_frac per-(league,sex)) already exists in config/leagues.yml. Handball is the prime candidate — but gated (see P2), never granted on the headline.
- P2 (sequencing change) — The harness's FIRST real job is handball-resume validation on Lengjan, not football tuning. Before autumn-2026 first fixture: build the currently-unbuilt harness (R/clv.R, R/experiment.R, R/experiment-gate.R, R/backtest-walkforward.R, scripts/0Nb_walkforward.R, config/experiments.yml), generalise its football-only default to handball (confirm compute_settlement handball tie_threshold), and wire the local 06_settle.R forward-CLV capture for the handball MALE cell only (female {} in config). Time-to-verdict on a pooled (handball,male) CLV SPRT is ~3-8 in-season weeks IF live from fixture one.
- P3 (hold) — Keep handball kelly_frac at 0.05. Do NOT raise it on the +11.1% headline: ~98% of that PnL was on foreign EUR books (verified 98.3% EUR×150 fingerprint), the bettable male cell is +7.5% (CI [-9.4,+24.3], not significant) and −31.8% in the 2026 Lengjan era. Promote only after a male paper-shadow arm SPRT-graduates under the multi-gate (CLV up AND OOS log-loss not worse AND capture_rate ≥ 0.7), by operator hand-promotion.
- P4 (unchanged) — Per-market-within-football stays unjustified and per-market kelly_frac stays forbidden (double-counts the K2 calibration axis). Every per-market football CI straddles zero; market heterogeneity belongs on the calibration axis only. Football gets pooled-(league,sex) calibration + harness, as already specified — and is NOT the harness's first validation target.
- P5 (unchanged) — Basketball gets no sizing change: indistinguishable from zero and from football (block CI [-15.7,+18.2]), 2026 = −10.0%. Revisit only on a resumed-season Lengjan track record.
- P6 (data) — Add a bookmaker column/partition to the ledger so future handball ROI is separable Lengjan-vs-foreign — the single ledger change that makes 'is this edge forward-bankable' answerable without EUR-fingerprinting, and the prerequisite for ever trusting a realised per-sport ROI as a Lengjan signal.

### Additional decision points (per-sport)

- Handball sizing trigger — confirm the rule: handball kelly_frac stays 0.05 and is raised ONLY after a handball-iceland MALE paper-shadow arm SPRT-graduates on pooled Lengjan CLV under the multi-gate (CLV up AND OOS log-loss not worse AND capture_rate ≥ 0.7), by operator hand-promotion. The +11.1% / +19.4% headlines (female + foreign-book) explicitly do NOT trigger it.
- Female handball — decide whether to keep it out of scope entirely (it is not on Lengjan; team_names.female={}) or, if KSÍ/Lengjan ever lists Olisdeild kvenna, require its own forward Lengjan track record before the +19.4%/+31.1% female signal is treated as bettable. Do not let the female cell's strength leak into the male sizing decision.
- Harness scope generalisation — confirm extending the football-default harness (backtest.md, plan Phase 1 'football-only hard stop') to handball: which CLI/report gates flip from football-only, and that the local 06_settle.R CLV capture covers the handball MALE cell (per-sex name map, male only). This is the gating dependency for any autumn-2026 handball validation.
- Pre-registration before resume — agree that a handball protocol.json (primary_metric=clv, edge_min=0.01, n_min, alpha/beta, axis_owner) is written and committed BEFORE the autumn-2026 first fixture, so the validation cannot become post-hoc metric shopping on a hot start.
- Ledger bookmaker column — decide whether to add a bookmaker column/partition (and optionally a raw-vs-recal p column per §4.7). Without it, no future realised per-sport ROI can be cleanly attributed to Lengjan, and the per-sport differentiation programme keeps relying on the EUR×150 fingerprint as a proxy.
- Basketball/football re-baseline — confirm neither sport's kelly_frac changes on this evidence (both indistinguishable from zero), and that the per-sport differentiation budget is spent on handball validation, not on tuning football per-market or basketball at all.

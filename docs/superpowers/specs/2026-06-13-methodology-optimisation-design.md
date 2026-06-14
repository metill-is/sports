# Sports betting methodology optimisation — design specification

> Status: design. Authored 2026-06-13. Scope: football_iceland (the only active-betting cell); basketball/handball on seasonal pause.
> Companion plan: `docs/superpowers/plans/2026-06-13-methodology-optimisation-plan.md`.
> Standing context: the forensic verdict for this system is **NO demonstrable edge** (real ledger ~ -3.7% all-time; current football backtest -5.1% ROI on n=79, ranking last of 7 strategies; flat-stake-everything would be +8.9%). This spec is written to be honest and evidence-gated, not to rubber-stamp enthusiasm.

## 0. Executive verdict

The user asked two things:

1. **Should we start calibrating Kelly fractions by market?**
2. **Should we add logit-scale recalibration of model predictions based on historical calibration?**

**Both answers are NO at current data.** Neither divergence is supported by the evidence:

- Per-market ROI is statistically indistinguishable from noise. Every 95% bootstrap ROI confidence interval straddles zero and all three markets' intervals mutually overlap; the minimum detectable ROI edge at current sample sizes is +22% to +36% against an observed spread of +21.5%; the live per-market settled counts (21–31) are an order of magnitude below the K2 promotion bar of 100.
- The model is already well-calibrated. Aggregate calibration gap is 0.0000; pooled logit slope 0.909; the joint likelihood-ratio test for recalibrated-vs-raw is non-significant (p=0.149); held-out Brier improvement is ~0.1% and **its sign flips across train/test splits**; an oracle recalibration fit directly on the held-out set buys only -0.00002 Brier — there is essentially nothing to correct out of sample.

**What we will build instead:** evidence-gated *scaffolds* that ship behaviourally inert, plus a validation and live-experiment harness, so that:

- tuning is **gated on accumulated evidence**, never on a hot in-sample cell;
- the right experiment (a CLV-instrumented paper shadow arm) yields a verdict in **weeks**, where PnL would need **years**;
- the machinery exists and is tested when basketball/handball resume and when football volume eventually clears the gates.

This converts "no edge, don't tune" from a dead end into a disciplined research programme: measure first, then size.

## 1. Honest sample-size accounting (the load-bearing constraint)

All three components are governed by one fact: at single-cell Icelandic-football volume the per-market evidence to *act* is many seasons to decades away.

Measured accrual rate (football_iceland male, 393 settled over a 419-day span):

| market | settled bets/week | weeks to n=100 (K2 bar) | weeks to n=500 (gate floor) | weeks to n≈3,580 (5% edge, 80% power) |
|---|---|---|---|---|
| moneyline | ~2.1 | ~33 (but already past via candidates) | ~5.6 years | — |
| spread | ~1.4 | ~12+ | ~5.8 years | **~48 years** |
| total | ~1.9 | already past | ~5.0 years | — |

The market carrying the only borderline calibration signal (spread, slope ~0.901) is also the thinnest and slowest-accruing in the **kept** universe — the signal is in the market the data starves. The candidates universe (n≈1951) has power but is the **wrong population**: 87% spread, ~84% never-placed `dropped_low_ev` bets whose calibration need not transfer to the +EV bet subset that moves money.

**Therefore the realistic near-term scope is POOLED (league,sex) calibration plus the harness.** Every per-market activation path is built but documented as multi-season-to-decade deferred, with a quantified calendar horizon so the deferral is conscious, not abandoned.

## 2. Current methodology (verified against live code)

The stake formula, assembled at `R/decide-pipeline.R:223-226`:

```
bet_amount = round( current_pool
                  × kelly_raw            # joint SLSQP Kelly over the full posterior return matrix
                  × portfolio_lambda     # Stage-2 daily-budget scaling, 1.0 unless budget binds
                  × min(kelly_frac × calibration, kelly_ceiling) )   # K5 clamp
```

Key separations that make the two asks tractable:

- **`kelly_raw`** is solved by `solve_kelly_joint()` (NLOPT_LD_SLSQP) from the S×B posterior return matrix `R` (`build_return_matrix`), **not** from the point probability `p`. So `p` and the stake-defining `kelly_raw` are structurally separable.
- **`p`** is born in exactly one place, `R/decide-kelly.R:231` (`p <- colMeans(R > 0)`), a pure Monte-Carlo win frequency. It feeds EV (`:233`), the keep filter (`:241`), and the candidates/recommendations tables. There is no probability-level recalibration anywhere today.
- **`calibration`** (`cands$calib`) is the Beta-Binomial K1 multiplier from `compute_calibration()`, clamped `[0.5, 1.5]`, that scales `kelly_frac` at the **stake** level — it never touches `p`. The K2 split (`compute_calibrations()`) already promotes a market to its own multiplier at ≥100 settled bets.
- **`kelly_frac`** is the per-(league,sex) Browne γ from `leagues.yml` (football male 0.10 / female 0.05). It is **not** per-market.

### 2.1 VERIFIED live K2 state (corrects the "no-op" claim)

Running `compute_calibrations(list(sport="football",country="iceland"), "male")` on live data returns:

```
$aggregate  0.883
$moneyline  0.868     # promoted — n >= 100
$total      0.911     # promoted — n >= 100
```

The live system **already applies distinct per-market multipliers** for football male moneyline and total. Any new calibration scheme that "returns the aggregate for all markets" is therefore **not** a no-op — it would regress these live multipliers toward 0.883 (shrinking total stakes ~3.2%, growing moneyline ~1.6%). This is the single most important correction to the kelly-by-market design and is reflected in §3.

## 3. Component A — Hierarchical per-market calibration shrinkage (scaffold, default OFF)

### 3.1 What this is and is NOT

- It is a **partial-pooling estimator on the calibration axis** that produces the existing `cands$calib` factor — **no new multiplier enters the stake chain**.
- It is **NOT** a per-market `kelly_frac`. `kelly_frac` stays per-(league,sex); market heterogeneity lives only on the calibration axis. Adding per-market `kelly_frac` would double-count the K2 axis and is forbidden (documented in `sports-betting.md` and `model-decide.md`).
- It is **NOT** a `p`-recalibration (that is Component B) and **NOT** a closing-line `market_mult` (CLV reconstruction is non-viable; see §5).

### 3.2 Statistical model (Normal-Normal partial pooling, closed form)

Work on the log-calibration-ratio scale. For each enabled market `m` in a (league,sex) cell, from settled ledger rows (`settled & !is.na(win) & !is.na(p)`, scoped to sport/country/sex, restricted to market `m`):

```
W_m = Σ win ;  E_m = Σ p ;  n_m = nrow
y_m = log((W_m + 0.5) / (E_m + 0.5))        # Haldane-Anscombe continuity correction
v_m = 1 / (W_m + 0.5)                         # Poisson-count sampling variance
```

Hierarchy anchored on the **existing per-market K2 estimate** (the correction):

```
y_m     ~ Normal(theta_m, v_m)
theta_m ~ Normal(mu_anchor_m, tau^2)
```

where **`mu_anchor_m = log(calib_m_today)`** — the multiplier `compute_calibrations()` returns for market `m` today (its own K2 estimate when promoted, else the aggregate). This is the load-bearing fix: at zero estimable between-market dispersion the scheme returns **exactly today's per-market multipliers**, not the aggregate.

Closed-form shrinkage:

```
B_m       = tau^2 / (tau^2 + v_m)             # in [0,1], monotone in n_m
theta_hat = mu_anchor_m + B_m * (y_m - mu_anchor_m)
calib_m   = clamp(exp(theta_hat), 0.5, 1.5)   # K4 floor + envelope, asserted in a test
```

Population dispersion `tau` (3 markets only → never free-fit):

```
tau2_mom = max(0, var(y_m) - mean(v_m))
tau2     = min(tau2_mom, tau2_cap)
tau2_cap = (log(tau_mult_cap))^2,  tau_mult_cap = 1.07   # ±7% typical divergence, well below the 22-36% noise floor
```

If `tau2_mom == 0` (expected at current n), every `B_m → 0` and each market returns its anchor → **byte-identical to today's compute_calibrations()**.

### 3.3 Entry gates (anti-overfitting)

- A market contributes to `var(y_m)` only at `n_m ≥ min_n_unlock` (default 30) — a single lucky early win cannot inflate dispersion.
- Require `≥ 2` eligible markets before estimating `tau` at all.
- Below either gate, return today's multipliers unchanged.

### 3.4 Adversarial fixes folded in

1. **Anchor to existing K2 estimates, not the aggregate** (§3.2). Reduces to today's behaviour, not a regression.
2. **`tau_mult_cap` tightened 1.15 → 1.07.** With `var(y)` estimated on 3 points, the cap is the only real backstop; 7% sits well below the 22–36% documented noise floor.
3. **Loud fallback.** After the ledger read inside the hierarchical computer, assert `nrow(led) > 0` when the ledger dir exists and dispatch was requested; emit `cli_alert` when the function returns anchors due to an empty/failed read (the evidence documents `read_table("ledger")` resolving the wrong root and returning 0 rows). This distinguishes "inert because read failed" from "inert because tau2_mom==0".
4. **Daily-budget invariant test** (§3.6).
5. **Cross-market calib-spread health row** as a HARD prerequisite before any cell can activate.

### 3.5 Integration points

- `R/decide-calibration.R` — add `compute_calibrations_hier()` with the **same return contract** as `compute_calibrations()` (`list(aggregate, moneyline?, spread?, total?)`), so the per-bet lookup at `decide-pipeline.R:211-214` is unchanged. To avoid a double Parquet scan, pass the already-read ledger frame through rather than re-reading inside.
- `R/decide-pipeline.R:201` — dispatch only: call `compute_calibrations_hier()` when `betting$calibration$hierarchical` is TRUE, else `compute_calibrations()`. Lines `:211-226` are **unchanged** (the stake chain, K5, min_bet, kelly_raw, portfolio_lambda all untouched).
- `config/leagues.yml` — optional `betting.calibration` block; defaults reproduce today; ship `hierarchical: false` everywhere.
- `config/leagues.schema.json` — extend `betting.properties` (keep `additionalProperties: false`).

```yaml
betting:
  kelly_frac: { male: 0.10, female: 0.05 }   # UNCHANGED — per-(league,sex) Browne γ
  calibration:                                # NEW, optional; absent = today's K2 behaviour
    hierarchical: false                       # opt-in; flipped only by accrued evidence
    min_n_unlock: 30
    tau_mult_cap: 1.07
```

### 3.6 Daily-budget bypass — required fix (invariant gap)

`portfolio_optimise` computes `eff_stakes = kelly_frac × match_kelly_sum` using the **scalar** `kelly_frac`; it never sees `cands$calib` (or any per-market multiplier). The final `bet_amount` does apply per-market `calib` inside `min(kelly_frac × calib, ceiling)`. So when a per-market `calib` drifts toward 1.5, `Σ bet_amount` can exceed `daily_budget_frac × current_pool` — the governor is upstream of and blind to the per-market axis this component perturbs. This gap is small today (calib≈0.88–0.91) but the component's purpose is to make `calib` diverge.

**Required:** add a test asserting `Σ(kept bet_amount) ≤ daily_budget_frac × current_pool` under a fixture where one market's `calib = 1.5` and another's `= 0.5`. If the assertion can fail, push the per-market multiplier into the `eff_stakes` computation that feeds `portfolio_optimise` so the budget cap is measured against the same quantity that is actually staked. The component must not claim "daily budget cap untouched" without this.

### 3.7 Invariant compliance

| Invariant | Status |
|---|---|
| K1 (Beta-Binomial) | Preserved as the per-market likelihood; the per-market K1 estimate IS the anchor. |
| K2 (split by market) | Generalised: hard n≥100 cliff → smooth pooling anchored on the existing K2 multipliers; markets below `min_n_unlock` return their anchor (= today). |
| K3 (prior_weight=30) | Anchors each W/E near its market value before the hierarchy shrinks. |
| K4 (floor 0.5, [0.5,1.5]) | Re-imposed by an explicit, tested post-exp clamp. |
| K5 (kelly_ceiling 0.25) | Binds at `:224` exactly as before. |
| K6 (recompute each call) | Holds — closed-form, stateless, reads the live ledger. |
| Daily budget | **Gap fixed** per §3.6 (test + optional eff_stakes routing). |
| Leak-free / L1–L4 | Reads only settled ledger rows; never writes the ledger. |

## 4. Component B — Logit-scale probability recalibration (scaffold, default OFF)

### 4.1 Shape (grounded in the OOS evidence)

A 2-parameter `invlogit(a + b·logit(p))` (Platt-on-logit) is the maximum complexity the data supports — the only detectable error is a mild slope (0.909); isotonic/spline/3-param-beta overfit a step the oracle (-0.00002 Brier) says is not there. The layer is **exactly identity by default** (`a=0, b=1`), departs from identity only via **ridge shrinkage toward identity** as data accrues, and goes live for a cell only after the §4.4 gate passes on **two consecutive runs**.

### 4.2 Where it sits — at p-birth, K1 neutralised when active

Inject in `kelly_joint()` immediately after `p <- colMeans(R > 0)` (`decide-kelly.R:231`), **before** `assert_outcome_prob_coherent()` (:232), `ev` (:233), and the keep filter (:241). `kelly_raw` continues to solve from the **raw** return matrix `R` (:248). So recalibration governs the EV gate and the reported `p`, not stake magnitude (the joint optimiser owns magnitude). Reshaping `kelly_raw` (importance-reweighting posterior draws) is explicitly deferred.

```r
#' @noRd
recalibrate_p <- function(p, a = 0, b = 1, eps = 1e-6) {
  if (a == 0 && b == 1) return(p)                 # exact no-op fast path (the default)
  pc <- pmin(pmax(p, eps), 1 - eps)
  stats::plogis(a + b * stats::qlogis(pc))
}
```

### 4.3 No double-counting — REPLACES K1, enforced at runtime

The K1 stake multiplier and a logit slope `b<1` correct the **same** model-over-confidence signal at two different levels. They are made **mutually exclusive per (league,sex) cell**: when recalibration is active, force `cands$calib <- 1` and `calibs <- list(aggregate = 1)`.

**REQUIRED FIX (over the component design's checklist):** make this a **runtime assertion** in `decide-pipeline.R`, not a graduation checklist. Abort the decide run if, for a cell, `recal_active()` is TRUE **and** `compute_calibrations()` would still return any non-unit multiplier that gets applied. This matches how spread-sign-flip, invalid-input, and outcome-coherence are all runtime-aborted in this repo. A `cands$calib == 1 whenever recal_active()` regression test is a hard CI test.

Two consequences must be documented in the rule files (not silent):

- **Stake side-effect.** Forcing `calib` from ~0.88 to 1.0 is a **~+14% stake increase** for any activated cell, justified by a calibration correction the OOS evidence calls noise. The graduation checklist must list the live stake-delta as an explicit line item. (Alternative considered and rejected as more invasive: keep the K1 shrinkage and re-derive the K1 multiplier against the recalibrated `p` so the stake axis stays shrunk; at slope 0.909 the residual double-correction would be small, but it muddies the "one replaces the other" answer. The clean mutual-exclusion is preferred.)
- **Half-application.** An active cell sizes magnitude off the **uncalibrated** posterior (`kelly_raw` from raw `R`) while only the EV gate and reported `p` are recalibrated. Acceptable (the joint optimiser owns magnitude) but documented as a conscious inconsistency until the deferred importance-reweighting phase.

### 4.4 The fit — walk-forward, leak-free, ridge-to-identity

New file `R/decide-recalibration.R`. Penalised logistic of realised `win` on `logit(p)`, on settled rows whose match resolved **strictly before** the decided match (`match_date < as_of`, `as_of = run_date`), settled-only, frozen-`p` only — never re-derived from belief stores.

`fit_penalised(y, x, tau, centre)` maximises the binomial log-likelihood minus `0.5·tau·((a-centre[1])² + (b-centre[2])²)` (IRLS with a ridge in the normal equations, or `optim(method="BFGS")` — no new dependency). `tau = prior_weight / n` (prior_weight default 60, ≈2× K3): as `n→∞`, `tau→0` and it relaxes to the MLE; at n=200, `tau=0.3` leaves the map heavily pulled to identity. **A cell that passes the gate departs from identity gradually, never as an abrupt full-MLE switch** — this is the single most important safeguard over a naive Platt fit. Pooled coefficients shrink to `(0,1)`; per-market coefficients shrink to pooled with a K3-style partial-pool weight, and only fit per market at `n_market ≥ 100` (mirrors K2).

Two fit sources:

- `fit_source = "ledger"` (default): 393 settled rows, frozen placement `p`. Below the 500 gate floor — used for the audit trail, not gate power.
- `fit_source = "candidates"`: candidates ⋈ results via `compute_settlement()` (reuses the per-(sport,country) `tie_threshold`), n≈1951 — gives the gate power, but on the **wrong population** (87% spread, ~84% never-placed). Required: in the candidates branch apply the `match_date < as_of` boundary to the **candidate** rows explicitly; and any per-market verdict must be confirmed on **both** universes, with the kept-universe n (spread ≈83, years out) the binding constraint before a flip.

Refresh cadence: per `decide_league()` call (K6 cadence); `as_of = run_date` makes the leak boundary automatic.

### 4.5 Validation gate (human-in-the-loop; config records the verdict)

`recalibration_gate()` returns `list(pass, brier_delta, slope_p, n, n_consecutive)`. A cell is whitelisted only if ALL hold:

1. **Sample** — `n ≥ gate_min_n` (500) in the leak-free fit set.
2. **OOS improvement, same sign at ≥3 forward cut points** (today FAILS — sign flips at the 75% split).
3. **Material** — mean OOS Brier delta ≤ **-0.002** (two orders of magnitude above the oracle ceiling -0.00002, so noise cannot open it). This is a HARD floor, never loosened to chase a marginal cell.
4. **Slope evidence** — Wald `b==1` rejects at 0.05 AND the `b` CI excludes 1 (today: p≈0.046, CI [0.82,1.00] touches 1 → borderline FAIL).
5. **Hysteresis** — passes on **2 consecutive run_dates** before the cell leaves identity.

The gate runs offline (`scripts/0Nx_recal_gate.R` or a chunk in the report). On current data the flag stays `false` for every cell.

### 4.6 Coherence-abort — a live-decider HALT risk (required prerequisite)

`assert_outcome_prob_coherent()` (`decide-kelly.R:232`, def `:183`) `cli_abort()`s the **whole decide run** if any (market,line) group's outcome probabilities sum to > 1+1e-6. Platt-on-logit is per-outcome monotone and **not** simplex-preserving. Full 1X2 groups are handled by `renormalise_exhaustive_outcomes()`, but Lengjan commonly lists **home+away only** (partial sets) which cannot be renormalised and rely on the 1e-6 tolerance. A Platt fit with `b` far from 1 can push a 2-outcome group over the tolerance and **abort the live decider**.

**Required:** the gate that flips a cell to `enabled:true` MUST include a coherence stress test running `recalibrate_p + renormalise` on that cell's recent partial-outcome odds, confirming no group exceeds 1+1e-6; and `renormalise_exhaustive_outcomes` must cap **2-outcome** groups at sum ≤ 1 too, removing the dependence on the tolerance for partial sets. Failure mode documented as "enabling a cell can abort the live decider".

### 4.7 Persistence + provenance (required fix)

New accretive store `data/beliefs/recalibration/` (mirrors `fit_diagnostics`): `sport, country, sex, run_date, market, a, b, n, fit_through, gate_passed, is_identity`. Partitioned `c(sport, country, sex, run_date)`.

**Ledger-p provenance (gap B):** once a cell is active, the placer freezes the **recalibrated** `p` into the ledger (single `p` column). A later `compute_calibration()` over that cell would compute `Σ p` over recalibrated probabilities and show ~zero residual miscalibration, **masking** whether recal is still warranted, and corrupting the validation harness's `ledger_asof()` K1 reconstruction. **Required choice (document in rule files):** either (a) record the RAW `p` alongside recal `p` in a new ledger column so K1 reconstruction stays measurable, or (b) state that K1 is permanently retired for an active cell and calibration monitoring reads the audit store, never re-derives a K1 multiplier from recal-`p` ledger rows. The harness must be made consistent with whichever is chosen.

Also document (in `backtest.md` + `model-decide.md`): when recalibration is ACTIVE, `candidates.p` is the recalibrated value, not raw `colMeans(R>0)` — any backtest scoring `candidates.p` must know which it is reading; persist `is_recalibrated` / raw `p` so the raw frequency is recoverable.

### 4.8 Config

`config/leagues.schema.json` — add `recalibration` under `betting.properties` (schema FIRST; `betting` is `additionalProperties:false`). `config/leagues.yml` — `betting.recalibration: { enabled: false }` per cell. When OFF, the stake chain is byte-for-byte unchanged; when ON the only delta is `cands$calib` forced to 1.

## 5. Component C — Validation + live-experiment harness (read-only, off-CI)

### 5.1 Walk-forward replay→decide loop (leak-free validator)

New file `R/backtest-walkforward.R`, football-iceland only. For each cutoff `d`:

1. `seed <- as.integer(format(d, "%Y%m%d"))`.
2. `fit_league(..., fit_date=d, end_date=d, seed=seed, root=wf_root)` into an **isolated tempdir** — never `data/beliefs` (belief-store leak).
3. **Pre-slice** `odds[scraped_at <= as.POSIXct(d)+dhours(12)]` BEFORE `prepare_odds` — load-bearing, because `prepare_odds` has no upper `scraped_at` bound and its `slice_max(scraped_at)` would otherwise pick a post-cutoff scrape. **Regression-test this pre-slice** (a leaking validator falsely greenlights live changes).
4. `compute_calibrations(..., root = ledger_asof(d))` filtered to rows settled before `d` (documented approximation under the reschedule fallback).
5. `decide_league(..., run_date=d, root=wf_root, write=FALSE)`.
6. Settle matches in `(d, d+horizon]` via `compute_settlement()` with the per-(sport,country) `tie_threshold`.

Primary scoring = OOS log-loss / Brier and CLV, **not** PnL (PnL MDE is +22–36% ROI). `scripts/0Nb_walkforward.R` CLI; output `data/backtest/walkforward/` (gitignored). Run detached — each cutoff is a full Stan fit (hours for a season sweep).

### 5.2 CLV — FORWARD CAPTURE ONLY (required fix)

Post-hoc CLV reconstruction is **not viable**: measured ledger→odds join coverage is **10.7%** (date gap: odds store starts 2026-04-12, ledger goes back to 2025-04-07, only 69 of 420 rows in-window; plus team-name normalisation — the odds store keeps as-scraped Lengjan names, the ledger carries canonical federation names). A "primary" CLV metric over ~10% of rows is a biased, non-representative slice.

**Therefore:** drop the read-time historical `attach_clv()` / `check_clv_drift()` as a primary metric on the historical ledger. The prerequisite is a forward **closing-odds capture project**: persist the last pre-kickoff snapshot at settle time, **from the local `06_settle.R`** (where `sex` and canonical names are in scope — the odds store has no `sex` column and the normalisation map is per-sex, so capture-at-settle is the only path that avoids reproducing the ~11% wall). Apply `normalise_lengjan_team_names()` (per-sex inverse map) when joining, or capture before names are lost. Until forward capture accrues, the SPRT-on-CLV gate correctly reports "accumulating".

CLV key: match identity + `market` + `outcome` + `line`, **not `sex`** (odds store has no `sex`). `kickoff` from `schedules.kickoff_time` when present (it is a nullable column, frequently NA today → defaults to `match_date` end-of-day, a coarser proxy). Record the closing snapshot's lead-time; flag CLV for bets whose close is >Xh pre-kickoff as lower-confidence. NA-close → **dropped from the mean, never zeroed** (15% of matches have only one snapshot).

### 5.3 Paper shadow / A-B arm

New file `R/experiment.R`. `run_experiment_arm()` runs `decide_league()` with the candidate transform and `write=FALSE`, writing to an **isolated** store `data/experiments/approach=*/...` — **never** `recommendations` (so the placer's P1 dedup, which reads only `recommendations`/`ledger`, can never see shadow rows) and **never** under `data/decisions/`. Each arm perturbs exactly ONE factor with the colliding mechanism explicitly neutralised (recalib arm forces `cands$calib<-1`; per-market-kf arm vectorises `kelly_frac[market]` and must also route through `eff_stakes` per §3.6). A pre-registered `data/experiments/<label>/protocol.json` (label, methodology_hash, axis_owner, primary_metric=clv, n_min, edge_min, alpha, beta) is written BEFORE the run — no post-hoc metric shopping (the P=0.998 multiple-comparisons trap). Experiment writing is gated behind a local env flag; CI's `Rscript scripts/04_decide.R` runs the control path only.

### 5.4 Bayesian sequential graduation gate (SPRT on CLV)

New file `R/experiment-gate.R`. Decision statistic = **paired** CLV (Arm B − Arm A on the same fixture/odds), Normal-Normal posterior with a no-edge prior `theta ~ Normal(0, 0.02²)`; Wald SPRT on the CLV LLR (`mu1 = edge_min = 0.01`, `mu0 = 0`, sigma with a t/inverse-gamma small-n correction). Verdict ∈ {accumulate (n<n_min=30), graduate, abandon (sticky), continue}. Graduation requires **(CLV up) AND (OOS log-loss not worse) AND (capture_rate ≥ 0.7)** — multi-gate, never one number — and is an **operator action** (hand-promote with the K1/K2 double-count + stake-delta checklist), never an automatic config write.

The gate peeks on every settle, so only a sequential test bounds the type-I inflation the evidence quantifies (P=0.998 of a spurious >10% cell). **B's CI wiring is explicitly rejected** — the shadow arm is never in any workflow (CI writing under `data/` violates the non-atomic-write race and two enforcement tests).

### 5.5 Health hooks (read-only)

Add to `pipeline_health()`, each `safe()`-wrapped:

- `check_clv_drift()` — rolling mean log-CLV over forward-captured settled rows in a 21-day window; OK / WARN / FAIL (`clv_warn_logodds=-0.01`, `clv_fail_logodds=-0.04` as inline named constants). Low variance → fires in weeks, long before PnL would. (Active only once forward capture exists.)
- `check_experiment_progress()` — informational (OK/WARN, never FAIL): paired n vs n_min and current post_p ± CI.
- `check_calib_market_spread()` — the cross-market calib-spread row from §3.4, a HARD prerequisite for any hierarchical activation.

### 5.6 Isolation enforcement (required)

`tests/testthat/test-experiment-ci-isolation.R` must assert: (1) no CI workflow writes under `data/`; (2) the placer/autoplace never reads `data/experiments/`; (3) `data/experiments/` and any CLV store are **NOT prefix-matches** of any `read_table("ledger")` glob or `compute_settlement()` input path (mirroring `test-healthcheck-ci-isolation.R`'s literal-token guard), so settlement can never mutate a shadow row; (4) the CLV store is written ONLY from the local `06_settle.R`, derived-on-read from the immutable ledger, never a mutation of it.

## 6. Time-to-verdict (honest calendar)

| Path | Metric | Verdict horizon |
|---|---|---|
| Paper shadow arm, pooled CLV | CLV (low variance) | **weeks** (~50–150 paired bets at ~15–25/week) — the recommended live experiment |
| Per-market CLV graduation | CLV, per market | **~year+** (spread ~1.4 placed/week to 100 paired obs) |
| Recalibration gate (candidates source) | OOS Brier | has power now but wrong population; kept-universe binding constraint is **years** out |
| Per-market kelly/calibration divergence (kept) | ROI | **multiple seasons to decades** (spread n=500 ≈ 5.8y; 5% edge ≈ 48y) |

The user's few-thousand-ISK live-experiment budget is best spent on the CLV-instrumented paper shadow arm in §5 — it produces a defensible verdict in weeks. PnL-based per-market tuning is not reachable on this volume in any near-term horizon.

## 7. Decision points

See the plan's "Decision points" section — these are choices the user must make at implementation time (recalibration K1-handoff style, tau cap, fit source, CLV capture trigger, experiment-budget cap, gate thresholds).


## 8. The per-sport axis (added 2026-06-14)

> Status: design addendum. The §0 verdict ("NO demonstrable edge", written football-only) is **correct but narrow**. This section corrects the scope: the load-bearing heterogeneity axis in this system is **per-sport**, not per-market-within-football — and `kelly_frac` is *already* per-`(league,sex)` in `config/leagues.yml`, so the differentiation knob exists. All numbers below are verified against the live ledger (`read_table("ledger")`, 1,928 settled rows, span 2025-02-12 → 2026-05-31).

### 8.0 The corrected, honest picture

The original optimisation question was framed entirely on football_iceland (the only active-betting cell). That framing made the verdict *true* but *non-general*: football's break-even-and-noisy result does **not** describe the other two Iceland sports. The realised per-sport ledger edge, with correlation-aware (block-bootstrap-by-match, R≈8–10k) 95% CIs:

| cell (iceland, settled) | n bets / matches | point ROI | 95% block CI | excludes 0? |
|---|---|---|---|---|
| handball | 424 / 171 | **+11.1%** | [-1.85, +24.42]% | no (closest; P(ROI≤0)=4.7%, on the boundary) |
| basketball | 257 / 121 | **+1.2%** | [-15.7, +18.2]% | no |
| football | 407 / 226 | **+0.8%** | [-15.4, +19.0]% | no |

**The football finding stands and does not generalise.** Football is genuinely break-even and noisy (backtest -5.1% ROI on n=79, ranking last of 7 strategies). But handball is a *different distribution*: +11.1% realised, 61% hit vs 54.4% implied, equal-weight per-match ROI +21.3% [+10.5, +32.3] (excludes zero), and both the fair-odds null (one-sided p=0.028) and a vig-loaded null (p=0.0025) reject under block resampling. There is a real directional signal in handball that does not exist in football. Basketball sits with football: indistinguishable from zero, and indistinguishable from football.

So the correct conclusion is **not** "no edge, stop here" — it is "**no edge in football; a promising-but-unproven edge in handball; nothing in basketball**", and the right structural response is to differentiate **per-sport**, not per-market. This is the high-value path the football-only framing missed.

The directional handball signal is, however, **fragile and underpowered**, and must clear two hard gates before any sizing change. Both gates currently FAIL.

### 8.1 GATE 1 — Bookmaker attribution: is the handball edge Lengjan-bankable?

**Finding: NO. The handball headline was realised on books the forward pipeline cannot stake.** The forensic pass independently re-derived this two ways, and the live ledger confirms it:

- **EUR×150 conversion fingerprint** (re-run on the live ledger as part of this addendum): **417/424 (98.3%)** of settled handball-iceland rows divide by 150 onto a clean cent-precise EUR grid, carrying **+99,456 of the +99,880 PnL**. Only 7 rows (+424 ISK) are non-EUR-grid. Round-50-ISK (the Lengjan stake fingerprint) covers only 21%. The legacy importer `R/bets/import_gsheets.R` (commit `f08de2fe`) sets `EUR_ISK <- 150` for the foreign-book tabs; EpicBet/CoolBet transact in EUR, Lengjan in ISK.
- **`docs/runbooks/bankroll-and-extraction.md`** states it outright: "handball iceland (+99,880) was placed on the foreign books" and "`country == iceland` is NOT a Lengjan proxy."

**The Lengjan-bettable cell is MALE only.** `config/leagues.yml` maps only *Olisdeild karla* (male) with `team_names.female = {}` ("no women's handball on Lengjan currently"). The +11.1% is a cross-sex aggregate; the female cell — which the pipeline **cannot stake** — carries the strongest signal:

| handball iceland | n | ROI | 2026 (Lengjan era) |
|---|---|---|---|
| **male** (bettable) | 298 | **+7.5%** | **−31.8%** (−18,327 ISK, n=24) |
| female (NOT on Lengjan) | 126 | +19.4% | +31.1% (n=19) |

The bettable male cell's all-time ROI is +7.5% with a block CI of **[-9.4, +24.3]%, P(ROI≤0)=0.19 — not significant**, and dropping its top 5 matches turns it **negative**. Decisively, the only window where live Lengjan placement was even possible (2026) shows the bettable male cell at **−31.8%** (verified: −18,326.95 ISK on 24 bets). **The edge did not reproduce when the book changed.** System-wide, the legacy `pipeline` source (the Lengjan-automation proxy) is net **−8,161** across all leagues; the entire +97,530 system profit is in the manual/EUR `gsheets` source.

**Implication:** sizing handball up on the strength of +11.1% would scale real Lengjan money on an edge demonstrated only on books the pipeline cannot trade — and, where it *can* trade (male, 2026), the realised result is sharply negative.

### 8.2 GATE 2 — No forward/backtest validation exists for handball

The leak-free `candidates`/`recommendations` store begins `run_date = 2026-04-25` — **after** the basketball/handball regular seasons paused in late April. Confirmed coverage: handball = **30 candidate rows** across 4 run_dates (2026-04-27 → 2026-05-07); basketball = 89; football = 4,448. `bt_load_universe()` returns only **4 handball / 12 basketball** decide-time bets.

Therefore a **decide-time-odds backtest of handball model edge is structurally impossible today**, and there is no pre-2026-04-25 odds history to extend it backwards (a replay re-fits the model but cannot fabricate decide-time odds — `backtest.md`). Handball's edge lives **only** in the realised, cross-bookmaker, integrity-compromised ledger. The model's forward, Lengjan-native edge is **unverified**.

**The remedy is forward-only.** When handball resumes (autumn 2026), it must be re-validated on Lengjan via the §5 harness instrument — the paper-shadow arm + forward CLV capture + paired-CLV Wald SPRT — exactly the machinery this spec already designs (but which is **not yet built**: `R/clv.R`, `R/experiment.R`, `R/experiment-gate.R`, `R/backtest-walkforward.R`, `scripts/0Nb_walkforward.R`, `config/experiments.yml` are all missing). Minimum sequence:

1. **Pre-season:** build/enable the harness, generalise its football-only default to handball (confirm `compute_settlement()`'s handball `tie_threshold` is wired), and pre-register a `protocol.json` for a handball_iceland **male** paper-shadow arm (`primary_metric=clv`, `edge_min=0.01`, `n_min`, α/β).
2. **In-season:** run the control decider live at the **current** `kelly_frac = 0.05` (do not raise); each settle, the **local** `06_settle.R` captures the last pre-kickoff Lengjan snapshot to `data/clv/` (per-sex team-name map; male only); accrue paired CLV.
3. **Verdict:** Wald SPRT on pooled (handball, male) CLV. Graduate **only** on the multi-gate — **CLV up AND OOS log-loss not worse AND capture_rate ≥ 0.7** — and only by an operator hand-promotion with the K1/K2-double-count + live-stake-delta checklist. **Time-to-verdict ≈ 3–8 in-season weeks** at handball's peak cadence (117 settled bets in Oct 2025; median odds 1.90 < football's 2.02 → lower CLV variance → tighter SPRT), *provided the harness is live from the first fixture*.

### 8.3 Adversarial findings folded in (why the handball headline is fragile even before the gates)

Five independent lenses all returned `claim_holds=false`:

- **Effective-n / correlation collapse.** 424 bets are only 171 match-blocks (2.48 bets/match); Kish effective count ≈ 76 match-blocks, ≈ 53 winning blocks. The block SE (6.78pp) is ~18% wider than naive (5.72pp); the naive per-bet CI's near-significance is an artefact of ignoring within-match correlation. At this n the MDE (80% power) is ~19% ROI vs an observed 11% → achieved power ≈ 37%; ~500 matches needed.
- **Concentration.** Top 10 of 171 matches = 97.8% of PnL; two months (2025-03, 2025-10) supply ≈ 89,000 of 99,880; excluding the two best months collapses ROI to +2.0%.
- **Regime decay.** Every Iceland sport flips positive→negative 2025→2026 (handball +12.6%→+2.4%, basketball +5.3%→−10.0%, football +4.6%→−8.1%). Monotone decay is *not* statistically established (P≈0.47) — equally consistent with "one good spring + reversion." Crucially the headline is a **spring** phenomenon; the single prior **autumn** (2025) is the weakest block (+8.4%, model overconfidence +9.6pp) — and autumn-2026 is exactly the season we'd be forecasting. Model claims edge +11.6pp vs realised +6.7pp: discount the stated edge.
- **Obtainability.** Profitable turnover ran on low-limit recreational EUR accounts (max single stake ~30–75 EUR) now totalling ~1,136 EUR — the regime where scaling triggers stake caps / account-flagging. The runbook itself advises against building scrapers for these books ("no proven edge to exploit"). Lengjan's worse odds (a 4–8% margin haircut) would erode what little edge remains.
- **Data integrity.** 91/424 settled handball rows (21%, +40,624 PnL ≈ 41% of the headline) have **no joinable result** in `data/facts/results/` — immutable (L4) hand-entered gsheets outcomes, unverifiable. 10 of the 324 joinable rows are settled in contradiction with the (line, score) the ledger itself stores (e.g. a clearly-OVER total recorded as an under win). `compute_settlement()` would never produce these; recorded checkable PnL diverges from current-logic settlement by ~8,762 ISK.

None of these *disprove* a handball edge — the directional signal survives — but together they mean the +11.1% is far weaker evidence than the point estimate suggests, and every line of it points to the same remedy: **forward, Lengjan-native, book-clean validation before sizing.**

### 8.4 Reframed methodology priorities

The original spec's machinery is correct; only the **scope and ordering of its first real use** change.

1. **Per-sport `kelly_frac` / calibration differentiation is the high-value path** (supersedes the implication that all tuning is multi-season-deferred). The cross-sport ROI spread (+11.1% / +1.2% / +0.8%) is the largest, most interpretable heterogeneity in the system, and the knob (`kelly_frac` per-`(league,sex)`) already exists. **Handball is the prime candidate** — but gated, not granted.
2. **The harness's FIRST real job is handball-resume validation, not football tuning.** Phase 1 (walk-forward) is a calibration check; the *edge* verdict for handball comes from forward CLV. Generalising the harness's football-only default to handball, and wiring the local `06_settle.R` CLV capture for the handball **male** cell, is now the leading near-term deliverable — scheduled to be **live before the autumn-2026 first fixture**.
3. **Per-market-within-football remains unjustified** (every per-market CI straddles zero; MDE +22–36% vs observed +21.5%; live counts 21–31 vs the K2 bar of 100) — **and per-market `kelly_frac` stays forbidden** (double-counts the K2 calibration axis; §3.1). Market heterogeneity lives only on the calibration axis. The per-sport axis is where the differentiation budget should go.
4. **Basketball and football get no sizing change.** Football: pooled-(league,sex) calibration + harness only, as already specified. Basketball: no edge to size; revisit only on a resumed-season Lengjan track record.
5. **`kelly_frac = 0.05` for handball is the knob NOT to raise** until a handball **male** paper-shadow arm SPRT-graduates on pooled CLV under the multi-gate. The few-thousand-ISK live-experiment budget is the ordinary control-arm betting; the shadow arm is paper-only.

### 8.5 What would settle the handball question (and unblock sizing)

- A forward, **Lengjan-only, MALE** track record from the autumn-2026 first fixture: ≥50–150 settled bets / paired-CLV observations recorded against Lengjan's actual posted lines, with realised ROI / paired CLV clearing zero.
- A **paired-CLV Wald SPRT** graduation on pooled (handball, male) Lengjan CLV (`edge_min=0.01`) — leak-free, per-bet, immune to the effective-n collapse that plagues realised PnL — combined with the multi-gate.
- Evidence the prices **hold at scale** on Lengjan (posted limits vs recommended stakes at the current `kelly_frac`), since the historical edge lived at recreational foreign-book stakes.
- A bookmaker column (or partition) so future handball ROI is separable Lengjan-vs-foreign — the single ledger change that makes "is this edge forward-bankable" answerable without fingerprinting.

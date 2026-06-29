# Design — (S,D) lean-Gaussian football model on the backtest experiment

**Date:** 2026-06-29
**Status:** Phase 1 design, approved for spec review
**Scope:** football_iceland (male + female), 2026 season

## 1. Context & motivation

The repo once carried an experimental family of football models that put a
**continuous bivariate likelihood on `(S, D) = (total goals, signed goal
difference) = (home+away, home−away)`** instead of a count model on each team's
goals. The means reuse the standard per-team offence/defence parameters; only
the outcome space and likelihood differ. The variants are archived under
`_legacy/sports/football/iceland/Stan/` (a `gaussian_v1/v2` ladder plus a
Student-t ladder `direct_SD`, `SD_v3..v6`, `totalgoals-v-goaldiff`) with the R
audit harness at `_legacy/sports/R/backtest/student_t_audit/`.

The April-2026 "Stan Audit Phase 4" benchmarked these against the production
bivariate Poisson (BVP) and returned a **negative verdict**: BVP won on both
1×2 (1.1%) and totals (2.1%) *in-sample calibration*, so production football
stayed on `Stan/football_iceland/bivariate_poisson_no_inflation.stan`.

That verdict scored calibration against **outcomes**, not against the **market**
on the **bettable slate** — which is the question this experiment actually cares
about. We revisit the model through the existing walk-forward / model-vs-market
harness, and — before fitting anything — we re-examined the covariance, which is
where the old variants were weakest.

## 2. Diagnostic findings (data-story for the covariance)

A read-only second-moment diagnostic on 2021–2026 Icelandic results (conditional
means from a per-season Maher independent-Poisson fit; script committed at
`docs/reports/2026-sd-gaussian-backtest/diagnostic.R`) established:

The **mean-induced Poisson backbone holds**. For independent
`hg~Poisson(λ_h)`, `ag~Poisson(λ_a)`:
`Var(S)=Var(D)=E[S]=λ_h+λ_a` and `Cov(S,D)=E[D]=λ_h−λ_a`. Empirically all three
are clean lines through the origin in the model means:

| quantity | male | female | Poisson null |
|---|---|---|---|
| `φ_S = Var(S)/E[S]` | 0.84 | 0.91 | 1.0 |
| `φ_D = Var(D)/E[S]` | 0.89 | 0.90 | 1.0 |
| `ψ  = Cov(S,D)/E[D]` | 0.89 | 1.34 | 1.0 |
| resid corr(S,D) | 0.12 | 0.11 | — |
| kurtosis `z_S / z_D` | 3.2 / 3.6 | 3.1 / 3.2 | 3.0 |

Key reads:

- The conditional dispersion is **slightly below Poisson** (`φ≈0.85`), not above.
  (The unconditional `Var(S)/E[S]≈1.24` was entirely between-match spread of the
  means.) So the covariance should be **mean-scaled with a mild shrinkage**, not
  constant and not overdispersed.
- The old `gaussian_v1` was genuinely mis-specified: a **constant** `σ_S≈1.72`
  (Var≈2.96) is too wide for a 2-goal-expectation game and about right for a
  4-goal one — a directional totals miscalibration. Tying variance to `E[S]`
  fixes it.
- `ψ` is clearly **per-sex** (0.89 vs 1.34); `φ_S≈φ_D` (justifies a shared `φ`).
- Tails are **mild** (kurtosis 3.1–3.6, heavier on the margin for men); a plain
  Gaussian is defensible (consistent with the legacy fitted `ν≈12`).

## 3. Goal & hypothesis

Does a continuous Gaussian on `(S, D)` with a **mean-induced, mean-scaled
covariance** beat (a) the de-vigged Lengjan line and (b) the production BVP,
**out-of-sample on the 2026 bettable slate** — especially on **totals**, where
BVP's implicit `φ=1` overstates variance (data `φ≈0.85`)? That `φ<1` shrinkage is
the concrete mechanism by which the model could win over/under; the
continuous-approximation/discreteness penalty is what could sink it. OOS market
skill settles it.

## 4. The model — `Stan/football_iceland/2d_gaussian_sd.stan`

Derived from `bivariate_poisson_no_inflation.stan` by copying the **`data`,
`parameters` (off/def random walk, per-team home advantage, `mean_log_goals`),
and `transformed parameters` (offense/defense RW) blocks verbatim**, dropping
BVP's `alpha_mu3`/`beta_mu3_strength_diff` correlation parameters, and replacing
the likelihood. Keeping the `data` block identical to BVP means `prepare_data()`
feeds it unchanged.

**Means (identical to BVP).** Per match:
```
λ_h = exp(mean_log_goals + off_h − def_a)        # off_h includes home advantage
λ_a = exp(mean_log_goals + off_a − def_h)
E[S] = λ_h + λ_a ;  E[D] = λ_h − λ_a
```
The log link guarantees `λ>0`, hence `E[S]>0` and `|E[D]|<E[S]` — positive-
definiteness is structural.

**Covariance (2 new scalar parameters `φ`, `γ`).**
```
σ²(n) = φ · E[S]_n              # Var(S)=Var(D), shared φ (lean choice)
ρ(n)  = tanh(γ · E[D]_n / E[S]_n)
Σ_n   = σ²(n) · [[1, ρ(n)], [ρ(n), 1]]
```
The `tanh` keeps `|ρ|<1` for any mismatch (guards the female `ψ/φ≈1.47` PD risk).
`γ=1` recovers the exact Poisson identity `ρ=E[D]/E[S]`.

**Likelihood.** `obs_SD[n] ~ multi_normal(mu_SD[n], Σ_n)` on integer `(S, D)`
(naive continuous approximation; the discreteness cost is part of what we
measure). `Σ_n` is match-specific, so the likelihood loops over matches (not the
single-`Σ` vectorised form of the legacy `gaussian_v1`).

**Priors (from §2).** `φ ~ Normal(0.88, 0.1)` truncated `>0`;
`γ ~ Normal(1.1, 0.4)`. The diagnostic measures `ψ = Cov(S,D)/E[D]`; since
`Cov(S,D) ≈ γ·φ·E[D]` for small `ρ`, that maps to `γ ≈ ψ/φ ≈ 1.05` (male) /
`1.47` (female), which the `γ` prior spans. All off/def/RW/home/`mean_log_goals`
priors are copied from BVP unchanged. `φ`, `γ` are estimated per fit → **per-sex
automatically** (one fit per sex).

**`generated quantities`.** Draw continuous `(S, D)` per `pred` match, discretise
to integer `goals1_pred`/`goals2_pred` with the parity + non-negativity nudge
(ported from `totalgoals-v-goaldiff`: enforce `S,D` same parity, `|D|≤S`,
`hg=(S+D)/2`, `ag=(S−D)/2`). This makes the model emit exactly the
`goals1_pred`/`goals2_pred` that `extract_posteriors()` already consumes — so
`beliefs_latest` and every downstream market probability are produced with **zero
changes to the decide/score chain**.

**Net:** same mean model, ~same parameter count, likelihood family swapped. The
OOS skill delta isolates the Gaussian-`(S,D)`-vs-bivariate-Poisson question.

## 5. Harness integration (minimal)

- **`bt_wf_sd_decide(stan_model)`** — new closure in `R/backtest-walkforward.R`,
  mirroring `bt_wf_default_decide` but calling `fit_league(league = <football_
  iceland league list with `stan_model` overridden to the (S,D) path>, …)`. No
  engine changes — `bt_walkforward()` already injects `decide_fn`. A small helper
  builds the league list with the overridden `stan_model`.
- **Driver:** add a `--model {bvp|sd}` flag to `scripts/0Nb_walkforward.R` so the
  existing orchestration is reused; `sd` selects `bt_wf_sd_decide`, re-fit mode
  only (no saved extracts exist for the (S,D) model).
- **Season = 2026 only** for the bettable comparison: there is **no pre-2026
  Lengjan odds history** (hard constraint, `.claude/rules/backtest.md`), so the
  market-skill backtest cannot extend earlier. Cutoffs = **per-round** completion
  dates; **both sexes**.
- **Cost:** the BVP arm runs in cheap **REUSE** mode over the same cutoffs; the
  `(S,D)` arm **re-fits each cutoff** (rounds-so-far × ~minutes/fit × 2 sexes ≈ a
  few hours). **Run detached** (`nohup … & disown`). A single bad fit is skipped
  by the harness's existing `tryCatch`, never aborting the sweep.

## 6. Outputs & decision rule

- `bt_devig()` → `bt_skill(by = "market")` + `bt_skill_ci()` (match-clustered
  bootstrap) for **`(S,D)` vs market** and **BVP vs market**, plus a head-to-head
  on the shared bettable slate, broken out by **moneyline / totals / spread**.
- Report: `docs/reports/2026-sd-gaussian-backtest.qmd` — the diagnostic from §2,
  the model definition, and the OOS skill tables + calibration plots.
- **Promote-to-Phase-2 rule (stated up front):** the `(S,D)` model advances only
  if it shows **non-negative market skill on totals with a CI lower bound not
  worse than BVP's** — a real, directional totals improvement, not noise. A null
  or negative result is a publishable verdict and Phase 2 does not run.

## 7. Testing & verification

- `(S,D)` model **compiles** via `cmdstan_model()`; a **smoke fit** (few iters)
  produces `goals1_pred`/`goals2_pred` → `extract_posteriors()` returns a valid
  `beliefs_latest` (schema check).
- `bt_wf_sd_decide` returns candidates with finite `p`; the existing walk-forward
  **leak-free guards G1–G9 apply unchanged** (same isolated-tempdir fit path).
- **PPC on a full fit before trusting the sweep:** recovered posterior `P(draw)`,
  `Var(S|E[S])` slope (≈ `φ≈0.88`), and `ρ` vs `E[D]/E[S]` match the §2
  diagnostic.
- Tests live in `tests/testthat/test-backtest-sd.R` (or extend
  `test-walkforward.R`); a fake `decide_fn` exercises the orchestration without
  Stan, consistent with the existing harness tests.

## 8. Scope boundaries

**In Phase 1:** one model (lean Gaussian, shared `φ`, `tanh`-bounded `γ`), 2026
season, both sexes, the harness hook, the report, the verdict.

**Out of Phase 1 (Phase 2, gated on a positive verdict — separate spec):**
generalise the harness to carry both models first-class; **persist `(S,D)`
extracts** so it REUSE-replays cheaply; add a standing `(S,D)`-vs-BVP-vs-market
panel to `docs/dashboard/experiment.qmd`. Also deferred: Student-t `ν`, separate
`φ_S`/`φ_D`, the richer per-team-σ / match-dependent-ρ forms, a Gaussian-copula
decoupling of marginals from dependence, and any production swap.

**Optional secondary (only if the 2026 bettable n is too thin for a usable CI):**
an **outcome-only** calibration arm (`bt_oos_scores`, no odds) on pre-2026
seasons — costs extra `(S,D)` re-fits but adds calibration power without market
data. Not core; decide after seeing the 2026 CI width.

## 9. Risks & open questions

- **Sampler geometry.** The `(S,D)` Gaussian over the RW funnel may need
  `adapt_delta` tuning; the harness already skips a cutoff whose fit trips the
  divergence gate, so a few skipped cutoffs degrade gracefully.
- **Discreteness.** Treating integer `(S, D)` as continuous is the known weakness
  the April audit flagged; measuring its OOS cost is the point. A continuity-
  corrected likelihood is a Phase-2 refinement, not Phase 1.
- **Thin 2026 sample.** Per-round cutoffs over a partial 2026 season give a
  modest match count; the match-clustered CI (`bt_skill_ci`) is the honest guard,
  and the optional outcome-only arm (§8) is the fallback for power.
- **Mean estimate in the diagnostic** used a static per-season Maher fit, not the
  RW means the real model uses; the *shape* (Var ∝ E[S], Cov ∝ E[D]) is robust to
  this, but the exact `φ`/`γ` priors are weakly informative and the data will
  move them.

## 10. File-level change list (for the implementation plan)

1. `Stan/football_iceland/2d_gaussian_sd.stan` — new model (derive from BVP).
2. `R/backtest-walkforward.R` — `bt_wf_sd_decide()` + league-override helper.
3. `scripts/0Nb_walkforward.R` — `--model {bvp|sd}` flag.
4. `docs/reports/2026-sd-gaussian-backtest.qmd` — comparison report.
5. `docs/reports/2026-sd-gaussian-backtest/diagnostic.R` — the §2 diagnostic
   (committed with this spec).
6. `tests/testthat/test-backtest-sd.R` — compile/smoke + decide-closure + leak.

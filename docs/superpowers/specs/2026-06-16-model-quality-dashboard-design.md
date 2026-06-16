# Model-Quality Experiment Dashboard — Design

**Date:** 2026-06-16
**Status:** Design (awaiting review → implementation plan)
**Author:** Brynjólfur Gauti Jónsson (with Claude)
**Supersedes/extends:** `docs/reports/2026-backtest.qmd` (two-arm backtest report), `.claude/rules/backtest.md`

---

## 1. Motivation & framing

The existing `2026-backtest.qmd` answers a retrospective question — *did the kept
bets make money, and where*. This project reframes the whole thing around the
user's actual goal, stated verbatim:

> *"Can I make a profit on those [Lengjan's] bad odds as an experiment in
> statistical modelling."*

This is a **controlled experiment**, not a P&L report. The governing philosophy
is an *optimal Bayesian model + decision process + self-updating bankroll
dynamics* — **not** betting-specific nuance. Lengjan is a state-run monopoly
(lottery-like): it posts odds **late** and **rarely moves them**. That single
fact reshapes the design twice over (see §3 and §6).

The dashboard instruments the experiment's hypothesis chain end to end:

> **optimal Bayesian model** *(is it right? → calibration / PIT / score-distribution)*
> → **soft static line** *(where is it wrong? → market-calibration / bias map)*
> → **model sharper than the line** *(model-vs-market skill, by stratum)*
> → **+EV** → **self-updating Kelly/bankroll** *(is the risk model honest? → bankroll-side calibration)*
> → **realised profit** *(the slow lagging confirmation)*

The **leading indicators** (model + market calibration, skill) accumulate over
*every match and every priced fixture*, so they reach informative precision far
faster than realised PnL (placed bets are in the tens). The **model-misspecification
diagnostics are the experiment's improvement engine**: they say *how to make the
model sharper*, which widens the edge over the soft line. The dashboard makes
that loop visible and self-updating.

## 2. Goals & non-goals

**Goals**

- A new, growing **dashboard** overview (`docs/dashboard/`) housing the
  model-quality experiment, **self-updating as new results land** (one command /
  CI re-render).
- Break every model-side diagnostic out by **sex × division × market** (the
  user's explicit ask), on the arm where the data actually supports it.
- Surface **trends/signals that hint at model-form changes** — PIT, score
  distribution, stratified calibration, model-vs-market disagreement — each
  mapped to a *named* Stan-model fix.
- Replace the (inapplicable) CLV idea with a **line-softness / market-calibration**
  diagnostic: where is the static monopoly line biased, i.e. where are the
  "bad odds" to exploit.
- An **evidence-gated OOS bake-off** scaffold so a flagged defect becomes a
  falsifiable model change validated out-of-sample before adoption.
- **Bankroll-side calibration** dials testing the self-updating-bankroll thesis.

**Non-goals**

- Betting-specific tactics (line shopping, multi-book arbitrage, staking
  heuristics beyond the existing joint-Kelly process).
- A live/real-time web app. "Easily updated" = a single recompute+render step
  (local or CI), not streaming.
- Division-level **PnL** on the money arm — structurally impossible (see §3).
- Re-deriving statistics in browser JS. All statistics stay in R (§4).
- Backfilling historical CLV (not viable; the static line gives none anyway).

## 3. Data reality (the binding constraints)

From a full inventory of the parquet stores (`data/`):

1. **Two arms, very different weight.**
   - *Money arm* (stored-decision PnL, `bt_load_universe(strategy="kept")` →
     `bt_run`): football-iceland-2026 = **67 settled male bets + 2 female**.
     Fixed-era (post 2026-05-14) ≈ **43 male + 1 female**.
   - *Model arm* (walk-forward calibration + model-vs-market): every *candidate*
     and *match* contributes — 1–2 orders of magnitude more data.
2. **`division` is absent from `candidates` / `recommendations` / `ledger`.**
   It exists natively in `results`, `schedules`, and
   `beliefs/extracts/.../predicted_matches.parquet`. Therefore:
   - **Money-arm division breakdown is out** (no column + tiny n + a lossy
     Lengjan-vs-federation name join that mislabels ~52% of moneyline fixtures).
   - **Model-arm division breakdown is clean**: `predicted_matches` carries
     `division` on the *federation-name* key, *before* the Lengjan odds join can
     mangle names. Division is attached at belief-reconstruction time (§7,
     Phase 2), sidestepping the `team_names`-map mismatch entirely.
3. **De-vigging is fully available.** All 168 priced football fixtures carry a
   complete 3-way moneyline book — `q_market` is computable for every one;
   completeness does not vary by division.
4. **The underused asset:** `predicted_matches.parquet` is the **full joint
   posterior-predictive score distribution per match** (4000 draws binned to
   `(home_goals, away_goals, count)`, with `division`, `fit_date`, `sex`).
   Today it is consumed *only* to reconstruct beliefs for REUSE-mode
   walk-forward. It is the substrate for the most actionable diagnostics in §6,
   and they are **pure functions of it + `results`** — no Stan, self-updating on
   render.

**Sample-size stance (user is a statistician):** do *not* gate/hide thin cells.
Show every pre-registered stratum with its uncertainty; let the reader weight a
noisy cell appropriately. Hierarchical shrinkage is offered as an *optional
smoothed view*, not a gatekeeper. The sin to avoid is hiding a suggestive
pattern, not showing one with wide intervals.

## 4. Architecture — decoupled R → JSON → Quarto dashboard

The repo already separates **compute (R)** from **presentation (JS front-end)**
via a JSON contract for the public site (`R/publish-*.R` → `data/publish/*.json`
→ metill-platform). This design reuses that pattern so the front-end is
**swappable** without touching statistics.

```
data/beliefs/extracts/.../predicted_matches.parquet   # joint posterior-predictive (underused asset)
data/{facts/results, decisions/*, facts/odds}         # existing stores

R/  (ALL heavy stats — reuse + extend the bt_* engine)
  backtest-metrics.R      EXTEND: Murphy REL/RES/UNC; `by=` on bt_skill_ci
  backtest-pit.R          NEW: randomised PIT, score-distribution, scoreline residual grid
  backtest-marketcal.R    NEW: line-softness (de-vig Lengjan as a forecaster), bias map, line-stability
  backtest-recalib.R      NEW: hierarchical recalibration (lme4) → shrunk per-stratum (CITL, slope) + shrunk skill
  backtest-bankroll.R     NEW: realised-vs-predicted return variance / drawdown dials
  backtest-divisions.R    NEW: attach division (+ round) to the model-arm universe from extracts
  backtest-walkforward.R  EXTEND: emit per-match OOS predictive draws; two-model bake-off

R/dashboard-export.R      NEW: compute every diagnostic → tidy JSON
scripts/0Nd_dashboard.R   NEW: entry point — recompute + export + render (read-only; CI-safe)
scripts/0Nb_bakeoff.R     NEW: detached two-model OOS bake-off (fit-mode; not on render)

data/dashboard/*.json     NEW: the data contract — stratified, tidy, front-end-agnostic
data/backtest/bakeoff/    NEW (gitignored): stored bake-off results read by the lab page

docs/dashboard/experiment.qmd   NEW: Quarto `format: dashboard`; R-native small-multiples + OJS slicing
```

**Why Quarto `format: dashboard` (not a bespoke JS app):** the deliverables are
dense statistical graphics (consistency-banded reliability diagrams, PIT
histograms, scoreline residual heatmaps, shrinkage forest plots) — far easier
and more *correct* in R/ggplot than reimplemented in d3/observable-plot. For
trend-spotting across ~30 strata, **small multiples beat click-one-filter-at-a-
time**. Quarto's dashboard format gives real card/page/tabset layout; OJS adds
live filtering only where it helps. Reuses the user's Quarto fluency + the
render-to-update cadence already in CI. The JSON seam keeps a future JS app a
cheap swap (it reads the same `data/dashboard/` contract).

**Update path:** `Rscript scripts/0Nd_dashboard.R` → recompute from parquet →
re-export JSON → re-render the dashboard. Wire into CI weekly like the existing
report, or run locally. **Read-only on the money path** (no ledger writes;
CI-safe against the local placer — a `tests/testthat/` isolation test enforces
it, mirroring `test-healthcheck-ci-isolation.R`).

**Conventions** (per `.claude/rules/r-conventions.md`, `r-claude-ergonomics.md`):
`#' @export` roxygen on public `bt_*`/`dashboard_*` fns; base pipe `|>`;
`here::here()`; `metill::theme_metill()` ggplot base; `ragg::agg_png()` for any
saved plot; testthat-3 TDD mirroring `test-backtest-metrics.R`; non-ASCII via
`\uXXXX` (Icelandic labels at the publish/dashboard boundary only — internal
schemas stay English).

## 5. Dashboard pages = the hypothesis chain

Six pages, each growing independently (add a card / export field without
touching the others):

1. **Experiment status** — cumulative scorecard. Headline model-vs-market skill
   + match-clustered CI; leading indicators (model & market calibration summary,
   skill) vs lagging (realised PnL with variance). The glanceable top.
2. **Is the model right?** — calibration, randomised PIT (total-goals + Skellam),
   score-distribution (the quantified *"under-predicts draws by X%"* line),
   scoreline residual heatmap — stratified.
3. **Where is the line soft?** — market calibration / bias map (favourite–longshot,
   draw, over/under), disagreement → who's-right, line-stability monitor —
   stratified. *The "bad odds" edge map.*
4. **Am I sharper than the line?** — model-vs-market skill by sex × division ×
   market, raw and hierarchically shrunk, with CIs / `P(edge>0)`.
5. **Is the risk model honest?** — bankroll-side calibration dials (realised vs
   predicted return variance, drawdown).
6. **Model-update lab** — OOS bake-off results when a model variant is under
   test; the evidence gate.

## 6. Diagnostic catalogue

Each diagnostic below states **what it computes**, the **model-defect it
indicts**, its **R home**, and **stratification**. Markets resolve identically
at decide- and settle-time (`build_return_matrix` / `compute_settlement`):
moneyline 3-way, spread 3-way (handicap tie = draw), total 2-way.

### 6.1 Randomised PIT histograms — the omnibus misspecification compass *(page 2)*

- **Computes:** per match, the randomised Probability Integral Transform of the
  observed count against the posterior-predictive draws (Czado–Gneiting–Held
  2009, valid for *discrete* outcomes):
  `u = F(k-1) + V·[F(k) − F(k-1)]`, `V ~ U(0,1)`, `F` the empirical predictive
  CDF from the draws. Computed on three marginals: **total goals** `h+a`,
  **goal difference** `h−a` (Skellam), and **home / away separately**. Season
  histogram of `u`; KS/χ² uniformity test as a one-number summary (the *shape*
  is the actionable artefact, not the p-value).
- **Indicts:** U-shaped → predictive too narrow / **under-dispersed**
  (→ Negative-Binomial margins or a `λ₃` correlation term); ∩-shaped → too wide
  (→ RW innovation variance / priors too loose); sloped/asymmetric → **bias**
  (→ home-advantage form). Total-goals PIT indicts the intensity/dispersion law;
  Skellam PIT indicts home-advantage + the draw mass; per-side PIT indicts
  attack/defence asymmetry.
- **Leak-free:** computed on the **as-of** predictive — for each match, the most
  recent extract `fit_date` *strictly before* `match_date` (mirrors the
  walk-forward freshness discipline; avoids in-sample optimism).
- **R home:** `R/backtest-pit.R::bt_rpit(draws, y)` (one marginal) +
  `bt_pit_table(predicted_matches, results, marginal, by)`.
- **Stratify by:** sex, division, market-relevant marginal.

### 6.2 Score-distribution checks — draw deficit, total-goals law, scoreline residuals *(page 2)*

- **Computes:**
  - **Predicted vs observed draw rate.** Predicted = mean over matches of
    `P(h=a)` from the draws; observed = realised draw indicator. Report
    `Δ = observed − predicted` with a binomial/bootstrap CI. Empirical anchors:
    observed **18.1% male / 13.8% female**, **0–0 ≈ 2.7–3.0%**.
  - **Total-goals distribution:** observed histogram of `h+a` overlaid on the
    pooled predictive — over/under-dispersion shows as a fatter/thinner tail.
  - **Goal-difference (Skellam) distribution:** observed vs predicted; the spike
    at 0 *is* the draw problem, flanking asymmetry is home-advantage error.
  - **Exact-scoreline residual heatmap:** observed − predicted frequency on the
    `h × a` grid. **Off-diagonal vs diagonal structure separates a *correlation*
    problem (→ Dixon–Coles τ / `λ₃`) from a *marginal* problem (→ dispersion).**
- **Indicts:** the current model is `bivariate_poisson_no_inflation` — the name
  says coupling/inflation is *off*. A persistent draw deficit is the canonical
  signal for Dixon–Coles low-score correction or a bivariate-Poisson `λ₃`
  (Karlis–Ntzoufras 2003). This is the diagnostic family that maps most directly
  to a Stan edit.
- **R home:** `R/backtest-pit.R::bt_score_dist(predicted_matches, results, by)`
  returning draw-rate gap, total-goals law, Skellam law, scoreline-residual grid.
- **Stratify by:** sex, division.

### 6.3 Stratified-by-covariate reliability — calibration error as a residual surface *(page 2)*

- **Computes:** model-`p`-vs-realised-frequency calibration (extends
  `bt_calibration`), **bucketed** by the covariate cuts below, each with
  **Jeffreys** (not Wald) bin intervals and **Bröcker–Smith consistency bands**
  (simulate `y ~ Bernoulli(p̄_k)` under the perfect-calibration null; shade the
  5–95% band so small-n wiggles are visibly within noise). 5 equal-count bins in
  thin strata.
- **Cuts → the fix each one points at:**
  | Bucket | A systematic slope here motivates… |
  |---|---|
  | favourite/underdog (`p` bins) | over/under-confidence → over-dispersion term or logit recalibration |
  | **early vs late season** (`round` / `match_date` rank) | **the prior / RW initial variance** — nothing else tests this |
  | low vs high total-goals regime | the Poisson/dispersion structure |
  | home vs away outcome | home-advantage parameter form |
  | division / team strength | a mis-shrunk pooling level → hierarchical-variance change |
- **R home:** extend `R/backtest-metrics.R::bt_calibration` with consistency
  bands + Jeffreys (`bt_calibration_bands`); buckets via `by=` on attached
  covariate columns (§7 attaches `division`, `round`, predicted-total bucket).
- **Stratify by:** the covariate cut + sex.

### 6.4 Line-softness / market-calibration — the "bad odds" edge map *(page 3, replaces CLV)*

CLV is inapplicable: a static, late, non-moving monopoly line gives ≈0 closing
value. The exploitable structure is **persistent bias**, not movement.

- **Computes:** treat the **de-vigged Lengjan line** (`q_market` from `bt_devig`)
  as a *competing forecaster* and ask where it is miscalibrated:
  - **Market reliability:** `q_market` vs realised `y` (is the line itself
    calibrated?), same bands/intervals as 6.3.
  - **Favourite–longshot bias:** binned `q_market` vs realised — does the line
    systematically over-price favourites / under-price longshots?
  - **Draw bias:** moneyline draw outcome — mean `q_market` vs realised draw rate.
  - **Over/under skew:** total market — mean `q_market(over)` vs realised over rate.
  - **Disagreement → who's-right, *stratified*:** extend the existing
    disagreement-band table (`gap = p − q_market`) with `by = sex/division/market`.
    *If the market beats the model in a regime where they disagree, that regime
    is a model weakness with an external second opinion — independent
    corroboration of a PIT/score-distribution finding.*
  - **Line-stability monitor:** from the `odds` store, per
    `(match, market, line, outcome)` count distinct prices across pre-kickoff
    snapshots → quantify *how often Lengjan actually moves a price*. Confirms the
    static-line hypothesis with data rather than assuming it.
- **R home:** `R/backtest-marketcal.R` (`bt_market_calibration`,
  `bt_market_bias`, `bt_line_stability`); disagreement-`by` extends the qmd
  pattern into `bt_disagreement(mkt, by)`.
- **Stratify by:** sex, division, market.

### 6.5 Model-vs-market skill, hierarchically shrunk *(pages 1 & 4)*

- **Computes:** `bt_skill(mkt, by = c("sex","division","market"))` for the raw
  per-stratum Brier/log-loss skill, plus **`by=` added to `bt_skill_ci`** so each
  stratum gets a match-clustered bootstrap CI (currently CI pools all rows — see
  §7). Then a **hierarchical** view (optional smoothed layer, not a gate):
  - **Recalibration:** mixed-effects logistic
    `y ~ logit_p + (1 + logit_p | sex/division)` (lme4 — installed, render-fast) →
    **shrunk per-stratum (calibration-in-the-large, slope)** with intervals.
    Slope < 1 ⇒ overconfident (shrink extreme `p`); CITL ≠ 0 ⇒ stratum-mean bias
    (→ missing division intercept / insufficient pooling).
  - **Shrunk skill:** a hierarchical model on the *per-match paired Brier
    difference* `d_i = BS_model − BS_market` with nested-normal random effects →
    shrunk per-stratum skill + **`P(edge > 0)`**. lme4 (installed) for the
    render-fast point+interval; an **optional `brms` variant behind a flag** for
    the full posterior (the user's house Bayesian tool — gives `P(edge>0)`
    directly; requires install + compile, so cache the compiled model and keep
    lme4 the default render path).
  - **Murphy decomposition** of each stratum's Brier into REL/RES/UNC. **Lead
    division tables with the skill score, never raw Brier** — UNC (base-rate
    variance) differs by cell, so raw Brier is not cross-stratum comparable;
    skill is difficulty-normalised by construction (model and market face the
    same UNC per cell).
- **Multiplicity:** the hierarchy *is* the control (Gelman–Hill–Yajima) — thin
  cells shrink toward their parent, deflating spurious extremes. **Pre-register
  the full sex × division × market grid; show every cell every render; label
  per-cell results exploratory.** Report `P(edge>0)` rather than corrected
  p-values.
- **R home:** extend `R/backtest-metrics.R` (Murphy, `by=` on `bt_skill_ci`);
  new `R/backtest-recalib.R` (hierarchical recalibration + shrunk skill).

### 6.6 Bankroll / decision-side calibration *(page 5)*

- **Computes:** is the realised risk of the Kelly/self-updating-bankroll process
  consistent with what the model implied? From `predicted_matches` draws +
  `ledger`:
  - **Realised vs predicted return variance.** Each posterior draw → each placed
    bet's win/lose (via the market resolution rule) → per-bet / per-match-day
    implied P&L distribution. Standardise realised `z_t = (pnl − E[pnl]) / sd[pnl]`
    and test mean-0/unit-variance/tail calibration. *This is a PIT on the
    bankroll's own return forecast.* Realised variance > predicted ⇒ Kelly
    fraction implicitly too aggressive (under-modelled bet correlation) even if
    probabilities are calibrated.
  - **Drawdown vs predicted drawdown.** Simulate the season max-drawdown
    distribution from the implied returns; locate realised max drawdown's
    predicted quantile.
  - **Kelly-implied vs realised log-wealth volatility** (half-Browne fraction ×
    full-Kelly prediction).
- **Honesty:** badly underpowered at tens of placed bets — render as
  **monitoring dials with wide CIs**, feeding the paper-shadow / SPRT harness,
  *not* a tuning signal (consistent with the standing "don't tune on noise"
  verdict).
- **R home:** `R/backtest-bankroll.R`.

### 6.7 Evidence-gated OOS bake-off — the model-update loop *(page 6)*

The diagnostics **flag** a defect; the walk-forward harness **validates** the
fix before adoption:

```
1. SEASON DIAGNOSTIC SCAN (cheap, on-render) — §6.1–6.4 flag a SPECIFIC defect
2. NAME THE FIX (data-story first) — U-PIT→NegBin/λ₃; draw deficit→Dixon–Coles τ;
   fav. miscalibration→logit recalibration; early-season→prior/RW init var.
   Write the sign/range/prior story for each new parameter (per the user's
   "data-stories" rule) before implementing.
3. WALK-FORWARD OOS BAKE-OFF — re-fit BOTH model variants at every cutoff
   (fit-mode, real Stan); compare OOS Brier/log-loss/CRPS AND re-check the
   flagged diagnostic OOS (did the PIT actually un-bend?).
4. ADOPT IFF: OOS proper score improves with a match-clustered CI excluding zero
   AND the targeted diagnostic improves OOS. Else: shelve as a default-OFF
   scaffold (the standing pattern).
```

- **Key design points:** the gate is a **proper score, OOS, with a
  match-clustered CI** (reuse `bt_skill_ci`'s resample unit for the model-A-vs-B
  Brier delta). Re-check the *targeted* diagnostic OOS, not just the aggregate
  (a NegBin swap may leave global Brier flat while fixing a high-scoring-regime
  miscalibration — still a win if the data-story holds). Compute diagnostics on
  **walk-forward OOS** predictions, never in-sample.
- **Cost:** fit-mode is ~60 min/fit × cutoffs → a **detached** script
  (`scripts/0Nb_bakeoff.R`, `nohup … & disown` per the user's long-run rule),
  writing `data/backtest/bakeoff/<variant>.json`. The dashboard **reads the
  stored result** (page 6); it does not recompute on render. REUSE-mode stays
  the fast self-updating *monitoring* path; fit-mode is the *adjudication* path.
- **R home:** extend `R/backtest-walkforward.R` (two-variant compare + emit
  per-match OOS draws so OOS PIT is leak-free by construction).

## 7. Plumbing changes to existing code

1. **`bt_skill_ci(scored, by = NULL, …)`** — add a `by` arg; loop the
   match-clustered resample within each group (≈5 lines). Currently pools all
   rows, blocking per-stratum CIs.
2. **Division (+ round) attachment, model arm** — `R/backtest-divisions.R`
   attaches `division` and `round` to `wf_bets` from the extract's *own*
   `division` column on the **federation-name** key
   `(sex, match_date, home_team, away_team)`, at belief-reconstruction time
   (inside / alongside `bt_wf_beliefs_from_extract`), *before* the Lengjan odds
   join. This avoids the `team_names`-map mismatch (the latent
   `bt_wf_require_pre_cutoff_odds` issue) for division entirely. Fixtures that
   fail to match → `division = NA` labelled `"unknown"`, never silently dropped.
   **Money arm gets no division** (out of scope per §3).
3. **`bt_calibration`** — add consistency bands + Jeffreys intervals
   (`bt_calibration_bands`), keeping the existing `by=` semantics.
4. **Murphy decomposition** in `bt_skill` (or a sibling `bt_brier_decomp`).
5. **No change to the money arm or `bt_run`** beyond reading — the existing
   report stays valid; the dashboard is additive.

## 8. The JSON data contract (`data/dashboard/`)

`R/dashboard-export.R` writes one tidy JSON per diagnostic family, each a
long/tidy array of records carrying its stratification keys so the front-end can
pivot/facet without recompute. Indicative set:

- `meta.json` — render timestamp, season, data-window, n by arm/stratum,
  pre-registered grid definition.
- `calibration_model.json` — `(sex, division, market, bin, mean_p,
  realised_freq, n, lo, hi, band_lo, band_hi)`.
- `calibration_market.json` — same shape for `q_market` (line-softness).
- `pit.json` — `(sex, division, marginal, u_bin, density, null_lo, null_hi)` +
  uniformity test stats.
- `score_dist.json` — draw-rate gap, total-goals law, Skellam law,
  scoreline-residual grid, by `(sex, division)`.
- `skill.json` — `(sex, division, market, brier_model, brier_market,
  brier_skill, ci_lo, ci_hi, n, n_matches, rel, res, unc)` + shrunk variants
  (`skill_shrunk`, `p_edge_gt0`, `cal_slope`, `citl`).
- `disagreement.json` — bands × strata.
- `line_stability.json` — move-frequency summary.
- `bankroll.json` — realised-vs-predicted variance/drawdown dials.
- `bakeoff.json` — latest variant comparison (from the detached run; may be
  absent → page 6 shows an empty-state callout).

`meta.json` plus a JSON-Schema validator (mirroring `config/` validators) lets
the front-end fail loud on a contract break.

## 9. Front-end (`docs/dashboard/experiment.qmd`)

- `format: dashboard` (Quarto ≥1.4), `embed-resources: true`, `metill::theme_metill()`.
- A `setup` chunk reads `data/dashboard/*.json` (not the parquet stores — the
  dashboard consumes the *contract*, keeping render fast and the compute/present
  seam clean). Falls back to an empty-state callout per page when a JSON is
  absent (mirrors the existing `wf-empty-note` pattern).
- R chunks render small-multiple ggplots faceted by stratum; **OJS cells** add
  client-side sex/division/market filtering where a single linked view beats
  small multiples (e.g. the 30-cell skill grid).
- Six pages per §5; each page is a Quarto dashboard `# Page`. Cards via `##`
  rows / `###` columns; KPI value-boxes on page 1.
- Icelandic labels at this boundary only (`\uXXXX` / `metill::isk/hlutf/tala`).

## 10. Testing strategy

TDD per `testing-r-packages` / `.claude/rules`:

- **`R/backtest-pit.R`** — `bt_rpit` uniformity on synthetic well-specified
  draws (KS p large); U-shape on deliberately under-dispersed draws; draw-rate
  gap exact on a hand-built fixture; scoreline-residual grid sums to ~0.
- **`bt_skill_ci(by=)`** — per-group CI equals the old pooled CI when one group;
  groups independent.
- **Division attachment** — federation-name join is lossless on a football-male
  fixture; `team_names`-map cell still attaches division (no name dependence);
  unmatched → `"unknown"`, never dropped.
- **Market-calibration / line-stability** — bias signs on constructed soft lines;
  stability monitor counts moves correctly.
- **Recalibration** — shrinkage pulls a thin cell toward the parent; slope=1,
  CITL=0 recovered on calibrated synthetic data.
- **CI-isolation test** — `tests/testthat/test-dashboard-ci-isolation.R` greps
  the export/script for ledger writes (mirrors `test-healthcheck-ci-isolation.R`).
- **Quarto render** — `quarto render docs/dashboard/experiment.qmd` builds clean
  on the current data (verify before claiming done).

## 11. Implementation phasing

Comprehensive scope → several PRs; the dashboard appears once enough engine
exists to populate pages 1–3, then grows. Suggested sequence:

- **P1 — Joint-distribution diagnostics** (`backtest-pit.R`): PIT +
  score-distribution + scoreline residuals. Highest signal-per-effort, pure
  functions of `predicted_matches` + `results`. TDD.
- **P2 — Stratification + division + market-calibration** (`backtest-divisions.R`,
  `backtest-marketcal.R`, extend `backtest-metrics.R`): division/round
  attachment, `by=` on `bt_skill_ci`, Murphy, consistency bands/Jeffreys,
  line-softness + stability.
- **P3 — Hierarchical recalibration + shrunk skill** (`backtest-recalib.R`):
  lme4 (installed) render-fast; optional cached `brms` (needs install + compile).
- **P4 — Export + dashboard front-end** (`dashboard-export.R`,
  `scripts/0Nd_dashboard.R`, `docs/dashboard/experiment.qmd`): JSON contract +
  pages 1–4 + CI re-render wiring + isolation test. **First visible deliverable.**
- **P5 — Bankroll-side calibration** (`backtest-bankroll.R`) → page 5.
- **P6 — OOS bake-off harness** (extend `backtest-walkforward.R`,
  `scripts/0Nb_bakeoff.R`) → page 6 (model-update lab).

Each phase is independently shippable and testable; the front-end can land after
P2–P3 and accrete pages 5–6 later.

## 12. Risks & open questions

- **`team_names`-map division attachment** — resolved for division by keying on
  the federation name pre-odds-join (§7.2), but verify the female cell attaches
  correctly (it carries a map). Regression test required.
- **brms render cost** — keep `brms` behind a flag with a cached compiled model;
  default the render-fast lme4/glmmTMB path so weekly CI re-render stays cheap.
- **OJS + `format: dashboard` maturity** — Quarto 1.9.38 is installed and
  supports the dashboard layout + OJS data passing (verified). `format: html`
  with `.tabset` remains a lighter fallback if a specific OJS interaction
  misbehaves.
- **Bankroll dials underpowered** — present as monitoring only; do not let a
  noisy season-1 dial drive a Kelly change.
- **Multiplicity drift** — the pre-registered grid must be fixed and shown in
  full every render; resist adding post-hoc "interesting cell" cuts.
- **OOS bake-off compute** — fit-mode is expensive and detached; the dashboard
  must degrade gracefully when `bakeoff.json` is stale/absent.

## 13. References

Brier (1950); Murphy (1973, REL/RES/UNC); DeGroot–Fienberg (1983); Dawid
(1982/1984, calibration/PIT); Gneiting & Raftery (2007, proper scoring rules);
Gneiting, Balabdaoui & Raftery (2007, calibration + sharpness, PIT);
Czado, Gneiting & Held (2009, randomised PIT for counts); Bröcker & Smith (2007,
consistency bars); Cox (1958) / Van Calster et al. (2016, calibration
slope/intercept hierarchy); Efron–Morris (1975) / Gelman–Hill (2007, shrinkage);
Gelman–Hill–Yajima (2012) / Gelman–Loken (2013, multiplicity via multilevel
models); Dixon–Coles (1997); Karlis–Ntzoufras (2003, bivariate Poisson / draw
inflation); Sauer (1998, market efficiency).

# Plan 7 series — repo-side roadmap

> **For agentic workers:** this is a **roadmap stub**, not an executable plan. It catalogues five follow-up plans named in Plan 7a's self-review checklist (2026-04-29) so the repo's plan history isn't silently load-bearing on Obsidian notes. The design rationale and step-by-step specs live in the Metill Obsidian vault — see _Canonical source_ below.

**Canonical source:** [`Sports/Knowledge/Betting Optimisation/next-actions.md`](obsidian://Sports/Knowledge/Betting%20Optimisation/next-actions) (Plan 7 series section). Plan 7a's full design is at [`Sports/Knowledge/Betting Optimisation/Historical/kelly-stake-restoration-design-2026-04-29.md`](obsidian://) §"Beyond restoration".

**Why this stub exists:** Two repo-side dependencies referenced Plans 7b–7f without those plans existing in `docs/superpowers/plans/`:

1. The 2026-05-24 lower-divisions plan ([`docs/superpowers/plans/2026-05-24-football-iceland-lower-divisions.md`](2026-05-24-football-iceland-lower-divisions.md)) deferred LD4 inclusion to "Plan 7d in the Obsidian roadmap".
2. The 2026-05-02 operational `kelly_frac` cut (per the [`project_kelly_frac_cut_2026_05_02`](../../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_kelly_frac_cut_2026_05_02.md) memory note) applied a 25% override to Browne defaults — explicitly framed as a hold-the-line measure until Plan 7d ships.

When that work begins, fork this stub into per-plan files (`2026-XX-XX-plan-7b-clv-capture.md`, etc.) and execute via `superpowers:executing-plans`.

## Series overview

The Plan 7 series turns the ad-hoc `kelly_frac` and ratio-based calibration into a single hierarchical Bayesian decision-theoretic model. Integration order is sequential — each plan layers on the previous one's signal.

| # | Title | Repo-side prereq | Obsidian status | Repo-side priority |
|---|---|---|---|---|
| **7a** | Kelly stake restoration | — | ✅ Implemented 2026-04-29 (see `R/decide-pipeline.R::decide_league()` + `.claude/rules/model-decide.md` _Stake formula_) | — |
| **7b** | Closing-line value (CLV) capture | Ledger schema migration (`closing_odds: double`); placer hook for pre-tipoff scrape | Queued | Medium — small additive surface; unblocks 7c's signal density |
| **7c** | Baker-McHale Bayesian calibration | 7b for sharper signal; ~30 settled bets/cell with CLV, ~100 without | Queued | High — replaces the conjugate-Beta multiplier in `R/decide-calibration.R::compute_calibration()` |
| **7d** | Hierarchical `kelly_frac` pooling | 7c (lives in the same hierarchical model) | Queued | **High — load-bearing** for LD4 inclusion and for the 2026-05-02 kelly_frac override |
| **7e** | EV threshold from `σ_eps` | 7c with stable `σ_eps` posteriors per cell | Future | Low — research extension |
| **7f** | CVaR-Kelly objective | Backtest harness validation | Research | Deferred |

After 7d lands, every constant in the bet-sizing path that is **not** a hard invariant (K5 ceiling, B3 per-match cap, `min_bet`) becomes Bayesian-estimated from data.

## Repo-side surfaces each plan would touch

This is the changing-files view — useful for sizing and for noticing collisions with other in-flight work.

### Plan 7b — CLV capture

- **Schema migration:** `R/storage-schemas.R::schemas()$ledger` gains `closing_odds: double`. `R/placer-ledger.R::append_to_ledger()` schema-validation update.
- **New placer hook:** `R/placer-place.R` or a sibling — a `capture_closing_odds(match_id, market, outcome, line)` call wired into a separate cron lane (the placer itself is local-only; CLV scrape can be CI-side because it doesn't open the bet-slip).
- **New utility:** `R/decide-clv.R::compute_clv_calibration(league, sex)` returning a per-cell CLV-based multiplier.
- **Ledger backfill:** new column starts NA on legacy rows; `compute_clv_calibration()` skips those gracefully.

Effort estimate (per Obsidian): ~80 lines.

### Plan 7c — Baker-McHale Bayesian calibration

- **New Stan model:** `Stan/calibration_baker_mchale.stan` (hierarchical logistic regression on `(α, β, σ_eps)` per cell — full spec in Obsidian).
- **New R wrapper:** `R/decide-calibration-bayes.R` wrapping the cmdstanr fit + posterior summary.
- **Adapter on existing API:** `R/decide-calibration.R::compute_calibration()` gains a `method = c("ratio", "bayes")` argument, `"ratio"` as legacy default during transition.
- **Persistent cell-level fit:** likely a new tree under `data/beliefs/calibration_bayes/cell={key_sex}/` keyed by month so refits don't refit the world.
- **Test coverage:** golden-fit reproducibility test + an A/B test comparing the two methods' shrinkage on the current ledger.

Effort estimate (per Obsidian): ~150 lines + Stan model + golden fits.

### Plan 7d — Hierarchical `kelly_frac` pooling

- **Stan model extension:** add `kelly_frac_{l,s} ~ N(μ, τ²)` hierarchy to the 7c model (or keep separate; design choice TBD during 7c integration).
- **Decide-layer rewire:** `R/decide-pipeline.R::decide_league()` reads `kelly_frac` from the posterior fit instead of `config/leagues.yml::*.betting.kelly_frac`.
- **Config role flip:** `leagues.yml::*.betting.kelly_frac` becomes a _prior-mean override_ rather than the active value. Schema migration in `config/leagues.schema.json` to reflect this (probably just a comment + maybe a renamed field like `kelly_frac_prior`).
- **Subsumes operational override:** the 25% cut from 2026-05-02 ([`project_kelly_frac_cut_2026_05_02`](../../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_kelly_frac_cut_2026_05_02.md)) becomes redundant — partial-pooling shrinks small-sample cells toward the global `μ` automatically. Delete the override + the memory note.
- **K3 + K5 soften:** the minimum-sample-size hard-cutoff becomes a soft prior; the K5 ceiling becomes a soft prior too (the hard `kelly_ceiling = 0.25` clamp stops being load-bearing in normal operation — keep it as a guardrail).
- **Unblocks LD4:** the 2026-05-24 lower-divisions plan's LD4 exclusion (amateur tier-5 funnel posteriors) can be revisited under the hierarchical model — partial pooling toward `μ` should tame the cold-start funnels rather than letting them dominate stake sizes.

### Plan 7e — Adaptive `ev_threshold` from `σ_eps`

- **Decide-layer one-liner:** replace the per-league `betting.ev_threshold: 0.0` read with `z_α × posterior_mean(sigma_eps)`. `α = 0.05` and typical `σ_eps ≈ 0.03` → EV threshold ~5%.
- **Config role flip:** like 7d, `ev_threshold` becomes a floor / override rather than the active value.

Worth revisiting after 2–3 months of CLV data once 7c's `σ_eps` posteriors stabilise.

### Plan 7f — CVaR-Kelly (research)

- **Decide-kelly rewire:** replace `kelly_joint()`'s `E[log W]` objective with `CVaR_α[log W]` (the average log-wealth in the worst α% of posterior draws).
- **A/B test in backtest harness:** prereq — quantify growth-rate cost vs drawdown reduction in `_legacy/sports/R/backtest/betting_pnl/` (or its re-ported equivalent).

Deferred. The existing `kelly_frac × calibration` multiplicative chain already provides ~half-Kelly's worth of conservative shrinkage; CVaR is a larger architectural change for marginal additional safety margin.

## Cross-cutting prerequisites

These aren't part of any individual plan but enable the series:

1. **Settled-bet density.** 7c needs ~30 bets/cell with CLV (7b shipped) or ~100 without. As of 2026-05-24 the ledger has hundreds of settled bets across all active cells; the bottleneck is more about CLV than count.
2. **Calibration history.** When 7c lands, we'll want to backfill `closing_odds` from cached odds scrapes if `data/facts/odds/` has them at sufficient density around tip-off; otherwise CLV starts forward-only.
3. **Backtest harness re-port.** `_legacy/sports/R/backtest/betting_pnl/` was Phase 1-shipped pre-monorepo. 7f's A/B test would need it ported under the current schema, which is its own ~150-line port.

## Out of scope for this stub

- Detailed Stan code (lives in Obsidian Plan 7a design doc §"Beyond restoration" with a draft for 7c).
- Brainstorm-level "how to integrate 7d into 7c's Stan model" — TBD during execution; the Obsidian doc flags this as a design choice rather than prescribing a particular factoring.
- Backtest design for any of 7b–7f — Obsidian `Sports/Knowledge/Betting Optimisation/Theory/_index` and `Historical/betting-pnl-harness-*` are the starting points.

## Self-review checklist

This roadmap stub should be replaced as each plan executes. When forking into a full plan doc:

- [ ] Use the `2026-XX-XX-plan-7N-{slug}.md` naming.
- [ ] Cite the Obsidian source in the frontmatter or first paragraph.
- [ ] Use `superpowers:writing-plans` to flesh out the per-task `- [ ]` checklist.
- [ ] Update the table in this file with the new plan's path.
- [ ] If the plan supersedes operational memory (e.g. 7d → `project_kelly_frac_cut_2026_05_02`), note the supersession in both the new plan and the affected memory file.

## See also

- [`docs/superpowers/plans/2026-04-29-kelly-stake-restoration-plan.md`](2026-04-29-kelly-stake-restoration-plan.md) — Plan 7a, the restoration prerequisite.
- [`docs/superpowers/plans/2026-05-24-football-iceland-lower-divisions.md`](2026-05-24-football-iceland-lower-divisions.md) — the 2026-05-24 plan whose LD4 exclusion refers to 7d.
- [`.claude/rules/sports-betting.md`](../../.claude/rules/sports-betting.md) — _Plan 7 series — active forward roadmap_ section points at the Obsidian roadmap.
- [`.claude/rules/model-decide.md`](../../.claude/rules/model-decide.md) — current stake formula chain that 7c/7d will reshape.

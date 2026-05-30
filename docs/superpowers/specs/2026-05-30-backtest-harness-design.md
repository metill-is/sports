# Backtest Harness — Design

**Date:** 2026-05-30
**Status:** Approved (brainstorm), pending spec review → implementation plan
**Topic:** Simulate historical bet placement and analyse outcomes ("what would have been working, and how well")

## 1. Goal & non-goals

### Goal

A reusable, composable engine plus a Quarto report that **replays historical
betting decisions against actual results** and decomposes the outcome, so we
can answer:

- How well has the workflow's actual strategy performed (PnL, ROI, hit-rate)?
- Where is the edge — which markets, leagues, sexes, divisions, odds ranges?
- Is the model calibrated (predicted `p` vs realised frequency)?
- How do alternative strategies (every +EV bet, flat-stake, favourites-only)
  compare to what we actually did?

The engine is **general** — it replays *any* bet subset under *any* stake rule
— but the first shipped report defaults to evaluating our **actual recommended
picks**, with the wider candidate universe available as comparison layers.

### Non-goals (this phase)

- **Not** a forward optimiser / parameter search (no auto-tuning of
  `kelly_frac`, thresholds). It surfaces where edge exists; acting on that is a
  separate decision.
- **Not** a re-fitting engine. Phase 1 consumes the already-stored, leak-free
  `candidates` table. Extending history backward via `0Nr_replay.R` re-fits is
  Phase 2.
- **Not** wired into CI or the money path. Read-only on `data/decisions/`,
  including the ledger.

## 2. Data substrate

All inputs already exist and are git-tracked. Confirmed columns (2026-05-30):

| Source (Parquet / DuckDB view) | Columns used | Role |
|---|---|---|
| `data/decisions/candidates/` (`candidates`) | `run_id, sex, match_date, home_team, away_team, market, outcome, line, p, odds, ev, kelly_raw, stage, sport, country, run_date` | **The bet universe.** Every evaluated (match × market × outcome × line) per decide run, with `stage ∈ {kept, dropped_low_ev, dropped_min_bet, dropped_invalid_input}`. `p` frozen at decide-time → **leak-free**. |
| `data/decisions/recommendations/` (`recommendations`) | adds `kelly` (effective fraction), `bet_amount` (recorded stake) | Kept bets' **actual stakes** + pool reconstruction. |
| `data/facts/results/` (`results`) | scorelines | Outcomes for settlement. |
| `data/decisions/ledger/` (`ledger`) | `..., odds_placed, bet_amount, settled, win, pnl` | Realisable-subset cross-check; realised-PnL pool reconstruction. **Read-only.** |
| `config/bankroll.yml` | `initial_pool` (23 610), `kelly_ceiling` (0.25), `daily_budget_frac` (0.05), `daily_budget_min_isk` (1000), `max_match_stake_default` (1.0) | Stake-formula constants. |

**Coverage:** `candidates` / `recommendations` span 2026-04-25 → present (~5
weeks at spec time): ~2 849 evaluated bets, of which 154 `kept`. All six cells
(football/basketball/handball × male/female) where decide ran.

**Leak-freeness.** `candidates.p` was computed by a fit that only saw matches
on or before `run_date`, and `run_date` precedes each match's kickoff. So
joining `candidates` → `results` introduces no look-ahead. The backtest does
**not** read the belief stores; for football, per-fit predictions live in
`beliefs/extracts/` (not `beliefs_archive`), but neither is needed because
`candidates` already froze the market-level `p`.

## 3. Architecture

A linear, pure pipeline. Each stage is a unit with one job, a typed interface,
and no hidden state:

```
bt_load_universe(filter)        ─┐
                                 ├─►  bt_run(universe, results, stake_rule, initial_pool)  ─►  per-bet settled frame
config/bankroll.yml ─────────────┘            │ (reuses compute_settlement)                          │
                                              ▼                                                       ▼
                                       stake rule fn                                      bt_metrics() / bt_calibration()
                                  (rolling | fixed)                                                    │
                                                                                                       ▼
                                                                          scripts/0Nb_backtest.R  →  data/backtest/*.parquet
                                                                                                       │
                                                                                                       ▼
                                                                              docs/reports/2026-backtest.qmd
```

Determinism: given the same `candidates` + `results` snapshot, every function
returns identical output (no RNG, no clock reads inside the engine — the report
stamps `date: today`).

## 4. Components

### 4.1 `R/backtest-universe.R`

```r
bt_load_universe(root = here::here("data"),
                 strategy = c("kept", "positive_ev", "all"),
                 leagues = NULL, sex = NULL,
                 from = NULL, to = NULL)  -> tibble
```

- Reads `candidates`; left-joins `recommendations` on the full bet key
  (`run_date, sport, country, sex, match_date, home_team, away_team, market,
  outcome, line`) to attach `kelly` + `bet_amount` where the bet was `kept`.
- `strategy` selects the bet subset (the *strategy is the filter*):
  - `kept` — `stage == "kept"` (our actual picks; default).
  - `positive_ev` — every candidate with `ev > 0`, regardless of `stage`
    (a bet can be dropped for sub-threshold EV or min-stake yet still have
    `ev > 0`; this strategy bets all of them).
  - `all` — every candidate row.
- Returns one row per bet with: bet key, `p`, `odds`, `ev`, `kelly_raw`,
  `kelly` (NA if not kept), `bet_amount_recorded` (NA if not kept), `stage`,
  `strategy_in` (which strategies include this row).
- Empty-with-columns tibble if no rows match (house convention).

### 4.2 `R/backtest-stake.R`

Stake rules are functions `(universe_with_results) -> universe + stake column`.
Two are shipped. Both respect the per-run *slate* structure (all bets sharing a
`run_date` are sized off the same pool snapshot, mirroring the live joint-Kelly
that co-sizes a slate).

**Rolling** (`stake_rolling`) — path-dependent, the money story:

1. Order runs by `run_date`.
2. For each run, `pool = initial_pool + Σ pnl` over bets whose match settled
   *strictly before* this `run_date`.
3. Stake per bet:
   - **Kept bets:** scale the recorded fraction to the rolling pool —
     `stake = kelly × pool`, where `kelly` is the recorded effective fraction.
     (At the live pool this reproduces the recorded `bet_amount`; on the
     counterfactual rolling pool it scales proportionally.)
   - **Counterfactual bets** (no recorded `kelly`): estimate the effective
     fraction as `kelly_raw × shrink_run`, where
     `shrink_run = median(kelly / kelly_raw)` over the kept bets in the **same
     run** (fallback: global median; final fallback: `kelly_frac × 1.0`
     from config). Then `stake = effective_fraction × pool`.
4. Apply the **daily-budget cap**: if `Σ stake` for the run exceeds
   `max(daily_budget_frac × pool, daily_budget_min_isk)`, scale that run's
   stakes down proportionally (mirrors the live Stage-2 cap). Kept-only replays
   are already under the cap, so this only bites on counterfactual strategies.
5. Round to whole ISK (matches live `round()`).

**Fixed** (`stake_fixed`) — constant reference pool, no compounding:

- `stake = effective_fraction × ref_pool`, `ref_pool = initial_pool` by default
  (configurable). Same effective-fraction logic as above but the pool never
  moves, so cross-strategy / cross-market ROI is comparable without compounding
  variance. No daily-budget cap (it would reintroduce path dependence).

Both return the universe with a numeric `stake` column; never mutate inputs.

### 4.3 `R/backtest-engine.R`

```r
bt_run(universe, results,
       stake_rule = stake_rolling,
       initial_pool = NULL,                 # default from bankroll.yml
       match_date_window_days = 3L) -> tibble
```

- **Reuses `compute_settlement(bets, results, match_date_window_days)`** from
  `R/settle.R` to derive `win` + `pnl` per bet — *not reimplemented*, so the
  backtest's win/push/boundary semantics are identical to the live decider and
  settle layer (strict-inequality spread/total boundaries).
- Win/loss is independent of stake. The engine first determines `win` per bet
  via `compute_settlement`, then `stake_rule` walks runs chronologically:
  `pnl = stake × (odds − 1)` on a win, `−stake` on a loss, updating the pool
  after each run's matches settle.
- Returns one row per bet: bet key + `p, odds, ev, stake, win, pnl,
  pool_before, pool_after, cum_pnl, run_date`.
- The `match_date_window_days = 3` reschedule fallback matches
  `settle_ledger()` so backtested settlement tracks how the live ledger
  actually resolves rescheduled fixtures.

### 4.4 `R/backtest-metrics.R`

```r
bt_metrics(settled, by = NULL) -> tibble        # grouped KPI table
bt_calibration(settled, n_bins = 10, by = NULL) -> tibble
bt_baselines(universe, results, initial_pool) -> tibble   # strategy comparison
```

- `bt_metrics`: `n_bets, total_staked, total_pnl, roi (= total_pnl /
  total_staked), yield (= total_pnl / n_bets, avg ISK profit per bet),
  hit_rate, avg_odds, final_pool, max_drawdown, sharpe_like (= mean per-bet
  return / sd of per-bet return)`. `by` accepts any of `market, sport, country,
  sex, division, ev_bucket, odds_bucket`. `division` is joined from
  `schedules` on the match key (NA where unavailable, e.g. cup/single-division
  cells).
- `bt_calibration`: bins bets by predicted `p`, returns
  `(bin, mean_p, realised_freq, n)` for reliability plots. `by` enables the
  **decomposition** the project requires (calibration aggregates are weak
  evidence until broken down by division/sex/market — see
  `feedback_calibration_aggregates`).
- `bt_baselines`: runs the engine for the comparison strategies (§8) and
  returns a tidy comparison frame.

### 4.5 `scripts/0Nb_backtest.R`

CLI that assembles the universe, runs the engine for the default + comparison
strategies under both stake models, and writes tidy Parquet to `data/backtest/`
for the report. Flags: `--strategy {kept|positive_ev|all}`,
`--stake {rolling|fixed|both}` (default `both`), `--from`, `--to`,
`--league`, `--sex`. Prints a summary table. Read-only on inputs.

### 4.6 `docs/reports/2026-backtest.qmd`

Self-contained HTML report (mirrors `2026-cumulative-xpts.qmd`: `is_IS.UTF-8`
locale, `code-fold`, `embed-resources`, `theme: cosmo`, dplyr/ggplot2/gt,
`devtools::load_all`). Sections:

1. **Headline** — rolling-bankroll cumulative PnL curve (pool over time);
   KPI cards (total PnL, ROI, hit-rate, final pool, max drawdown).
2. **Where the edge is** — fixed-fraction ROI broken down by market / league /
   sex / division / odds-bucket / EV-bucket (gt tables + bar charts).
3. **Calibration** — reliability plot (predicted vs realised), overall and
   decomposed by market & sex.
4. **Strategy comparison** — our picks vs every +EV vs flat-stake vs
   favourites-only: PnL curves + ROI table.
5. **Caveats** — realisable-subset note, counterfactual-stake approximation,
   window length (§6).

Verified with `quarto render` before claiming completion.

## 5. Stake model — worked detail

Live formula (from `.claude/rules/model-decide.md`):
`bet_amount = round(kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling) × current_pool)`,
further capped by the daily budget.

- The composite `kelly = kelly_raw × portfolio_lambda × min(kelly_frac ×
  calibration, kelly_ceiling)` is **recorded** in `recommendations` for kept
  bets. So we never re-evaluate the formula for kept bets — we use the recorded
  `kelly` (or `bet_amount`) directly. This makes the kept-bet rolling replay
  *exactly* faithful: the compounding the live system experienced is already
  encoded in the recorded stakes.
- **Pool reconstruction & validation:** `pool_at_run = bet_amount / kelly` from
  any kept bet must agree (within rounding) with `initial_pool + Σ pnl settled
  before run`. The engine asserts these two reconstructions agree as a
  self-test; divergence flags a settlement-window mismatch.
- **Counterfactual fidelity ceiling:** `portfolio_lambda` and `calibration`
  are *not* stored in `candidates`, so dropped bets' exact stakes are
  unknowable. The `shrink_run` estimate (§4.2) is the best principled
  reconstruction; the report labels counterfactual curves as *approximate*.
  Phase 2 may persist λ/calibration into `candidates` to make these exact.

## 6. Validity & caveats (surfaced in the report)

1. **Leak-free** picks (`candidates.p` frozen pre-match); settle reused for
   boundary consistency.
2. **Obtainability:** `candidates.odds` are decide-time prices; some bets were
   never placeable (early-kickoff delisting — e.g. the 2026-05-30 ÍBV miss). A
   secondary **realisable-subset** view intersects the universe with the
   `ledger` (bets actually placed) so we can compare theoretical vs realisable
   performance.
3. **Counterfactual stakes are approximate** (§5).
4. **Window** ≈ 5 weeks at spec time; small-sample caution on per-division
   slices. The report shows `n_bets` everywhere and avoids over-claiming thin
   cells.

## 7. Metrics catalogue

`n_bets, total_staked, total_pnl, roi, yield, hit_rate, avg_odds,
final_pool, max_drawdown, sharpe_like`, each available grouped by
`market | sport | country | sex | division | ev_bucket | odds_bucket`, plus
calibration `(mean_p, realised_freq, n)` per bin and decomposition.

## 8. Strategy / baseline catalogue

- **our_picks** — `kept` (default headline).
- **all_positive_ev** — every `ev > 0` candidate.
- **flat_stake** — `kept` bets at a constant ISK stake (isolates selection
  skill from Kelly sizing).
- **favourites** — bet the lowest-odds outcome of each match (naïve baseline
  the model must beat).

All four run through the same `bt_run`; comparison is a `bind_rows` + group.

## 9. Output data format

`data/backtest/` (Parquet), **gitignored** (regenerable from candidates +
results; keeps the hot repo lean). Files:
`per_bet.parquet` (engine output, all strategies × both stake models, tagged
`strategy` + `stake_model`), `metrics.parquet` (grouped KPIs),
`calibration.parquet`. The `.qmd` regenerates these if absent.

## 10. Testing strategy

`tests/testthat/test-backtest-{universe,stake,engine,metrics}.R`, testthat ed.3,
self-sufficient fixtures (small hand-built candidates/results tibbles with known
PnL):

- **engine:** win/loss/push PnL arithmetic; reuse of `compute_settlement`
  (a spread/total boundary case that the existing settle tests already pin);
  reschedule-window fallback.
- **stake (rolling):** pool walks correctly across runs; daily-budget cap
  scales a synthetic over-budget slate; `shrink_run` counterfactual estimate.
- **stake (fixed):** no compounding; constant ref pool.
- **universe:** strategy filters select the right rows; recommendations join
  attaches `kelly`/`bet_amount` only to kept.
- **metrics:** ROI/hit-rate/drawdown on a known series; calibration binning;
  baseline strategies produce sane comparison.
- **validation:** the two pool reconstructions agree on a fixture.

No network, no RNG, no clock inside engine/metrics.

## 11. Scope & phasing

- **Phase 1 (this build):** candidates-era backtest, all cells, engine +
  metrics + CLI + report + tests.
- **Phase 2 (later, separate spec):** extend history backward via
  `0Nr_replay.R` re-fits (football only); persist `portfolio_lambda` +
  `calibration` into `candidates` for exact counterfactual stakes; optional
  in-report comparison of replayed vs live picks.

## 12. File layout & conventions

```
R/backtest-universe.R     # bt_load_universe
R/backtest-stake.R        # stake_rolling, stake_fixed
R/backtest-engine.R       # bt_run  (reuses R/settle.R::compute_settlement)
R/backtest-metrics.R      # bt_metrics, bt_calibration, bt_baselines
scripts/0Nb_backtest.R    # CLI
docs/reports/2026-backtest.qmd
tests/testthat/test-backtest-*.R
data/backtest/            # gitignored output
```

- Public functions `@export` + roxygen; internal helpers `@noRd`.
- Flat `R/` layout matching `decide-*`, `extract-*`.
- `0N`-letter script naming, as `0Nr_replay.R`.
- `devtools::document()` to refresh NAMESPACE; `devtools::test()` green;
  `quarto render` clean — all three before completion.
- New rule file `.claude/rules/backtest.md` (paths: `R/backtest-*.R`,
  `scripts/0Nb_backtest.R`, `docs/reports/2026-backtest.qmd`) documenting the
  leak-free contract, stake-reconstruction logic, and caveats.

## 13. Resolved decisions

| Decision | Choice |
|---|---|
| Engine generality | General (any subset × any stake), picks-first default. |
| Stake model | **Both** — rolling headline + fixed-fraction comparison. |
| Time scope | Phase 1 = candidates era; replay-extension = Phase 2. |
| Output | Quarto report + reusable R fns + CLI. |
| Output data | `data/backtest/`, gitignored. |
| Settlement | Reuse `compute_settlement` (no reimplementation). |
| CI | None — read-only, local analysis tool. |

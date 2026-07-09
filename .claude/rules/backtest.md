# Backtest harness

Loads when working on `R/backtest-*.R`, `scripts/0Nb_backtest.R`, or
`docs/reports/2026-backtest.qmd`.

- **Read-only on the money path.** The harness never writes the ledger and is
  never wired into CI. It reads `candidates` / `recommendations` / `results`.
- **Leak-free contract.** `candidates.p` is frozen at decide-time (pre-match);
  joining to `results` introduces no look-ahead. Do not re-derive `p` from
  belief stores in the engine.
- **Reuse `compute_settlement()`** for win/pnl — never reimplement settlement
  boundaries. The engine looks up `tie_threshold` per `(sport, country)` the
  same way `settle_ledger()` does, so backtested settlement is self-consistent
  with the live decider.
- **Stake reconstruction.** Kept bets use the recorded effective `kelly`
  (faithful rolling Kelly — compounding is baked into recorded stakes).
  Counterfactual bets use `kelly_raw * median(kelly/kelly_raw)` per run; these
  are flagged approximate in the report.
- **Min-bet floor is opt-in (`min_bet`).** By default the stake rules
  (`stake_fixed` / `stake_rolling`) apply **no** minimum — they place every bet
  at `round(frac × pool)`, so ~82 % of football stakes land below Lengjan's
  200 kr floor. This is the deliberate "idealised large-pool" view. Pass
  `min_bet = <isk>` (forwarded through `bt_run`'s `...`) to **drop** any bet
  whose stake — after the daily-budget cap — falls below it, mirroring the live
  decider's `dropped_min_bet` stage (dropped bets contribute no pnl to the
  rolling pool). The report carries both views: the headline/by-league sections
  are no-floor; the "200 kr floor" section re-runs the same picks floored at
  `config/leagues.yml::football_iceland.betting.min_bet`. The floor is a
  stake-level row filter, not a fraction change — `bt_effective_fraction` is
  unaffected. Regression-guarded by `min_bet = 0` tests in
  `test-backtest-stake.R`.
- **Default scope is football only.** The CLI (`scripts/0Nb_backtest.R`) and the
  report (`docs/reports/2026-backtest.qmd`) default to `leagues = "football"` —
  the backtest is meant to judge football specifically, and basketball/handball
  are on seasonal pause. The engine (`bt_load_universe`) stays general; widen
  with `--league all` (CLI) or `leagues = NULL` (library) when they resume.
- **Bug-era excluded by default.** `bt_load_universe(exclude_pre_fix = TRUE)`
  (the default) drops decide runs before the 2026-05-13 spread sign-flip fix
  (`run_date < bt_spread_fix_date()` = 2026-05-14); those candidates carry
  contaminated spread EV that current code cannot reproduce (the forensic
  review found the entire +ROI headline was one such bet). `--include-bug-era`
  / `exclude_pre_fix = FALSE` opts back in. There is no pre-2026 odds history,
  so a replay cannot extend the *bettable* backtest backwards — "re-baseline"
  means fixed-era-only, not a re-fit.
- **Output** `data/backtest/` is gitignored and regenerable via
  `Rscript scripts/0Nb_backtest.R`.
- Phase 2 (separate spec): extend history via `0Nr_replay.R` re-fits (football
  only); persist `portfolio_lambda` + calibration into `candidates`.

## Two arms in `2026-backtest.qmd`

The report now carries both backtests, re-rendering weekly:

1. **Stored-decision backtest** (original): PnL/ROI/calibration of the *actual
   kept decisions*, bug-era excluded. The money question, faithful stakes.
2. **Walk-forward (`R/backtest-walkforward.R`)**: the *model's* OOS calibration
   over every bettable candidate, plus a **model-vs-market** comparison. The QMD
   calls `bt_walkforward_reuse(sex, season)` in setup (REUSE mode — reconstructs
   beliefs from `beliefs/extracts/predicted_matches`, no Stan, ~70s/season) so it
   self-updates; gated on `wf_available` so it skips cleanly when extracts are
   absent. REUSE mode walks `predicted_matches` with `uncount(count)` to rebuild a
   `beliefs_latest`-shaped tibble, avoiding a full re-fit.

**Model-vs-market scoring** lives in `R/backtest-metrics.R`: `bt_devig()` adds
the margin-free market probability `q_market` (normalise `1/odds` within each
`(match, market, line)` book; drops incomplete books via `sum(p)~=1` and pushes
via `sum(win)==1` — note moneyline AND spread are 3-way here). `bt_skill()`
returns model/market Brier+log-loss + skill scores; `bt_skill_ci()` is the
**match-clustered** bootstrap CI (outcome rows within a fixture are dependent, so
resample matches, not rows). The comparison is leak-free in time but
selection-conditioned on the bettable slate — frame as "competitive with the
line on the bets we'd consider," not "beats the bookmaker."

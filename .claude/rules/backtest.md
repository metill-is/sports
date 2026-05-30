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
- **Output** `data/backtest/` is gitignored and regenerable via
  `Rscript scripts/0Nb_backtest.R`.
- Phase 2 (separate spec): extend history via `0Nr_replay.R` re-fits (football
  only); persist `portfolio_lambda` + calibration into `candidates`.

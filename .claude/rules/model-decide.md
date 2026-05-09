---
paths:
  - "R/model-*.R"
  - "R/decide-*.R"
  - "R/pipeline-freshness.R"
  - "Stan/**"
  - "config/leagues.yml"
  - "config/bankroll.yml"
  - "scripts/03_fit.R"
  - "scripts/03b_*.R"
  - "scripts/04_decide.R"
---

# Model + Decide Layers

> Authoritative theory + invariants live in the Metill Obsidian vault
> under `Sports/Knowledge/Sports Models/_MOC.md` (Bayesian model theory)
> and `Sports/Knowledge/Betting Optimisation/_MOC.md` (Kelly / portfolio
> / calibration). This file is the project-side quick reference.

## Model layer

- `fit_league(league_key, sex)` is the only public entry point. Loads
  config, calls `prepare_data()` → `fit_model()` → `extract_posteriors()`,
  writes `beliefs/latest/` (overwrite) and `beliefs/archive/`
  (accretive per `fit_date`).
- `prepare_data()` is pure — reads Parquet facts, returns
  `list(stan_data, pred_d, teams)`. No file I/O beyond `read_table()`.
- `fit_model()` is a pure cmdstanr wrapper — takes stan_data + stan_path,
  returns the fit. Callers save to disk.
- `extract_posteriors()` materialises posterior draws as the canonical
  `beliefs_latest` tibble (per-draw-per-match).
- Stan models live in `Stan/{league_key}/{file}.stan`. `leagues.yml`'s
  `stan_model` field uses this relative path.
- Daily driver: `Rscript scripts/03_fit.R`. The `needs_refit()`
  predicate in `R/pipeline-freshness.R` short-circuits (league × sex)
  pairs whose last `fit_date` is at least the latest completed
  `match_date`. Use `--force` to refit unconditionally. Run detached
  for long backfills (wall-clock several hours with 1000 MCMC iters):
  ```
  nohup Rscript scripts/03_fit.R --force > /tmp/fit.log 2>&1 & disown
  ```
- Historical backfill: `scripts/03b_backfill_football_iceland.R` fills
  in pre-round fits for early-season rounds where the daily-driver
  hadn't yet produced a partition. Each row in the script's
  `fits_to_run` tibble runs one
  `fit_league(end_date = ..., schedule_horizon_days = 200L)` call;
  partition is `fit_date=<end_date>`. Skip-if-exists keeps the script
  re-runnable. Used by the publisher's lookahead-free `xg_for/xpts`
  aggregation (see publish layer) and the forest-plot pre-season
  baseline.

## Decide layer

- `decide_league(league_key, sex)` is the public entry — chains
  `prepare_odds` + `kelly_joint` + `portfolio_optimise` +
  `compute_calibration`, writes candidates (with `stage` column) +
  recommendations Parquet.
- Joint Kelly is the only mode (per 2026-03-06 memory note).
- **Stake formula (post Plan 7a, 2026-04-29):**
  `bet_amount = round(kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling) × current_pool)`.
  - `kelly_raw` is the unconstrained joint Kelly fraction from
    `kelly_joint()`; bounded above by `max_match_stake` (per-league
    override or `bankroll$max_match_stake_default`, default
    1.0 = unconstrained).
  - `kelly_frac` is the §7.2 multiplicative shrinkage (Browne γ);
    per-league in `leagues.yml::*.betting.kelly_frac` (scalar or
    `{male, female}`).
  - `calibration` is the Beta-Binomial multiplier from
    `compute_calibration()`, clamped to `[0.5, 1.5]`.
  - `kelly_ceiling` is the K5 hard cap (default 0.25 in
    `bankroll.yml`).
- Per-sex `kelly_frac` supported via object form in `betting:` config
  (basketball/football use per-sex; handball uses scalar).
  Browne-grounded defaults: male 0.20, female 0.10–0.15. Note: per
  the 2026-05-02 operational override
  ([memory: project_kelly_frac_cut_2026_05_02](../../.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_kelly_frac_cut_2026_05_02.md)),
  per-cell values are currently scaled to ~25 % of the Browne defaults.
- Daily driver: `Rscript scripts/04_decide.R`. Wall-clock ~seconds.
  Runs on every odds scrape via `decide-publish.yml`'s `workflow_run`
  trigger so recommendations stay fresh as odds drift.

## See also

- Placement rules (P1–P4) and ledger invariants (L1–L4):
  `.claude/rules/sports-betting.md`
- Stan model conventions (cmdstanr, non-centred parameterisations):
  `.claude/rules/stan-conventions.md`
- Publish layer (consumes beliefs/extracts/ for football,
  beliefs/fits/ for basketball/handball):
  `.claude/rules/publish-layer.md`

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
  writes `beliefs/latest/` (overwrite). The accretive per-`fit_date`
  store depends on the league: basketball + handball iceland write to
  `beliefs/archive/`; football iceland writes per-fit summary Parquets
  to `beliefs/extracts/` instead (Phase 3b, 2026-05-04 —
  `extract_football_iceland()` is invoked from inside `fit_league` and
  the legacy `beliefs_archive` per-draw write is skipped). The
  `force_archive_write = TRUE` override exists only for the one-off
  2026-05-04 → 2026-05-25 backfill in
  `scripts/03c_backfill_football_archive_2026_05.R`; daily fits never
  set it.
- `needs_refit()` consults both `beliefs/archive/` and
  `beliefs/extracts/` and takes the freshest `fit_date` across the two
  stores, so the predicate works uniformly across leagues regardless of
  which store is canonical for that cell.
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
- **Team-name normalisation (post 2026-05-11):** `prepare_odds` calls
  `normalise_lengjan_team_names()` to rewrite Lengjan-side strings
  ("Grindavík kv", "Þróttur Rvk kv") to federation-canonical form
  ("Grindavík", "Þróttur R.") so the per-match join against
  `beliefs_latest` succeeds. The inverse map comes from
  `leagues.yml::*.lengjan.team_names[[sex]]` — same source the placer
  uses in the forward direction. Unmapped names pass through with a
  `cli_alert_warning`; the join still produces a warn-and-skip at
  `decide-pipeline.R:131` (`"decide: no beliefs for ..."`). Invertibility
  of the map (each Lengjan rendering ← one canonical) is enforced by
  both the normaliser and `validate_team_names_config()`.
  A `team_names` value may be a **list** of acceptable renderings, not
  just a scalar — used when Lengjan shows one team under more than one
  byte-distinct string (e.g. football_iceland female `Grindavík/Njarðvík`
  → `[Grindavík / Njarðvík kv, Grindavik/Njarðvík kv]`).
  `tn_renderings()` (`R/config.R`) collapses scalar-or-list to a rendering
  vector (primary first); the decode inverse points every rendering at the
  one canonical, and the placer's `resolve_bet_match_id()` tries every
  rendering against the live page so the bet resolves regardless of which
  form Lengjan currently shows.
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

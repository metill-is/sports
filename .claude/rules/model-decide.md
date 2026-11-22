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
- What a fit trains on is `model_training_results()` (`R/model-prepare.R`),
  shared by `prepare_data()`, both Iceland extractors' strength
  trajectories and `needs_refit()`. It drops two kinds of real result:
  the league's `training_filter`, and **forfeits** — the sport's walkover
  score in `.FORFEIT_SCORES` (basketball 20-0, handball 10-0; football has
  none, its 3-0 is indistinguishable from a played one). A league table
  counts forfeits, so it is built from `.played_results()` instead.
  Why forfeits: Þróttur V. (men's basketball) ended its 25-game history on
  three 20-0 / 0-20 walkovers, and the fit failed the Stan diagnostic gate
  on that team's volatility on 2026-09-16, twice, on data that had passed
  on 2026-09-05. A gate failure that names one team's parameters is a data
  question before it is a sampler question.
- `fit_model()` is a pure cmdstanr wrapper — takes stan_data + stan_path,
  returns the fit. Callers save to disk.
- **Engine switch (2026-11-16).** `fit_league()` reads an optional league key
  `engine` (`stan` | `inla`, absent = `stan`; schema enum in
  `config/leagues.schema.json`). No league sets it, so every fit is Stan.
  `engine: inla` routes to `fit_model_v2_inla()` (`R/model-v2-fit.R`), which
  fits football model **v2.0** (`football_v2_options()`: one strength walk
  per team, reversion 0.8 at the season break, fixed style, league-wide
  scoring-level and home-advantage walks) with R-INLA and returns a fit
  under the production Stan variable names (`fv2_as_fit()`,
  `R/model-v2-adapter.R`), so `extract_posteriors()`, the extractors and the
  publisher run unchanged. It is football-only, refuses a cell with fewer
  than two seasons of results before the season being forecast (INLA was
  measurably worse than NUTS on one earlier season), has its own gate (`fv2_inla_gate()`; the Stan sampler gate has no
  INLA counterpart) and writes a `fit_diagnostics` row with `n_obs` and `NA`
  sampler metrics, because `needs_refit()` reads `n_obs` off the newest row.
  That row also carries `engine`, `model_version`, the fit's hyperparameter
  sds as JSON (`hyper_sd`) and `jump_checked`; Stan rows read as `NA` there.
  The gate's fifth check compares each hyperparameter with the previous INLA
  fit of the cell (`fv2_previous_hyper()`, strictly earlier date, same model
  version, gate passed) and refuses a move by more than a factor 3 (the
  largest between consecutive re-estimates in the 2022 to 2026 walk-forward
  was 1.65). When no previous fit is on record `fit_league()` says in the
  log that the check will not run, and the row has `jump_checked = FALSE`:
  a check that did not run is never reported as passed. If a refused change
  is real, rerun with `SPORTS_INLA_ACCEPT_JUMP=1`; the accepted values become
  the next reference.
  **Repeatability (2026-11-17):** the engine fits on one thread and passes
  `internal.opt = FALSE` to R-INLA (`fv2_fit_inla()`), so that a refit of the
  same data returns the same numbers. Of 140 production fits of the two
  cells, one R process each, on one machine: on four threads no two agreed,
  on one thread with the option on 39 of 40 did, on one thread with it off
  40 of 40. On four threads a fixture's expected goals varied between
  processes with an sd of 0.005 to 0.009 at the median (the Monte Carlo
  standard error of 4,000 draws is 0.021) and by up to 0.17 between two
  processes. One thread costs about 30 s of a men's fit's 108 s. Agreement
  across machines was not measured. `fit_league()` passes no `threads`, and
  `test-model-v2.R` pins the thread count, the option and a two-process
  comparison.
  **What "current strength" means on this engine:** each team's state at its
  next match, and the league levels of the first fixture week. The simulators
  hold a team at one state for all its remaining matches and the forecast of
  the next match reads the state at that match, so both read the same state
  (`cur_slot`, `cur_week_nr` in `fv2_build()`). Before a season that state
  has crossed the break: it has reverted toward the division mean and carries
  the break variance. Teams whose first match is beyond the fit horizon are
  carried into the new season as likelihood-free states
  (`fv2_carry_slots()`), so a simulated table never mixes teams on both
  sides of a season break. Who reverts, and toward which mean, is defined
  once (`fv2_reversion_groups()`) and read by both the INLA precision and
  the Stan reference program.
  `run_fit_targets()` must keep `engine` in its whitelisted slice
  (`test-pipeline-run-isolation.R`). INLA is not in `DESCRIPTION`: the code
  guards with `requireNamespace()` (for INLA and for `sn`, which INLA only
  suggests but its posterior sampler needs) and the tests skip without them. Switching
  a league is the owner's decision; `test-model-v2.R` pins "no league is on
  the INLA engine" so the switch has to edit that expectation. Evidence and
  cutover plan: `docs/reports/2026-11-15-football-v2-proposal.qmd`.
  **Weekly Stan reference fit (2026-11-22, decision 5).** Once a week
  `engine-reference.yml` runs `scripts/engine_reference.R`: for each football
  cell the v2.0 specification is built from the latest data by
  `fv2_production_inputs()` -- the same function `fit_model_v2_inla()` builds
  its inputs with, so the two cannot drift -- and fitted by R-INLA as
  `fit_league()` would fit it that day (its `.fit_seed()`, full integration,
  one thread, `fv2_inla_gate()`; on one thread that reproduces the production
  fit of the same data) and by NUTS with
  `research/football-v2/stan/fv2_rev.stan` (`fv2_weekly_reference()`,
  `R/model-v2-reference.R`). It compares the two engines' outcome
  probabilities fixture by fixture; with no fixture inside the horizon it
  compares every pairing of the season's league divisions on the next season's
  first day (a week ahead if later), the break path a pre-season fit takes, so
  it runs all year. A mean absolute difference above 0.01
  (`FV2_REFERENCE_MAX_MEAN_ABS_DIFF`), a NUTS fit past the health layer's
  bar for a Stan fit (divergences over 0.5 % of transitions or Rhat above
  1.02, `health_thresholds()`; or no Rhat at all), or an INLA fit without a
  mode or failing its gate does not pass; the summary row goes to
  `data/health/engine_reference/` and `check_engine_reference()` warns. The
  Stan program stays in `research/` until the switch.
- `extract_posteriors()` materialises posterior draws as the canonical
  `beliefs_latest` tibble (per-draw-per-match).
- Stan models live in `Stan/{league_key}/{file}.stan`. `leagues.yml`'s
  `stan_model` field uses this relative path.
- Daily driver: `Rscript scripts/03_fit.R`. The `needs_refit()`
  predicate in `R/pipeline-freshness.R` short-circuits (league × sex)
  pairs whose last `fit_date` is at least the latest completed
  `match_date` and whose training row count is unchanged: a recount of
  `model_training_results()` at that `fit_date` must equal the fit's
  `fit_diagnostics` `n_obs` (the daily fit runs before that day's evening
  games, which arrive dated on the fit date; a fit with no `n_obs` falls
  back to the date rule) — unless the next 14 days hold fixtures the newest fit
  predicted none of (the pre-season / long-break rule, spec 2026-09-16
  §8). That rule reads the newest fit's predictions from `extracts/` and
  `archive/` together (the extract keeps only publish divisions, so
  handball's `PO` games live in the archive), skips unreadable files one
  by one, and ignores fixtures with a team that has no completed result
  (`prepare_data()` drops those, so no fit could ever cover them). Use
  `--force` to refit unconditionally. Run detached
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
  `cli_alert_warning`; the join still produces `decide_league()`'s per-match
  warn-and-skip (`"decide: no beliefs for ..."`). Invertibility
  of the map (each Lengjan rendering ← one canonical) is enforced by
  both the normaliser and `validate_team_names_config()`.
  An unmapped pass-through can land on another fixture's canonical key:
  sex-less DOM rows (all of football's) reach both cells, so in the female
  cell the men's "FH" v "Valur" lands on the key of "FH kv" v "Valur kv".
  `prepare_odds()` drops, with a warning naming the fixture, any sex-less row
  with a name among the other sex's `team_names` renderings and neither among
  this cell's (so a men's listing with one renamed team is caught too). A
  listing neither map knows still passes through, and a cell with no
  `team_names` of its own drops nothing. The rule relies on disjoint male and female rendering sets, which
  `test-decide-odds.R` asserts for football on the real config. When one
  scrape lists a fixture under two spellings, each (market, line) is priced
  from one listing, primary rendering first. Each match is priced from its
  latest scrape only (rows sharing one `scraped_at`, which every scrape run
  stamps on all its rows), so a selection or line missing from the newest
  scrape is treated as withdrawn; on the DOM scraper a failed detail-page
  fetch looks identical, so that match's spread/total drop out until the next
  good scrape.
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
  ([memory: project_kelly_frac_cut_2026_05_02](/Users/brynjolfurjonsson/.claude/projects/-Users-brynjolfurjonsson-sports/memory/project_kelly_frac_cut_2026_05_02.md)),
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

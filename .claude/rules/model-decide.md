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
  publisher run unchanged. This branch is football's; handball and basketball
  have their own (below). It refuses a cell with fewer
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
  processes. One thread costs about 30 s of a men's fit's 108 s.
  **Across machines it does not hold (measured 2026-11-24, PR #120):**
  R-INLA's Linux build runs on MKL, whose code path follows the CPU, so a
  GitHub runner with an Intel CPU and one with an AMD CPU return different
  fits of the same data. `fit.yml` and `engine-reference.yml` both take
  whichever `ubuntu-latest` runner GitHub assigns. AMD EPYC 7763, 9V74 and
  9V45 agreed with each other bit for bit; Intel Xeon 6973P-C, 6975P-C and
  Platinum 8573C did not agree with them. The difference starts in the last
  bits but does not stay there: on a flat hyperparameter posterior the
  optimiser ends at a different point of the ridge. Two MKL code paths on
  one runner (default against `MKL_CBWR=COMPATIBLE`, real handball and
  basketball cells) moved the observation sds by at most 0.5 %, each model's
  team spread by up to 7 % and single components (the club / season / week
  split) by up to 56 %. On the simulated handball league of
  `test-model-v2-hbbb.R` it decided the gate (club sd 1.23 on AMD, 9.08e9 on
  Intel). Measured on handball/basketball fits; football runs the same
  binary. So a fit is repeatable on one machine, not across runner vendors.
  The known lever is `MKL_CBWR=COMPATIBLE`, the only setting that gave
  bit-identical fits on Intel and AMD. Its cost was measured for handball and
  basketball only, on one AMD runner: 7.9 against 7.9 s for men's handball,
  35.0 against 35.3 s for men's basketball, 35.8 against 27.0 s for women's
  basketball (two fits each). It was not measured on Intel or for football.
  It is not set; pinning it changes every engine's numbers and is the owner's
  call. `fit_league()` passes no `threads`, and
  `test-model-v2.R` pins the thread count, the option and a two-process
  comparison.
  **Team order (2026-11-26):** `fv2_build()` numbers teams in byte order
  (`sort(method = "radix")`), the same under every collation. The index sets
  R-INLA's node order, and reordering moved the football cells' outcome
  probabilities by 0.0016 to 0.0025 on average and states by up to 0.11
  posterior sd (C against en_US.UTF-8, data of 2026-11-23); testthat sorts
  under C and production scripts under en_US, so a locale-dependent order
  made tests and production fit different models (`test-model-v2-team-order.R`).
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
  fit of the same data when both runners have the same CPU vendor, which
  nothing guarantees: see Repeatability) and by NUTS with
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
  Stan program stays in `research/` until the switch. NUTS samples 2,000
  iterations per chain (4 chains, 1,000 warmup): at 1,000 the men's cell
  failed the Rhat bar on every run from its first (1.026 on 2026-11-23, 1.041
  the next day) on one slowly mixing hyperparameter, the strength walk's sd
  `sigma[1]` (Rhat 1.031, bulk ESS 147, while every forecast was at 1.007
  and every state at 1.005); at 2,000 it was 1.0026 (ESS 329) and nothing
  exceeded 1.004, for 348 s against 232 s on the Mac.
- **Handball / basketball on the INLA engine (switch path, 2026-11-23).**
  `engine: inla` on `handball_iceland` or `basketball_iceland` routes to
  `fit_model_v2_hbbb()` (`R/model-v2-hbbb.R`): v2 (margin, total) of
  `docs/reports/2026-11-15-hb-bb-v2/report.md`, two `fit_v2_inla()` fits with
  the latent components drawn jointly (`latent =`). Handball is the static
  form on final scores with no draw shift (D4 open); basketball has the
  within-season AR(1), trains on regulation-time scores (rows without them
  leave the likelihood but keep their state), takes a level-score shift per
  division (men) or one (women) from each training row's leave-one-out
  predictive, and sends level regulation draws through overtime, so
  `goals*_pred` are final scores and never level. `hbbb_v2_as_fit()` emits
  the Stan 2DT names (`cur_*`, `offense[r,k]`, `home_advantage_*`,
  `mean_goals`, and the simulator scalars set so its bivariate draw is the
  v2 Gaussian: one sd, one correlation, `nu = 1e6`). Gate
  `hbbb_v2_gate()`: mode status 0 for both halves, each model's team spread
  (root sum of squares of the club, season and week sds) and the home
  advantage, observation sds and mean total inside `HBBB_V2_SANITY`, no sd
  above 5 times its prior bound, basketball regulation coverage of the latest
  season at least 90 %, and the jump check against
  `fv2_previous_hyper(version = HBBB_V2_VERSION)` on the observation sds and
  team spreads only (`HBBB_V2_JUMP_CHECKED`): the club / season split is
  weakly identified, so a per-component floor or jump would stop fits for
  nothing. The diagnostics row is
  the football engine's (`persist_fit_diagnostics_inla()`, `n_obs` =
  `prepare_data()`'s N), plus, for basketball, `n_obs_regulation` (the rows
  with regulation scores), which `needs_refit()` recounts: a game card that
  arrives after the fit starts a refit though the row count stands. R-INLA
  returns draws sorted by hyperparameter configuration, so the total fit's
  draws are shuffled before draw i of each half is paired
  (`hbbb_v2_pair()`). A half whose summary is numerically unusable (mode
  search failed, or an sd not finite or above 5 times its prior bound,
  `hbbb_v2_half_unusable()`) is fitted once more, restarting R-INLA's
  optimiser at its own mode (`hbbb_v2_fit_halves()`), and the fit records
  `attempts` per half; the gate then judges the second attempt as it would
  the first. Why: R-INLA's Linux build runs on MKL, whose code path follows
  the CPU, and on the simulated handball league the total model's numerical
  Hessian at the mode came out nearly singular on Intel Xeon runners and not
  on AMD EPYC ones, which end at a nearby point of the same flat ridge (club
  sd 9.08e9; the engine CI failures of 2026-11-23/24). The restart lands on
  the same mode to ten digits and recomputes the Hessian there (club sd
  1.25). A
  cold refit would repeat the first attempt bit for bit. No league is switched.
  **Weekly Stan reference fit (2026-11-24).** `scripts/engine_reference.R`
  also runs each handball and basketball cell (`hbbb_v2_weekly_reference()`,
  `R/model-v2-hbbb-reference.R`): the spec `hbbb_v2_spec()` builds, fitted by
  R-INLA as `fit_league()` would that day (`hbbb_v2_reference_inla()`: its
  `.fit_seed()`, 4,000 draws, full integration, the production pairing and
  gate) and by NUTS with `docs/reports/2026-11-15-hb-bb-v2/stan/v2_margin.stan`
  and `v2_total.stan` (`hbbb_v2_reference_nuts()`; the Stan data is the same
  spec, `hbbb_v2_stan_data()`, with basketball rows lacking regulation scores
  dropped from the likelihood as R-INLA's `NA` drops them). It compares, per
  fixture, the result probabilities (home, level, away; regulation time for
  basketball) and the probabilities of clearing the half-point spread and
  total lines nearest the expected margin and total, from the predictive
  locations and sds (no score draws, no level-score shift, no overtime: both
  are the same for either engine). With no fixture inside the horizon it
  compares every pairing of each division on the date of the next scheduled
  fixture (a break inside the season keeps the season; women's handball on
  2026-11-24 had none until 2026-12-10), or a week ahead in the next season
  when nothing is scheduled (`hbbb_v2_break_fixtures()`). Limits
  (`HBBB_REFERENCE_LIMITS`, set from measured differences in
  `docs/evidence/hbbb-engine-reference/`, which says which limits were set
  after seeing which runs: 2026-10-09 is the one held-out in-season date,
  and the break has none beyond one cell): on the mean over the compared
  fixtures, within a season 0.01 on the result and 0.025 on each line
  (largest measured 0.0039 and 0.0116 over eleven cell-runs at three dates)
  and across the season break 0.02 and 0.05 (0.0065 and 0.0250); on any
  single fixture, 0.04 on a result probability and 0.075 on a line within a
  season, 0.06 and 0.14 across the break, about twice the largest measured
  (0.0203 / 0.0370 and 0.0302 / 0.0704). The single-fixture limits catch an
  error confined to a few fixtures (one team's indices, one division's map)
  that the means hide: one team's margin off by about 0.6 goals (handball)
  or 1.6 points (basketball) for the median team within a season, against 1
  to 3 goals and 7 to 9.5 points with the means alone
  (`docs/evidence/hbbb-engine-reference/diagnostics/detection.R`). A run is
  across the break while any fixture team has no
  match (with a response) yet in the fixture's season
  (`hbbb_v2_across_break()`, decided per team: such a team's state is the
  AR(1)'s reversion of the last season, so its forecast reads the club /
  season split however many others have played). The break is looser
  because that split, which the data identify only as a sum, reaches the
  forecast, and the engines split it differently (basketball's off-season
  totals 0.4 to 0.8 points apart on average, NUTS converged). The row
  stores the limits it was judged by (`threshold`, `line_threshold`,
  `max_threshold`, `line_max_threshold`) and the health check reads them.
  **The NUTS bar** is the health layer's (divergences under 0.5 % of
  transitions per half, Rhat at most 1.02) applied to what the comparison
  reads, each fixture's predictive location and the observation sd
  (`hbbb_v2_rhat_groups()`; the row's `nuts_rhat_scope` is `"forecasts"`,
  football's `"hyperparameters and states"`). The forecasts were at most
  1.007 on every passing fit while the hyperparameters reached 1.076 (bulk
  ESS 46) and the club and season levels 1.046 (men's basketball margin,
  2026-10-09), the same split the gate's jump check leaves ungated. The
  other groups' Rhat goes to the run's `diagnostics_*.csv`. NUTS runs at
  `adapt_delta` 0.99 (women's basketball margin: 41 divergences in 4,000 at
  0.95, none at 0.99, 1.7 times the sampling time), and each chain samples
  2,000 iterations instead of 1,000 across the season break, where the
  forecast reads the slow club / season split (men's basketball off-season:
  forecast Rhat 1.023 at 1,000, 1.003 at 2,000).
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

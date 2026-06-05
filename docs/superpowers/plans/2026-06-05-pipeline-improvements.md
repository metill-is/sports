# Pipeline Improvement Roadmap — 2026-06-05

Synthesised from a 6-investigator research workflow (data · model · betting ·
publish/metill.is · decision-analyses · round-fix) whose claims were verified
against the live repo. Companion to the auto-place decision analysis
(`docs/reports/2026-autoplace-decision.qmd`).

## Framing (read first)

The forensic verdict stands: **no demonstrable betting edge** at the captured
sample (n≈56, −8.1%). Therefore **stake/threshold/model tuning on PnL is
noise-chasing** and is gated on evidence throughout this plan. The defensible
work now is, in order: (1) the round-column fix (asked); (2) cheap read-only
observability + write-boundary correctness; (3) the two concrete real-money
bugs, made urgent by the now-live autoplace agent; (4) edge-measurement
analyses (CLV, by-market) — the only metrics with statistical power at this
sample size; (5) model-quality diagnostics before any model-structure change.

### Verification corrections (trust these over the raw investigator reports)

- **CLV exact `(match,market,outcome,line)` join hits only ~10% of placed bets**,
  not ~73% — spread/total lines drift, so CLV needs line-interpolation + de-vig.
  Build cost is M–L, not trivial.
- **`kickoff_time` is NOT on disk** in the schedules store (despite a memory note
  claiming PR #24 added it) — no true pre-kickoff closing cutoff exists; CLV must
  proxy with last-scrape-before-match-day.
- **By-market ROI magnitudes quoted by investigators did not reproduce** on the
  live ledger. Live recomputation: spread +21.5% (n=102), moneyline −5.6%
  (n=149), total −6.8% (n=153). Direction is real and powered; re-measure before
  acting.
- **`current_pool` is inflated ~4.5×**: initial 23,610 + 83,317 settled pnl, of
  which football is −6,973 and *paused* handball is +90,315. Football is staked
  off phantom capital — a live over-staking bug.

## Phase 0 — round fix + write-boundary hardening (no sign-off, no deps)

### 0a. Round column (THE asked deliverable) — DERIVE, do not scrape

KSÍ exposes no umferð/round (verified via live WebFetch of results + upcoming
views; committed fixture has zero `umferð` text), so scraping is impossible;
deriving is the only path. `round` is a dead column today — no consumer reads it
(`model-prepare.R:216` and `round-cutoff.R:52` derive their own per-team index;
publishers derive matchweek locally), so populating it is purely additive.
Backfill is safe: `round` is **not** in the upsert natural key
(`storage.R:229-236`) and new-row-wins on collision (`:253-254`), so re-derive +
upsert overwrites NA in place with zero row churn.

1. `R/derive-round.R::derive_league_round(df)` — for non-cup divisions, within
   each `(sport, country, sex, season, division)` set
   `round = dplyr::dense_rank(match_date)`. Gate on `division != "CUP"` (cup dates
   are knockout bracket rounds, left NA). Roxygen-document the postponement
   approximation.
2. Apply in `R/ingest.R` immediately before the `upsert_table` calls — all three
   scrapers benefit; bodies keep `round = NA_integer_` as the pre-derive default.
3. `scripts/0Nm_backfill_round.R` — read results + schedules, apply, upsert back,
   commit rewritten Parquet. Idempotent. In-script assertion: per
   `(season, division)`, `min(round)==1` and round monotonic non-decreasing with
   `match_date`.
4. `tests/testthat/test-derive-round.R` — (a) starts at 1 per group; (b)
   monotonic; (c) same-date matches share a round; (d) cup rows stay NA; (e)
   regression guard that nothing reads the schema round.

Defer (low value): cup bracket-round labelling; KKI/HSI source-column check
(both sports paused).

### 0b. Normalise `scraped_at` to UTC-tagged + migrate (no sign-off)

51 UTC-tagged vs 42 naive odds partitions → `unify_schemas` fails →
`read_table` silently falls back to first-fragment-wins, blocking safe schema
evolution. Force `scraped_at = lubridate::with_tz(Sys.time(), "UTC")`
(`ingest-lengjan-odds.R:256`) + one-off re-tag migration (values unchanged,
Iceland==UTC).

## Phase 1 — cheap read-only observability (no sign-off, no deps)

- **dropped_min_bet instrumentation** — health row: count + summed EV of
  `dropped_min_bet` vs `kept` candidates per cell/run (the min-bet floor drops
  ~2.5× more +EV bets than it keeps under the 25%-Browne cut).
- **odds soft-fail vs empty signal** — write `data/health/last_odds_scrape.json`
  with per-league `{status: fetched|empty|soft_failed, n_rows, ts}` so
  `/pipeline-doctor` stops needing log-grep.
- **team-name lint** — offline script diffing odds team names vs `leagues.yml`
  `team_names` keys (stringdist suggestions); catches a Lengjan rename in a day.
- **schedule-active cold-start guard** — distinguish "dir missing" (all-active
  correct) from "read errored" (corruption → FAIL signal).

## Phase 2 — real-money bug fixes (NEEDS SIGN-OFF; urgent, autoplace is live)

- **Scope `current_pool` to active cells** (`config.R` `load_bankroll` +
  `bankroll.yml`) — stop sizing football off the 4.5× inflated pool. Sign-off on
  segmentation policy (per-active-cell vs hard cap).
- **Football-scoped drawdown circuit-breaker** (`auto-place.R` + `health.R`) — the
  only drawdown guard fires at `current_pool < 60% of initial`, masked by
  paused-cell winnings; football could lose the whole 23.6k deposit invisibly.
  Per-cell cumulative-pnl stop-loss since 2026-06-05 that refuses to place.
  Depends on the pool-scoping fix. Sign-off on threshold.

## Phase 3 — edge-measurement analyses (the only powered metrics)

- **CLV tracker** (`R/clv.R` + `docs/reports/2026-clv.qmd` + health row) — needs
  no settled outcomes, signal per placed bet immediately; the only weeks-timescale
  read on the autoplace experiment. Build with line-interpolation + de-vig +
  last-scrape-before-match-day proxy (see corrections). **Prerequisite evidence
  for any stake change.**
- **Realised by-market edge decomposition** with Wilson/bootstrap CIs from the
  settled ledger (powered, unlike the backtest). Cheap, standalone.
- **Capture-rate & miss-reason time-series** — is autoplace closing the gap?
  Needs the placer to persist per-attempt outcomes to an append-only
  `placement_log` (local-only, never touches the ledger).

## Phase 4 — model quality (NEEDS SIGN-OFF; gated on a diagnostic)

- **Whole-population model-validation report** (`R/model-validate.R` +
  `2026-model-calibration.qmd`) — wire `loo::loo` (log_lik already emitted, never
  run) + PIT/reliability of full match-outcome probs by division/season/phase.
  Read-only, no sign-off. **Prerequisite for any model-structure change.** Also
  the powered test of the season-calibration hypothesis.
- Then, gated on the diagnostic: cheap **100-day time-gap cap lift** probe
  (`model-prepare.R:219/300/316`); if validated, the **between-season strength
  shock** (winter break currently treated as a 100-day in-season gap — the
  model carries strength across winter nearly unchanged, the structural cause of
  stale early-season strengths). Hierarchical home advantage; promoted-team tier
  prior. All need backtest validation via `0Nr_replay.R`.

## Site (metill-platform, ~/metill-platform/; all NEED SIGN-OFF)

- **Wire up the dead rank-bump chart** — shipped 2026-05-29, never imported;
  `standings_history.json` is fetched every page load and discarded. Highest
  impact-per-hour on the site.
- **Public model track-record / reliability view** — the site shows forecasts
  with zero accuracy evidence; data already ships lookahead-free.
- **Cross-repo contract drift guard** in `validate_publish.py` — fail the deploy
  when a published cell has no consumer route (the silent-404 failure mode).
- Surface the already-published `team_strengths_history` form curve; trim unused
  page DATASETS fetches.

## Dependencies

Stake changes (kelly_frac revert, per-market calibration) gated on **CLV showing
positive value + a backtest**. Model-structure changes gated on the
**validation report**. Drawdown breaker depends on pool-scoping. Everything in
Phases 0–1 is dependency-free and shippable today.

## Note: research-workflow hygiene

The research investigators ran with the default (writable) workflow agent and
one made stray comment-only edits to `R/model-prepare.R` + the football Stan
model (reverted). Future pure-research fan-outs should use `agentType: 'Explore'`
(read-only).

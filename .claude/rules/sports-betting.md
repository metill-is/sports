---
paths:
  - "R/decide-*.R"
  - "R/placer-*.R"
  - "config/leagues.yml"
  - "config/bankroll.yml"
  - "scripts/place_bets.R"
  - "scripts/preview_bets.R"
  - "tests/testthat/test-decide-*.R"
  - "tests/testthat/test-placer-*.R"
---

# Betting Pipeline Conventions (post Plan 6 + Plan 7a)

> Authoritative theory + invariants live in the Metill Obsidian vault under
> [`Sports/Knowledge/Betting Optimisation/`](obsidian://). Read the
> [`_MOC`](obsidian://Sports/Knowledge/Betting%20Optimisation/_MOC) for
> the cold-start reading order; this file is the project-side quick
> reference.

## Architecture (current)

The decide layer and the placer are now both in this repo:

```
config/leagues.yml      ── per-league betting config + team_names + Stan model
config/bankroll.yml     ── global initial_pool, kelly_ceiling, max_match_stake_default,
                           daily_budget_frac, daily_budget_min_isk
        │
        ▼
R/decide-pipeline.R::decide_league(league, sex)
        │
        ├── R/decide-odds.R::prepare_odds()                 (read odds Parquet, filter horizon)
        ├── R/decide-kelly.R::kelly_joint()                 (Stage-1 SLSQP, capped by max_match_stake)
        ├── R/decide-portfolio.R::portfolio_optimise()      (Stage-2 daily-budget scaling)
        ├── R/decide-calibration.R::compute_calibration()   (Beta-Binomial multiplier in [0.5, 1.5])
        └── §7.2 chain in decide_league():
              shrink_eff = min(kelly_frac × calibration, kelly_ceiling)
              kelly      = kelly_raw × portfolio_lambda × shrink_eff
              bet_amount = round(kelly × current_pool)
        │
        ▼
data/decisions/recommendations/sport=*/country=*/run_date=*/   (Parquet, ephemeral per run_date)
        │
        ▼
R/placer-pipeline.R::place_bets()                            (LOCAL ONLY — never on CI)
        │
        ├── R/placer-validate.R                              (pre-flight team_names + recs schema)
        ├── R/placer-load.R::dedup_against_ledger()          (P1 idempotency)
        ├── R/placer-login.R                                 (LENGJAN_USER / LENGJAN_PASS from .Renviron)
        ├── R/placer-navigate.R::extract_matches()           (find Lengjan match IDs)
        ├── R/placer-place.R::place_bet()                    (P3/P4 enforcement)
        └── R/placer-ledger.R::append_to_ledger()            (only writer; canonical Parquet)
        │
        ▼
data/decisions/ledger/sport=*/country=*/   (Parquet, append-only, immutable per row)
```

CSV dual-write was retired by Plan 6; Parquet is canonical.

## `config/leagues.yml::*.betting` schema (post Plan 7a)

```yaml
betting:
  # §7.2 multiplicative shrinkage; per-(league, sex) Browne γ
  kelly_frac:
    male: 0.20
    female: 0.10        # tighter for cold-start cells; or scalar form: kelly_frac: 0.20
  # Stage-1 solver cap; rarely binds (defaults to bankroll.max_match_stake_default)
  max_match_stake: 0.50
  ev_threshold: 0.0     # min EV to enter optimiser
  markets:
    moneyline: true
    spread: true
    total: true
  scoring:
    has_ties: true       # false for basketball
    tie_threshold: 0.5   # 0 for football, 0.5 for handball
  min_bet: 200           # ISK floor; bets below are dropped post-shrinkage
  max_age_hours: 48      # warn if odds older than this
```

## `config/bankroll.yml` schema

```yaml
initial_pool: 23610            # known deposits at workspace start
daily_budget_frac: 0.05        # max fraction of current_pool per day (Stage-2 cap)
daily_budget_min_isk: 1000     # ISK floor — never under-bet on a quiet day
kelly_ceiling: 0.25            # K5 hard cap on kelly_frac × calibration_multiplier
max_match_stake_default: 1.0   # Stage-1 default if a league doesn't override
```

`current_pool` is **not** a config field — it's derived per run by
`R/config.R::load_bankroll()` as `initial_pool + Σ(ledger.pnl[settled])`.

## Stake formula

The bet-amount decomposition (always — this is the canonical chain
documented in
[`code-map.md`](obsidian://Sports/Knowledge/Betting%20Optimisation/code-map)):

```
shrink_eff = min(kelly_frac × calibration_multiplier, kelly_ceiling)
kelly      = kelly_raw × portfolio_lambda × shrink_eff
bet_amount = round(kelly × current_pool)
```

| Factor | Computed by | Default / source |
|---|---|---|
| `kelly_raw` | `R/decide-kelly.R::kelly_joint()` SLSQP | unconstrained, capped at `max_match_stake` |
| `portfolio_lambda` | `R/decide-portfolio.R::portfolio_optimise()` | 1.0 unless daily budget binds |
| `kelly_frac` | `config/leagues.yml::*.betting.kelly_frac` | per-(league, sex) Browne γ ≈ 0.10–0.25 |
| `calibration_multiplier` | `R/decide-calibration.R::compute_calibration()` | Beta-Binomial in `[0.5, 1.5]`, prior_weight=30 |
| `kelly_ceiling` | `config/bankroll.yml` | 0.25 (K5 invariant) |
| `current_pool` | `R/config.R::load_bankroll()` | `initial_pool + Σ(settled pnl)` |

## Invariants (canonical set in Obsidian
[`rules.md`](obsidian://Sports/Knowledge/Betting%20Optimisation/rules))

### Kelly fraction calibration (K1–K6)

- **K1** — `calibration_multiplier = clamp((w₀·r₀ + Σwin) / (w₀ + Σp), 0.5, 1.5)`
- **K2** — Per (league, sex). Split by market only at ≥ 100 settled bets per market.
- **K3** — `prior_weight = 30` anchors near 1.0 until ~30 settled bets per cell.
- **K4** — Floor on multiplier = 0.5; never collapse to zero.
- **K5** — Ceiling on `kelly_frac × calibration_multiplier` = 0.25 (mechanical clamp in `decide_league()`).
- **K6** — Recomputed every `decide_league()` call from the live ledger.

### Placement (P1–P4)

- **P1** — Idempotent: `dedup_against_ledger()` runs before any browser session opens.
- **P2** — Ledger records *actual* Lengjan odds (`odds_placed`), not the recommendation's odds.
- **P3** — If live odds drift > 1 % from the recommendation, recompute Kelly stake at the new odds.
- **P4** — Reject if the bet is no longer +EV at live odds (status `not_positive_ev`).

### Ledger immutability (L1–L4)

- **L1** — A row in the ledger means money was committed on Lengjan. No "logged but never placed" state.
- **L2** — Rows are never deleted.
- **L3** — Bet parameters (`odds_placed`, `bet_amount`, `outcome`, `line`) are frozen at write time.
- **L4** — Settlement only fills `settled` / `win` / `pnl`; nothing else changes.

### Recommendations + bankroll (R1–R4, B1–B3)

- **R1** — `recommendations` Parquet is ephemeral, overwritten every `decide_league()` run.
- **R2** — Recommendations exclude bets already in the ledger (anti-join on match + market + outcome + line).
- **R3** — Recommendations exclude past matches (`match_date >= run_date`).
- **R4** — Odds must be fresher than `betting.max_age_hours`.
- **B1** — All ledger rows are outstanding until settled.
- **B2** — `bankroll = initial_pool + Σ(settled pnl)`.
- **B3** — Per-match hard cap = `max_match_stake × kelly_ceiling` of bankroll (post Plan 7a default: `0.50 × 0.25 = 0.125`).

## Local-only enforcement

`R/placer-*.R` is **never** wired into CI. The
`tests/testthat/test-placer-ci-isolation.R` test fails the build if any
`.github/workflows/*.yml` references `R/placer-`, `place_bets`,
`preview_bets`, `placer_pipeline`, or `LENGJAN_*`.

## Skill reference

The four skills under `.claude/skills/` are model-invocable and intentionally
unforked (see `tests/testthat/test-skill-conventions.R`):

| Skill | Purpose |
|---|---|
| `/bet` | Show current recommendations or run the decide layer to refresh |
| `/place-bets` | Preview pending bets, then place after user confirmation |
| `/sports-update` | Run the full pipeline (ingest + odds + fit + decide + publish) |
| `/add-league` | Walk-through to add a new league to `config/leagues.yml` |

## Key gotchas

- **C-locale R + non-ASCII literals**: when the system locale is `C` (no
  UTF-8), R's source parser silently mangles non-ASCII string literals to
  the placeholder text `<U+XXXX>`. Always use `\uxxxx` escapes for
  Icelandic characters in R sources (e.g. `"Úrslit"` not `"Úrslit"`).
  See the Plan 7a placer-fix commit for the canonical example. `R CMD check`
  enforces this.
- **`no_match_id` on bet placement**: almost always means
  `config/leagues.yml::*.lengjan.team_names.{male|female}` is empty or
  missing the team. Pre-flight `validate_team_names_config()` aborts on this
  before login.
- **Ledger schema drift**: `append_to_ledger()` requires the canonical
  column set from `schemas()$ledger$names`. Missing `settled` is the most
  common gap — placer-pipeline.R must include it (defaults to `FALSE` at
  placement, flipped by settlement).
- **`box::use()` inside `withr::with_dir()`**: relative paths break.
  Use `source()` + `new.env()` instead.

## Plan 7 series — active forward roadmap

Hand-tuned constants in the bet-sizing path are queued for replacement by a
hierarchical Bayesian model. See
[`next-actions.md`](obsidian://Sports/Knowledge/Betting%20Optimisation/next-actions)
in Obsidian for the current roadmap (7b CLV capture → 7c Baker-McHale
calibration → 7d hierarchical kelly_frac pooling → 7e adaptive
ev_threshold → 7f optional CVaR-Kelly).

---
paths:
  - "Sports/R/bets/**"
  - "Sports/**/config/bets.yml"
  - "Sports/**/R/run_bets.R"
  - "Sports/**/history/**"
---

# Betting Pipeline Conventions

## System architecture

The betting system has a strict separation of concerns:

1. **Sports pipeline** generates `recommendations.csv` (ephemeral, overwritten every run)
2. **lengjan-bets** reads recommendations, places bets on Lengjan, and writes to `bets_log.csv`
3. **Kelly fractions** are computed dynamically from calibration ratio per league

The Sports pipeline **never** writes to `bets_log.csv`. See `Sports/Knowledge/Betting Optimisation/_MOC.md` in the Metill Obsidian vault for the full rule set and placement conventions.

## `bets.yml` schema

```yaml
sport: "handball"                    # Sport name
country: "denmark"                   # Country
sex: ["male", "female"]              # Which sexes to bet on

bankroll:
  kelly_frac: 0.10                   # Fraction of full Kelly to bet (0.04–0.20)
  kelly_frac_male: 0.12              # Optional: override kelly_frac for male (resolved in run.R loop)
  kelly_frac_female: 0.08            # Optional: override kelly_frac for female
  max_match_kelly: 1.0               # Max total Kelly fraction per match in Stage 1 optimiser
  ev_threshold: 0.00                 # Min EV edge to enter optimiser
  min_bet_amount: 200                # Minimum bet size
  bet_digits: 0                      # Rounding (0 = whole numbers)
  currency: "kr"                     # ISK or EUR

scoring:
  has_ties: true                     # false for basketball
  tie_threshold: 0.5                 # 0 for football, 0.5 for handball (goal diff for draw)

markets:
  outcome: true                      # 1x2 market
  handicap: false                    # Asian/European handicap
  totals: true                       # Over/under

predictions:
  path: "results"                    # Where posterior files live
  max_age_hours: 48                  # Warn if posteriors older than this
  divisions: [1, 2, 3, 4]           # Optional: filter by division (football only)

odds:
  source: "lengjan-odds"             # "local", "lengjan-odds", or "gsheets"
  lengjan_odds_path: "../../../lengjan-odds/data/handball_denmark"  # Relative from league dir
  booker: "Lengjan"                  # Booker name filter

history:
  enabled: true
  path: "history"                    # Where bets_log.csv lives (written by lengjan-bets only)
```

## Current pool

The current bankroll is **not** a `bets.yml` field. `step_bet.R` computes it per-run from `Sports/config/bankroll.yml::initial_pool` (23,610 kr as of 2026-03-06, matching actual deposits) plus the sum of `pnl` on settled rows in `bets_log.csv`. Do not add `cur_pool` to `bets.yml` — it will be silently ignored.

## Module architecture (`Sports/R/bets/`)

All modules use `box::use()`. Entry point is `run.R::run_betting_pipeline(cfg)`.

| Module | Exports | Purpose |
|---|---|---|
| `run.R` | `run_betting_pipeline(cfg)` | Orchestrator: loads posterior + odds → runs joint Kelly → deduplicates → outputs recommendations |
| `calibration.R` | `compute_calibration()` | Bayesian calibration multiplier from settled bet history |
| `kelly.R` | `format_bet_text()` | EV + stake formatting (shared by joint optimiser) |
| `kelly_joint.R` | `build_return_matrix()`, `build_indicators()`, `get_kelly_joint()`, `collect_match_bets()`, `run_joint_kelly()`, `parse_handicap()` | Joint cross-market Kelly (draw-level, SLSQP, push/void aware) |
| `diagnostics_joint.R` | `run_diagnostics()`, `print_diagnostics()` | Per-match diagnostics report |
| `odds.R` | `load_odds(cfg)` | Dispatcher: local/lengjan-odds/gsheets |
| `output.R` | `print_market()`, `dedup_against_log()`, `compute_bankroll()` | Display, ledger dedup, bankroll |
| `history.R` | P&L plots, calibration analysis, `recommend_kelly()` | Offline analysis (deprecated for runtime use — see calibration.R) |

## Bayesian calibration

At pipeline runtime, `step_bet.R` calls `compute_calibration()` which computes a **multiplier** per sex:

```
multiplier = (prior_weight × prior_ratio + Σwins) / (prior_weight + Σmodel_prob)
effective_kelly = base_kelly × clamp(multiplier, floor, ceiling)
```

1. Reads settled bets from `bets_log.csv` for the league
2. Computes Bayesian pseudo-count multiplier (starts updating from bet 1, no hard cutoff)
3. Clamps multiplier to `[floor=0.5, ceiling=1.5]` (configurable in `bankroll.yml`)
4. Multiplies the static `bets.yml` base value (does NOT replace it)

Defaults in `config/bankroll.yml` under `calibration:` key. Per-league override in `bets.yml`.

`prior_weight` controls adaptation speed: higher = more bets needed to move away from prior.
With `prior_weight=10`, ~10 expected wins of data halve the prior's influence.

## Per-league daily cap

`max_league_exposure` in `bankroll.yml` (default 0.20) caps any single league's total kelly fraction per day. Prevents a poorly-calibrated league from getting its full unconstrained stake on quiet days when the daily budget (`max_daily_exposure`) doesn't bind. Enforced in `run.R` after kelly_frac scaling.

## Odds sources

| Source | Config | How it loads | Used by |
|---|---|---|---|
| `local` | `odds.path` | Reads CSVs from league's `data/` dir | football/england |
| `lengjan-odds` | `odds.lengjan_odds_path` | Reads CSVs from `lengjan-odds/data/{key}/` (deduped to latest scrape per match×line) | handball/*, football/italy |
| `gsheets` | `odds.gsheets_id` | Downloads from Google Sheets | basketball/iceland, handball/iceland |

## Market types

### 1x2 (outcome)
Standard home/draw/away. For no-tie sports (basketball), draw column is dropped.

### Handicap
Two line types detected automatically:
- **Whole-goal** (±1, ±2): European 3-way — draw-after-handicap is bettable with its own odds
- **Fractional** (±0.5, ±1.5): Asian 2-way — no draw possible
- No-tie sports (basketball) route all lines through Asian 2-way

### Totals (over/under)
Many-to-many join between posterior total goals and bookmaker limit lines.

## Kelly criterion

Single per-match optimiser across all markets simultaneously. Uses full posterior draw matrix (no collapse to point probabilities). Correctly handles:
- Mutually exclusive outcomes within a market
- Cross-market correlation (e.g., home win + home -0.5 HC)
- Posterior uncertainty (automatic fractional Kelly behaviour)
- Push/void outcomes on integer lines (return = 0 instead of -1)

Uses `build_return_matrix()` → `get_kelly_joint()` with SLSQP (analytic gradients). Returns diagnostics (growth rate, worst-case wealth, effective bet count) per match. Pre-filters to positive-EV bets via `ev_threshold`. Stage 1 computes raw optimal fractions (default `max_match_kelly: 1.0`); all scaling is handled by Stage 2 (portfolio + calibration).

**Daily bankroll budget:** After all leagues produce recommendations, `run.R` checks total kelly fraction per date. If it exceeds `max_daily_exposure` (default 0.75 in `config/bankroll.yml`), all allocations for that date are scaled proportionally.

### kelly_frac guidelines

**kelly_frac** = fraction of full Kelly stake. Applied by `format_bet_text()`. At runtime, may be overridden by `compute_calibration()` (adaptive Kelly).

#### Priority resolution

`step_bet.R` resolves the effective `kelly_frac`:

1. **Base**: `kelly_frac_{sex}` from `bets.yml` (falls back to `kelly_frac`)
2. **× Bayesian multiplier**: from `compute_calibration()` (always applied, prior-weighted)
3. Result is the effective `kelly_frac` used downstream

## Recommendations output

The pipeline writes `Sports/recommendations.csv` with columns:
`sport, country, sex, date, division, heima, gestir, market, outcome, o, p, ev, kelly, bet_amount, change, limit, booker`

This file is ephemeral (overwritten every run) and consumed by `lengjan-bets`.

## Ledger (`bets_log.csv`)

Written **only** by `lengjan-bets/R/pipeline.R::log_placed_bet()` after confirmed placement. Columns:
`date_recommended, date_match, sport, country, sex, market, home, away, outcome, odds, probability, ev, kelly_frac, bet_amount, info, win, pnl, source`

- `win` and `pnl` are `NA` at placement time — filled by `step_settle.R`
- `odds` records actual Lengjan odds at placement (rule P2), not recommended odds
- Rows are never deleted (rule L2)

## Key gotchas

- **`box::use()` relative paths** resolve from the calling file's directory, not the working directory
- **`stats::setNames` unavailable** in box modules — use `unlist()` or `rlang::set_names()` instead
- **Vectorise lookups** in `mutate()` with `ifelse()`, not `[[` (which isn't vectorised)
- **Team name mappings** for Lengjan live in `lengjan-odds/config/team_names_{sport}_{country}.csv`
- **sex=all partition** in Parquet store: settlement writes here; queries must filter `sex != "all"`

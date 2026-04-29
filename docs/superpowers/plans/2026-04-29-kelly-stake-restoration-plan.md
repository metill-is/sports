# Plan 7a: Kelly Stake Restoration

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Restore the §7.2 multiplicative-shrinkage decomposition (`bet_amount = raw_kelly × portfolio_lambda × kelly_frac × calibration × cur_pool`) in the decide pipeline while preserving the joint Kelly + portfolio + calibration math, and add a global K5 ceiling clamp.

**Architecture:** Split the overloaded `kelly_frac` parameter in `kelly_joint()` into a per-match solver cap (`max_match_stake`, default 1.0) so the optimiser solves unconstrained joint Kelly. Restore `kelly = kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling)` in `decide_league()`. Add `kelly_ceiling: 0.25` to `bankroll.yml` (global K5 invariant) and `max_match_stake: 1.0` per-league override (defaults to 1.0 in `bankroll.yml`). Re-grade Iceland-tier leagues to Browne-grounded `kelly_frac ≈ 0.20` defaults.

**Tech Stack:** R + testthat (edition 3), devtools, cmdstanr (unchanged), no new dependencies.

**Reference design:** [Sports/Knowledge/Betting Optimisation/Historical/kelly-stake-restoration-design-2026-04-29.md](../../../#) (Obsidian, Metill vault) — full theoretical justification with §7 cross-references.

---

## File Structure

| Path | Action | Responsibility |
|---|---|---|
| `R/decide-kelly.R` | Modify | Rename `kelly_frac` parameter → `max_match_stake`; update roxygen |
| `R/decide-pipeline.R` | Modify | Restore multiplicative chain; read `kelly_ceiling` from bankroll; pass `max_match_stake` to `kelly_joint()` |
| `R/config.R` | Modify | `load_bankroll()` exposes `kelly_ceiling` (default 0.25) and `max_match_stake_default` (1.0) |
| `config/bankroll.yml` | Modify | Add `kelly_ceiling: 0.25`, `max_match_stake_default: 1.0` |
| `config/leagues.yml` | Modify | Re-grade `kelly_frac` defaults; add per-league `max_match_stake` override slot |
| `tests/testthat/test-decide-kelly.R` | Modify | Update calls to use `max_match_stake =`; add cap-binding test |
| `tests/testthat/test-decide-pipeline.R` | Modify | Update stake expectations; add ceiling-clamp test |
| `tests/testthat/test-config.R` | Modify | Add `kelly_ceiling` field expectation in bankroll fixture |
| `tests/testthat/test-targets-slice-isolation.R` | Modify | Update inline `betting$kelly_frac` references (semantic still valid) |
| `CLAUDE.md` | Modify | Update "Decide layer" subsection to document restored chain |
| `Sports/Knowledge/Betting Optimisation/code-map.md` | Modify (Obsidian) | Refresh formula factor decomposition + line refs |
| `Sports/Knowledge/Betting Optimisation/Historical/kelly-stake-restoration-design-2026-04-29.md` | Update (Obsidian) | Mark `status: implemented` post-merge |

---

## Tasks

### Task 1: Split `kelly_joint()` into `max_match_stake` + (post-hoc) caller-applied `kelly_frac`

**Files:**
- Modify: `R/decide-kelly.R:135-186`
- Test: `tests/testthat/test-decide-kelly.R`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-decide-kelly.R`:

```r
test_that("kelly_joint with max_match_stake = 1.0 returns unconstrained joint Kelly", {
  set.seed(42)
  draws <- tibble::tibble(
    draw_id = 1:1000,
    home_goals = rpois(1000, 1.5),
    away_goals = rpois(1000, 1.0)
  )
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line    = c(NA_real_, NA_real_),
    odds    = c(1.80, 4.50)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 1.0)
  # The joint optimiser, unconstrained, allocates non-trivially to both bets
  # (correlated negative — they're mutually exclusive moneyline outcomes).
  expect_gt(sum(out$bets$kelly_raw), 0.10)
  expect_lte(sum(out$bets$kelly_raw), 1.0)
})

test_that("kelly_joint with max_match_stake = 0.05 enforces sum-stake cap", {
  set.seed(42)
  draws <- tibble::tibble(
    draw_id = 1:1000,
    home_goals = rpois(1000, 1.5),
    away_goals = rpois(1000, 1.0)
  )
  bets <- tibble::tibble(
    market  = c("moneyline", "moneyline"),
    outcome = c("home", "away"),
    line    = c(NA_real_, NA_real_),
    odds    = c(1.80, 4.50)
  )
  out <- kelly_joint(draws, bets, max_match_stake = 0.05)
  expect_lte(sum(out$bets$kelly_raw), 0.05 + 1e-6)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-decide-kelly.R")'`
Expected: FAIL with `unused argument (max_match_stake = ...)`.

- [ ] **Step 3: Rename parameter in `kelly_joint()`**

In `R/decide-kelly.R`, modify the signature and body. Replace lines 135-145 (roxygen + signature):

```r
#' @param max_match_stake Per-match solver cap (Σ f_j ≤ max_match_stake).
#'   Default 1.0 = unconstrained joint Kelly. The §7.2 fractional-Kelly
#'   shrinkage `kelly_frac` is applied by the caller (`decide_league()`)
#'   after the optimiser returns, *not* via this cap.
#' @param ev_threshold Minimum EV per bet to consider. Bets below threshold
#'   are zeroed before optimisation.
#' @param tie_threshold Reserved for future use (tie-handling in handball
#'   scoring); currently unused.
#' @return List with `bets` (input cols + p, ev, kelly_raw), `match_pnl`
#'   (numeric S), `match_kelly_sum` (sum of kelly_raw), `diagnostics` (list).
#' @export
kelly_joint <- function(beliefs, bets,
                        max_match_stake = 1.0,
                        ev_threshold = 0.0,
                        tie_threshold = 0) {
```

Replace line 158 (inside body):

```r
    sol <- solve_kelly_joint(R_keep, max_stake = max_match_stake)
```

Replace lines 178-184 (diagnostics list):

```r
    diagnostics = list(
      n_bets          = nrow(bets),
      n_kept          = sum(keep),
      max_match_stake = max_match_stake,
      nloptr_status   = solver_status,
      nloptr_iter     = solver_iterations
    )
```

- [ ] **Step 4: Update existing tests in test-decide-kelly.R**

Three existing tests (lines 34, 69, 84) call `kelly_joint(... kelly_frac = 1.0 ...)`. Rename to `max_match_stake = 1.0`:

```r
# Line 34
out <- kelly_joint(draws, bets, max_match_stake = 1.0, ev_threshold = 0.0)
# Line 69
out <- kelly_joint(draws, bets, max_match_stake = 1.0)
# Line 84
out <- kelly_joint(draws, bets, max_match_stake = 1.0)
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-decide-kelly.R")'`
Expected: PASS — all 6 tests (4 existing + 2 new).

- [ ] **Step 6: Commit**

```bash
git add R/decide-kelly.R tests/testthat/test-decide-kelly.R
git commit -m "refactor(decide-kelly): rename kelly_frac param to max_match_stake

Decouples the per-match solver cap from the §7.2 multiplicative shrinkage.
The post-hoc kelly_frac multiplier is applied by decide_league() after the
optimiser returns, restoring the documented bet_amount decomposition.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

### Task 2: Add `kelly_ceiling` and `max_match_stake_default` to `load_bankroll()`

**Files:**
- Modify: `config/bankroll.yml`
- Modify: `R/config.R:77-100`
- Test: `tests/testthat/test-config.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-config.R`:

```r
test_that("load_bankroll exposes kelly_ceiling and max_match_stake_default with defaults", {
  tmp_yaml <- tempfile(fileext = ".yml")
  writeLines(c(
    "initial_pool: 10000",
    "daily_budget_frac: 0.05",
    "daily_budget_min_isk: 1000"
  ), tmp_yaml)
  on.exit(unlink(tmp_yaml))
  b <- load_bankroll(path = tmp_yaml, ledger_root = tempfile())
  expect_equal(b$kelly_ceiling, 0.25)
  expect_equal(b$max_match_stake_default, 1.0)
})

test_that("load_bankroll honours explicit kelly_ceiling override", {
  tmp_yaml <- tempfile(fileext = ".yml")
  writeLines(c(
    "initial_pool: 10000",
    "daily_budget_frac: 0.05",
    "daily_budget_min_isk: 1000",
    "kelly_ceiling: 0.15"
  ), tmp_yaml)
  on.exit(unlink(tmp_yaml))
  b <- load_bankroll(path = tmp_yaml, ledger_root = tempfile())
  expect_equal(b$kelly_ceiling, 0.15)
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-config.R")'`
Expected: FAIL with `b$kelly_ceiling is NULL`.

- [ ] **Step 3: Update `config/bankroll.yml`**

Append two lines to `config/bankroll.yml`:

```yaml
# Kelly fraction safety ceiling (K5 invariant): never overbet even with
# perfect calibration. Clamps `kelly_frac × calibration_multiplier` from above.
kelly_ceiling: 0.25
# Default per-match solver cap when a league does not override it. 1.0 means
# the joint Kelly optimiser is unconstrained at the per-match level — the
# §7.2 shrinkage and §10 portfolio scaling are the active stake limiters.
max_match_stake_default: 1.0
```

- [ ] **Step 4: Update `load_bankroll()` to expose the fields**

In `R/config.R`, modify `load_bankroll()`. After the `current_pool` derivation (around line 95) and before the `return(cfg)`:

```r
  if (is.null(cfg$kelly_ceiling)) cfg$kelly_ceiling <- 0.25
  if (is.null(cfg$max_match_stake_default)) cfg$max_match_stake_default <- 1.0
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-config.R")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add config/bankroll.yml R/config.R tests/testthat/test-config.R
git commit -m "feat(config): add kelly_ceiling + max_match_stake_default to bankroll

kelly_ceiling enforces the K5 invariant (never overbet beyond 0.25 of
bankroll on a single match even with perfect calibration drift).
max_match_stake_default lets leagues override the per-match Kelly cap;
default 1.0 leaves the joint optimiser unconstrained.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

### Task 3: Restore the multiplicative chain in `decide_league()`

**Files:**
- Modify: `R/decide-pipeline.R:100-210`
- Test: `tests/testthat/test-decide-pipeline.R`

- [ ] **Step 1: Write failing tests for the restored chain + ceiling clamp**

Append to `tests/testthat/test-decide-pipeline.R`:

```r
test_that("decide_league applies kelly_frac × calibration as multiplicative shrinkage", {
  # Synthetic: single bet, p=0.40, odds=2.5 → raw Kelly ≈ 0.10
  # With kelly_frac = 0.25, calibration = 1.0, expect kelly ≈ 0.025
  fixture <- decide_pipeline_fixture()
  fixture$league$betting$kelly_frac <- 0.25
  fixture$bankroll$kelly_ceiling <- 0.25
  fixture$bankroll$current_pool <- 10000

  recs <- with_decide_fixture(fixture, function(root) {
    decide_league(league = fixture$league, sex = "male",
                  bankroll = fixture$bankroll, root = root, write = FALSE)
  })
  expect_gt(nrow(recs), 0)
  # kelly is now multiplicative: kelly_raw × λ × min(kelly_frac × calib, ceiling)
  # Raw Kelly for p=0.40, odds=2.5 ≈ 0.10; multiplied by 0.25 → 0.025
  expect_lt(recs$kelly[1], 0.05)
  expect_gt(recs$kelly[1], 0.01)
})

test_that("decide_league clamps kelly_frac × calibration at kelly_ceiling", {
  # With kelly_frac = 0.30 and calibration_multiplier = 1.5, the product is 0.45;
  # the K5 ceiling at 0.25 must clamp it.
  fixture <- decide_pipeline_fixture()
  fixture$league$betting$kelly_frac <- 0.30
  fixture$bankroll$kelly_ceiling <- 0.25
  fixture$mocked_calibration <- 1.5  # forces ceiling to bind

  recs <- with_decide_fixture(fixture, function(root) {
    decide_league(league = fixture$league, sex = "male",
                  bankroll = fixture$bankroll, root = root, write = FALSE)
  })
  # kelly_raw × λ × 0.25 ≤ kelly_raw (ceiling clamped)
  # Without clamp it would be kelly_raw × 0.45
  expect_lt(recs$kelly[1], recs$kelly_raw[1] * 0.26)
  expect_gt(recs$kelly[1], recs$kelly_raw[1] * 0.24)
})
```

The `decide_pipeline_fixture()` and `with_decide_fixture()` helpers already exist in `test-decide-pipeline.R` — verify by `grep`. If `mocked_calibration` is not yet a fixture field, add it (Task 3 step 2).

- [ ] **Step 2: Add `mocked_calibration` plumbing to fixture**

If the fixture doesn't already support a calibration override, modify `R/decide-calibration.R::compute_calibration()` to accept a `mock_value` argument that bypasses ledger reading:

```r
compute_calibration <- function(league, sex, root = here::here("data"),
                                mock_value = NULL) {
  if (!is.null(mock_value)) return(mock_value)
  # ... existing ratio computation ...
}
```

And in `decide_league()`, pass through:

```r
calib <- compute_calibration(league, sex, root = root,
                             mock_value = bankroll$mocked_calibration)
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-decide-pipeline.R")'`
Expected: FAIL — current code does not multiply by `kelly_frac` post-optimiser.

- [ ] **Step 4: Restore the chain in `decide_league()`**

In `R/decide-pipeline.R`, modify Step 5 (the per-match Kelly call, lines 141-146):

```r
    kj <- kelly_joint(
      mb[, c("draw_id", "home_goals", "away_goals"), drop = FALSE],
      bets_in,
      max_match_stake = betting$max_match_stake %||% bankroll$max_match_stake_default,
      ev_threshold    = betting$ev_threshold %||% 0.0
    )
```

Modify the package construction (lines 149-154) — `kelly_frac` here is now the post-hoc multiplier carried into the portfolio layer:

```r
    packages[[match_key]] <- list(
      match_key       = match_key,
      match_pnl       = kj$match_pnl,
      match_kelly_sum = kj$match_kelly_sum,
      kelly_frac      = kelly_frac_val   # post-hoc multiplier
    )
```

Modify Step 9 (the final stake computation, lines 198-201):

```r
  # 8. Calibration multiplier (clamped to [floor, ceiling] inside compute_calibration)
  calib <- compute_calibration(league, sex, root = root,
                               mock_value = bankroll$mocked_calibration)

  # 9. Restored §7.2 multiplicative chain with K5 ceiling clamp
  shrink_eff <- pmin(kelly_frac_val * calib, bankroll$kelly_ceiling)
  cands$kelly <- cands$kelly_raw * cands$lambda * shrink_eff
  cands$bet_amount <- round(cands$kelly * bankroll$current_pool)
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-decide-pipeline.R")'`
Expected: PASS — both new tests + all existing tests.

- [ ] **Step 6: Run full decide-layer test suite**

Run: `Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", filter = "decide")'`
Expected: PASS — all decide-layer tests (calibration, kelly, odds, pipeline, portfolio, validation).

- [ ] **Step 7: Commit**

```bash
git add R/decide-pipeline.R R/decide-calibration.R tests/testthat/test-decide-pipeline.R
git commit -m "feat(decide): restore §7.2 multiplicative kelly_frac shrinkage with K5 ceiling

decide_league() now computes:
  kelly = kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling)

This restores the documented decomposition from
Sports/Knowledge/Betting Optimisation/code-map.md that the Plan-3 → Plan-4
rewrite collapsed by overloading kelly_frac as the optimiser cap. The math
(joint Kelly + portfolio + calibration) is unchanged; only the post-hoc
shrinkage is restored.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

### Task 4: Re-grade `kelly_frac` defaults in `config/leagues.yml`

**Files:**
- Modify: `config/leagues.yml`
- Test: `tests/testthat/test-config-betting-schema.R` (verify schema still passes)

- [ ] **Step 1: Update each league's `betting:` block**

In `config/leagues.yml`:

```yaml
basketball_iceland:
  ...
  betting:
    kelly_frac:
      male: 0.20      # was: 0.10 (now post-hoc multiplier; Browne-grounded)
      female: 0.15    # was: 0.10 (smaller sample → more shrinkage)
    max_match_stake: 0.50   # NEW: per-match solver cap; rarely binds
    ev_threshold: 0.0
    markets:
      moneyline: true
      spread: true
      total: true
    scoring:
      has_ties: false
      tie_threshold: 0
    min_bet: 200
    max_age_hours: 48

handball_iceland:
  ...
  betting:
    kelly_frac: 0.20         # was: 0.10
    max_match_stake: 0.50    # NEW
    ev_threshold: 0.0
    ...

football_iceland:
  ...
  betting:
    kelly_frac:
      male: 0.20             # was: 0.15
      female: 0.10           # was: 0.075
    max_match_stake: 0.50    # NEW
    ev_threshold: 0.0
    ...
```

- [ ] **Step 2: Run schema test**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-config-betting-schema.R")'`
Expected: PASS — schema accepts both scalar and per-sex kelly_frac forms.

- [ ] **Step 3: Run full test suite**

Run: `Rscript -e 'devtools::test()'`
Expected: PASS — all tests across config, storage, decide, placer, etc.

- [ ] **Step 4: Commit**

```bash
git add config/leagues.yml
git commit -m "config(leagues): re-grade kelly_frac to Browne-grounded defaults

Now that kelly_frac is the §7.2 multiplicative shrinkage (not a solver cap),
the Browne (2000) finite-horizon argument and the §7.5 joint-Kelly empirical
growth curve put the right multiplier between 0.20 and 0.25 for Iceland-tier
ROI history. Female cells get a slightly tighter shrinkage to reflect smaller
sample sizes.

Adds per-league max_match_stake: 0.50 as a Stage-1 safety cap that should
rarely bind in practice.

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

### Task 5: Verify on real data — Grindavík vs Valur regression check

**Files:**
- No file changes; smoke-test only

- [ ] **Step 1: Re-run decide layer**

Run: `Rscript run.R --league basketball_iceland --sex male --step decide`
Expected: completes without error.

- [ ] **Step 2: Inspect the regenerated recommendation**

Run:

```bash
Rscript -e '
suppressPackageStartupMessages(devtools::load_all())
sports::rebuild_duckdb()
con <- DBI::dbConnect(duckdb::duckdb(), "sports.duckdb", read_only = TRUE)
print(DBI::dbGetQuery(con, "
  SELECT match_date, home_team, away_team, market, outcome,
         p, odds, ev, kelly, bet_amount
  FROM recommendations
  WHERE run_date = (SELECT MAX(run_date) FROM recommendations)
    AND home_team = '\''Grindavík'\''
"))
'
```

Expected: one row with `bet_amount` in the **500–800 ISK range** (was 2,818 ISK pre-restoration). With `kelly_frac=0.20`, `calib≈0.92`, `kelly_ceiling=0.25`, raw Kelly ≈ 0.027, and `current_pool ≈ 113,000`:

```
shrink_eff = min(0.20 × 0.92, 0.25) = 0.184
kelly      = 0.027 × 1.0 × 0.184    = 0.00497
bet_amount = round(0.00497 × 113000) ≈ 562
```

If `bet_amount` is outside `[500, 800]`, debug calibration multiplier — calling `compute_calibration("basketball_iceland", "male")` directly should return ~0.92.

- [ ] **Step 3: Commit a data refresh (if values changed)**

The decide step writes `data/decisions/{candidates,recommendations}/`. If the Parquet file changed:

```bash
git add data/decisions/candidates data/decisions/recommendations
git commit -m "data: re-decide basketball_iceland male after kelly_frac restoration $(date -u +%Y-%m-%dT%H:%MZ)

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

---

### Task 6: Update `CLAUDE.md` and Obsidian docs

**Files:**
- Modify: `CLAUDE.md` (the "Decide layer" subsection under "Conventions")
- Modify (Obsidian): `Sports/Knowledge/Betting Optimisation/code-map.md` formula table
- Modify (Obsidian): `Sports/Knowledge/Betting Optimisation/Historical/kelly-stake-restoration-design-2026-04-29.md` frontmatter `status: implemented`

- [ ] **Step 1: Update `CLAUDE.md` Decide layer subsection**

Find the "Decide layer" bullet block and amend the bet-amount formula description. After the line "- Joint Kelly is the only mode (per 2026-03-06 memory note).", insert:

```markdown
- Stake formula (post Plan 7a): `bet_amount = round(kelly_raw × portfolio_lambda × min(kelly_frac × calibration, kelly_ceiling) × current_pool)`. `kelly_frac` is the §7.2 multiplicative shrinkage (Browne γ); `max_match_stake` (default 1.0, per-league override) is a Stage-1 solver cap that rarely binds; `kelly_ceiling` (default 0.25) is the K5 hard cap.
```

- [ ] **Step 2: Update Obsidian `code-map.md` formula table**

Use Obsidian MCP `edit_note` (or `write_note` with full content if simpler). The factor decomposition table at the top should now read:

```markdown
| Factor | What it does | Where computed |
|---|---|---|
| `raw_kelly` | Stage-1 joint Kelly fraction; bounded above by `max_match_stake` | `R/decide-kelly.R::kelly_joint()` |
| `portfolio_lambda` | Stage-2 cross-match daily-budget scaling | `R/decide-portfolio.R::portfolio_optimise()` |
| `kelly_frac × calibration` | §7.2 multiplicative shrinkage, clamped at `kelly_ceiling` (K5) | `R/decide-pipeline.R::decide_league()` |
| `cur_pool` | Bankroll = initial + settled PnL | `R/config.R::load_bankroll()` |
```

- [ ] **Step 3: Mark design doc as implemented**

In `Sports/Knowledge/Betting Optimisation/Historical/kelly-stake-restoration-design-2026-04-29.md`, update frontmatter `status: implemented` and append an "Implemented" note linking to the merge commit.

- [ ] **Step 4: Commit doc changes**

```bash
git add CLAUDE.md
git commit -m "docs: document restored stake formula in CLAUDE.md

Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>"
```

(Obsidian doc edits are not in the git repo — they update directly via MCP.)

---

## Self-review checklist

- [ ] Spec coverage: every section of the design doc maps to a task above (restoration → Tasks 1–4; verification → Task 5; docs → Task 6).
- [ ] No placeholders: every step has runnable code or an exact command.
- [ ] Type consistency: `max_match_stake` is the new param name everywhere (`kelly_joint()` signature, callers, tests, config keys).
- [ ] Out of scope (deferred to follow-up plans): CLV capture (Plan 7b), Baker-McHale Bayesian calibration (Plan 7c), hierarchical kelly_frac pooling (Plan 7d), EV-threshold from σ_eps (Plan 7e), CVaR-Kelly (Plan 7f).

# GISKÓ optimal match predictions — design

**Date:** 2026-06-25
**Status:** Design (approved in brainstorming)
**Scope:** A reusable R tool that, for each upcoming FIFA World Cup 2026 match,
emits the GISKÓ-optimal predicted scoreline + expected points, and a per-round
joker (`hirðfífl`) recommendation — all derived from the existing WC posterior
in `data/wc/fit/sim_inputs.rds`.

## 1. Problem

[GISKÓ](https://gisko.is) is a free WC-2026 prediction game (100,000 ISK prize).
We already run a Bayesian WC forecast (`R/wc-*.R`, `scripts/wc/`) that produces a
full posterior over team strengths and a per-draw bivariate-Poisson scoreline
model. The task: turn that posterior into the entry that **maximises expected
GISKÓ points** under the game's scoring rule.

Only the **per-match scoreline** predictions and the **joker** are live. The
structural predictions (group rankings, bracket, champion) had an 11 Jun 2026
deadline and are locked; they are out of scope (see §8).

## 2. GISKÓ scoring rule (verified verbatim, rules §06–07)

Each match is worth up to **5 points**, additive partial credit:

| Component | Points | Condition |
|---|---|---|
| Outcome | 2 | predicted sign of (home − away) matches actual (win/draw/loss) |
| Home goals | 1 | predicted home goals == actual home goals |
| Away goals | 1 | predicted away goals == actual away goals |
| Goal difference | 1 | predicted (home − away) == actual (home − away) |

Worked examples from the rules: predict 2–1, actual 2–1 → 5; predict 3–1, actual
2–1 → 3 (outcome + away goals only).

**Joker (`hirðfífl`, §07):** doubles (×2) the points from one chosen match.
Six jokers over the tournament — one each for group rounds 1, 2, 3 and the
Round-of-32, Round-of-16, Round-of-8. Reassignable until the chosen match kicks
off. There is exactly one joker per round; jokers do not carry across rounds.

**Achievable per-match scores are {0, 1, 2, 3, 5} — never 4.** Exact goals for
both teams forces the GD and outcome too (→5); any GD hit forces the outcome
(→ ≥3). This is a free correctness invariant for tests.

## 3. Decision theory

Let `(H, A)` be the posterior-predictive scoreline for a match and `S(a, b; H, A)`
the GISKÓ score of guessing `(a, b)`. The Bayes-optimal guess maximises
`E[S(a, b; H, A)]` over the posterior predictive.

### Lemma 1 — the optimal scoreline depends only on marginals

By linearity of expectation and the additive rule:

```
E[S(a,b)] = 2·P(outcome = sign(a−b)) + P(H = a) + P(A = b) + P(H − A = a − b)
```

Every term is a **marginal** functional: the 1X2 outcome probabilities, the
home-goal PMF, the away-goal PMF, and the goal-difference (Skellam) PMF. The
joint cell `P(H = a, A = b)` is **not** needed to choose the scoreline.

**Caveat that makes this "use the full posterior", not "assume independence":**
the GD and outcome marginals encode the within-match correlation from the shared
λ₃ component. They must be tabulated from the joint, never from
`home_pmf ⊗ away_pmf`. So the engine integrates *both* parameter uncertainty
(over posterior draws) and within-match correlation; the decision is then a
marginal functional of that correctly-integrated predictive.

The optimum is found by brute force over a small grid `a, b ∈ {0..max_goals}`
(default 8 → 81 candidates). It is in general **neither** the modal scoreline
**nor** the rounded mean, because the four terms pull toward different cells and
the 2-point outcome weight dominates.

### Lemma 2 — the joker recommendation (expected-points objective)

For a round of matches with chosen scorelines, total points with the joker on
match `j` is `T_j = Σ_m X_m + X_j` (the joker adds a second copy of `X_j`). So
`E[T_j] = Σ_m E[X_m] + E[X_j]`, maximised by **doubling the match with the
highest expected optimal points**. Correlations do not change this — they change
only `Var(T_j)` and tail probabilities. Because each round grants its own joker
(no carry-over), greedy-within-round is globally optimal under expected points.

So the scoreline optimiser's per-match `E[X_m]` directly ranks the joker. One
computation serves both decisions.

### Where the full joint posterior genuinely matters

Not for the expected-points *recommendation* (Lemmas 1–2), but for the
**distribution** of a round's points — variance, quantiles, P(score ≥ target) —
which is the substrate for a future rank-aware ("win the pool") joker (§8).
Conditional on a posterior draw the matches are independent (model assumption,
`R/wc-simulate.R:19–22`), so the posterior-predictive round-total distribution is
a **mixture over draws of within-draw convolutions** of the per-match score PMFs.
This captures the epistemic correlation that per-match marginals discard.

## 4. Architecture

One new module `R/gisko.R` (the WC code uses a flat `R/wc-*.R` layout; `R/gisko.R`
matches it) and one driver `scripts/wc/gisko.R`. The engine reuses the model's
internal rate function so it cannot drift from the forecast.

### Substrate: per-draw bivariate-Poisson scoreline matrix

```r
.gisko_bvpois_pmf(lambdas, max_goals = 8L)   # -> (max+1)x(max+1) matrix P(H=h, A=a)
```

Analytic bivariate-Poisson PMF for rates `c(λ_h, λ_a, λ₃)`:
`P(h,a) = e^{−(λ_h+λ_a+λ₃)} Σ_{k=0}^{min(h,a)} λ_h^{h−k}/(h−k)! · λ_a^{a−k}/(a−k)! · λ₃^k/k!`,
truncated to the grid and renormalised. This is the noise-free analogue of the
sampler `.wc_rbvpois()` (`R/wc-simulate.R:47`).

```r
gisko_predictive_matrix(home, away, sim_inputs, venue = "neutral", max_goals = 8L)
```

For each posterior draw: get rates via `.wc_match_lambdas(home, away, venue, ...)`
(`R/wc-simulate.R:26`), build the per-draw PMF, accumulate. Returns:
- `pbar`: integrated joint PMF (mean over draws) — correlation- and
  parameter-uncertainty-aware. All marginals read off this.
- `per_draw`: a list/array of per-draw PMFs (or cached λ to rebuild them),
  needed for the round-distribution in §5.

### Scoring core (source-agnostic — takes marginals/PMF, not a model)

```r
gisko_match_points(pred_home, pred_away, act_home, act_away)  # exact §06 rule, 0..5
gisko_score_grid(max_goals)            # precomputed score for every (pred, actual) cell
gisko_expected_points(a, b, marg)      # Lemma-1 formula from a marginals object
gisko_optimal_scoreline(pbar, max_goals = 8L)
#   -> list(home, away, exp_points, p_exact, modal_home, modal_away,
#           top = <ranked candidate table>, optimal_differs_from_modal = <lgl>)
```

`marg` is a small list: `p_outcome = c(home, draw, away)`, `home_pmf`,
`away_pmf`, `gd_pmf` — all derived from `pbar` (or, for cross-checking, parsed
from `predictions.json`). Keeping the core ignorant of the model makes it
trivially testable and lets it consume either source.

### Adapters

```r
gisko_marginals_from_pbar(pbar)        # joint matrix -> marg (the production path)
gisko_marginals_from_predictions(row)  # parse predictions.json / predictions tibble
#   row (group fixtures only) -> marg, for an independent cross-check
```

## 5. Round + joker analysis (posterior-predictive)

```r
gisko_round_distribution(matches, sim_inputs, max_goals = 8L)
```

`matches` is a tibble of `{home, away, venue, round, label}` (defaults to the
upcoming group fixtures from `wc_group_fixtures()`; knockout pairings passed
explicitly once known). Steps:

1. **Pass 1 (decision):** for each match build `pbar`, choose the optimal
   scoreline, record `E[X_m]`, `P(exact)`, and the integrated per-match score PMF
   over {0,1,2,3,5}.
2. **Joker (Lemma 2):** rank matches by `E[X_m]`; the per-round recommendation is
   the highest-`E` still-unstarted match in each round.
3. **Pass 2 (uncertainty):** per draw, rebuild each match's conditional score PMF
   (given the chosen scoreline) from cached λ; convolve across matches (matches
   are conditionally independent given the draw) to get the within-draw round
   total; mix over draws. Returns the round-total PMF, its mean/sd/quantiles, and
   — per joker candidate — the same summaries (joker adds a second convolution of
   that match's score PMF). Reported for transparency and as the rank-aware hook;
   the *recommendation* stays expected-points (Lemma 2).

Convolution support is tiny ({0,1,2,3,5} per match), so this is cheap even at
full draw count.

## 6. Driver `scripts/wc/gisko.R`

- Loads `data/wc/fit/sim_inputs.rds`; derives upcoming group fixtures via
  `wc_group_fixtures(wc_structure())`; accepts explicit knockout pairings via CLI
  (`--pairings file.csv` or repeated `--match "TeamA vs TeamB @neutral #R32"`).
- Runs `gisko_round_distribution()`; prints a tidy table per match: round,
  fixture, **optimal scoreline**, `E[points]`, `P(exact)`, `P(outcome)`, and a
  `≠modal` flag; then the per-round joker line.
- `options(width = 120)`; output also written to `data/wc/gisko/optimal_picks.json`
  (gitignored — regenerable) for re-reading without recompute.
- Read-only on the money path; never wired into CI (no betting, no ledger). It
  only reads the posterior cache and writes a scratch JSON.

## 7. Testing (`tests/testthat/test-gisko.R`, testthat 3)

- `gisko_match_points`: the two rules examples (2-1/2-1→5; 3-1/2-1→3); draw cases;
  **property: never returns 4** over a full goal grid; both-exact→5; GD-hit→≥3.
- `gisko_expected_points` vs an independent brute-force reference over a random
  `pbar`.
- `gisko_optimal_scoreline`: a hand-built `pbar` where modal ≠ optimal — assert
  the optimiser's `exp_points` beats the modal scoreline's and the flag is set;
  degenerate (point-mass) `pbar` ⇒ optimum == mode.
- `.gisko_bvpois_pmf`: rows/cols sum to 1 (pre-truncation); independence case
  (λ₃ = 0) factorises into the product of two Poisson PMFs; mean matches λ_h, λ_a;
  **GD PMF from a correlated (λ₃ > 0) matrix differs from the independence
  assumption** (guards against the Lemma-1 caveat).
- `gisko_round_distribution`: round-total mean equals Σ per-match `E[X_m]` (+ the
  jokered match again) — ties the convolution back to linearity; degenerate
  posterior ⇒ point-mass round total.
- Cross-check: engine marginals for a group fixture match `predictions.json`
  within MC tolerance (skipped if the published file is absent).

## 8. Out of scope / future layers

- **Structural optimiser** (group-ranking assignment, bracket, champion): locked
  this tournament. The method is noted for a future tournament: group placement
  is a linear-assignment problem per group on the `p_first..p_fourth` matrix;
  bracket/champion maximise expected round points over the `placement_probs`
  marginals under the tree constraints.
- **Rank-aware objective:** maximise P(finishing 1st) rather than expected points.
  Needs the live leaderboard and uses the §5 round-total *distribution* (variance
  helps when behind). The posterior substrate is built to support it; the
  objective swap is the only addition.
- **Rendered HTML view:** not requested (tool emits a table + JSON).

## 9. Files

| Path | Change |
|---|---|
| `R/gisko.R` | new — scoring core, bvpois PMF, predictive matrix, adapters, round/joker |
| `scripts/wc/gisko.R` | new — driver/CLI |
| `tests/testthat/test-gisko.R` | new — §7 |
| `NAMESPACE` | regenerated (roxygen exports for public `gisko_*`) |

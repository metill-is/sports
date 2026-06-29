# (S,D) lean-Gaussian backtest — Phase 1 Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Evaluate a continuous Gaussian `(S,D)=(total, signed diff)` football model — reusing the production BVP mean structure with a mean-induced, mean-scaled covariance — out-of-sample against the de-vigged Lengjan line and against BVP, on the 2026 bettable slate.

**Architecture:** A new Stan model (`2d_gaussian_sd.stan`) copies BVP's data/parameters/transformed-parameters verbatim and swaps the bivariate-Poisson likelihood for a bivariate Gaussian on `(S,D)` whose covariance is induced by the means (`Var(S)=Var(D)=φ·E[S]`, `Cov=tanh(γ·E[D]/E[S])·φ·E[S]`). It emits the same `goals1_pred`/`goals2_pred` the rest of the pipeline already consumes, so the only R changes are a walk-forward decide closure that fits the new model, a `--model` CLI flag, and a comparison report. No engine or downstream-scoring changes.

**Tech Stack:** R package (`devtools`/`testthat` 3/`roxygen2`), `cmdstanr` + Stan, `arrow`/Parquet, the existing `R/backtest-*.R` walk-forward harness, Quarto.

**Design doc:** [docs/superpowers/specs/2026-06-29-sd-gaussian-backtest-design.md](../specs/2026-06-29-sd-gaussian-backtest-design.md)

## Global Constraints

- **2026 season only** for the bettable (vs-market) backtest — no pre-2026 Lengjan odds exist.
- **Never on CI / read-only money path.** Nothing here writes the ledger; Stan-compiling tests are `skip_on_ci()`.
- **R conventions:** base pipe `|>`, `here::here()` for paths, `#' @export` + `devtools::document()` for new exported functions, tidyverse `snake_case`.
- **Stan conventions:** `cmdstanr` only; non-centred parameterisations; `generated quantities` must emit `goals1_pred`/`goals2_pred` (so `extract_posteriors()` works) and `vector[N] log_lik`.
- **Leak-free:** reuse the existing walk-forward guards (G1–G9); each fit uses `end_date == cutoff`.
- **Worktree caution:** `here::here()`/`devtools::test()` can resolve to the main checkout in a worktree. Run tests from the worktree root and prefer `testthat::test_file("tests/testthat/<file>")` after `devtools::load_all()`.

---

### Task 1: The `(S,D)` lean-Gaussian Stan model

**Files:**
- Create: `Stan/football_iceland/2d_gaussian_sd.stan`
- Test: `tests/testthat/test-backtest-sd.R` (compile test only here)

**Interfaces:**
- Produces: a Stan model whose `data` block is byte-compatible with `bivariate_poisson_no_inflation.stan` (so `prepare_data()` feeds it unchanged) and whose `generated quantities` emit `goals1_pred`, `goals2_pred`, `goal_diff_pred`, `total_goals_pred`, `log_lik`. Covariance parameters: `real<lower=0> phi`, `real gamma_rho` (the spec's `γ`; renamed to avoid Stan's `gamma` distribution keyword).

- [ ] **Step 1: Write the model file**

Create `Stan/football_iceland/2d_gaussian_sd.stan`:

```stan
/**
 * Football (S, D) lean-Gaussian model.
 *
 * Reuses the production bivariate-Poisson mean model VERBATIM (log-link RW
 * offence/defence, per-team home advantage, scalar mean_log_goals) and swaps
 * the likelihood for a bivariate Gaussian on (S, D) = (total, signed diff)
 * with a MEAN-INDUCED, MEAN-SCALED covariance:
 *
 *   lambda_h = exp(mean_log_goals + off_h - def_a)
 *   lambda_a = exp(mean_log_goals + off_a - def_h)
 *   E[S] = lambda_h + lambda_a ;  E[D] = lambda_h - lambda_a
 *   Var(S) = Var(D) = phi * E[S]                 (shared phi, ~0.88)
 *   rho      = tanh(gamma_rho * E[D] / E[S])     (PD-safe, |rho| < 1)
 *   Cov(S,D) = rho * phi * E[S]
 *
 * phi and gamma_rho are the only parameters added vs BVP (which itself drops
 * alpha_mu3 / beta_mu3). Design + diagnostic:
 * docs/superpowers/specs/2026-06-29-sd-gaussian-backtest-design.md
 */

data {
  int<lower=0> K;
  int<lower=0> N;
  int<lower=0> N_rounds;
  array[N] int<lower=1, upper=K> team1;
  array[N] int<lower=1, upper=K> team2;
  array[N] int<lower=1, upper=N_rounds> round1;
  array[N] int<lower=1, upper=N_rounds> round2;
  matrix<lower=0>[K, N_rounds] time_between_matches;
  array[N] int<lower=0> goals1;
  array[N] int<lower=0> goals2;

  // Prediction data (declared for data-contract parity with BVP; unused here)
  int<lower=0> N_top_teams;
  array[N_top_teams] int<lower=0> top_teams;
  vector[N_top_teams] time_to_next_games;

  int<lower=0> N_pred;
  array[N_pred] int<lower=1, upper=K> team1_pred;
  array[N_pred] int<lower=1, upper=K> team2_pred;
  vector[N_pred] pred_timediff1;
  vector[N_pred] pred_timediff2;
}

transformed data {
  array[N] vector[2] obs_SD;
  for (n in 1:N) {
    obs_SD[n][1] = goals1[n] + goals2[n];
    obs_SD[n][2] = goals1[n] - goals2[n];
  }
  matrix[K, N_rounds] delta_t;
  for (k in 1:K)
    for (r in 1:N_rounds)
      delta_t[k, r] = sqrt(time_between_matches[k, r]);
}

parameters {
  sum_to_zero_vector[K] off0;
  array[N_rounds] sum_to_zero_vector[K] z_off;
  vector[K] z_sigma_off;
  real<lower=0> scale_sigma_off;
  real mean_sigma_off;

  sum_to_zero_vector[K] def0;
  array[N_rounds] sum_to_zero_vector[K] z_def;
  vector[K] z_sigma_def;
  real<lower=0> scale_sigma_def;
  real mean_sigma_def;

  real mean_log_goals;

  vector<lower=0>[K] home_advantage_off;
  vector<lower=0>[K] home_advantage_def;

  // (S, D) covariance: mean-scaled shrinkage phi and correlation slope gamma_rho
  real<lower=0> phi;
  real gamma_rho;
}

transformed parameters {
  array[N_rounds] vector[K] offense;
  array[N_rounds] vector[K] defense;
  vector<lower=0>[K] sigma_off = exp(mean_sigma_off + z_sigma_off * scale_sigma_off);
  vector<lower=0>[K] sigma_def = exp(mean_sigma_def + z_sigma_def * scale_sigma_def);

  offense[1] = off0;
  defense[1] = def0;
  for (i in 2:N_rounds) {
    offense[i] = offense[i - 1] + delta_t[, i] .* sigma_off .* z_off[i];
    defense[i] = defense[i - 1] + delta_t[, i] .* sigma_def .* z_def[i];
  }
}

model {
  off0 ~ std_normal();
  def0 ~ std_normal();
  for (i in 1:N_rounds) {
    z_off[i] ~ std_normal();
    z_def[i] ~ std_normal();
  }
  z_sigma_off ~ std_normal();
  scale_sigma_off ~ exponential(2);
  mean_sigma_off ~ normal(-4, 2);
  z_sigma_def ~ std_normal();
  scale_sigma_def ~ exponential(2);
  mean_sigma_def ~ normal(-5, 2);
  home_advantage_off ~ std_normal();
  home_advantage_def ~ std_normal();
  mean_log_goals ~ normal(log(1.5), 1);

  phi ~ normal(0.88, 0.1);
  gamma_rho ~ normal(1.1, 0.4);

  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];

    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;

    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);

    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];

    obs_SD[n] ~ multi_normal([ES, ED]', Sigma);
  }
}

generated quantities {
  vector[K] home_advantage_tot = home_advantage_off + home_advantage_def;
  vector[K] cur_offense_away = offense[N_rounds];
  vector[K] cur_defense_away = defense[N_rounds];
  vector[K] cur_strength_away = cur_offense_away + cur_defense_away;
  vector[K] cur_offense_home = cur_offense_away + home_advantage_off;
  vector[K] cur_defense_home = cur_defense_away + home_advantage_def;
  vector[K] cur_strength_home = cur_offense_home + cur_defense_home;
  vector[K] cur_offense = (cur_offense_away + cur_offense_home) / 2;
  vector[K] cur_defense = (cur_defense_away + cur_defense_home) / 2;
  vector[K] cur_strength = cur_offense + cur_defense;

  vector[N] log_lik;
  for (n in 1:N) {
    real off_h = offense[round1[n], team1[n]] + home_advantage_off[team1[n]];
    real def_h = defense[round1[n], team1[n]] + home_advantage_def[team1[n]];
    real off_a = offense[round2[n], team2[n]];
    real def_a = defense[round2[n], team2[n]];
    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;
    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);
    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];
    log_lik[n] = multi_normal_lpdf(obs_SD[n] | [ES, ED]', Sigma);
  }

  array[N_pred] int<lower=0> goals1_pred;
  array[N_pred] int<lower=0> goals2_pred;
  array[N_pred] int goal_diff_pred;
  array[N_pred] int<lower=0> total_goals_pred;

  for (n in 1:N_pred) {
    real off_h = offense[N_rounds, team1_pred[n]] + home_advantage_off[team1_pred[n]];
    real def_h = defense[N_rounds, team1_pred[n]] + home_advantage_def[team1_pred[n]];
    real off_a = offense[N_rounds, team2_pred[n]];
    real def_a = defense[N_rounds, team2_pred[n]];
    real lambda_h = exp(mean_log_goals + off_h - def_a);
    real lambda_a = exp(mean_log_goals + off_a - def_h);
    real ES = lambda_h + lambda_a;
    real ED = lambda_h - lambda_a;
    real var_SD = phi * ES;
    real rho = tanh(gamma_rho * ED / ES);
    matrix[2, 2] Sigma;
    Sigma[1, 1] = var_SD;
    Sigma[2, 2] = var_SD;
    Sigma[1, 2] = rho * var_SD;
    Sigma[2, 1] = Sigma[1, 2];

    vector[2] draw = multi_normal_rng([ES, ED]', Sigma);
    int S = to_int(round(fmax(0, draw[1])));
    int D = to_int(round(draw[2]));
    // Parity: S and D must share parity, since hg = (S + D) / 2 is an integer.
    if ((S + D) % 2 != 0) {
      if (D > 0) {
        D -= 1;
      } else if (D < 0) {
        D += 1;
      } else {
        S += 1;
      }
    }
    if (D > S) D = S;       // |D| cannot exceed S (no negative goals)
    if (D < -S) D = -S;
    goals1_pred[n] = (S + D) / 2;
    goals2_pred[n] = (S - D) / 2;
    goal_diff_pred[n] = goals1_pred[n] - goals2_pred[n];
    total_goals_pred[n] = goals1_pred[n] + goals2_pred[n];
  }
}
```

- [ ] **Step 2: Write the compile test**

Create `tests/testthat/test-backtest-sd.R`:

```r
test_that("2d_gaussian_sd.stan compiles", {
  skip_on_ci()
  skip_if_not(nzchar(Sys.getenv("SPORTS_TEST_STAN")), "set SPORTS_TEST_STAN=1 to run Stan compile")
  stan_path <- here::here("Stan", "football_iceland", "2d_gaussian_sd.stan")
  expect_true(file.exists(stan_path))
  mod <- cmdstanr::cmdstan_model(stan_path)
  expect_s3_class(mod, "CmdStanModel")
})
```

- [ ] **Step 3: Run the compile test**

Run: `SPORTS_TEST_STAN=1 Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-backtest-sd.R")'`
Expected: PASS (model compiles; first run takes ~30–60s for compilation).

- [ ] **Step 4: Commit**

```bash
git add Stan/football_iceland/2d_gaussian_sd.stan tests/testthat/test-backtest-sd.R
git commit -m "feat(stan): (S,D) lean-Gaussian football model (mean-induced covariance)"
```

---

### Task 2: Walk-forward decide closure for the `(S,D)` model

**Files:**
- Modify: `R/backtest-walkforward.R` (add two functions near `bt_wf_extract_decide`)
- Test: `tests/testthat/test-backtest-sd.R` (append)

**Interfaces:**
- Consumes: `load_leagues()`, `fit_league(league=, sex=, fit_date=, end_date=, …)`, `decide_league(league_key=, …, return_candidates=TRUE)`, `bt_wf_max_age_hours()` (all existing).
- Produces:
  - `bt_wf_sd_league(stan_model = "football_iceland/2d_gaussian_sd.stan")` → a football_iceland league list with `stan_model` overridden. Internal (`@noRd`).
  - `bt_wf_sd_decide(stan_model = "football_iceland/2d_gaussian_sd.stan")` → a `decide_fn` closure `(root, run_date, sex, ledger_asof)` that fits the `(S,D)` model as-of `run_date` into `root`, then `decide_league(..., return_candidates = TRUE)`. Exported.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-backtest-sd.R`:

```r
test_that("bt_wf_sd_league overrides only the stan_model", {
  base <- load_leagues()[["football_iceland"]]
  lg <- bt_wf_sd_league()
  expect_equal(lg$stan_model, "football_iceland/2d_gaussian_sd.stan")
  expect_equal(lg$sport, base$sport)
  expect_equal(lg$country, base$country)
})

test_that("bt_wf_sd_decide fits the (S,D) model and returns candidates", {
  captured <- new.env()
  testthat::local_mocked_bindings(
    fit_league = function(league, sex, ...) {
      captured$stan_model <- league$stan_model
      captured$sex <- sex
      invisible(NULL)
    },
    decide_league = function(...) tibble::tibble(stage = "kept", p = 0.5, odds = 2.0)
  )
  fn <- bt_wf_sd_decide()
  out <- fn(root = withr::local_tempdir(), run_date = as.Date("2026-05-15"), sex = "male")
  expect_equal(captured$stan_model, "football_iceland/2d_gaussian_sd.stan")
  expect_equal(captured$sex, "male")
  expect_true(all(c("p", "odds") %in% names(out)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-backtest-sd.R")'`
Expected: FAIL — `could not find function "bt_wf_sd_league"` / `"bt_wf_sd_decide"`.

- [ ] **Step 3: Implement the closure**

Add to `R/backtest-walkforward.R` (immediately after `bt_wf_extract_decide`):

```r
#' football_iceland league list with the (S,D) Gaussian Stan model.
#'
#' Overrides only `stan_model` so `fit_league(league = …)` fits the (S,D) model
#' through the exact same prepare/fit/extract path as the production BVP.
#' @param stan_model Stan path relative to `Stan/`. Default the (S,D) model.
#' @return The football_iceland league list with `stan_model` replaced.
#' @noRd
bt_wf_sd_league <- function(stan_model = "football_iceland/2d_gaussian_sd.stan") {
  lg <- load_leagues()[["football_iceland"]]
  lg$stan_model <- stan_model
  lg
}

#' Decide closure that fits the (S,D) Gaussian model as-of `d`, then decides.
#'
#' The (S,D) analogue of [bt_wf_default_decide()]: it fits the (S,D) model (not
#' the config's BVP) into the isolated `root` with `end_date == run_date`
#' (leak-free), then decides over the pre-sliced odds. `write_archive = FALSE`
#' skips the football publish extracts (which assume BVP parameters); the
#' `beliefs_latest` write the decider needs is unconditional.
#' @param stan_model Stan path relative to `Stan/`. Default the (S,D) model.
#' @return A `decide_fn` closure `(root, run_date, sex, ledger_asof)`.
#' @export
bt_wf_sd_decide <- function(stan_model = "football_iceland/2d_gaussian_sd.stan") {
  league <- bt_wf_sd_league(stan_model)
  function(root, run_date, sex, ledger_asof = NULL) {
    fit_league(
      league = league, sex = sex,
      fit_date = run_date, end_date = run_date,
      seed = as.integer(format(run_date, "%Y%m%d")),
      schedule_horizon_days = 200L, root = root,
      write_archive = FALSE
    )
    decide_league(
      league_key = "football_iceland", sex = sex,
      run_date = run_date, root = root, write = FALSE,
      max_age_hours = bt_wf_max_age_hours(),
      return_candidates = TRUE
    )
  }
}
```

- [ ] **Step 4: Document + run tests to verify they pass**

Run: `Rscript -e 'devtools::document(); devtools::load_all(); testthat::test_file("tests/testthat/test-backtest-sd.R")'`
Expected: PASS (compile test skips without `SPORTS_TEST_STAN`; the two new tests pass). `document()` adds `bt_wf_sd_decide` to `NAMESPACE`.

- [ ] **Step 5: Commit**

```bash
git add R/backtest-walkforward.R NAMESPACE man/ tests/testthat/test-backtest-sd.R
git commit -m "feat(backtest): bt_wf_sd_decide — fit (S,D) model in the walk-forward"
```

---

### Task 3: `--model {bvp|sd}` CLI selector

**Files:**
- Modify: `R/backtest-walkforward.R` (add `wf_select_decide_fn`)
- Modify: `scripts/0Nb_walkforward.R` (parse `--model`, use selector, stamp output filenames)
- Test: `tests/testthat/test-backtest-sd.R` (append)

**Interfaces:**
- Produces: `wf_select_decide_fn(model = c("bvp","sd"), reuse = FALSE, source_root = here::here("data"))` → a `decide_fn`. `sd` returns `bt_wf_sd_decide()`; `bvp` returns `bt_wf_extract_decide(source_root)` when `reuse`, else `bt_wf_default_decide`. `model = "sd"` with `reuse = TRUE` is an error (no saved extracts). Internal (`@noRd`).

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-backtest-sd.R`:

```r
test_that("wf_select_decide_fn picks the right closure", {
  expect_identical(wf_select_decide_fn("bvp", reuse = FALSE), bt_wf_default_decide)
  expect_true(is.function(wf_select_decide_fn("bvp", reuse = TRUE)))
  expect_true(is.function(wf_select_decide_fn("sd", reuse = FALSE)))
  expect_error(wf_select_decide_fn("sd", reuse = TRUE), "reuse")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-backtest-sd.R")'`
Expected: FAIL — `could not find function "wf_select_decide_fn"`.

- [ ] **Step 3: Implement the selector**

Add to `R/backtest-walkforward.R` (after `bt_wf_sd_decide`):

```r
#' Select the walk-forward decide closure for a model + mode.
#'
#' `model = "sd"` re-fits the (S,D) model and cannot REUSE (no saved extracts).
#' `model = "bvp"` uses [bt_wf_extract_decide()] under `reuse`, else
#' [bt_wf_default_decide()].
#' @param model "bvp" or "sd".
#' @param reuse Replay saved extracts (bvp only).
#' @param source_root Live data root for the reuse path.
#' @return A `decide_fn` closure.
#' @noRd
wf_select_decide_fn <- function(model = c("bvp", "sd"), reuse = FALSE,
                                source_root = here::here("data")) {
  model <- match.arg(model)
  if (identical(model, "sd")) {
    if (isTRUE(reuse)) {
      stop("--reuse is unavailable for model=sd (no saved (S,D) extracts); use --per-round or --as-of",
        call. = FALSE
      )
    }
    return(bt_wf_sd_decide())
  }
  if (isTRUE(reuse)) bt_wf_extract_decide(source_root) else bt_wf_default_decide
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-backtest-sd.R")'`
Expected: PASS.

- [ ] **Step 5: Wire the flag into the driver script**

In `scripts/0Nb_walkforward.R`, after the line `reuse <- has_flag("reuse")`, add:

```r
model <- get_flag("model", "bvp")
if (!model %in% c("bvp", "sd")) stop("--model must be 'bvp' or 'sd'", call. = FALSE)
```

Then replace the whole `if (isTRUE(reuse)) { … } else { … }` block that assigns `decide_fn` and `cutoffs` so that `decide_fn` comes from the selector while the cutoff logic is unchanged. The new block:

```r
decide_fn <- wf_select_decide_fn(model, reuse = reuse, source_root = here::here("data"))

if (isTRUE(reuse)) {
  ext_dir <- here::here(
    "data", "beliefs", "extracts", "sport=football",
    "country=iceland", paste0("sex=", sex)
  )
  if (!dir.exists(ext_dir)) stop("--reuse: no extracts at ", ext_dir, call. = FALSE)
  fds <- sort(as.Date(sub("fit_date=", "", list.files(ext_dir))))
  if (!is.null(season_str)) fds <- fds[format(fds, "%Y") == season_str]
  if (length(fds) == 0L) stop("--reuse: no saved extract fit_dates", call. = FALSE)
  cutoffs <- fds
} else {
  cutoffs <- if (isTRUE(per_round)) {
    if (is.null(season_str)) stop("--per-round requires --season YYYY", call. = FALSE)
    season <- as.integer(season_str)
    dates <- as.Date(character())
    for (n in seq_len(50L)) {
      cd <- suppressWarnings(suppressMessages(
        compute_round_cutoff_date(results, season = season, round_cutoff = n, quiet = TRUE)
      ))
      if (is.null(cd)) break
      dates <- c(dates, cd)
    }
    if (length(dates) == 0L) stop("No completed rounds for season ", season, call. = FALSE)
    dates
  } else {
    if (is.null(as_of_str)) stop("--as-of YYYY-MM-DD required (or --per-round --season YYYY, or --reuse)", call. = FALSE)
    d <- as.Date(as_of_str)
    if (is.na(d)) stop("--as-of: could not parse '", as_of_str, "'", call. = FALSE)
    d
  }
}
```

Then change the `mode` line and the two output filenames so models do not collide:

```r
mode <- if (isTRUE(reuse)) "REUSE saved fits" else "RE-FIT per cutoff"
cli::cli_h1("Walk-forward {league_key}/{sex} [{model} | {mode}]: {length(cutoffs)} cutoff(s), horizon={horizon_days}d")
```

and lower in the file:

```r
arrow::write_parquet(wf$bets, file.path(out_dir, paste0("bets_", model, "_", sex, ".parquet")))
arrow::write_parquet(wf$scores, file.path(out_dir, paste0("scores_", model, "_", sex, ".parquet")))
```

- [ ] **Step 6: Verify the script parses and the BVP path is unchanged**

Run: `Rscript scripts/0Nb_walkforward.R --sex male --model bvp --reuse --season 2026`
Expected: completes (REUSE mode, seconds), writes `data/backtest/walkforward/bets_bvp_male.parquet` + `scores_bvp_male.parquet`, prints the scores tibble.

- [ ] **Step 7: Commit**

```bash
git add R/backtest-walkforward.R scripts/0Nb_walkforward.R man/
git commit -m "feat(backtest): --model {bvp|sd} flag + model-stamped walk-forward outputs"
```

---

### Task 4: Run the experiment (single-cutoff smoke → PPC → detached sweep)

**Files:** none created — this task runs the harness and verifies behaviour. Outputs land in `data/backtest/walkforward/` (gitignored, regenerable).

- [ ] **Step 1: Single-cutoff (S,D) smoke through the harness**

Run: `SPORTS_FIT_ITER_WARMUP=300 SPORTS_FIT_ITER_SAMPLING=300 Rscript scripts/0Nb_walkforward.R --sex male --model sd --as-of 2026-05-15`
Expected: one re-fit completes in an isolated tempdir (no real-data writes), prints the OOS scores tibble with `n > 0`, writes `bets_sd_male.parquet`. This proves end-to-end: `(S,D)` fit → `extract_posteriors` → `decide_league` → settlement → scoring.
If the fit trips the divergence gate, the cutoff is skipped with a warning (harness behaviour) — re-run with `SPORTS_FIT_ADAPT_DELTA=0.99` to confirm it is geometry, not a bug.

- [ ] **Step 2: PPC on a standalone full fit**

Run (verifies the covariance recovers the diagnostic):

```bash
Rscript -e '
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
root <- withr::local_tempdir()
res <- read_table("results", filter = list(sport = "football", country = "iceland", sex = "male"))
res <- res[res$match_date <= as.Date("2026-06-15"), ]
bt_wf_seed_results(res, root)
Sys.setenv(SPORTS_FIT_ITER_WARMUP = "500", SPORTS_FIT_ITER_SAMPLING = "500")
fit_league(league = bt_wf_sd_league(), sex = "male",
           fit_date = as.Date("2026-06-15"), end_date = as.Date("2026-06-15"),
           chains = 2L, seed = 1L, root = root, write_archive = FALSE)
fit <- readRDS(file.path(root, "beliefs", "fits", "sport=football",
                         "country=iceland", "sex=male", "fit.rds"))
print(fit$summary(c("phi", "gamma_rho", "mean_log_goals")))
print(fit$diagnostic_summary())
'
```
Expected: `phi` posterior mean ≈ 0.8–0.95 (near the diagnostic `φ≈0.84`), `gamma_rho` ≈ 1.0–1.5, no divergences (or few). If `phi`/`gamma_rho` are dominated by the prior (posterior ≈ prior), note it — it means the per-match likelihood is weakly informative about the covariance scalars and the comparison rests mostly on the means; still valid, but record it for the report.

- [ ] **Step 3: Launch the detached full sweep (both models, both sexes)**

```bash
nohup Rscript scripts/0Nb_walkforward.R --sex male   --model sd  --season 2026 --per-round > /tmp/wf_sd_male.log   2>&1 & disown
nohup Rscript scripts/0Nb_walkforward.R --sex female --model sd  --season 2026 --per-round > /tmp/wf_sd_female.log 2>&1 & disown
Rscript scripts/0Nb_walkforward.R --sex male   --model bvp --reuse --season 2026
Rscript scripts/0Nb_walkforward.R --sex female --model bvp --reuse --season 2026
```
Expected: the two `sd` runs take a few hours each (one Stan fit per round); the two `bvp` reuse runs finish in seconds. Final state: `bets_{bvp,sd}_{male,female}.parquet` + `scores_*` in `data/backtest/walkforward/`. Monitor: `tail -f /tmp/wf_sd_male.log`.

- [ ] **Step 4: Sanity-check the outputs exist and carry the scoring columns**

```bash
Rscript -e '
suppressPackageStartupMessages(library(arrow)); suppressPackageStartupMessages(library(dplyr))
d <- here::here("data", "backtest", "walkforward")
for (f in list.files(d, pattern = "^bets_.*\\.parquet$")) {
  b <- read_parquet(file.path(d, f))
  cat(f, "rows", nrow(b), "cols:", paste(intersect(c("p","odds","win","market","outcome","line"), names(b)), collapse=","), "\n")
}'
```
Expected: each `bets_*` has `p`, `odds`, `win`, `market`, `outcome`, `line` and `nrow > 0`. (No commit — outputs are gitignored.)

---

### Task 5: Comparison report

**Files:**
- Create: `docs/reports/2026-sd-gaussian-backtest.qmd`

**Interfaces:**
- Consumes: `data/backtest/walkforward/bets_{bvp,sd}_{male,female}.parquet`; `bt_devig()`, `bt_skill(by="market")`, `bt_skill_ci(by="market")` (existing). Note: walk-forward `bets` lack `sport`/`country`/`sex` columns, so the report adds them (it knows the cell from the filename) before `bt_devig`.

- [ ] **Step 1: Write the report**

Create `docs/reports/2026-sd-gaussian-backtest.qmd`:

````markdown
---
title: "(S,D) lean-Gaussian vs BVP vs market — 2026 walk-forward"
format: html
execute:
  echo: false
  warning: false
---

```{r setup}
suppressPackageStartupMessages({
  devtools::load_all(here::here(), quiet = TRUE)
  library(dplyr); library(arrow); library(ggplot2)
})
wf_dir <- here::here("data", "backtest", "walkforward")

load_bets <- function(model) {
  out <- list()
  for (sex in c("male", "female")) {
    f <- file.path(wf_dir, paste0("bets_", model, "_", sex, ".parquet"))
    if (!file.exists(f)) next
    b <- read_parquet(f)
    if (nrow(b) == 0L) next
    b$sport <- "football"; b$country <- "iceland"; b$sex <- sex; b$model <- model
    out[[sex]] <- b
  }
  if (length(out) == 0L) tibble::tibble() else bind_rows(out)
}

bets <- bind_rows(load_bets("bvp"), load_bets("sd"))
have_data <- nrow(bets) > 0L
```

```{r}
#| eval: !expr have_data
skill <- bets |>
  group_by(model) |>
  group_modify(~ {
    sc <- bt_devig(.x)
    if (nrow(sc) == 0L) return(tibble::tibble())
    s <- bt_skill(sc, by = "market")
    ci <- bt_skill_ci(sc, by = "market")
    left_join(s, ci, by = "market")
  }) |>
  ungroup()
knitr::kable(skill, digits = 4,
  caption = "Model-vs-market skill by market (brier_skill > 0 beats the de-vigged line; skill_lo/hi = match-clustered 90% CI)")
```

```{r}
#| eval: !expr have_data
#| fig-width: 9
#| fig-height: 5
plt <- skill |>
  filter(!is.na(brier_skill)) |>
  ggplot(aes(market, brier_skill, fill = model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = skill_lo, ymax = skill_hi),
    position = position_dodge(width = 0.7), width = 0.2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "OOS Brier skill vs the de-vigged Lengjan line, 2026",
       subtitle = "above 0 = beats the market; bars are 90% match-clustered CIs",
       x = NULL, y = "Brier skill (1 - model/market)")
tryCatch(plt + metill::theme_metill(), error = function(e) plt + theme_minimal())
```

```{r}
#| eval: !expr (!have_data)
cat("No walk-forward outputs found in", wf_dir,
    "\nRun Task 4 (scripts/0Nb_walkforward.R --model {bvp,sd} ...) first.")
```

## Second-moment diagnostic (covariance data-story)

Regenerate with `Rscript docs/reports/2026-sd-gaussian-backtest/diagnostic.R`.

![male](2026-sd-gaussian-backtest/sd_diag_male.png)

![female](2026-sd-gaussian-backtest/sd_diag_female.png)
````

- [ ] **Step 2: Render to verify it builds**

Run: `quarto render docs/reports/2026-sd-gaussian-backtest.qmd`
Expected: produces `docs/reports/2026-sd-gaussian-backtest.html` with the skill table + plot (if Task 4 outputs exist) or the "no outputs" note (if not). Either way, **the render must succeed**.

- [ ] **Step 3: Read the verdict against the promote rule**

Inspect the skill table: the `(S,D)` model advances to Phase 2 only if its **`totals` `brier_skill` is non-negative with `skill_lo` not worse than BVP's**. Record the verdict (and any "covariance scalars prior-dominated" caveat from Task 4 Step 2) in the report's prose.

- [ ] **Step 4: Commit**

```bash
git add docs/reports/2026-sd-gaussian-backtest.qmd
git commit -m "feat(report): 2026 (S,D)-vs-BVP-vs-market walk-forward comparison"
```

---

## Self-review

- **Spec coverage:** §4 model → Task 1. §5 harness hook (`bt_wf_sd_decide`, `--model`) → Tasks 2–3. §5 run (2026, per-round, both sexes, BVP-reuse vs SD-refit) → Task 4. §6 outputs (`bt_devig`/`bt_skill` by market, CI, promote rule) → Task 5. §7 testing (compile, smoke, leak-free reuse, PPC) → Task 1 Step 3, Task 4 Steps 1–2. §2 diagnostic → committed with the spec, embedded in Task 5. Optional pre-2026 outcome-only arm (§8) is correctly deferred (not built). ✓
- **Placeholder scan:** all steps carry real code/commands; no TBD/TODO. ✓
- **Type consistency:** `bt_wf_sd_league` / `bt_wf_sd_decide` / `wf_select_decide_fn` used identically across Tasks 2–4; Stan covariance params `phi` + `gamma_rho` consistent between model, PPC (Task 4 Step 2), and report prose; output filenames `bets_{model}_{sex}.parquet` consistent between Task 3 and Task 5. ✓

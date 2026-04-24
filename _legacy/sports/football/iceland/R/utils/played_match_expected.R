# Posterior-predictive aggregation over played matches.
#
# For each posterior draw x played match in the current season's top division,
# simulate (Y1, Y2) from the bivariate Poisson (no diagonal inflation — see
# Stan/bivariate_poisson_no_inflation.stan) using the reconstructed rates,
# then aggregate per team into per-draw season-to-date totals of xg_for,
# xg_against, xpts. Writes results/{sex}/team_expected_by_draw.csv.
#
# Entry point: aggregate_played_match_expected(sex).
#
# See DESIGN-xpts-2026-04-23.md for rationale (decisions D1-D10).

library(tidyverse)
library(posterior)
library(here)

# --- Internal helpers -----------------------------------------------------

# Row-wise log-sum-exp, numerically stable. Avoids a matrixStats dependency.
.row_log_sum_exp <- function(m) {
  m_max <- do.call(pmax, as.data.frame(m))
  m_max + log(rowSums(exp(m - m_max)))
}

# Vectorised Poisson-2D log-PMF (trivariate reduction). Mirrors the
# poisson_2d_log_lpmf function in the no-inflation Stan file. log_lambda*
# are vectors across draws; g1, g2 scalar integer counts.
.log_pmf_bivariate_poisson <- function(g1, g2, log_lambda1, log_lambda2, log_lambda3) {
  lambda1 <- exp(log_lambda1)
  lambda2 <- exp(log_lambda2)
  lambda3 <- exp(log_lambda3)

  K <- min(g1, g2)
  if (K < 0L) {
    return(rep(-Inf, length(log_lambda1)))
  }

  n <- length(log_lambda1)
  log_terms <- matrix(NA_real_, nrow = n, ncol = K + 1L)
  for (k in 0:K) {
    log_terms[, k + 1L] <-
      k * log_lambda3 +
      (g1 - k) * log_lambda1 +
      (g2 - k) * log_lambda2 -
      (lgamma(k + 1) + lgamma(g1 - k + 1) + lgamma(g2 - k + 1))
  }

  lse <- if (ncol(log_terms) == 1L) log_terms[, 1L] else .row_log_sum_exp(log_terms)
  result <- -(lambda1 + lambda2 + lambda3) + lse

  # Stan short-circuits when lambda3 <= 1e-4 to independent Poissons.
  small <- lambda3 <= 1e-4
  if (any(small)) {
    result[small] <-
      dpois(g1, lambda1[small], log = TRUE) +
      dpois(g2, lambda2[small], log = TRUE)
  }
  result
}

# Simulate (Y1, Y2) from the bivariate Poisson via trivariate reduction.
# Each vector is length n_draws.
.simulate_match <- function(mu1, mu2, mu3) {
  n <- length(mu1)
  X1 <- rpois(n, exp(mu1))
  X2 <- rpois(n, exp(mu2))
  X3 <- rpois(n, exp(mu3))
  list(y1 = X1 + X3, y2 = X2 + X3)
}

# --- Public entry point ---------------------------------------------------

#' Aggregate posterior-predictive expected values for played matches
#'
#' For the given sex's current-season top-division played matches, simulates
#' posterior-predictive (home_goals, away_goals) per parameter draw using the
#' fitted bivariate Poisson with correlation (no diagonal inflation).
#' Aggregates per team into per-draw season-to-date totals of expected goals
#' for, expected goals against, and expected points. Writes the per-draw
#' per-team tibble to `results/{sex}/team_expected_by_draw.csv`.
#'
#' Runs a log-likelihood cross-check on a random sample of matches: the
#' reconstructed log_lik[n] posterior mean must agree with the Stan-emitted
#' log_lik[n] posterior mean within 4 × MC SE. Aborts on mismatch so Stan-side
#' model changes can't silently drift the R-side reconstruction.
#'
#' @param sex "male" or "female"
#' @param write_csv Whether to persist the output tibble to CSV (default TRUE)
#' @param check_sample_size How many played matches to sample for the log_lik
#'   cross-check (default 5L; pass 0L to skip)
#' @return Per-team per-draw tibble (columns: team, .draw, xg_for, xg_against,
#'   xpts). Returned invisibly.
#' @export
aggregate_played_match_expected <- function(
  sex,
  write_csv = TRUE,
  check_sample_size = 5L
) {
  stopifnot(sex %in% c("male", "female"))

  fit <- readr::read_rds(here::here("results", sex, "fit.rds"))
  model_d_all <- readr::read_csv(
    here::here("results", sex, "model_d.csv"),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(model_row = dplyr::row_number())
  teams <- readr::read_csv(
    here::here("results", sex, "teams.csv"),
    show_col_types = FALSE
  )

  current_season <- max(model_d_all$season, na.rm = TRUE)
  matches <- model_d_all |>
    dplyr::filter(
      season == current_season,
      division == 1,
      !is.na(home_goals)
    )

  out_path <- here::here("results", sex, "team_expected_by_draw.csv")

  if (nrow(matches) == 0L) {
    empty <- tibble::tibble(
      team = character(),
      .draw = integer(),
      xg_for = numeric(),
      xg_against = numeric(),
      xpts = numeric()
    )
    if (write_csv) readr::write_csv(empty, out_path)
    message(sprintf(
      "played_match_expected[%s]: no current-season div-1 played matches; wrote empty CSV",
      sex
    ))
    return(invisible(empty))
  }

  # --- Posterior draws ----------------------------------------------------

  rv <- fit$draws(c(
    "offense", "defense",
    "home_advantage_off", "home_advantage_def",
    "mean_log_goals",
    "alpha_mu3", "beta_mu3_strength_diff",
    "log_lik"
  )) |>
    posterior::as_draws_rvars()

  n_draws <- posterior::ndraws(rv$mean_log_goals)

  offense_dr <- posterior::draws_of(rv$offense) # [draws, N_rounds, K]
  defense_dr <- posterior::draws_of(rv$defense)
  ha_off_dr <- posterior::draws_of(rv$home_advantage_off) # [draws, K]
  ha_def_dr <- posterior::draws_of(rv$home_advantage_def)
  mlg_dr <- as.numeric(posterior::draws_of(rv$mean_log_goals))
  a_mu3_dr <- as.numeric(posterior::draws_of(rv$alpha_mu3))
  b_mu3_dr <- as.numeric(posterior::draws_of(rv$beta_mu3_strength_diff))
  loglik_dr <- posterior::draws_of(rv$log_lik) # [draws, N_model]

  # --- Per-match simulation -----------------------------------------------

  check_n <- as.integer(min(check_sample_size, nrow(matches)))
  check_idx <- integer(0)
  recon_ll <- NULL
  if (check_n > 0L) {
    # Deterministic sample; withr scopes the seed so downstream RNG is
    # unaffected.
    check_idx <- withr::with_seed(42L, sample.int(nrow(matches), check_n))
    recon_ll <- matrix(NA_real_, nrow = n_draws, ncol = check_n)
  }

  team_per_match <- vector("list", nrow(matches))

  for (m in seq_len(nrow(matches))) {
    mi <- matches[m, ]
    t1 <- mi$home_nr
    t2 <- mi$away_nr
    r1 <- mi$home_round
    r2 <- mi$away_round

    off_home <- offense_dr[, r1, t1] + ha_off_dr[, t1]
    def_home <- defense_dr[, r1, t1] + ha_def_dr[, t1]
    off_away <- offense_dr[, r2, t2]
    def_away <- defense_dr[, r2, t2]

    mu1 <- mlg_dr + off_home - def_away
    mu2 <- mlg_dr + off_away - def_home

    strength_diff <- abs(off_home + def_home - off_away - def_away)
    logit_rho <- a_mu3_dr + b_mu3_dr * strength_diff
    # log_inv_logit(x) = plogis(x, log.p = TRUE), numerically stable
    mu3 <- plogis(logit_rho, log.p = TRUE) + 0.5 * (mu1 + mu2)

    sim <- .simulate_match(mu1, mu2, mu3)

    pts_home <- ifelse(sim$y1 > sim$y2, 3L, ifelse(sim$y1 == sim$y2, 1L, 0L))
    pts_away <- ifelse(sim$y2 > sim$y1, 3L, ifelse(sim$y2 == sim$y1, 1L, 0L))

    team_per_match[[m]] <- dplyr::bind_rows(
      tibble::tibble(
        .draw      = seq_len(n_draws),
        team       = teams$team[t1],
        xg_for     = sim$y1,
        xg_against = sim$y2,
        xpts       = pts_home
      ),
      tibble::tibble(
        .draw      = seq_len(n_draws),
        team       = teams$team[t2],
        xg_for     = sim$y2,
        xg_against = sim$y1,
        xpts       = pts_away
      )
    )

    slot <- match(m, check_idx)
    if (!is.na(slot)) {
      recon_ll[, slot] <- .log_pmf_bivariate_poisson(
        mi$home_goals, mi$away_goals, mu1, mu2, mu3
      )
    }
  }

  team_expected <- dplyr::bind_rows(team_per_match) |>
    dplyr::summarise(
      xg_for = sum(xg_for),
      xg_against = sum(xg_against),
      xpts = sum(xpts),
      .by = c(team, .draw)
    ) |>
    dplyr::arrange(team, .draw)

  # --- Log-likelihood cross-check -----------------------------------------

  if (check_n > 0L) {
    stan_ll <- loglik_dr[, matches$model_row[check_idx], drop = FALSE]
    stan_mean <- colMeans(stan_ll)
    recon_mean <- colMeans(recon_ll)
    pooled_sd <- pmax(apply(stan_ll, 2, sd), apply(recon_ll, 2, sd))
    mc_se <- pooled_sd / sqrt(n_draws)
    tol <- 4 * mc_se
    diff <- abs(stan_mean - recon_mean)
    bad <- diff > tol
    if (any(bad)) {
      err <- tibble::tibble(
        match_row  = matches$model_row[check_idx][bad],
        game_date  = matches$date[check_idx][bad],
        home       = matches$home[check_idx][bad],
        away       = matches$away[check_idx][bad],
        stan_mean  = stan_mean[bad],
        recon_mean = recon_mean[bad],
        diff       = diff[bad],
        tol        = tol[bad]
      )
      stop(sprintf(
        "played_match_expected[%s]: log_lik reconstruction disagrees with Stan on %d/%d sampled matches (>4xMC SE). This usually means the R-side likelihood drifted from the Stan file. Rows:\n%s",
        sex, sum(bad), check_n,
        paste(utils::capture.output(print(err)), collapse = "\n")
      ))
    }
  }

  # --- Invariant checks ---------------------------------------------------

  # Every goal is both scored and conceded, so per-draw sums must agree.
  per_draw_bal <- team_expected |>
    dplyr::summarise(
      diff = sum(xg_for) - sum(xg_against),
      .by = .draw
    )
  max_bal <- max(abs(per_draw_bal$diff))
  if (max_bal > 1e-6) {
    stop(sprintf(
      "played_match_expected[%s]: xg_for vs xg_against imbalance (max abs = %.3g). Bug.",
      sex, max_bal
    ))
  }

  # --- Persist & return ---------------------------------------------------

  if (write_csv) readr::write_csv(team_expected, out_path)
  message(sprintf(
    "played_match_expected[%s]: wrote %d rows (%d teams x %d draws) to %s",
    sex, nrow(team_expected), dplyr::n_distinct(team_expected$team), n_draws, out_path
  ))
  invisible(team_expected)
}

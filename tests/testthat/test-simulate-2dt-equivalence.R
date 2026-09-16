# tests/testthat/test-simulate-2dt-equivalence.R
# Pins the R match generators to the Stan models they copy (spec 2026-09-16
# §11). A real fit on the synthetic facts fixture; the generator then replays
# Stan's own prediction fixtures from the SAME posterior draws. Per fixture the
# two must agree in distribution, and the league table built from the replay
# must match the one the pre-2026-09-16 path built from Stan's goals*_pred,
# within Monte Carlo error. Sampler health is not under test, so the model is
# run directly rather than through fit_model()'s diagnostics gate.
#
# The fit sees a NOISED copy of the fixture's 2DT results. The committed scores
# are exact functions of the team index (84-82, 84-81, ...), which a model with
# a residual scale cannot fit: the posterior collapses (rho at 1, nu stuck or
# near 1, R-hat 2-3), home minus away becomes a constant per fixture, and the
# comparisons below degenerate -- P(home win) is 1 on both sides and every
# table probability is 0 or 1 -- while which corner the sampler lands in
# depends on its inits. Seeded noise gives a proper posterior. The committed
# fixture files are untouched.
#
# The models' tails are heavy (nu >= 1, with a few draws below 4), so a sample
# sd or Pearson correlation is ruled by single draws. The per-fixture checks
# use the IQR and Spearman's rank correlation instead, plus a per-draw PIT of
# both sides' scores against the Stan GQ block restated as an oracle.
#
# Not checked here: that both scores share ONE mixing variable. At the nu these
# fits land on (~20) a second, independent one barely moves any statistic
# below; the "both scores share one mixing variable" test in
# test-simulate-2dt.R guards it directly, at nu = 5.

# Per-sport noise on the fixture's 2DT scores (points / goals).
EQUIV_NOISE_SD <- c(basketball = 10, handball = 3)
# Per-fixture tolerances. Each difference of two 2000-draw estimates has a
# sampling sd near 0.037 (IQR ratio) and 0.03 (Spearman), so these sit about
# five sds out. Calibrated 2026-09-16 with tools/calibrate-2dt-equivalence.R,
# which replays every fixture under 200 seeds against these fits' fixed Stan
# draws: the largest deviations were 0.14 and 0.11 (and 0.046 against the
# P(home win) check's 0.06, the tightest margin). The PIT threshold holds the
# 24 KS tests' joint false-alarm rate near 2% should the draws ever change
# (another platform, a new CmdStan) -- re-run that script when they do.
EQUIV_IQR_TOL <- 0.20
EQUIV_SPEARMAN_TOL <- 0.15
EQUIV_PIT_MIN_P <- 0.001

# The fixture's facts root, with seeded rounded-normal noise on every 2DT score
# (integers, never negative). Only the 2DT partitions are rewritten.
.noised_facts_root <- function(env) {
  root <- fixture_facts_root(env = env)
  results <- read_table("results", root = root)
  results <- results[results$sport %in% names(EQUIV_NOISE_SD), , drop = FALSE]
  # A fixed row order, so the noise does not depend on the order the
  # partitions are read back in.
  results <- dplyr::arrange(
    results, .data$sport, .data$sex, .data$season, .data$division,
    .data$match_date, .data$home_team, .data$away_team
  )
  n <- nrow(results)
  noise <- withr::with_seed(20260919L, matrix(
    stats::rnorm(2L * n, sd = unname(EQUIV_NOISE_SD[results$sport])),
    ncol = 2L
  ))
  results$home_score <- pmax(0L, as.integer(round(results$home_score + noise[, 1])))
  results$away_score <- pmax(0L, as.integer(round(results$away_score + noise[, 2])))
  write_table(results, "results", root = root)
  root
}

.equivalence_fit <- function(sport, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )
  root <- .noised_facts_root(env)
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  prep <- prepare_data(league, "male", end_date = FIXTURE_END_DATE, root = root)
  model <- cmdstanr::cmdstan_model(here::here("Stan", league$stan_model))
  # init and adapt_delta as fit_model() sets them: cmdstanr's default random
  # inits can start basketball's rho link saturated at +-1, a singular scale
  # matrix, and that chain then never starts. diagnostics = NULL: sampler
  # health is not under test (above), so cmdstanr's checks would only print.
  fit <- model$sample(
    data = prep$stan_data, chains = 2L, parallel_chains = 2L,
    iter_warmup = 500L, iter_sampling = 1000L, seed = 20260916L,
    init = 0, adapt_delta = 0.95,
    refresh = 0L, show_messages = FALSE, show_exceptions = FALSE,
    diagnostics = NULL
  )
  # A chain that failed to start would silently halve the draws compared.
  expect_equal(
    as.integer(fit$return_codes()), c(0L, 0L),
    label = paste(sport, "chain return codes")
  )
  list(fit = fit, prep = prep, root = root)
}

# One fixture's two sides across draws, built the way simulate_league_season()
# builds them: the home side carries its home advantage on offence AND defence.
.fixture_sides <- function(si, match_fn, home, away) {
  pick <- function(team, col) {
    d <- si$team[si$team$team == team, , drop = FALSE]
    d[[col]][match(si$scalar$.draw, d$.draw)]
  }
  side <- function(team, is_home) {
    out <- list(off = pick(team, "cur_offense"), def = pick(team, "cur_defense"))
    if (is_home) {
      out$off <- out$off + pick(team, "home_advantage_off")
      out$def <- out$def + pick(team, "home_advantage_def")
    }
    for (col in attr(match_fn, "team_cols")) out[[col]] <- pick(team, col)
    out
  }
  list(home = side(home, TRUE), away = side(away, FALSE))
}

# The Stan GQ block restated as an oracle, independently of R/simulate-2dt.R:
# the per-draw bivariate t each model predicts one fixture from (the
# goals*_pred loops of Stan/basketball_iceland/2d_student_t_scalarsigma.stan
# and Stan/handball_iceland/2d_student_t.stan). Sigma is a SCALE matrix.
.oracle_2dt <- function(sport, sides, scalars) {
  h <- sides$home
  a <- sides$away
  out <- list(
    mu_h = scalars$mean_goals + h$off - a$def,
    mu_a = scalars$mean_goals + a$off - h$def,
    nu = scalars$nu
  )
  if (identical(sport, "basketball")) {
    d <- abs(h$off + h$def - a$off - a$def)
    t <- abs(h$off + h$def + a$off + a$def)
    eta <- scalars$alpha_rho + scalars$beta_rho * d +
      scalars$beta2_rho * t + scalars$beta3_rho * t * d
    out$s_h <- scalars$sigma
    out$s_a <- scalars$sigma
    out$rho <- 2 * stats::plogis(eta) - 1
    # 1 - rho^2 straight from the link, so it cannot cancel to 0 near |rho| = 1.
    out$one_minus_rho2 <- 4 * stats::plogis(eta) * stats::plogis(-eta)
  } else {
    out$s_h <- h$sigma_team
    out$s_a <- a$sigma_team
    out$rho <- scalars$rho
    out$one_minus_rho2 <- 1 - scalars$rho^2
  }
  out
}

# PIT of score pairs under the oracle: for a bivariate t with scale matrix
# Sigma, (y - mu)' Sigma^-1 (y - mu) / 2 ~ F(2, nu). Uniform across draws when
# location, scales, correlation and nu all match; a mismatch in any skews it.
.pit_2dt <- function(home, away, oracle) {
  e1 <- (home - oracle$mu_h) / oracle$s_h
  e2 <- (away - oracle$mu_a) / oracle$s_a
  q <- e1^2 + (e2 - oracle$rho * e1)^2 / oracle$one_minus_rho2
  stats::pf(q / 2, df1 = 2, df2 = oracle$nu)
}

.expect_generator_matches_stan <- function(sport) {
  f <- .equivalence_fit(sport)
  si <- .extract_sim_inputs_2dt(
    f$fit, f$prep$teams, sport,
    n_seasons = f$prep$stan_data$N_seasons
  )
  si$scalar$z_level <- 0
  si$scalar <- .season_level_2dt(si$scalar, 0L)
  match_fn <- .match_fn_2dt(sport)

  pg <- .compute_posterior_goals_2dt(f$fit, f$prep$pred_d)
  expect_gt(nrow(pg), 0L)

  se <- function(a, b) sqrt(stats::var(a) / length(a) + stats::var(b) / length(b))
  iqr_dev <- function(a, b) abs(stats::IQR(a) / stats::IQR(b) - 1)
  spearman <- function(a, b) stats::cor(a, b, method = "spearman")
  ks_p <- function(u) stats::ks.test(u, "punif")$p.value

  withr::local_seed(20260917L)
  for (g in unique(pg$game_nr)) {
    stan <- pg[pg$game_nr == g, ]
    stan <- stan[order(stan$.draw), ]
    what <- paste(sport, stan$home_team[1], "v", stan$away_team[1])
    # The PIT pairs each Stan score with its own draw's parameters.
    expect_equal(stan$.draw, si$scalar$.draw, label = paste(what, "draws"))
    sides <- .fixture_sides(si, match_fn, stan$home_team[1], stan$away_team[1])
    r <- match_fn(sides$home, sides$away, si$scalar)
    oracle <- .oracle_2dt(sport, sides, si$scalar)

    expect_lt(abs(mean(r$home) - mean(stan$home_score)),
      5 * se(r$home, stan$home_score),
      label = paste(what, "home mean")
    )
    expect_lt(abs(mean(r$away) - mean(stan$away_score)),
      5 * se(r$away, stan$away_score),
      label = paste(what, "away mean")
    )
    expect_lt(iqr_dev(r$home, stan$home_score), EQUIV_IQR_TOL,
      label = paste(what, "home IQR")
    )
    expect_lt(iqr_dev(r$away, stan$away_score), EQUIV_IQR_TOL,
      label = paste(what, "away IQR")
    )
    expect_lt(
      abs(spearman(r$home, r$away) -
        spearman(stan$home_score, stan$away_score)),
      EQUIV_SPEARMAN_TOL,
      label = paste(what, "Spearman")
    )
    expect_lt(
      abs(mean(r$home > r$away) -
        mean(stan$home_score > stan$away_score)), 0.06,
      label = paste(what, "P(home win)")
    )
    # Stan's draws against the oracle pin the extracted parameters; the
    # replay's against the same oracle pin the generator, draw by draw.
    expect_gt(ks_p(.pit_2dt(stan$home_score, stan$away_score, oracle)),
      EQUIV_PIT_MIN_P,
      label = paste(what, "Stan PIT")
    )
    expect_gt(ks_p(.pit_2dt(r$home, r$away, oracle)),
      EQUIV_PIT_MIN_P,
      label = paste(what, "replay PIT")
    )
  }

  # The table: Stan's window through the old path vs the same fixtures
  # through the simulator.
  div <- .iceland_division_codes(paste0(sport, "_iceland"), "male")[[1]]
  results <- read_table("results", root = f$root)
  played <- results[
    results$sport == sport & results$sex == "male" &
      results$season == 2100L & results$division == div, ,
    drop = FALSE
  ]
  tp <- .tie_params_pfi(sport)
  base_old <- .compute_base_points_2dt(
    played,
    has_ties = tp$has_ties, tie_threshold = tp$tie_threshold
  )
  old <- .compute_final_positions_2dt(
    pg, div, base_old, tp$has_ties, tp$tie_threshold,
    current_top_teams = NULL
  )
  window <- unique(pg[pg$division == div, c("game_nr", "home_team", "away_team")])
  window <- window[order(window$game_nr), c("home_team", "away_team")]
  base_new <- tibble::tibble(
    team = base_old$team,
    base_points = base_old$base_points,
    base_gd = as.integer(base_old$base_diff),
    base_gf = 0L
  )
  new <- withr::with_seed(20260918L, simulate_league_season(
    si$team, si$scalar, window, base_new,
    match_fn = match_fn,
    points_fn = .points_fn_2dt(tp$has_ties, tp$tie_threshold),
    tie_break = "jitter"
  ))$final_positions
  both <- dplyr::inner_join(
    old, new,
    by = c("team", "placement"), suffix = c("_stan", "_r")
  )
  expect_equal(nrow(both), nrow(old))
  expect_lt(max(abs(both$probability_stan - both$probability_r)), 0.06)
}

test_that("the basketball generator reproduces the Stan model's predictions", {
  .expect_generator_matches_stan("basketball")
})

test_that("the handball generator reproduces the Stan model's predictions", {
  .expect_generator_matches_stan("handball")
})

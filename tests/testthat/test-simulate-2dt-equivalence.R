# tests/testthat/test-simulate-2dt-equivalence.R
# Pins the R match generators to the Stan models they copy (spec 2026-09-16
# §11). A real fit on the synthetic facts fixture; the generator then replays
# Stan's own prediction fixtures from the SAME posterior draws. Per fixture the
# score moments must agree, and the league table built from the replay must
# match the one the pre-2026-09-16 path built from Stan's goals*_pred, within
# Monte Carlo error. Sampler health is not under test, so the model is run
# directly rather than through fit_model()'s diagnostics gate.

.equivalence_fit <- function(sport, env = parent.frame()) {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  skip_if(
    is.null(tryCatch(cmdstanr::cmdstan_path(), error = function(e) NULL)),
    "cmdstan not installed"
  )
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  prep <- prepare_data(league, "male", end_date = FIXTURE_END_DATE, root = root)
  model <- cmdstanr::cmdstan_model(here::here("Stan", league$stan_model))
  # diagnostics = NULL: sampler health is not under test (above), so cmdstanr's
  # post-sampling checks would only print noise.
  fit <- model$sample(
    data = prep$stan_data, chains = 2L, parallel_chains = 2L,
    iter_warmup = 500L, iter_sampling = 1000L, seed = 20260916L,
    refresh = 0L, show_messages = FALSE, show_exceptions = FALSE,
    diagnostics = NULL
  )
  list(fit = fit, prep = prep, root = root)
}

# One fixture replayed from the simulation inputs, sides built the way
# simulate_league_season() builds them.
.replay_fixture <- function(si, match_fn, home, away) {
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
  match_fn(side(home, TRUE), side(away, FALSE), si$scalar)
}

.expect_generator_matches_stan <- function(sport) {
  f <- .equivalence_fit(sport)
  si <- .extract_sim_inputs_2dt(
    f$fit, f$prep$teams, sport, n_seasons = f$prep$stan_data$N_seasons
  )
  si$scalar$z_level <- 0
  si$scalar <- .season_level_2dt(si$scalar, 0L)
  match_fn <- .match_fn_2dt(sport)

  pg <- .compute_posterior_goals_2dt(f$fit, f$prep$pred_d)
  expect_gt(nrow(pg), 0L)

  withr::local_seed(20260917L)
  for (g in unique(pg$game_nr)) {
    stan <- pg[pg$game_nr == g, ]
    stan <- stan[order(stan$.draw), ]
    r <- .replay_fixture(si, match_fn, stan$home_team[1], stan$away_team[1])
    what <- paste(sport, stan$home_team[1], "v", stan$away_team[1])
    se <- function(a, b) sqrt(stats::var(a) / length(a) + stats::var(b) / length(b))

    expect_lt(abs(mean(r$home) - mean(stan$home_score)),
              5 * se(r$home, stan$home_score), label = paste(what, "home mean"))
    expect_lt(abs(mean(r$away) - mean(stan$away_score)),
              5 * se(r$away, stan$away_score), label = paste(what, "away mean"))
    expect_lt(abs(stats::sd(r$home) / stats::sd(stan$home_score) - 1), 0.1,
              label = paste(what, "home sd"))
    expect_lt(abs(stats::sd(r$away) / stats::sd(stan$away_score) - 1), 0.1,
              label = paste(what, "away sd"))
    expect_lt(abs(stats::cor(r$home, r$away) -
                  stats::cor(stan$home_score, stan$away_score)), 0.12,
              label = paste(what, "correlation"))
    expect_lt(abs(mean(r$home > r$away) -
                  mean(stan$home_score > stan$away_score)), 0.06,
              label = paste(what, "P(home win)"))
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
    played, has_ties = tp$has_ties, tie_threshold = tp$tie_threshold
  )
  old <- .compute_final_positions_2dt(
    pg, div, base_old, tp$has_ties, tp$tie_threshold, current_top_teams = NULL
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
    old, new, by = c("team", "placement"), suffix = c("_stan", "_r")
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

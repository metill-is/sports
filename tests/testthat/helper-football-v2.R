# Toy leagues shared by the football v2 tests (test-model-v2.R for the
# package code, test-research-football-v2.R for the research prototype).

# For tests that fit with INLA. Where INLA (or sn, which its posterior sampler
# needs) is not installed they skip: right for CI Tests and for a laptop
# without it. In the job that exists to run them
# (.github/workflows/inla-engine.yml sets SPORTS_REQUIRE_INLA=1) a missing
# package is a failure, because a skip there is a green check for tests that
# never ran.
skip_if_no_inla <- function(have = vapply(
                              c(INLA = "INLA", sn = "sn"), requireNamespace, logical(1),
                              quietly = TRUE
                            )) {
  absent <- names(have)[!have]
  if (!length(absent)) {
    return(invisible(TRUE))
  }
  if (identical(Sys.getenv("SPORTS_REQUIRE_INLA"), "1")) {
    stop("SPORTS_REQUIRE_INLA=1 but not installed: ", paste(absent, collapse = ", "), call. = FALSE)
  }
  testthat::skip(paste(paste(absent, collapse = ", "), "not installed"))
}

toy_matches <- function() {
  data.frame(
    match_date = as.Date(c(
      "2021-05-03", "2021-05-06", "2021-05-12", "2021-09-20",
      "2022-05-02", "2022-05-11"
    )),
    home_team = c("A", "B", "A", "C", "A", "B"),
    away_team = c("B", "A", "C", "B", "B", "C"),
    home_score = c(2L, 0L, 1L, 3L, 1L, 2L),
    away_score = c(1L, 0L, 1L, 0L, 0L, 2L),
    season = c(2021L, 2021L, 2021L, 2021L, 2022L, 2022L),
    cell = c("L@2021", "L@2021", "L@2021", "L@2021", "L@2022", "L@2022")
  )
}

toy_fixture <- function() {
  data.frame(
    match_date = as.Date("2022-05-18"), home_team = "C", away_team = "A",
    home_score = NA_integer_, away_score = NA_integer_, season = 2022L, cell = "L@2022"
  )
}

toy_divisions <- function() {
  data.frame(
    team = c("A", "B", "C", "A", "B", "C"),
    season = c(2021L, 2021L, 2021L, 2022L, 2022L, 2022L),
    division = c("L1", "L1", "L2", "L1", "L2", "L1"),
    rank = c(1L, 1L, 2L, 1L, 2L, 1L)
  )
}

# Three teams that all stay in one division across a season break, one state
# slot each per season. With more than one stayer the reversion coefficient
# changes the prior, which it cannot do in toy_divisions() (one stayer).
stayers_matches <- function(seasons = 2021:2022) {
  one <- function(season) {
    monday <- as.Date(sprintf("%d-01-04", season)) + 7 * 17
    monday <- monday - (as.integer(format(monday, "%u")) - 1L)
    data.frame(
      match_date = monday + c(0, 2), home_team = c("A", "C"), away_team = c("B", "A"),
      home_score = c(1L, 0L), away_score = c(0L, 2L), season = season, cell = paste0("L1@", season)
    )
  }
  do.call(rbind, lapply(seasons, one))
}

stayers_divisions <- function(teams = c("A", "B", "C"), seasons = 2021:2022, division = "L1", rank = 1L) {
  d <- expand.grid(team = teams, season = seasons, stringsAsFactors = FALSE)
  d$division <- division
  d$rank <- rank
  d
}

# Covariance of one field implied by the steps Stan reads: every team starts
# at N(0, tau^2) and adds independent steps.
stan_form_cov <- function(spec, tau, sigma, kappa = 0) {
  sd <- fv2_stan_data(spec)
  step_var <- (sigma * sd$step_in + kappa * sd$step_brk)^2
  cum <- unlist(tapply(step_var, sd$slot_team, cumsum))
  same <- outer(sd$slot_team, sd$slot_team, "==")
  (tau^2 + outer(cum, cum, pmin)) * same
}

# Covariance of one field implied by the season-segment recursion Stan uses
# for a reverting break: the states are linear in (entry levels, innovations).
recursion_cov <- function(spec, tau, sigma, kappa, rho) {
  seg <- fv2_segments(spec)
  n <- spec$K + spec$S + seg$n_seg
  a <- vapply(seq_len(n), function(i) {
    e <- numeric(n)
    e[i] <- 1
    fv2_walk(
      seg, tau * e[seq_len(spec$K)], e[spec$K + seq_len(spec$S)],
      e[spec$K + spec$S + seq_len(seg$n_seg)], sigma, kappa, rho
    )
  }, numeric(spec$S))
  tcrossprod(a)
}

simulate_league <- function(n_team = 6L, n_week = 30L, seed = 11L) {
  set.seed(seed)
  teams <- paste0("T", seq_len(n_team))
  strength <- stats::rnorm(n_team, 0, 0.4)
  rows <- lapply(seq_len(n_week), function(w) {
    pair <- matrix(sample(n_team), ncol = 2)
    data.frame(
      match_date = FV2_EPOCH + 7 * (w + 20 * (w > n_week / 2)),
      home = pair[, 1], away = pair[, 2], season = 2021L + (w > n_week / 2)
    )
  })
  d <- do.call(rbind, rows)
  data.frame(
    match_date = d$match_date, home_team = teams[d$home], away_team = teams[d$away],
    home_score = stats::rpois(nrow(d), exp(0.45 + strength[d$home] - strength[d$away])),
    away_score = stats::rpois(nrow(d), exp(0.25 + strength[d$away] - strength[d$home])),
    season = d$season, cell = paste0("L@", d$season)
  )
}

toy_draws <- function(spec, n_draw = 3L, seed = 5L) {
  set.seed(seed)
  list(
    c_home = stats::rnorm(n_draw, 0.5, 0.05), c_away = stats::rnorm(n_draw, 0.3, 0.05),
    att = matrix(stats::rnorm(n_draw * spec$S, 0, 0.3), n_draw),
    def = matrix(stats::rnorm(n_draw * spec$S, 0, 0.3), n_draw),
    ha_att = matrix(stats::rnorm(n_draw * spec$K, 0, 0.05), n_draw),
    ha_def = matrix(stats::rnorm(n_draw * spec$K, 0, 0.05), n_draw)
  )
}

# What prepare_data() hands over for the toy league, with teams numbered in a
# different order from the spec's so the adapter has to map them.
toy_prep <- function() {
  m <- toy_matches()
  teams <- data.frame(team = c("C", "A", "B"), team_nr = 1:3)
  nr <- function(x) match(x, teams$team)
  appearance <- function(team, i) sum(c(m$home_team[seq_len(i)], m$away_team[seq_len(i)]) == team)
  list(
    teams = teams,
    stan_data = list(
      N = nrow(m), N_rounds = 5L,
      team1 = nr(m$home_team), team2 = nr(m$away_team),
      round1 = vapply(seq_len(nrow(m)), function(i) appearance(m$home_team[i], i), integer(1)),
      round2 = vapply(seq_len(nrow(m)), function(i) appearance(m$away_team[i], i), integer(1)),
      goals1 = m$home_score, goals2 = m$away_score
    ),
    pred_d = data.frame(
      game_nr = 1L, match_date = as.Date("2022-05-18"), home_team = "C", away_team = "A",
      division = "L"
    )
  )
}

# What prepare_data() hands over for any small league: the team registry in a
# chosen order, the Stan data the adapter reads, and the fixtures.
prep_like <- function(m, pred_d, team_order = sort(unique(c(m$home_team, m$away_team)))) {
  teams <- data.frame(team = team_order, team_nr = seq_along(team_order))
  nr <- function(x) match(x, teams$team)
  appearance <- function(team, i) sum(c(m$home_team[seq_len(i)], m$away_team[seq_len(i)]) == team)
  round1 <- vapply(seq_len(nrow(m)), function(i) appearance(m$home_team[i], i), integer(1))
  round2 <- vapply(seq_len(nrow(m)), function(i) appearance(m$away_team[i], i), integer(1))
  list(
    teams = teams,
    stan_data = list(
      N = nrow(m), N_rounds = max(round1, round2),
      team1 = nr(m$home_team), team2 = nr(m$away_team), round1 = round1, round2 = round2,
      goals1 = m$home_score, goals2 = m$away_score
    ),
    pred_d = pred_d
  )
}

# The goal rates production's simulators rebuild for one match from a fit's
# current strengths (R/extract-football-iceland.R), one value per draw.
simulator_rates <- function(fit, teams, home, away) {
  get <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
  h <- match(home, teams$team)
  a <- match(away, teams$team)
  mlg <- as.numeric(get("mean_log_goals"))
  off <- get("cur_offense_away")
  def <- get("cur_defense_away")
  list(
    home = as.numeric(exp(mlg + off[, h] + get("home_advantage_off")[, h] - def[, a])),
    away = as.numeric(exp(mlg + off[, a] - def[, h] - get("home_advantage_def")[, h]))
  )
}

# A football store under `root` (results for `n_season` seasons and one round
# of fixtures the week after the last result) with the league that reads it.
# `preseason = TRUE` puts the fixtures in the next season instead: one inside
# the 14-day horizon, the other teams' first matches four weeks later.
v2_store <- function(root, n_team = 6L, n_season = 3L, weeks = 10L, seed = 3L, preseason = FALSE) {
  set.seed(seed)
  teams <- paste0("T", seq_len(n_team))
  strength <- stats::rnorm(n_team, 0, 0.4)
  rounds <- expand.grid(week = seq_len(weeks), season = 2020L + seq_len(n_season))
  d <- do.call(rbind, lapply(seq_len(nrow(rounds)), function(i) {
    pair <- matrix(sample(n_team), ncol = 2)
    data.frame(
      season = rounds$season[i], round = rounds$week[i],
      match_date = as.Date(sprintf("%d-05-03", rounds$season[i])) + 7L * (rounds$week[i] - 1L),
      home = pair[, 1], away = pair[, 2]
    )
  }))
  results <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    season = as.integer(d$season), match_date = d$match_date,
    home_team = teams[d$home], away_team = teams[d$away],
    home_score = stats::rpois(nrow(d), exp(0.45 + strength[d$home] - strength[d$away])),
    away_score = stats::rpois(nrow(d), exp(0.25 + strength[d$away] - strength[d$home])),
    division = "BD", round = as.integer(d$round)
  )
  last <- max(results$match_date)
  schedules <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    season = max(results$season), match_date = last + 7L,
    home_team = teams[c(1L, 3L, 5L)], away_team = teams[c(2L, 4L, 6L)],
    division = "BD", round = weeks + 1L, kickoff_time = NA_character_
  )
  end_date <- last + 1L
  n_pred <- nrow(schedules)
  if (preseason) {
    opening <- as.Date(sprintf("%d-05-03", max(results$season) + 1L))
    schedules$season <- max(results$season) + 1L
    schedules$match_date <- opening + c(0L, 28L, 28L)
    schedules$round <- 1L
    end_date <- opening - 7L
    n_pred <- 1L
  }
  write_table(results, "results", root = root)
  write_table(schedules, "schedules", root = root)
  list(
    league = list(
      sport = "football", country = "iceland", sexes = "male",
      stan_model = "football_iceland/bivariate_poisson_no_inflation.stan"
    ),
    end_date = end_date, n_obs = nrow(results), n_pred = n_pred, teams = teams
  )
}

# What fv2_fit_inla() returns, as far as the gate reads it: draws that keep
# the entry-level constraint, a hyperparameter matrix and the mode status.
gate_fit <- function(spec, n_draw = 4L) {
  d <- toy_draws(spec, n_draw)
  centre <- function(x) {
    x[, spec$first_slot] <- x[, spec$first_slot] - rowMeans(x[, spec$first_slot])
    x
  }
  d$att <- centre(d$att)
  d$def <- centre(d$def)
  d$hyper <- matrix(
    rep(c(0.4, 0.05), each = n_draw), n_draw,
    dimnames = list(NULL, c("tau_att", "sigma_att"))
  )
  list(draws = d, mode_status = 0L)
}

# What the stored NUTS reference records about a fit, from draws in the
# engine-neutral layout (test-model-v2-reference.R and
# research/football-v2/make_reference.R share it, so the reference and the
# check are computed the same way): outcome probabilities and expected goals of
# every fixture, the hyperparameters, each team's current attack and defence
# state, and the league home advantage in the first fixture week.
reference_summary <- function(draws, spec, max_goals = 12L) {
  pr <- fv2_predict(draws, spec)
  g <- 0:max_goals
  probs <- t(vapply(seq_len(nrow(pr$lam_home)), function(i) {
    ph <- outer(g, pr$lam_home[i, ], stats::dpois)
    pa <- outer(g, pr$lam_away[i, ], stats::dpois)
    grid <- tcrossprod(ph, pa) / ncol(ph)
    grid <- grid / sum(grid)
    c(home = sum(grid[lower.tri(grid)]), draw = sum(diag(grid)), away = sum(grid[upper.tri(grid)]))
  }, numeric(3)))
  state <- function(x) {
    x <- x[, spec$cur_slot, drop = FALSE]
    data.frame(team = spec$teams, mean = colMeans(x), sd = apply(x, 2L, stats::sd), row.names = NULL)
  }
  h <- draws$hyper
  q <- function(p) apply(h, 2L, stats::quantile, p, names = FALSE)
  ha <- draws$c_home - draws$c_away + if (is.null(draws$lvl_ha)) 0 else draws$lvl_ha[, spec$cur_week_nr]
  list(
    probs = probs,
    xg = cbind(home = rowMeans(pr$lam_home), away = rowMeans(pr$lam_away)),
    hyper = data.frame(name = colnames(h), mean = colMeans(h), q05 = q(0.05), q50 = q(0.5), q95 = q(0.95), row.names = NULL),
    att = state(draws$att), def = state(draws$def),
    ha = c(mean = mean(ha), sd = stats::sd(ha))
  )
}

# Why a stored reference no longer belongs to the code: the names of what
# changed since it was made (empty when it still belongs).
reference_staleness <- function(ref, stan_file) {
  c(
    if (!identical(ref$opts, football_v2_options())) "football_v2_options()",
    if (!identical(ref$priors, fv2_priors())) "fv2_priors()",
    if (!file.exists(stan_file)) "the Stan reference program (missing)"
    else if (!identical(unname(tools::md5sum(stan_file)), ref$provenance$stan_md5)) "the Stan reference program"
  )
}

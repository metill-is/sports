# Football model v2: present the new engine's draws as the fit object
# the existing extract, publish and simulation layers already read.
#
# Those layers touch a fit through one method, fit$draws(name), and expect the
# production Stan program's variable names. Mapping the new parameters onto
# those names lets the INLA engine feed them unchanged:
#
#   mean_log_goals        = (c_home + c_away) / 2  (+ the current scoring level)
#   home_advantage_off[k] = (c_home - c_away) / 2 + ha_att[k]
#   home_advantage_def[k] = (c_home - c_away) / 2 + ha_def[k]
#                           (each + half the current league home-advantage level)
#   cur_offense_away[k]   = attack state at team k's latest match week
#   offense[r, k]         = attack state in the week of team k's r-th match
#   alpha_mu3             = a large negative number (no shared-rate term)
#   goals1_pred[n]        ~ Poisson(home rate of fixture n), per draw
#
# The league home advantage is split evenly between attack and defence. Only
# its total is identified; the even split is a presentation convention.

FV2_NO_SHARED_RATE <- -40

#' Fixtures from `prepare_data()$pred_d` in the layout `fv2_build()` reads.
#' @noRd
fv2_pred_from_prep <- function(pred_d, cell_of = function(division, season) paste(division, season, sep = "@")) {
  season <- as.integer(format(pred_d$match_date, "%Y"))
  unplayed <- rep(NA_integer_, nrow(pred_d))
  data.frame(
    match_date = pred_d$match_date, home_team = pred_d$home_team, away_team = pred_d$away_team,
    home_score = unplayed, away_score = unplayed, season = season,
    cell = cell_of(pred_d$division, season)
  )
}

#' A cmdstanr-fit-like object over engine-neutral draws.
#'
#' @param draws Posterior draws from `fv2_fit_inla()` or `fv2_fit_stan()`.
#' @param spec The spec they were fitted on; its fixtures must be
#'   `prep$pred_d` in `game_nr` order and its training rows the rows behind
#'   `prep$stan_data`, in the same order.
#' @param prep `prepare_data()` output: `stan_data`, `pred_d`, `teams`.
#' @return `list(draws = function(variables))`, returning a
#'   `posterior::draws_array`.
#' @noRd
fv2_as_fit <- function(draws, spec, prep, seed = 1L) {
  sd <- prep$stan_data
  K <- nrow(prep$teams)
  n_draw <- length(draws$c_home)
  team_ix <- match(prep$teams$team, spec$teams)
  stopifnot(!anyNA(team_ix), spec$K == K)
  stopifnot(
    identical(team_ix[sd$team1], spec$obs$home_team),
    identical(team_ix[sd$team2], spec$obs$away_team),
    identical(as.integer(sd$goals1), spec$obs$goals_home),
    length(spec$pred$home_slot) == nrow(prep$pred_d)
  )

  indexed <- function(m, name) {
    colnames(m) <- paste0(name, "[", seq_len(ncol(m)), "]")
    m
  }
  scalar <- function(x, name) matrix(x, ncol = 1L, dimnames = list(NULL, name))

  league_ha <- draws$c_home - draws$c_away
  level_goal <- 0
  if (!is.null(draws$lvl_goal)) {
    # WHY: the simulators hold strengths at their current value, so they get
    # the league-wide levels of the latest week with a played match too.
    latest <- max(spec$obs$week_nr)
    level_goal <- draws$lvl_goal[, latest]
    league_ha <- league_ha + draws$lvl_ha[, latest]
  }
  ha_off <- league_ha / 2 + draws$ha_att[, team_ix, drop = FALSE]
  ha_def <- league_ha / 2 + draws$ha_def[, team_ix, drop = FALSE]
  now <- spec$last_obs_slot[team_ix]
  off <- draws$att[, now, drop = FALSE]
  def <- draws$def[, now, drop = FALSE]

  n_round <- sd$N_rounds
  slot_at <- matrix(NA_integer_, n_round, K)
  slot_at[cbind(sd$round1, sd$team1)] <- spec$obs$home_slot
  slot_at[cbind(sd$round2, sd$team2)] <- spec$obs$away_slot
  for (k in seq_len(K)) {
    played <- which(!is.na(slot_at[, k]))
    latest <- pmax(findInterval(seq_len(n_round), played), 1L)
    slot_at[, k] <- slot_at[played[latest], k]
  }
  grid <- expand.grid(round = seq_len(n_round), team = seq_len(K))
  walk <- function(states, name) {
    m <- states[, slot_at[cbind(grid$round, grid$team)], drop = FALSE]
    colnames(m) <- sprintf("%s[%d,%d]", name, grid$round, grid$team)
    m
  }

  vars <- list(
    mean_log_goals = scalar((draws$c_home + draws$c_away) / 2 + level_goal, "mean_log_goals"),
    alpha_mu3 = scalar(rep(FV2_NO_SHARED_RATE, n_draw), "alpha_mu3"),
    beta_mu3_strength_diff = scalar(rep(0, n_draw), "beta_mu3_strength_diff"),
    home_advantage_off = indexed(ha_off, "home_advantage_off"),
    home_advantage_def = indexed(ha_def, "home_advantage_def"),
    home_advantage_tot = indexed(ha_off + ha_def, "home_advantage_tot"),
    cur_offense_away = indexed(off, "cur_offense_away"),
    cur_defense_away = indexed(def, "cur_defense_away"),
    cur_strength_away = indexed(off + def, "cur_strength_away"),
    cur_offense_home = indexed(off + ha_off, "cur_offense_home"),
    cur_defense_home = indexed(def + ha_def, "cur_defense_home"),
    cur_strength_home = indexed(off + ha_off + def + ha_def, "cur_strength_home"),
    offense = walk(draws$att, "offense"),
    defense = walk(draws$def, "defense"),
    lp__ = scalar(rep(0, n_draw), "lp__")
  )
  if (nrow(prep$pred_d)) {
    rates <- fv2_predict(draws, spec)
    set.seed(seed)
    count <- function(lam) matrix(stats::rpois(length(lam), t(lam)), n_draw)
    vars$goals1_pred <- indexed(count(rates$lam_home), "goals1_pred")
    vars$goals2_pred <- indexed(count(rates$lam_away), "goals2_pred")
  }

  list(
    draws = function(variables = NULL, ...) {
      want <- if (is.null(variables)) names(vars) else as.character(variables)
      absent <- setdiff(want, names(vars))
      if (length(absent)) {
        stop("no draws for variable(s): ", paste(absent, collapse = ", "), call. = FALSE)
      }
      posterior::as_draws_array(posterior::as_draws_matrix(do.call(cbind, vars[want])))
    },
    variables = names(vars),
    # WHY: fit_league() saves every fit with fit$save_object(); for this
    # engine the draws under their production names are the whole fit.
    save_object = function(file) saveRDS(vars, file)
  )
}

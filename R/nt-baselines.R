#' @include nt-data.R
NULL

# Baselines the national-team model is scored against: World Football Elo with
# an ordered-logit outcome link (plus a Poisson link for scorelines), and the
# 2026 production approach (club bivariate-Poisson model on internationals).

.nt_elo_k <- function(tournament) {
  continental <- c(
    "UEFA Euro", "Copa Am\u00e9rica", "African Cup of Nations", "AFC Asian Cup",
    "Gold Cup", "Confederations Cup", "Oceania Nations Cup",
    "CONCACAF Championship", "Africa Cup of Nations", "Olympic Games"
  )
  ifelse(grepl("qualification", tournament, ignore.case = TRUE), 40,
    ifelse(tournament == "FIFA World Cup", 60,
      ifelse(tournament %in% continental, 50,
        ifelse(tournament == "Friendly", 20, 30)
      )
    )
  )
}

#' World Football Elo ratings from a results history.
#'
#' K is 60 for World Cup finals, 50 for continental finals, 40 for qualifiers,
#' 20 for friendlies and 30 otherwise, scaled by the goal-difference multiplier;
#' the home side gets `home_adv` points unless the venue is neutral.
#'
#' @param results Played matches in the [nt_read_results()] schema.
#' @param home_adv Rating points added to a home side at a non-neutral venue.
#' @return List: `ratings` (named vector after the last match) and `pre_diff`
#'   (per row, the home-minus-away rating difference before the match,
#'   including the venue bonus).
#' @export
nt_elo <- function(results, home_adv = 100) {
  results <- results[order(results$match_date), , drop = FALSE]
  teams <- unique(c(results$home_team, results$away_team))
  rating <- stats::setNames(rep(1500, length(teams)), teams)
  k <- .nt_elo_k(results$tournament)
  h <- results$home_team
  a <- results$away_team
  hs <- results$home_score
  as_ <- results$away_score
  bonus <- ifelse(results$neutral, 0, home_adv)
  pre <- numeric(nrow(results))
  for (i in seq_len(nrow(results))) {
    dr <- rating[[h[i]]] - rating[[a[i]]] + bonus[i]
    pre[i] <- dr
    if (is.na(hs[i]) || is.na(as_[i])) next
    gd <- abs(hs[i] - as_[i])
    g <- if (gd <= 1) 1 else if (gd == 2) 1.5 else 1.75 + (gd - 3) / 8
    w <- if (hs[i] > as_[i]) 1 else if (hs[i] == as_[i]) 0.5 else 0
    step <- k[i] * g * (w - 1 / (1 + 10^(-dr / 400)))
    rating[[h[i]]] <- rating[[h[i]]] + step
    rating[[a[i]]] <- rating[[a[i]]] - step
  }
  list(ratings = rating, pre_diff = pre)
}

.nt_ordered_logit <- function(x, outcome) {
  nll <- function(par) {
    c1 <- par[2]
    c2 <- par[2] + exp(par[3])
    p_le_a <- stats::plogis(c1 - par[1] * x)
    p_le_d <- stats::plogis(c2 - par[1] * x)
    p <- ifelse(outcome == 1L, p_le_a,
      ifelse(outcome == 2L, p_le_d - p_le_a, 1 - p_le_d)
    )
    -sum(log(pmax(p, 1e-12)))
  }
  opt <- stats::optim(c(0.5, -0.5, 0), nll, method = "BFGS")
  list(slope = opt$par[1], cut_away = opt$par[2], cut_draw = opt$par[2] + exp(opt$par[3]))
}

#' Elo baseline forecasts for a set of fixtures.
#'
#' Ratings use every match before `cutoff`. The outcome link is an ordered
#' logit of away/draw/home on the rating difference, and the scoreline link two
#' Poisson regressions of home and away goals on it, both fitted on matches from
#' `link_start` to `cutoff`. Nothing after `cutoff` is used.
#'
#' @param results Output of [nt_read_results()].
#' @param cutoff Date; matches on or after it are ignored.
#' @param home,away Fixture team names.
#' @param venue 1 when `home` plays in its own country, else 0. A neutral
#'   fixture's forecast averages the fixture and its mirror image, so it does
#'   not depend on which side is listed first.
#' @param link_start First match date used to fit the two links.
#' @param max_goals Largest goal count per side in the scoreline grid.
#' @return List: `probs` (tibble `p_home, p_draw, p_away, eg_home, eg_away`)
#'   and `grid` (scoreline probabilities, fixtures x goals x goals).
#' @export
nt_elo_baseline <- function(results, cutoff, home, away, venue,
                            link_start = as.Date("2006-01-01"),
                            max_goals = 12L) {
  past <- results[
    results$match_date < cutoff &
      !is.na(results$home_score) & !is.na(results$away_score), ,
    drop = FALSE
  ]
  past <- past[order(past$match_date), , drop = FALSE]
  elo <- nt_elo(past)
  train <- past$match_date >= link_start
  x <- elo$pre_diff[train] / 100
  hs <- past$home_score[train]
  as_ <- past$away_score[train]
  outcome <- ifelse(hs > as_, 3L, ifelse(hs == as_, 2L, 1L))
  ol <- .nt_ordered_logit(x, outcome)
  fit_h <- stats::glm(hs ~ x, family = stats::poisson())
  fit_a <- stats::glm(as_ ~ x, family = stats::poisson())

  unknown <- setdiff(c(home, away), names(elo$ratings))
  if (length(unknown) > 0L) {
    cli::cli_abort("Team{?s} with no match before the cutoff: {.val {unknown}}.")
  }
  venue <- rep_len(venue, length(home))
  x_new <- (elo$ratings[home] - elo$ratings[away] + 100 * venue) / 100
  x_new <- unname(x_new)
  g <- 0:max_goals
  forecast <- function(x) {
    p_le_a <- stats::plogis(ol$cut_away - ol$slope * x)
    p_le_d <- stats::plogis(ol$cut_draw - ol$slope * x)
    lam_h <- unname(exp(stats::coef(fit_h)[1] + stats::coef(fit_h)[2] * x))
    lam_a <- unname(exp(stats::coef(fit_a)[1] + stats::coef(fit_a)[2] * x))
    grid <- array(0, dim = c(length(x), max_goals + 1L, max_goals + 1L))
    for (m in seq_along(x)) {
      grid[m, , ] <- outer(stats::dpois(g, lam_h[m]), stats::dpois(g, lam_a[m]))
    }
    list(
      p = cbind(1 - p_le_d, p_le_d - p_le_a, p_le_a), eg = cbind(lam_h, lam_a),
      grid = grid
    )
  }
  f <- forecast(x_new)
  # WHY: the links' intercepts absorb home advantage beyond the Elo bonus,
  # which would favour whichever side a neutral fixture lists first. A
  # neutral forecast is the average of the fixture and its mirror image.
  neutral <- which(venue == 0)
  if (length(neutral) > 0L) {
    m <- forecast(-x_new[neutral])
    f$p[neutral, ] <- (f$p[neutral, , drop = FALSE] + m$p[, 3:1, drop = FALSE]) / 2
    f$eg[neutral, ] <- (f$eg[neutral, , drop = FALSE] + m$eg[, 2:1, drop = FALSE]) / 2
    f$grid[neutral, , ] <- (f$grid[neutral, , , drop = FALSE] +
      aperm(m$grid, c(1L, 3L, 2L))) / 2
  }
  list(
    probs = tibble::tibble(
      p_home = f$p[, 1], p_draw = f$p[, 2], p_away = f$p[, 3],
      eg_home = f$eg[, 1], eg_away = f$eg[, 2]
    ),
    grid = f$grid,
    ratings = elo$ratings
  )
}

#' Per-draw rates of the production bivariate-Poisson model for fixtures.
#'
#' Vectorised twin of the production simulator's match primitive: the home
#' advantage of the home side is applied only when `venue` is 1, as the
#' production forecast did for host nations.
#'
#' @param sim_inputs The `list(team, scalar)` of per-draw production parameters.
#' @param home,away Fixture team names.
#' @param venue 1 when `home` plays in its own country, else 0.
#' @return List of draws x fixtures matrices `home`, `away` (the two
#'   independent components) and `shared` (the common component).
#' @export
nt_production_rates <- function(sim_inputs, home, away, venue) {
  tm <- sim_inputs$team
  sc <- sim_inputs$scalar[order(sim_inputs$scalar$.draw), , drop = FALSE]
  draws <- sc$.draw
  wide <- function(col) {
    teams <- unique(tm$team)
    out <- matrix(NA_real_, length(draws), length(teams), dimnames = list(NULL, teams))
    out[cbind(match(tm$.draw, draws), match(tm$team, teams))] <- tm[[col]]
    out
  }
  off <- wide("cur_offense")
  def <- wide("cur_defense")
  ha_off <- wide("home_advantage_off")
  ha_def <- wide("home_advantage_def")
  unknown <- setdiff(c(home, away), colnames(off))
  if (length(unknown) > 0L) {
    cli::cli_abort("Team{?s} not in the production fit: {.val {unknown}}.")
  }
  v <- matrix(venue, length(draws), length(home), byrow = TRUE)
  off_h <- off[, home, drop = FALSE] + v * ha_off[, home, drop = FALSE]
  def_h <- def[, home, drop = FALSE] + v * ha_def[, home, drop = FALSE]
  off_a <- off[, away, drop = FALSE]
  def_a <- def[, away, drop = FALSE]
  mu_h <- sc$mean_log_goals + off_h - def_a
  mu_a <- sc$mean_log_goals + off_a - def_h
  gap <- abs(off_h + def_h - off_a - def_a)
  mu3 <- stats::plogis(sc$alpha_mu3 + sc$beta_mu3_strength_diff * gap, log.p = TRUE) +
    0.5 * (mu_h + mu_a)
  list(home = exp(mu_h), away = exp(mu_a), shared = exp(mu3))
}

#' Scoreline probabilities of a bivariate Poisson, averaged over draws.
#'
#' @param rates Output of [nt_production_rates()].
#' @param max_goals Largest goal count kept per side.
#' @return Array fixtures x (max_goals + 1) x (max_goals + 1).
#' @export
nt_bivariate_scoreline_probs <- function(rates, max_goals = 12L) {
  n_fix <- ncol(rates$home)
  n_draw <- nrow(rates$home)
  g <- 0:max_goals
  n_g <- max_goals + 1L
  out <- array(0, dim = c(n_fix, n_g, n_g))
  for (m in seq_len(n_fix)) {
    ph <- outer(rates$home[, m], g, function(l, x) stats::dpois(x, l))
    pa <- outer(rates$away[, m], g, function(l, x) stats::dpois(x, l))
    ps <- outer(rates$shared[, m], g, function(l, x) stats::dpois(x, l))
    acc <- matrix(0, n_g, n_g)
    for (k in g) {
      keep <- seq_len(n_g - k)
      part <- crossprod(ph[, keep, drop = FALSE] * ps[, k + 1L], pa[, keep, drop = FALSE])
      acc[keep + k, keep + k] <- acc[keep + k, keep + k] + part
    }
    out[m, , ] <- acc / n_draw
  }
  out
}

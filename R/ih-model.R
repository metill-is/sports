#' @include v2-margin-total-inla.R
NULL

# International handball: the handball v2 (margin, total) model on R-INLA with
# nations as teams. A nation's state in a season is
# b[nation] + u[nation, season] (club level and AR(1) season level, as for the
# Iceland leagues; strength constant within a season), one state for the
# margin model and one for the total model. The margin model's home advantage
# enters with each match's venue (R/ih-data.R): most championship matches are
# on neutral ground. The model trains on the score after 60 minutes, so a
# level score is a draw and extra time is simulated on top (R/ih-simulate.R).

#' Prior scales for the nations model
#'
#' The handball v2 priors with wider bounds on the spread of strengths:
#' nations differ far more than the clubs of one league (a margin of 30 goals
#' between a top nation and a newcomer is not rare), so the club level's 1 %
#' bound is 30 goals instead of 10 and the season level's 12 instead of 8.
#' @return Named list, see [v2_priors()].
#' @noRd
ih_priors <- function() {
  utils::modifyList(v2_priors("handball"), list(u_b = 30, u_u = 12))
}

#' Fit the nations model and draw every nation's state for one season
#'
#' @param matches Played matches of one sex from [ih_read_matches()].
#' @param fixtures Matches to predict: `match_date`, `home_team`,
#'   `away_team`, `venue` (see [ih_venue()]). May have zero rows when
#'   `season` is given.
#' @param as_of Only matches dated before this day train the model.
#' @param season Season whose states are drawn; defaults to the season of the
#'   first fixture, else of `as_of`.
#' @param from_season First season that trains the model (default: all).
#' @param n_draws Joint posterior draws.
#' @param seed Seed of the posterior draws.
#' @param int_strategy R-INLA hyperparameter integration (`"auto"` integrates;
#'   `"eb"` is the plug-in mode, for tests).
#' @return List: `teams`; per draw (rows) `m_state`, `t_state` (draws by
#'   nations), `level`, `hfa`, `ha` (draws by nations), `sigma_m`, `sigma_t`;
#'   `fixtures` with `loc_m` and `loc_t` (draws by fixtures, as R-INLA's
#'   predictor gives them); `hyper_sd`, `fixed`, `mode_status` (margin,
#'   total), `n_train`, `n_train_60` (rows in the likelihood), `as_of`,
#'   `season`.
#' @noRd
ih_fit <- function(matches, fixtures, as_of, season = NULL, from_season = NULL,
                   n_draws = 4000L, seed = 1L, int_strategy = "auto") {
  as_of <- as.Date(as_of)
  train <- matches[matches$match_date < as_of & !is.na(matches$home_score) & !is.na(matches$away_score), , drop = FALSE]
  if (!is.null(from_season)) train <- train[train$season >= from_season, , drop = FALSE]
  fixtures <- as.data.frame(fixtures)
  if (is.null(season)) {
    season <- if (nrow(fixtures)) ih_season(min(fixtures$match_date)) else ih_season(as_of)
  }
  pred <- data.frame(
    match_date = as.Date(fixtures$match_date), season = ih_season(fixtures$match_date),
    division = rep("intl", nrow(fixtures)), home_team = fixtures$home_team, away_team = fixtures$away_team,
    stringsAsFactors = FALSE
  )
  unknown <- setdiff(c(pred$home_team, pred$away_team), c(train$home_team, train$away_team))
  if (length(unknown)) {
    cli::cli_abort("No training match before {as_of} for {.val {unknown}}.")
  }
  # WHY a carrier row: the season whose states are drawn must exist on the
  # season grid even when no fixture is in it; an unobserved row adds nothing
  # to the likelihood.
  if (!season %in% c(train$season, pred$season)) {
    pred <- rbind(pred, data.frame(
      match_date = as.Date(sprintf("%d-07-01", season)), season = season, division = "intl",
      home_team = train$home_team[1], away_team = train$away_team[1], stringsAsFactors = FALSE
    ))
  }
  teams <- sort(unique(c(train$home_team, train$away_team)))
  v <- build_v2_data(train, pred, teams)
  s_cur <- match(season, v$seasons)
  venue <- c(train$venue, fixtures$venue, rep(0L, nrow(pred) - nrow(fixtures)))
  y60 <- list(
    margin = train$home_60 - train$away_60,
    total = train$home_60 + train$away_60
  )
  cur <- (seq_len(v$K) - 1L) * v$NS + s_cur
  latent <- list(
    margin = list(b = 0L, u_s = cur, ha = 0L, hfa = 0L),
    total = list(b = 0L, u_s = cur, s_s = s_cur, alpha1 = 0L)
  )
  priors <- ih_priors()
  fits <- list()
  for (resp in c("margin", "total")) {
    fits[[resp]] <- fit_v2_inla(
      v, resp, priors,
      within = "none", int_strategy = int_strategy, n_samples = as.integer(n_draws),
      y = y60[[resp]], seed = as.integer(seed) + (resp == "total"), latent = latent[[resp]],
      venue = if (resp == "margin") venue else NULL
    )
  }
  sm <- fits$margin$samples
  st <- hbbb_v2_shuffle_draws(fits$total$samples, as.integer(seed) + 2L)
  if (!is.null(st$loc)) st$loc <- st$loc[withr::with_seed(as.integer(seed) + 2L, sample.int(nrow(st$loc))), , drop = FALSE]
  lm <- sm$latent
  lt <- st$latent
  m_state <- lm$b + lm$u_s[, as.character(cur), drop = FALSE]
  t_state <- lt$b + lt$u_s[, as.character(cur), drop = FALSE]
  colnames(m_state) <- colnames(t_state) <- teams
  ha <- lm$ha
  colnames(ha) <- teams
  nf <- nrow(fixtures)
  out <- list(
    teams = teams,
    m_state = m_state, t_state = t_state,
    level = as.vector(lt$alpha1) + as.vector(lt$s_s),
    hfa = as.vector(lm$hfa), ha = ha,
    sigma_m = sm$sigma, sigma_t = st$sigma,
    fixtures = fixtures,
    loc_m = if (nf) sm$loc[, seq_len(nf), drop = FALSE] else NULL,
    loc_t = if (nf) st$loc[, seq_len(nf), drop = FALSE] else NULL,
    hyper_sd = rbind(
      cbind(model = "margin", fits$margin$hyper_sd),
      cbind(model = "total", fits$total$hyper_sd)
    ),
    fixed = list(margin = fits$margin$fixed, total = fits$total$fixed),
    mode_status = c(margin = fits$margin$mode_status, total = fits$total$mode_status),
    n_train = nrow(train), n_train_60 = sum(!is.na(y60$margin)),
    as_of = as_of, season = season
  )
  class(out) <- "ih_fit"
  out
}

#' Margin and total locations of any pairing, per draw
#'
#' @param fit [ih_fit()] result.
#' @param home,away Nation names (vectors of one length, or matrices of
#'   draws by matches holding one pairing per draw).
#' @param venue Venue of each match from the home side's point of view, as
#'   in [ih_venue()]; recycled like `home`.
#' @param draws Rows of the fit's draws to use (default all).
#' @return list(loc_m, loc_t, sigma_m, sigma_t): draws-by-matches matrices and
#'   the per-draw sds.
#' @noRd
ih_locations <- function(fit, home, away, venue = 0L, draws = seq_len(nrow(fit$m_state))) {
  nd <- length(draws)
  as_draw_matrix <- function(x) if (is.matrix(x)) x else matrix(x, nd, length(x), byrow = TRUE)
  h <- as_draw_matrix(match(home, fit$teams))
  a <- as_draw_matrix(match(away, fit$teams))
  if (anyNA(h) || anyNA(a)) {
    bad <- unique(c(home[is.na(match(home, fit$teams))], away[is.na(match(away, fit$teams))]))
    cli::cli_abort("Not in the fit: {.val {bad}}.")
  }
  if (!is.matrix(venue)) venue <- matrix(rep_len(as.numeric(venue), ncol(h)), nd, ncol(h), byrow = TRUE)
  row <- matrix(draws, nd, ncol(h))
  host <- ifelse(venue < 0, a, h)
  pick <- function(m, idx) matrix(m[cbind(as.vector(row), as.vector(idx))], nd, ncol(h))
  loc_m <- pick(fit$m_state, h) - pick(fit$m_state, a) + venue * (fit$hfa[draws] + pick(fit$ha, host))
  loc_t <- fit$level[draws] + pick(fit$t_state, h) + pick(fit$t_state, a)
  list(loc_m = loc_m, loc_t = loc_t, sigma_m = fit$sigma_m[draws], sigma_t = fit$sigma_t[draws])
}

#' Home / draw / away probabilities after 60 minutes and expected goals
#'
#' @param fit [ih_fit()] result.
#' @param home,away,venue Pairings, see [ih_locations()].
#' @return data.frame: `p_home`, `p_draw`, `p_away` (the margin rounded, a
#'   level score is a draw), `eg_home`, `eg_away`, `mean_margin`,
#'   `mean_total`.
#' @noRd
ih_match_probs <- function(fit, home, away, venue = 0L) {
  loc <- ih_locations(fit, home, away, venue)
  p <- v2_outcome_probs(loc$loc_m, loc$sigma_m)
  mm <- colMeans(loc$loc_m)
  mt <- colMeans(loc$loc_t)
  data.frame(
    p_home = p$p_home, p_draw = p$p_draw, p_away = p$p_away,
    eg_home = (mt + mm) / 2, eg_away = (mt - mm) / 2,
    mean_margin = mm, mean_total = mt
  )
}

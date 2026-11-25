# Handball and basketball v2 (margin, total) on the production fit path.
# Reached only when a league's config says `engine: inla`; no league does, and
# the default engine is Stan. The model, its evidence and the cutover plan are
# in docs/reports/2026-11-15-hb-bb-v2/report.md ("Recommended model", "What
# changes, layer by layer").
#
# The extract, publish and season-simulation layers read a fit through
# fit$draws(name) under the production Stan program's variable names. This
# file fits the two halves of v2 with R-INLA, draws the latent components
# jointly, composes each team's state from them, and presents the result under
# those names:
#
#   net   = b[team] + u[team, season] (+ w[team-season, week], basketball)
#   pace  = the same terms of the total model + level[division, season] / 2
#   ha    = hfa + ha[team]
#   cur_offense_away[k] = (net + pace - L) / 2 at team k's next match,
#   cur_defense_away[k] = (net - pace + L) / 2,  L = mean over teams of pace
#   home_advantage_off = home_advantage_def = ha / 2
#   offense[r, k]       = the same rotation at team k's r-th training match
#   goals1_pred[n]      = whole-number scores from the v2 predictive
#                         (basketball: regulation time, then overtime)
#
# `R/v2-margin-total-adapter.R` holds the algebra (v2_as_attack_defence());
# `R/simulate-2dt.R` reads the scalars this adapter sets so that its draw is
# the v2 Gaussian (one score sd, one correlation, a very large nu).

#' Version of the handball / basketball model the INLA engine fits.
#' @noRd
HBBB_V2_VERSION <- "2.0"

#' Student-t degrees of freedom handed to the season simulator, which draws a
#' bivariate t: large enough that its draw is the v2 Gaussian (the mixing
#' variable has sd about 1 / sqrt(2 nu), here 0.0007).
#' @noRd
HBBB_V2_NU <- 1e6

#' Fewest overtime games in the training rows from which overtime points are
#' taken; with fewer, a five-minute share of the regulation total is used.
#' @noRd
HBBB_V2_OT_MIN_GAMES <- 20L

#' Share of the latest training season's basketball rows that must carry
#' regulation-time scores. Below it the fit would forecast the season from
#' too few of its own matches (the daily ingest fills a new game's scores the
#' next morning, at most 200 cards a run).
#' @noRd
HBBB_V2_MIN_REG_COVERAGE <- 0.9

#' Sanity ranges of the INLA gate, per sport, in goals or points: the league
#' home advantage, the two observation sds, the mean predicted total and each
#' model's team spread (`hbbb_v2_hyper_sd()`'s "sd: team") and, basketball
#' only, the within-season sd of each model (well identified: five to seven
#' points in the report, and the composite spread would hide its loss). Wide on purpose:
#' they catch a fit that has gone wrong, not a moved league. The team floor is
#' a twentieth of the club level's prior bound; R-INLA's degenerate mode with
#' the team effects switched off sits near its starting sd of 0.14 per
#' component.
#' @noRd
HBBB_V2_SANITY <- list(
  handball = list(hfa = c(-2, 4), sigma = c(2, 15), total = c(30, 80), team = c(0.5, 15)),
  basketball = list(
    hfa = c(-5, 10), sigma = c(4, 30), total = c(100, 230), team = c(1.25, 40),
    within = c(0.5, 40)
  )
)

#' Fewest latest-season basketball rows without regulation-time scores that
#' can fail the coverage check: early in a season one failed game card would
#' otherwise take a few rows below the share and stop the fit.
#' @noRd
HBBB_V2_MIN_REG_MISSING <- 4L

#' The hyperparameter sds the gate's jump check compares with the previous
#' fit: the observation sds and each model's team spread. The split of that
#' spread between club, season and week, the team home-advantage sd and the
#' division level's walk are weakly identified (one men's handball total fit
#' put its club sd at 0.57 with a 90 % interval from 0.003 to 2.3), so they
#' are stored but not gated.
#' @noRd
HBBB_V2_JUMP_CHECKED <- c(
  "margin sd: the Gaussian observations", "total sd: the Gaussian observations",
  "margin sd: team", "total sd: team"
)

#' The frozen v2 specification per sport and sex.
#'
#' Handball: the static form (strength constant within a season), final
#' scores, play-off rows on the top division's scoring level, no level-score
#' shift (decision D4, the men's draw shift, is the owner's and open; its
#' default is zero). Basketball: the within-season AR(1) with weekly
#' correlation 0.98, regulation-time scores, a level-score shift per division
#' for men and one for women, and overtime as a five-minute continuation.
#' @noRd
hbbb_v2_options <- function(sport, sex) {
  sport <- match.arg(sport, c("handball", "basketball"))
  sex <- match.arg(sex, c("male", "female"))
  basketball <- identical(sport, "basketball")
  list(
    within = if (basketball) "ar1" else "none",
    rho_week = 0.98,
    regulation = basketball,
    division_map = if (basketball) NULL else c(PO = "OD"),
    tie_shift = if (!basketball) "none" else if (sex == "male") "division" else "single",
    ot_share = 5 / 40
  )
}

#' Model version stored with an INLA fit of a sport.
#' @noRd
inla_model_version <- function(sport) {
  if (identical(sport, "football")) FOOTBALL_V2_VERSION else HBBB_V2_VERSION
}

#' The inputs of one v2 fit, built from the same rows `prepare_data()` used.
#'
#' Training rows are `model_training_results()` at `end_date` in date order,
#' checked row by row against `prep$stan_data`, so the per-round strengths
#' read by the extract index the same matches as the Stan path's. Fixtures
#' are `prep$pred_d`, with their season looked up in the schedules and
#' results they came from. Basketball trains on regulation-time scores; a row
#' without them keeps its indices (its state is still estimated) and leaves
#' the likelihood.
#'
#' Every state the adapter reads is a node `(team, season, team-season,
#' week, division)`: each side of each training row and fixture, and each
#' team's current state. The current state is a team's state at its next
#' fixture in the latest season with fixtures, else at its latest match of
#' that season, else its season level there (no week term), so before a
#' season starts it has crossed the break as the forecasts have.
#'
#' @param pred Fixture rows to use in place of `prep$pred_d`, with their own
#'   `season` (`match_date`, `season`, `division`, `home_team`, `away_team`):
#'   the weekly reference fit's pairings when no fixture lies inside the
#'   horizon (`hbbb_v2_break_fixtures()`).
#' @return A list: `v` (build_v2_data()), `opts`, `train`, `pred`, `y`
#'   (margin and total responses), `observed`, `coverage`, `reg_missing`
#'   (the latest season's rows without regulation scores), `last_season`,
#'   `s_cur` / `s_cur_label` (the current season's index and label),
#'   `cur_active` (teams with a match of the current season),
#'   `nodes`, `node_id` (per training side, fixture side and team), `slot`
#'   (node of each team's r-th training match, carried forward), `latent`
#'   (the components to draw per response), `ot`, `n_seasons`, `teams`.
#' @noRd
hbbb_v2_spec <- function(prep, league, sex, end_date, root = here::here("data"),
                         from_season = NULL, opts = hbbb_v2_options(league$sport, sex),
                         pred = NULL) {
  filt <- list(sport = league$sport, country = league$country, sex = sex)
  results <- as.data.frame(read_table("results", root = root, filter = filt))
  train <- as.data.frame(model_training_results(results, league, end_date = end_date, from_season = from_season))
  train <- train[order(train$match_date), , drop = FALSE]
  sd <- prep$stan_data
  teams <- prep$teams$team
  aligned <- nrow(train) == sd$N &&
    identical(match(train$home_team, teams), as.integer(sd$team1)) &&
    identical(match(train$away_team, teams), as.integer(sd$team2)) &&
    identical(as.integer(train$home_score), as.integer(sd$goals1)) &&
    identical(as.integer(train$away_score), as.integer(sd$goals2))
  if (!aligned) {
    cli::cli_abort(c(
      "The v2 training rows do not match {.fn prepare_data}'s ({nrow(train)} against {sd$N}).",
      i = "Pass the prep built for the same league, sex, end date and first season."
    ))
  }

  given <- !is.null(pred)
  if (given) {
    pred <- as.data.frame(pred)
    stopifnot(all(c("match_date", "season", "division", "home_team", "away_team") %in% names(pred)))
    pred$season <- as.integer(pred$season)
  } else {
    pred <- as.data.frame(prep$pred_d)
    pred$season <- integer(nrow(pred))
  }
  sched <- tryCatch(as.data.frame(read_table("schedules", root = root, filter = filt)), error = function(e) NULL)
  cols <- c("match_date", "home_team", "away_team", "season", "division")
  known <- rbind(if (!is.null(sched) && nrow(sched)) sched[, cols, drop = FALSE], results[, cols, drop = FALSE])
  if (nrow(pred) && !given) {
    key <- function(d) paste(format(as.Date(d$match_date)), d$home_team, d$away_team)
    pred$season <- as.integer(known$season[match(key(pred), key(known))])
    if (anyNA(pred$season)) {
      lost <- key(pred)[is.na(pred$season)]
      cli::cli_abort("No season found for fixture{?s} {.val {lost}} in the schedules or results.")
    }
  }

  score <- function(col, fallback) {
    if (!isTRUE(opts$regulation)) {
      return(as.numeric(fallback))
    }
    if (col %in% names(train)) as.numeric(train[[col]]) else rep(NA_real_, nrow(train))
  }
  y_home <- score("home_score_reg", train$home_score)
  y_away <- score("away_score_reg", train$away_score)
  observed <- !is.na(y_home) & !is.na(y_away)
  last_season <- max(train$season)
  coverage <- mean(observed[train$season == last_season])
  reg_missing <- sum(!observed[train$season == last_season])
  train_v <- train
  train_v$home_score <- y_home
  train_v$away_score <- y_away
  v <- build_v2_data(train_v, pred, teams, division_map = opts$division_map)

  within <- !identical(opts$within, "none")
  node <- function(k, s, ts, wk, d) {
    if (!within) {
      # WHY rep(): with no fixture, k is empty and a scalar NA would not fit beside it.
      ts <- rep(NA_integer_, length(k))
      wk <- rep(NA_integer_, length(k))
    }
    data.frame(k = as.integer(k), s = as.integer(s), ts = as.integer(ts), wk = as.integer(wk), d = as.integer(d))
  }
  side <- list(
    train_home = node(v$home, v$season, v$ts_home, v$wk, v$div),
    train_away = node(v$away, v$season, v$ts_away, v$wk, v$div),
    pred_home = node(v$home_pred, v$season_pred, v$ts_home_pred, v$wk_pred, v$div_pred),
    pred_away = node(v$away_pred, v$season_pred, v$ts_away_pred, v$wk_pred, v$div_pred)
  )
  s_cur <- if (v$N_pred > 0L) max(v$season_pred) else max(v$season)
  mapped <- function(d) {
    d <- as.character(d)
    hit <- d %in% names(opts$division_map)
    d[hit] <- unname(opts$division_map[d[hit]])
    d
  }
  cur_active <- logical(v$K)
  side$cur <- do.call(rbind, lapply(seq_len(v$K), function(k) {
    ahead <- which(v$season_pred == s_cur & (v$home_pred == k | v$away_pred == k))
    if (length(ahead)) {
      cur_active[k] <<- TRUE
      j <- ahead[1]
      return(if (v$home_pred[j] == k) side$pred_home[j, ] else side$pred_away[j, ])
    }
    played <- which(v$season == s_cur & (v$home == k | v$away == k))
    if (length(played)) {
      cur_active[k] <<- TRUE
      i <- played[length(played)]
      return(if (v$home[i] == k) side$train_home[i, ] else side$train_away[i, ])
    }
    # WHY: a team with no match in the current season still has a season level
    # there (the AR(1) carries it across the break); it has no week term. Its
    # division is that of its first scheduled match of the season when one is
    # known (a promoted or relegated side), else the one it last played in.
    rows <- c(which(v$home == k | v$away == k))
    d <- v$div[rows[length(rows)]]
    team <- teams[k]
    ahead <- known[(known$home_team == team | known$away_team == team) &
      known$season == v$seasons[s_cur] & as.Date(known$match_date) > as.Date(end_date), , drop = FALSE]
    if (nrow(ahead)) {
      first <- match(mapped(ahead$division[which.min(as.Date(ahead$match_date))]), v$divisions)
      if (!is.na(first)) d <- first
    }
    node(k, s_cur, NA_integer_, NA_integer_, d)
  }))
  key_of <- function(f) paste(f$k, f$s, f$ts, f$wk, f$d)
  all_nodes <- do.call(rbind, side)
  keys <- key_of(all_nodes)
  nodes <- all_nodes[!duplicated(keys), , drop = FALSE]
  rownames(nodes) <- NULL
  node_id <- lapply(side, function(f) match(key_of(f), keys[!duplicated(keys)]))

  slot <- matrix(NA_integer_, sd$N_rounds, v$K)
  slot[cbind(sd$round1, sd$team1)] <- node_id$train_home
  slot[cbind(sd$round2, sd$team2)] <- node_id$train_away
  for (k in seq_len(v$K)) {
    played <- which(!is.na(slot[, k]))
    latest <- pmax(findInterval(seq_len(sd$N_rounds), played), 1L)
    slot[, k] <- slot[played[latest], k]
  }

  base <- list(u_s = 0L, b = 0L)
  if (within) {
    w <- nodes[!is.na(nodes$ts), , drop = FALSE]
    base$w_t <- sort(unique((w$ts - 1L) * v$W + w$wk))
  }
  latent <- list(
    margin = c(base, list(ha = 0L, hfa = 0L)),
    total = c(
      base, list(s_s = 0L),
      stats::setNames(as.list(rep(0L, v$ND)), paste0("alpha", seq_len(v$ND)))
    )
  )

  ot <- NULL
  if (isTRUE(opts$regulation)) {
    went <- observed & y_home == y_away
    pts <- (train$home_score + train$away_score - y_home - y_away)[went]
    pts <- pts[!is.na(pts) & pts > 0]
    reg_total <- (y_home + y_away)[observed]
    ot <- if (length(pts) >= HBBB_V2_OT_MIN_GAMES) {
      list(mean = mean(pts), sd = stats::sd(pts), n = length(pts), source = "overtime games")
    } else {
      list(
        mean = opts$ot_share * mean(reg_total), sd = sqrt(opts$ot_share) * stats::sd(reg_total),
        n = length(pts), source = "share of regulation"
      )
    }
  }

  list(
    v = v, opts = opts, train = train, pred = pred,
    y = list(margin = y_home - y_away, total = y_home + y_away),
    observed = observed, coverage = coverage, reg_missing = reg_missing, last_season = last_season,
    s_cur = s_cur, s_cur_label = v$seasons[s_cur], cur_active = cur_active,
    nodes = nodes, node_id = node_id, slot = slot, latent = latent, ot = ot,
    n_seasons = as.integer(sd$N_seasons), teams = teams
  )
}

# Columns of a latent draw matrix by node index; aborts on a node not drawn.
.v2_nodes_of <- function(m, i, name) {
  j <- match(i, as.integer(colnames(m)))
  if (anyNA(j)) {
    cli::cli_abort("Latent component {.val {name}} was not drawn at node{?s} {.val {unique(i[is.na(j)])}}.")
  }
  m[, j, drop = FALSE]
}

#' Each node's state in one half of v2, per draw: club level + season level
#' (+ week term where the node has one and the model has the component).
#' @noRd
hbbb_v2_states <- function(lat, nodes, v) {
  s <- .v2_nodes_of(lat$b, nodes$k, "b") + .v2_nodes_of(lat$u_s, (nodes$k - 1L) * v$NS + nodes$s, "u_s")
  has_w <- !is.na(nodes$ts)
  if (!is.null(lat$w_t) && any(has_w)) {
    s[, has_w] <- s[, has_w] +
      .v2_nodes_of(lat$w_t, (nodes$ts[has_w] - 1L) * v$W + nodes$wk[has_w], "w_t")
  }
  s
}

#' Each node's division scoring level (total model), per draw.
#' @noRd
hbbb_v2_level <- function(lat, nodes, v) {
  alpha <- do.call(cbind, lat[paste0("alpha", seq_len(v$ND))])
  alpha[, nodes$d, drop = FALSE] + .v2_nodes_of(lat$s_s, (nodes$d - 1L) * v$NS + nodes$s, "s_s")
}

#' Shuffle one fit's draws as a block, with its own seed.
#'
#' R-INLA returns a fit's draws sorted by hyperparameter configuration
#' (`inla.posterior.sample()` sorts the configurations it samples). Two fits
#' with the same number of hyperparameters share their design points, so
#' pairing draw `i` of one with draw `i` of the other would pair matching
#' configurations of two independent posteriors. One permutation is applied
#' to every component and to the sd, so each draw stays one joint draw of its
#' fit; the caller's random stream is left where it was.
#' @noRd
hbbb_v2_shuffle_draws <- function(samples, seed) {
  n <- length(samples$sigma)
  perm <- withr::with_seed(as.integer(seed), sample.int(n))
  samples$sigma <- samples$sigma[perm]
  samples$latent <- lapply(samples$latent, function(m) m[perm, , drop = FALSE])
  samples
}

#' Compose the published quantities from the two fits' joint draws.
#'
#' Draw `i` of the margin fit is paired with draw `i` of the total fit, after
#' the total fit's draws were shuffled (`hbbb_v2_shuffle_draws()`): the two
#' halves are separate models with independent posteriors.
#'
#' @param samples_m,samples_t The `samples` of the margin and total fits
#'   (`latent` and `sigma`).
#' @return List: `net`, `pace` (draws by nodes), `ha` (draws by teams),
#'   `loc_m`, `loc_t` (draws by fixtures), `sigma_m`, `sigma_t`.
#' @noRd
hbbb_v2_compose <- function(samples_m, samples_t, spec) {
  v <- spec$v
  lm <- samples_m$latent
  lt <- samples_t$latent
  stopifnot(length(samples_m$sigma) == length(samples_t$sigma))
  net <- hbbb_v2_states(lm, spec$nodes, v)
  pace <- hbbb_v2_states(lt, spec$nodes, v) + hbbb_v2_level(lt, spec$nodes, v) / 2
  ha <- as.vector(lm$hfa) + .v2_nodes_of(lm$ha, seq_len(v$K), "ha")
  id <- spec$node_id
  list(
    net = net, pace = pace, ha = ha,
    loc_m = ha[, v$home_pred, drop = FALSE] + net[, id$pred_home, drop = FALSE] - net[, id$pred_away, drop = FALSE],
    loc_t = pace[, id$pred_home, drop = FALSE] + pace[, id$pred_away, drop = FALSE],
    sigma_m = samples_m$sigma, sigma_t = samples_t$sigma
  )
}

#' Final basketball scores from regulation-time scores.
#'
#' A level regulation draw goes to overtime, modelled as in
#' `v2_basketball_result_probs()`: a five-minute period continues the same
#' game, so its margin is `normal(ot_share * loc, sqrt(ot_share) * scale)`,
#' rounded; its points are `normal(ot$mean, ot$sd)`, rounded to the margin's
#' parity and at least the margin. A level period is followed by another.
#' After `max_periods` (which a real game does not reach) a draw still level
#' is settled by one point either way.
#'
#' @param home,away Draws-by-matches matrices of whole regulation scores.
#' @param loc_m Regulation margin locations, same shape.
#' @param scale_m Margin sd: one per draw, or a matrix like `loc_m`.
#' @param ot `list(mean, sd)` of the points scored in one overtime period.
#' @return `list(home, away)`, integer matrices with no level draw.
#' @noRd
v2_overtime_scores <- function(home, away, loc_m, scale_m, ot, ot_share = 5 / 40, max_periods = 10L) {
  stopifnot(identical(dim(home), dim(away)), identical(dim(home), dim(loc_m)))
  n_draw <- nrow(home)
  margin <- home - away
  total <- home + away
  left <- which(margin == 0L)
  for (p in seq_len(max_periods)) {
    if (!length(left)) break
    n <- length(left)
    sc <- if (is.matrix(scale_m)) scale_m[left] else rep_len(scale_m, n_draw)[(left - 1L) %% n_draw + 1L]
    m_p <- round(ot_share * loc_m[left] + sqrt(ot_share) * sc * stats::rnorm(n))
    t_p <- m_p + 2 * round((stats::rnorm(n, ot$mean, ot$sd) - m_p) / 2)
    t_p <- pmax(t_p, abs(m_p))
    margin[left] <- margin[left] + m_p
    total[left] <- total[left] + t_p
    left <- left[margin[left] == 0L]
  }
  if (length(left)) {
    flip <- sample(c(-1L, 1L), length(left), replace = TRUE)
    margin[left] <- margin[left] + flip
    total[left] <- total[left] + 1L
  }
  storage.mode(margin) <- "integer"
  storage.mode(total) <- "integer"
  list(home = (total + margin) %/% 2L, away = (total - margin) %/% 2L)
}

#' Leave-one-out predictive of a training row of a Gaussian model.
#'
#' The row's observation enters the latent predictor's posterior once, as a
#' Gaussian likelihood with variance `sigma2`; removing it from a posterior
#' with mean `m` and variance `v` gives the predictive the row would have had
#' without it: precision `1 / v - 1 / sigma2`, and a predictive sd that adds
#' the observation variance back. Exact for fixed hyperparameters.
#'
#' @return `list(mean, scale)`.
#' @noRd
v2_loo_gaussian <- function(m, v, y, sigma2) {
  prec <- 1 / v - 1 / sigma2
  # WHY the floor: with the hyperparameters integrated, a row's marginal
  # variance can reach the observation variance in a near-unidentified corner;
  # the row then predicts with the prior's width, not with an infinite one.
  prec <- pmax(prec, 1e-8)
  list(mean = (m / v - y / sigma2) / prec, scale = sqrt(1 / prec + sigma2))
}

#' Basketball's level-score shift, per fixture, from the margin fit.
#'
#' The shift is the logit of how much more often a regulation-time score is
#' level than the rounded Gaussian predictive says (`v2_fit_zero_shift()`).
#' The walk-forward behind the report fitted it on earlier seasons'
#' out-of-sample forecasts. A single production fit has none, so each
#' training row's leave-one-out predictive stands in for them
#' (`v2_loo_gaussian()`). Per division for men, one for women; a group with
#' fewer than five level scores gets 0, and a fixture in a division without
#' an estimate gets the single shift. Handball: 0 (decision D4 is open).
#'
#' @return `list(per_fixture, table)`.
#' @noRd
hbbb_v2_zero_shift <- function(fit_margin, spec) {
  n_pred <- spec$v$N_pred
  if (identical(spec$opts$tie_shift, "none")) {
    return(list(
      per_fixture = rep(0, n_pred),
      table = data.frame(group = "all", shift = 0, ties = NA_integer_, n = NA_integer_)
    ))
  }
  obs <- spec$observed
  y <- spec$y$margin[obs]
  loo <- v2_loo_gaussian(fit_margin$fitted$mean[obs], fit_margin$fitted$sd[obs]^2, y, fit_margin$sigma2)
  p0 <- v2_integer_pmf(rep(0, length(y)), loo$mean, loo$scale)
  p0 <- pmin(pmax(p0, 1e-12), 1 - 1e-12)
  tie <- y == 0
  one <- function(ix) if (sum(tie[ix]) < 5L) 0 else unname(v2_fit_zero_shift(p0[ix], tie[ix])[["shift"]])
  single <- one(seq_along(y))
  table <- data.frame(group = "all", shift = single, ties = sum(tie), n = length(y))
  per <- rep(single, n_pred)
  if (identical(spec$opts$tie_shift, "division")) {
    div <- spec$v$div[obs]
    groups <- split(seq_along(y), div)
    by <- vapply(groups, one, numeric(1))
    table <- rbind(table, data.frame(
      group = spec$v$divisions[as.integer(names(by))], shift = unname(by),
      ties = vapply(groups, function(ix) sum(tie[ix]), integer(1)),
      n = lengths(groups)
    ))
    per <- unname(by[as.character(spec$v$div_pred)])
    per[is.na(per)] <- single
  }
  rownames(table) <- NULL
  list(per_fixture = per, table = table)
}

#' The fit's hyperparameter sds (posterior means), named `"<response> <parameter>"`,
#' plus each model's team spread `"<response> sd: team"`: the root sum of
#' squares of the club, season and week sds, the sd of a team's strength
#' (R-INLA's AR(1) terms are parameterised by their marginal sd).
#' @noRd
hbbb_v2_hyper_sd <- function(fits) {
  unlist(lapply(c("margin", "total"), function(resp) {
    h <- fits[[resp]]$hyper_sd
    h <- h[startsWith(h$parameter, "sd: "), , drop = FALSE]
    team <- h$mean[h$parameter %in% c("sd: b", "sd: u_s", "sd: w_t")]
    stats::setNames(c(h$mean, sqrt(sum(team^2))), paste(resp, c(h$parameter, "sd: team")))
  }))
}

#' How many times its prior's 99 % bound an sd may reach before the gate
#' refuses the fit, and before `hbbb_v2_fit_halves()` fits that half again.
#' @noRd
HBBB_V2_SD_CEILING <- 5

#' The prior's 99 % bound for one of a v2 fit's hyperparameter sds.
#'
#' @param name The sd's name as `hbbb_v2_hyper_sd()` gives it,
#'   `"<response> sd: <term>"`.
#' @param priors Prior scales, see [v2_priors()].
#' @param within The spec's within-season form (`"none"`, `"rw1"`, `"ar1"`).
#' @return The bound, or `NA` for a name with no prior bound (the composite
#'   `sd: team`).
#' @noRd
hbbb_v2_sd_bound <- function(name, priors, within) {
  term <- sub("^(margin|total) sd: ", "", name)
  switch(term,
    "the Gaussian observations" = if (startsWith(name, "margin")) priors$u_sigma_m else priors$u_sigma_t,
    u_s = priors$u_u,
    b = priors$u_b,
    ha = priors$u_ha,
    s_s = priors$u_season,
    w_t = if (identical(within, "ar1")) priors$u_w_ar1 else priors$u_w,
    NA_real_
  )
}

#' Why one half's hyperparameter summary cannot be used, if it cannot.
#'
#' The numerical failures a second attempt can repair: R-INLA's mode search
#' failed, or an sd's posterior summary is not finite (a marginal R-INLA could
#' not build) or above `sd_ceiling` times its prior's bound. The last two are
#' what R-INLA returns when its numerical Hessian at the mode comes out nearly
#' singular in one direction: the asymmetric Gaussian it builds each marginal
#' from then reaches absurd values. On the simulated handball league of
#' `test-model-v2-hbbb.R` the MKL build R-INLA runs on Linux did that for the
#' total model on Intel Xeon runners and not on AMD EPYC runners (club sd
#' 9.08e9, one Hessian direction with sd 15.8 against at most 2 on AMD),
#' which end their search at a nearby point of the same flat ridge. These are
#' the gate's "mode not found" and "sd above its prior bound" breaches; the
#' gate's other checks are about the fit's values, not its numerics.
#'
#' @param fit One half as `fit_v2_inla()` returns it.
#' @param priors Prior scales, see [v2_priors()].
#' @param within The spec's within-season form.
#' @param sd_ceiling See `HBBB_V2_SD_CEILING`.
#' @return Character vector of reasons, empty when the summary is usable.
#' @noRd
hbbb_v2_half_unusable <- function(fit, priors, within, sd_ceiling = HBBB_V2_SD_CEILING) {
  reasons <- character()
  if (!isTRUE(v2_mode_ok(fit$mode_status))) {
    reasons <- sprintf("mode not found (mode.status = %s)", paste(format(fit$mode_status), collapse = ", "))
  }
  h <- fit$hyper_sd[startsWith(fit$hyper_sd$parameter, "sd: "), , drop = FALSE]
  name <- paste(fit$response, h$parameter)
  bound <- vapply(name, hbbb_v2_sd_bound, numeric(1), priors = priors, within = within)
  bad <- !is.finite(h$mean) | (!is.na(bound) & h$mean > sd_ceiling * bound)
  c(reasons, sprintf("%s = %.3g", name[bad], h$mean[bad]))
}

#' Checks a handball / basketball INLA fit must pass before it is used.
#'
#' 1. R-INLA's mode search succeeded for both halves (status 0).
#' 2. No sd at a bound: each model's team spread inside the sport's range
#'    (the degenerate mode R-INLA can settle in has the team effects switched
#'    off), and no sd above `sd_ceiling` times its prior's 99 % bound.
#' 3. Sanity, per sport: the league home advantage, the two observation sds
#'    and the mean predicted total inside `HBBB_V2_SANITY`.
#' 4. Basketball: regulation-time scores for at least
#'    `HBBB_V2_MIN_REG_COVERAGE` of the latest season's training rows, unless
#'    fewer than `HBBB_V2_MIN_REG_MISSING` rows lack them; and each model's
#'    within-season sd inside the sport's range.
#' 5. None of `HBBB_V2_JUMP_CHECKED` moved by more than a factor `max_jump`
#'    since the previous INLA fit of the cell (`previous_sd`; skipped, and
#'    recorded as not run, when there is none).
#'
#' @return `list(passed, breaches, evaluated, skipped, jump_checked)`.
#' @noRd
hbbb_v2_gate <- function(fits, spec, sport, previous_sd = NULL, max_jump = 3,
                         sd_ceiling = HBBB_V2_SD_CEILING,
                         sanity = HBBB_V2_SANITY[[sport]], priors = v2_priors(sport)) {
  breaches <- character()
  skipped <- character()
  for (resp in c("margin", "total")) {
    status <- fits[[resp]]$mode_status
    if (length(status) != 1L || is.na(status) || status != 0L) {
      shown <- if (length(status)) paste(format(status), collapse = ", ") else "missing"
      breaches <- c(breaches, sprintf("%s model: hyperparameter mode not found (mode.status = %s)", resp, shown))
    }
  }

  sds <- hbbb_v2_hyper_sd(fits)
  ceiling <- sd_ceiling * vapply(names(sds), hbbb_v2_sd_bound, numeric(1), priors = priors, within = spec$opts$within)
  bad <- !is.finite(sds) | (!is.na(ceiling) & sds > ceiling)
  if (any(bad)) {
    breaches <- c(breaches, sprintf(
      "hyperparameter sd above %g times its prior bound: %s",
      sd_ceiling, paste(sprintf("%s = %.3g", names(sds)[bad], sds[bad]), collapse = ", ")
    ))
  }
  team <- sds[c("margin sd: team", "total sd: team")]
  off <- !is.finite(team) | team < sanity$team[[1]] | team > sanity$team[[2]]
  if (any(off)) {
    breaches <- c(breaches, sprintf(
      "team spread outside [%g, %g]: %s", sanity$team[[1]], sanity$team[[2]],
      paste(sprintf("%s = %.3g", names(team)[off], team[off]), collapse = ", ")
    ))
  }

  outside <- function(x, r) !is.finite(x) || x < r[[1]] || x > r[[2]]
  hfa <- fits$margin$fixed["hfa", "mean"]
  sig <- sqrt(c(margin = fits$margin$sigma2, total = fits$total$sigma2))
  total <- if (nrow(fits$total$pred)) {
    mean(fits$total$pred$mean)
  } else {
    mean(fits$total$fitted$mean[spec$observed])
  }
  insane <- c(
    if (outside(hfa, sanity$hfa)) sprintf("home advantage %.2f outside [%g, %g]", hfa, sanity$hfa[[1]], sanity$hfa[[2]]),
    if (any(vapply(sig, outside, logical(1), r = sanity$sigma))) {
      sprintf("observation sds %s outside [%g, %g]", paste(sprintf("%s %.2f", names(sig), sig), collapse = ", "), sanity$sigma[[1]], sanity$sigma[[2]])
    },
    if (outside(total, sanity$total)) sprintf("mean predicted total %.1f outside [%g, %g]", total, sanity$total[[1]], sanity$total[[2]])
  )
  breaches <- c(breaches, insane)

  if (!is.null(sanity$within) && !identical(spec$opts$within, "none")) {
    week <- sds[c("margin sd: w_t", "total sd: w_t")]
    low <- !is.finite(week) | week < sanity$within[[1]] | week > sanity$within[[2]]
    if (any(low)) {
      breaches <- c(breaches, sprintf(
        "within-season sd outside [%g, %g]: %s", sanity$within[[1]], sanity$within[[2]],
        paste(sprintf("%s = %.3g", c("margin sd: w_t", "total sd: w_t")[low], week[low]), collapse = ", ")
      ))
    }
  }

  n_checks <- 4L
  if (isTRUE(spec$opts$regulation)) {
    n_checks <- 5L
    short <- !is.finite(spec$coverage) || spec$coverage < HBBB_V2_MIN_REG_COVERAGE
    if (short && (spec$reg_missing %||% Inf) >= HBBB_V2_MIN_REG_MISSING) {
      breaches <- c(breaches, sprintf(
        "regulation-time scores for %.0f %% of season %s's training rows (%d rows without), below %.0f %%",
        100 * spec$coverage, spec$last_season, as.integer(spec$reg_missing), 100 * HBBB_V2_MIN_REG_COVERAGE
      ))
    }
  }

  if (is.null(previous_sd)) {
    skipped <- c(skipped, "hyperparameter jump")
  } else {
    common <- intersect(intersect(names(previous_sd), names(sds)), HBBB_V2_JUMP_CHECKED)
    ratio <- sds[common] / previous_sd[common]
    jumped <- !is.finite(ratio) | ratio > max_jump | ratio < 1 / max_jump
    if (any(jumped)) {
      breaches <- c(breaches, sprintf(
        "hyperparameter moved by more than a factor %g since the previous fit: %s",
        max_jump, paste(common[jumped], collapse = ", ")
      ))
    }
  }

  list(
    passed = length(breaches) == 0L, breaches = breaches,
    evaluated = n_checks - length(skipped), skipped = length(skipped),
    jump_checked = !is.null(previous_sd)
  )
}

#' A cmdstanr-fit-like object over the composed v2 draws.
#'
#' @param comp Output of `hbbb_v2_compose()`.
#' @param spec The spec the fits were made on.
#' @param prep `prepare_data()` output for the same cell and date.
#' @param sport `"handball"` or `"basketball"`.
#' @param zero_shift Level-score shift per fixture (`hbbb_v2_zero_shift()`).
#' @param level_sd The total model's season-to-season sd of a division's
#'   level, for the simulator's step into a season the fit has not reached.
#' @param seed Seed for the score draws; the caller's random stream is left
#'   where it was.
#' @return `list(draws = function(variables), variables, save_object)`.
#' @noRd
hbbb_v2_as_fit <- function(comp, spec, prep, sport, zero_shift = 0, level_sd = 0, seed = 1L) {
  sport <- match.arg(sport, c("handball", "basketball"))
  v <- spec$v
  K <- v$K
  n_draw <- length(comp$sigma_m)
  stopifnot(nrow(prep$teams) == K, nrow(prep$pred_d) == v$N_pred)
  id <- spec$node_id
  indexed <- function(m, name) {
    colnames(m) <- paste0(name, "[", seq_len(ncol(m)), "]")
    m
  }
  scalar <- function(x, name) matrix(rep_len(x, n_draw), ncol = 1L, dimnames = list(NULL, name))

  pace_cur <- comp$pace[, id$cur, drop = FALSE]
  active <- spec$cur_active %||% rep(TRUE, K)
  if (!any(active)) active <- rep(TRUE, K)
  rot <- v2_as_attack_defence(
    comp$net[, id$cur, drop = FALSE], pace_cur, comp$ha,
    comp$sigma_m, comp$sigma_t,
    level = rowMeans(pace_cur[, active, drop = FALSE])
  )
  level <- rot$mean_goals
  off <- rot$offense
  def <- rot$defense
  ha_off <- rot$home_advantage_off
  ha_def <- rot$home_advantage_def

  n_round <- nrow(spec$slot)
  grid <- expand.grid(round = seq_len(n_round), team = seq_len(K))
  at <- spec$slot[cbind(grid$round, grid$team)]
  walk <- function(sign, name) {
    m <- (comp$net[, at, drop = FALSE] + sign * (comp$pace[, at, drop = FALSE] - level)) / 2
    colnames(m) <- sprintf("%s[%d,%d]", name, grid$round, grid$team)
    m
  }
  # WHY: the state already sits in the current season. Only when that is the
  # last season with results does the simulator's step into a later season
  # need the level's own season-to-season variance.
  step_sd <- if (spec$s_cur_label > spec$last_season) 0 else level_sd / 2

  vars <- list(
    home_advantage_off = indexed(ha_off, "home_advantage_off"),
    home_advantage_def = indexed(ha_def, "home_advantage_def"),
    home_advantage_tot = indexed(ha_off + ha_def, "home_advantage_tot"),
    cur_offense_away = indexed(off, "cur_offense_away"),
    cur_defense_away = indexed(def, "cur_defense_away"),
    cur_strength_away = indexed(off + def, "cur_strength_away"),
    cur_offense_home = indexed(off + ha_off, "cur_offense_home"),
    cur_defense_home = indexed(def + ha_def, "cur_defense_home"),
    cur_strength_home = indexed(off + ha_off + def + ha_def, "cur_strength_home"),
    offense = walk(1, "offense"),
    defense = walk(-1, "defense"),
    mean_goals = indexed(matrix(level, n_draw, spec$n_seasons), "mean_goals"),
    delta_mean_goals = scalar(0, "delta_mean_goals"),
    sigma_mean_goals = scalar(step_sd, "sigma_mean_goals"),
    nu = scalar(HBBB_V2_NU, "nu"),
    lp__ = scalar(0, "lp__")
  )
  if (identical(sport, "handball")) {
    vars$rho <- scalar(rot$rho, "rho")
    vars$sigma_team <- indexed(matrix(rot$sigma, n_draw, K), "sigma_team")
    vars$mean_sigma_team <- scalar(log(rot$sigma), "mean_sigma_team")
    vars$scale_sigma_team <- scalar(0, "scale_sigma_team")
  } else {
    vars$sigma <- scalar(rot$sigma, "sigma")
    vars$alpha_rho <- scalar(stats::qlogis((1 + rot$rho) / 2), "alpha_rho")
    vars$beta_rho <- scalar(0, "beta_rho")
    vars$beta2_rho <- scalar(0, "beta2_rho")
    vars$beta3_rho <- scalar(0, "beta3_rho")
  }
  if (v$N_pred > 0L) {
    scores <- withr::with_seed(as.integer(seed), {
      sc <- v2_sample_scores(comp$loc_m, comp$sigma_m, comp$loc_t, comp$sigma_t, zero_shift = zero_shift)
      if (identical(sport, "basketball")) {
        sc <- v2_overtime_scores(sc$home, sc$away, comp$loc_m, comp$sigma_m, spec$ot, ot_share = spec$opts$ot_share)
      }
      sc
    })
    vars$goals1_pred <- indexed(scores$home, "goals1_pred")
    vars$goals2_pred <- indexed(scores$away, "goals2_pred")
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
    save_object = function(file) saveRDS(vars, file)
  )
}

#' Fit both halves of v2 on a spec with R-INLA, latent components drawn.
#'
#' The one place the production engine's fitting settings live, so the
#' scorecard (`hbbb_v2_forecast_day()`) fits exactly as production does.
#'
#' A half whose summary is unusable (`hbbb_v2_half_unusable()`: the mode
#' search failed, or an sd's summary is not finite or above the gate's
#' ceiling) is fitted once more, restarting R-INLA's optimiser at that fit's
#' own mode. On the CI failure of 2026-11-23/24 the restart ended at the same
#' mode to ten digits and recomputed a well-conditioned Hessian there (club
#' sd 1.25 instead of 9.08e9). A cold refit would be no second attempt: on
#' one thread with `internal.opt = FALSE` it repeats the first bit for bit.
#' The second attempt is kept whatever it returns; the gate decides whether
#' it is used. Both attempts are deterministic, so a fit stays repeatable.
#'
#' @return `list(margin, total)` of `fit_v2_inla()` results, each with
#'   `attempts` (1 or 2) and, after a second attempt, `first_attempt` (why
#'   the first was unusable).
#' @noRd
hbbb_v2_fit_halves <- function(spec, sport, n_draws = 4000L, seed = 1L, int_strategy = "auto") {
  priors <- v2_priors(sport)
  fits <- list()
  for (resp in c("margin", "total")) {
    # WHY two seeds: R-INLA's latent sampler, seeded alike in two fits of the
    # same size, would draw the same normal vectors for both halves.
    fit_once <- function(theta_start) {
      fit_v2_inla(
        spec$v, resp, priors,
        within = spec$opts$within, rho_week = spec$opts$rho_week, int_strategy = int_strategy,
        n_samples = as.integer(n_draws), y = spec$y[[resp]],
        seed = as.integer(seed) + (resp == "total"), latent = spec$latent[[resp]],
        theta_start = theta_start
      )
    }
    fit <- fit_once(NULL)
    fit$attempts <- 1L
    why <- hbbb_v2_half_unusable(fit, priors, spec$opts$within)
    if (length(why) && length(fit$theta) && all(is.finite(fit$theta))) {
      cli::cli_alert_warning(c(
        "R-INLA's {resp} fit is unusable ({paste(why, collapse = '; ')}); ",
        "fitting it once more, restarting at its mode."
      ))
      theta <- fit$theta
      fit <- fit_once(theta)
      fit$attempts <- 2L
      fit$first_attempt <- why
    }
    fits[[resp]] <- fit
  }
  fits
}

#' The two halves' draws composed, the total fit's shuffled first
#' (`hbbb_v2_shuffle_draws()`), as the production engine pairs them.
#' @noRd
hbbb_v2_pair <- function(fits, spec, seed = 1L) {
  hbbb_v2_compose(fits$margin$samples, hbbb_v2_shuffle_draws(fits$total$samples, as.integer(seed) + 2L), spec)
}

#' Fit handball / basketball v2 with R-INLA and return a fit the extract and
#' publish layers can read.
#'
#' @param prep Output of `prepare_data()` for the same league, sex and date.
#' @param n_draws Joint posterior draws.
#' @param int_strategy R-INLA's hyperparameter integration; "auto" integrates
#'   (the published intervals carry the hyperparameters' uncertainty).
#' @param previous_sd The previous fit's hyperparameter sds for the gate's
#'   jump check (`fv2_previous_hyper()`); `NULL` when there is none.
#' @param accept_jump Let a fit through whose only breach is the jump check.
#' @return The adapted fit from `hbbb_v2_as_fit()`, with `$engine`,
#'   `$version`, `$gate`, `$hyper_sd`, `$n_obs`, `$attempts` (R-INLA fits per
#'   half, see `hbbb_v2_fit_halves()`) and the record of how it was made
#'   attached.
#' @noRd
fit_model_v2_hbbb <- function(prep, league, sex, end_date,
                              root = here::here("data"), from_season = NULL,
                              n_draws = 4000L, seed = 1L, int_strategy = "auto",
                              previous_sd = NULL, accept_jump = FALSE) {
  sport <- league$sport
  if (!sport %in% c("handball", "basketball")) {
    cli::cli_abort("{.fn fit_model_v2_hbbb} fits handball and basketball; {.val {sport}} is not one of them.")
  }
  opts <- hbbb_v2_options(sport, sex)
  spec <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = root, from_season = from_season, opts = opts)
  if (!any(spec$observed)) {
    # WHY before R-INLA: with every response missing its program fails
    # without saying why.
    cli::cli_abort(c(
      "{sport}/{league$country}/{sex}: no regulation-time scores in the {spec$v$N} training rows.",
      i = "The basketball model trains on regulation time; the results need {.field home_score_reg} and {.field away_score_reg}."
    ))
  }
  absent <- c("INLA", "sn")[!vapply(c("INLA", "sn"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    cli::cli_abort(c(
      "League asks for {.code engine: inla} but {.pkg {absent}} {?is/are} not installed.",
      i = "INLA installs from {.url https://inla.r-inla-download.org/R/stable}, sn from CRAN; or set {.code engine: stan}."
    ))
  }
  t0 <- Sys.time()
  fits <- hbbb_v2_fit_halves(spec, sport, n_draws = n_draws, seed = seed, int_strategy = int_strategy)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  gate <- hbbb_v2_gate(fits, spec, sport, previous_sd = previous_sd)
  jumped <- grepl("since the previous fit", gate$breaches, fixed = TRUE)
  if (isTRUE(accept_jump) && any(jumped)) {
    cli::cli_alert_warning(c(
      "INLA gate: hyperparameter jump accepted on request for {sport}/{league$country}/{sex}: ",
      "{gate$breaches[jumped]}"
    ))
    gate$breaches <- gate$breaches[!jumped]
    gate$passed <- length(gate$breaches) == 0L
    gate$jump_accepted <- TRUE
  }
  if (!gate$passed) {
    cli::cli_abort(c(
      "INLA fit for {sport}/{league$country}/{sex} failed its diagnostics gate.",
      stats::setNames(gate$breaches, rep("x", length(gate$breaches)))
    ))
  }

  shift <- hbbb_v2_zero_shift(fits$margin, spec)
  level_sd <- fits$total$hyper_sd$mean[fits$total$hyper_sd$parameter == "sd: s_s"]
  comp <- hbbb_v2_pair(fits, spec, seed = seed)
  fit <- hbbb_v2_as_fit(
    comp, spec, prep, sport,
    zero_shift = shift$per_fixture, level_sd = if (length(level_sd)) level_sd else 0, seed = seed
  )
  hyper <- rbind(
    cbind(response = "margin", fits$margin$hyper_sd),
    cbind(response = "total", fits$total$hyper_sd)
  )
  meta <- list(
    engine = "inla", version = HBBB_V2_VERSION, options = opts,
    int_strategy = int_strategy, gate = gate, hyper = hyper,
    hyper_sd = hbbb_v2_hyper_sd(fits),
    mode_status = c(margin = fits$margin$mode_status, total = fits$total$mode_status),
    attempts = c(margin = fits$margin$attempts, total = fits$total$attempts),
    n_obs = as.integer(prep$stan_data$N), n_trained = sum(spec$observed),
    # WHY: needs_refit() compares this with a recount, so a regulation score
    # that arrives after the fit starts a refit although the row count stands.
    n_obs_regulation = if (isTRUE(opts$regulation)) sum(spec$observed) else NA_integer_,
    n_teams = spec$v$K, current_season = spec$s_cur_label,
    regulation_coverage = if (isTRUE(opts$regulation)) spec$coverage else NA_real_,
    zero_shift = shift$table, overtime = spec$ot,
    seconds = secs, inla_version = as.character(utils::packageVersion("INLA"))
  )
  cli::cli_alert_info(paste0(
    "INLA engine, {sport} v{HBBB_V2_VERSION}: {sum(spec$observed)} of {prep$stan_data$N} matches in the likelihood, ",
    "{spec$v$K} teams; fits and draws {round(secs)} s; ",
    "gate passed ({gate$evaluated} checks, {gate$skipped} skipped)"
  ))
  fit[names(meta)] <- meta
  fit$save_object <- fv2_saver(fit$draws, meta)
  fit
}

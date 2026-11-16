# Football v2 prototype: ONE model specification, consumed by both engines.
#
# States live on a weekly calendar grid, one per (team, week) in which the team
# plays or has a fixture to predict. Weeks without a match are integrated out
# exactly: a random-walk step spanning d weeks has variance sigma^2 * d.
#
#   entry state        x[k, first] ~ N(0, tau^2), sum to zero over teams
#   in-season step     x[k, j] - x[k, j-1] ~ N(0, sigma^2 * weeks)
#   season-break step  x[k, j] - rho * x[k, j-1] - (1 - rho) * m ~ N(0, kappa^2 * breaks)
#                      (clock = "break" only; m = mean end-of-season state of
#                      the team's division; rho = 1 is a plain jump, and teams
#                      that change division always take a plain jump)
#   home goals         Poisson(exp(c_home + att[h] + ha_att[h] - def[a] + offsets))
#   away goals         Poisson(exp(c_away + att[a] - def[h] - ha_def[h] + offsets))
#   optional           a league-wide scoring level and a league-wide home
#                      advantage that each walk over the weeks (level_walk)
#
# Nothing here touches INLA or Stan: fv2_structure() gives the prior precision
# pieces for INLA's generic3, fv2_stan_data() the same steps for Stan.
# Nothing here knows about a particular country either: division structure and
# offset groups arrive as data (see data.R for the Iceland adapter).

FV2_EPOCH <- as.Date("2021-01-04")

fv2_week <- function(date) {
  as.integer(floor(as.numeric(as.Date(date) - FV2_EPOCH) / 7))
}

fv2_priors <- function() {
  list(
    c_mean = log(1.4), c_sd = 1,
    tau = c(1, 0.01),
    sigma = c(0.2, 0.01),
    kappa = c(1, 0.01),
    omega = c(0.3, 0.01),
    delta = c(0.3, 0.01),
    level = c(0.05, 0.01),
    beta_sd = 0.25
  )
}

# PC prior P(sd > u) = a is Exponential(rate) on the sd.
fv2_pc_rate <- function(ua) -log(ua[[2]]) / ua[[1]]

#' Build the model specification from training matches and fixtures to predict.
#'
#' League-agnostic: everything a competition system contributes arrives as
#' data. `divisions` says which league division each team plays in per season
#' and how the divisions rank; a `cell` column on the matches names the
#' offset group (for instance division-by-season, or league-by-season).
#'
#' @param train Played matches: match_date, home_team, away_team, home_score,
#'   away_score, season, and optionally cell.
#' @param pred Fixtures to predict (same columns, scores unused); both teams
#'   must appear in `train`.
#' @param clock "calendar": every step is weeks elapsed, off-season included.
#'   "break": steps inside a season count weeks, a step across seasons is one
#'   break jump with its own variance.
#' @param rho Reversion at the season break toward the mean of the team's
#'   division, for teams that stay in it (1 = none).
#' @param movers Give teams that changed division their own break variance and
#'   a mean shock.
#' @param div_offsets Offsets per `cell` for scoring rate and home advantage.
#' @param param "ad": attack and defence walks. "sq": overall strength and
#'   style (attack = s + q, defence = s - q).
#' @param style For param = "sq": "rw" (style walks), "static" (one value per
#'   team), "none".
#' @param level_walk Let the league-wide scoring level and home advantage
#'   walk over the weeks (two weekly random walks shared by all teams).
#' @param divisions Data frame team, season, division, rank (1 = top); needed
#'   for `movers` and `rho < 1`. Schedule knowledge, known before a season.
fv2_build <- function(train, pred = NULL,
                      clock = c("calendar", "break"), rho = 1,
                      movers = FALSE, div_offsets = FALSE,
                      param = c("ad", "sq"), style = c("rw", "static", "none"),
                      level_walk = FALSE,
                      divisions = NULL, priors = fv2_priors()) {
  clock <- match.arg(clock)
  param <- match.arg(param)
  style <- match.arg(style)
  train <- as.data.frame(train)
  need <- c("match_date", "home_team", "away_team", "home_score", "away_score", "season")
  stopifnot(all(need %in% names(train)), nrow(train) > 0L)
  stopifnot(!anyNA(train$home_score), !anyNA(train$away_score))
  stopifnot(rho > 0, rho <= 1)
  if (clock == "calendar") stopifnot(rho == 1, !movers)
  if (movers || rho < 1) {
    stopifnot(all(c("team", "season", "division", "rank") %in% names(divisions)))
  }
  if (div_offsets) stopifnot("cell" %in% names(train))

  teams <- sort(unique(c(train$home_team, train$away_team)))
  K <- length(teams)
  if (is.null(pred) || !nrow(pred)) {
    pred <- train[0, , drop = FALSE]
  } else {
    pred <- as.data.frame(pred)
    stopifnot(all(pred$home_team %in% teams), all(pred$away_team %in% teams))
  }

  appearances <- function(d, is_pred) {
    data.frame(
      team = match(c(d$home_team, d$away_team), teams),
      week = fv2_week(c(d$match_date, d$match_date)),
      season = as.integer(c(d$season, d$season)),
      is_pred = rep(is_pred, 2L * nrow(d))
    )
  }
  app <- rbind(appearances(train, FALSE), appearances(pred, TRUE))
  app <- app[order(app$team, app$week, app$is_pred), , drop = FALSE]
  key <- paste(app$team, app$week)
  slots <- app[!duplicated(key), c("team", "week", "season"), drop = FALSE]
  slots$has_obs <- !app$is_pred[!duplicated(key)]
  rownames(slots) <- NULL
  S <- nrow(slots)
  slots$slot <- seq_len(S)
  slot_of <- function(team, date) {
    match(paste(match(team, teams), fv2_week(date)), paste(slots$team, slots$week))
  }
  first_slot <- as.integer(tapply(slots$slot, slots$team, min))
  last_obs_slot <- as.integer(tapply(slots$slot[slots$has_obs], slots$team[slots$has_obs], max))

  lookup <- function(team_nr, season, column) {
    if (is.null(divisions)) {
      return(rep(NA, length(team_nr)))
    }
    divisions[[column]][match(paste(teams[team_nr], season), paste(divisions$team, divisions$season))]
  }

  is_step <- !(slots$slot %in% first_slot)
  steps <- data.frame(slot = slots$slot[is_step], prev = slots$slot[is_step] - 1L)
  steps$team <- slots$team[steps$slot]
  d_week <- slots$week[steps$slot] - slots$week[steps$prev]
  d_season <- slots$season[steps$slot] - slots$season[steps$prev]
  is_break <- clock == "break" & d_season > 0L
  steps$class <- ifelse(is_break, "brk", "in")
  steps$delta <- ifelse(is_break, d_season, d_week)
  steps$from_season <- slots$season[steps$prev]
  steps$from_div <- as.character(lookup(steps$team, steps$from_season, "division"))
  rank_change <- as.numeric(lookup(steps$team, steps$from_season, "rank")) -
    as.numeric(lookup(steps$team, slots$season[steps$slot], "rank"))
  steps$move <- ifelse(is_break & !is.na(rank_change), sign(rank_change), 0)
  if (movers) steps$class[steps$move != 0] <- "mov"
  reverts <- steps$class == "brk" & steps$move == 0 & !is.na(steps$from_div)
  steps$rho <- ifelse(reverts, rho, 1)
  stopifnot(all(steps$delta > 0))

  promoted <- relegated <- numeric(S)
  if (movers) {
    up <- down <- numeric(S)
    up[steps$slot] <- as.numeric(steps$move > 0)
    down[steps$slot] <- as.numeric(steps$move < 0)
    promoted <- as.numeric(unlist(tapply(up, slots$team, cumsum)))
    relegated <- as.numeric(unlist(tapply(down, slots$team, cumsum)))
  }

  match_part <- function(d) {
    list(
      home_slot = slot_of(d$home_team, d$match_date),
      away_slot = slot_of(d$away_team, d$match_date),
      home_team = match(d$home_team, teams),
      away_team = match(d$away_team, teams),
      cell = if (is.null(d$cell)) rep("all", nrow(d)) else as.character(d$cell)
    )
  }
  obs <- match_part(train)
  obs$goals_home <- as.integer(train$home_score)
  obs$goals_away <- as.integer(train$away_score)
  prd <- match_part(pred)
  cells <- sort(unique(c(obs$cell, prd$cell)))
  obs$cell_nr <- match(obs$cell, cells)
  prd$cell_nr <- match(prd$cell, cells)
  weeks <- sort(unique(fv2_week(c(train$match_date, pred$match_date))))
  obs$week_nr <- match(fv2_week(train$match_date), weeks)
  prd$week_nr <- match(fv2_week(pred$match_date), weeks)

  list(
    teams = teams, K = K, slots = slots, S = S,
    first_slot = first_slot, last_obs_slot = last_obs_slot, steps = steps,
    promoted = promoted, relegated = relegated,
    obs = obs, pred = prd, N = nrow(train), cells = cells, weeks = weeks,
    opts = list(
      clock = clock, rho = rho, movers = movers, div_offsets = div_offsets,
      param = param, style = style, level_walk = level_walk
    ),
    priors = priors
  )
}

#' Prior precision pieces of one state field: Q = init / tau^2 + in / sigma^2
#' (+ brk / kappa^2 + mov / kappa_m^2).
fv2_structure <- function(spec) {
  S <- spec$S
  st <- spec$steps
  piece <- function(cls) {
    s <- st[st$class == cls, , drop = FALSE]
    if (!nrow(s)) {
      return(NULL)
    }
    n <- nrow(s)
    D <- Matrix::sparseMatrix(
      i = rep(seq_len(n), 2L), j = c(s$slot, s$prev), x = c(rep(1, n), -s$rho),
      dims = c(n, S)
    )
    pull <- s$rho < 1
    if (any(pull)) {
      grp <- paste(s$from_div, s$from_season)
      for (g in unique(grp[pull])) {
        rows <- which(pull & grp == g)
        members <- s$prev[rows]
        w <- (1 - s$rho[rows[1L]]) / length(members)
        D <- D - Matrix::sparseMatrix(
          i = rep(rows, each = length(members)), j = rep(members, times = length(rows)),
          x = w, dims = c(n, S)
        )
      }
    }
    Matrix::crossprod(D * sqrt(1 / s$delta))
  }
  out <- list(
    init = Matrix::sparseMatrix(i = spec$first_slot, j = spec$first_slot, x = 1, dims = c(S, S)),
    `in` = piece("in"), brk = piece("brk"), mov = piece("mov")
  )
  out[!vapply(out, is.null, logical(1))]
}

#' Design of the 2N training rows (home goals, then away goals) or of the
#' fixtures to predict, as sparse maps from each latent block to the predictor.
fv2_design <- function(spec, which = c("obs", "pred")) {
  which <- match.arg(which)
  m <- spec[[which]]
  n <- length(m$home_slot)
  rows <- seq_len(2L * n)
  home <- seq_len(n)
  away <- n + seq_len(n)
  sp <- function(i, j, x, ncol) Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(2L * n, ncol))
  att <- sp(rows, c(m$home_slot, m$away_slot), 1, spec$S)
  def <- sp(rows, c(m$away_slot, m$home_slot), -1, spec$S)
  out <- list(
    att = att, def = def,
    ha_att = sp(home, m$home_team, 1, spec$K),
    ha_def = sp(away, m$home_team, -1, spec$K),
    c_home = as.numeric(rows <= n), c_away = as.numeric(rows > n)
  )
  if (spec$opts$div_offsets) {
    n_cell <- length(spec$cells)
    out$g_goal <- sp(rows, c(m$cell_nr, m$cell_nr), 1, n_cell)
    out$g_ha <- sp(rows, c(m$cell_nr, m$cell_nr), rep(c(0.5, -0.5), each = n), n_cell)
  }
  if (isTRUE(spec$opts$level_walk)) {
    n_week <- length(spec$weeks)
    out$lvl_goal <- sp(rows, c(m$week_nr, m$week_nr), 1, n_week)
    out$lvl_ha <- sp(rows, c(m$week_nr, m$week_nr), rep(c(0.5, -0.5), each = n), n_week)
  }
  if (spec$opts$movers) {
    # WHY: a shock shifts a team's attack and defence together, so it enters a
    # row as the attacker's count minus the defender's. `def` already carries
    # the minus sign, hence the sum.
    out$promoted <- as.numeric(att %*% spec$promoted + def %*% spec$promoted)
    out$relegated <- as.numeric(att %*% spec$relegated + def %*% spec$relegated)
  }
  out
}

#' Which Stan program implements a spec: stan/fv2.stan (attack and defence
#' walks), stan/fv2_sq.stan (strength with an optional fixed style) or
#' stan/fv2_rev.stan (either, with reversion at the season break).
fv2_stan_program <- function(spec, est_rho = FALSE) {
  o <- spec$opts
  if (o$clock == "break" && (o$rho < 1 || est_rho)) {
    return("fv2_rev.stan")
  }
  if (o$param == "sq") "fv2_sq.stan" else "fv2.stan"
}

#' The same prior as fv2_structure(), in the form the Stan programs read.
#'
#' @param est_rho Estimate the reversion coefficient instead of fixing it at
#'   the spec's value (Stan only; INLA keeps it inside the precision matrix).
fv2_stan_data <- function(spec, est_rho = FALSE) {
  o <- spec$opts
  program <- fv2_stan_program(spec, est_rho)
  stopifnot(!o$movers, o$rho == 1 || program == "fv2_rev.stan")
  if (o$param == "sq") stopifnot(o$style != "rw", !o$div_offsets)
  p <- spec$priors
  out <- list(
    K = spec$K, S = spec$S, N = spec$N,
    home_slot = spec$obs$home_slot, away_slot = spec$obs$away_slot,
    home_team = spec$obs$home_team,
    goals_home = spec$obs$goals_home, goals_away = spec$obs$goals_away,
    use_style = as.integer(o$param == "sq" && o$style == "static"),
    c_prior_mean = p$c_mean, c_prior_sd = p$c_sd,
    rate_tau = fv2_pc_rate(p$tau), rate_sigma = fv2_pc_rate(p$sigma),
    rate_kappa = fv2_pc_rate(p$kappa), rate_omega = fv2_pc_rate(p$omega)
  )
  if (program == "fv2_rev.stan") {
    stopifnot(!o$div_offsets)
    return(c(
      out, fv2_segments(spec),
      list(
        n_field = if (o$param == "sq") 1L else 2L, est_rho = as.integer(est_rho), rho_fixed = o$rho,
        use_level = as.integer(isTRUE(o$level_walk)), n_week = length(spec$weeks),
        match_week = spec$obs$week_nr, week_step = c(0, sqrt(diff(spec$weeks))),
        rate_level = fv2_pc_rate(p$level)
      )
    ))
  }
  stopifnot(!isTRUE(o$level_walk))
  st <- spec$steps
  step_in <- step_brk <- numeric(spec$S)
  step_in[st$slot[st$class == "in"]] <- sqrt(st$delta[st$class == "in"])
  step_brk[st$slot[st$class == "brk"]] <- sqrt(st$delta[st$class == "brk"])
  n_cell <- if (o$div_offsets) length(spec$cells) else 0L
  c(out, list(
    slot_team = spec$slots$team, first_slot = spec$first_slot,
    step_in = step_in, step_brk = step_brk,
    use_break = as.integer(o$clock == "break"),
    n_cell = n_cell,
    cell = if (n_cell) spec$obs$cell_nr else integer(0),
    rate_delta = fv2_pc_rate(p$delta)
  ))
}

#' Season segments of the state chains, for the Stan program with reversion.
#'
#' A segment is one team's run of slots within one season. Inside a segment
#' the state walks week by week; a segment starts from the entry level or from
#' the previous segment's last state through the season-break step.
#'
#' @return Index arrays in Stan's layout. `seg_order` lists segments season by
#'   season (`wave_start`/`wave_end` index into it), so every segment's
#'   predecessor and every division mean it reverts toward are already
#'   computed when it is reached.
fv2_segments <- function(spec) {
  stopifnot(spec$opts$clock == "break", !spec$opts$movers)
  sl <- spec$slots
  st <- spec$steps
  brk <- st[st$class == "brk", , drop = FALSE]
  starts <- sort(c(spec$first_slot, brk$slot))
  seg_of_slot <- findInterval(sl$slot, starts)
  n_seg <- length(starts)
  seg_last <- as.integer(tapply(sl$slot, seg_of_slot, max))
  seg_prev <- integer(n_seg)
  seg_stay <- numeric(n_seg)
  seg_group <- integer(n_seg)
  seg_brk_sd <- numeric(n_seg)
  if (nrow(brk)) {
    at <- match(brk$slot, starts)
    seg_prev[at] <- seg_of_slot[brk$prev]
    seg_stay[at] <- as.numeric(brk$rho < 1 | (brk$move == 0 & !is.na(brk$from_div)))
    grp <- ifelse(seg_stay[at] > 0, paste(brk$from_div, brk$from_season), NA)
    seg_group[at] <- ifelse(is.na(grp), 0L, match(grp, unique(stats::na.omit(grp))))
    seg_brk_sd[at] <- sqrt(brk$delta)
  }
  season <- sl$season[starts]
  seg_order <- order(season, seq_len(n_seg))
  waves <- sort(unique(season))
  pos <- match(waves, season[seg_order])
  step_in <- numeric(spec$S)
  step_in[st$slot[st$class == "in"]] <- sqrt(st$delta[st$class == "in"])
  list(
    n_seg = n_seg, seg_of_slot = seg_of_slot, seg_first = starts, seg_last = seg_last,
    seg_team = sl$team[starts], seg_prev = seg_prev, seg_stay = seg_stay,
    seg_group = seg_group, n_group = max(seg_group, 0L), seg_brk_sd = seg_brk_sd,
    seg_order = seg_order, n_wave = length(waves),
    wave_start = pos, wave_end = c(pos[-1] - 1L, n_seg), step_in = step_in
  )
}

#' One state field from its innovations: the R twin of `walk()` in
#' stan/fv2_rev.stan, used to test that the recursion and the precision in
#' `fv2_structure()` describe the same prior.
fv2_walk <- function(seg, lvl, z, z_brk, sigma, kappa, rho) {
  ca <- cumsum(sigma * seg$step_in * z)
  drift <- ca[seg$seg_last] - ca[seg$seg_first]
  start <- finish <- numeric(seg$n_seg)
  for (w in seq_len(seg$n_wave)) {
    segs <- seg$seg_order[seg$wave_start[w]:seg$wave_end[w]]
    gsum <- gcnt <- numeric(max(seg$n_group, 1L))
    for (s in segs[seg$seg_group[segs] > 0]) {
      g <- seg$seg_group[s]
      gsum[g] <- gsum[g] + finish[seg$seg_prev[s]]
      gcnt[g] <- gcnt[g] + 1
    }
    for (s in segs) {
      if (seg$seg_prev[s] == 0L) {
        start[s] <- lvl[seg$seg_team[s]]
      } else {
        prev <- finish[seg$seg_prev[s]]
        jump <- kappa * seg$seg_brk_sd[s] * z_brk[s]
        if (seg$seg_stay[s] > 0) {
          centre <- gsum[seg$seg_group[s]] / gcnt[seg$seg_group[s]]
          start[s] <- centre + rho * (prev - centre) + jump
        } else {
          start[s] <- prev + jump
        }
      }
      finish[s] <- start[s] + drift[s]
    }
  }
  start[seg$seg_of_slot] + ca - ca[seg$seg_first[seg$seg_of_slot]]
}

#' Predictive Poisson rates for the fixtures in `spec$pred`, from posterior
#' draws in the engine-neutral layout (draws in rows).
fv2_predict <- function(draws, spec) {
  m <- spec$pred
  eta_home <- draws$c_home + draws$att[, m$home_slot, drop = FALSE] +
    draws$ha_att[, m$home_team, drop = FALSE] - draws$def[, m$away_slot, drop = FALSE]
  eta_away <- draws$c_away + draws$att[, m$away_slot, drop = FALSE] -
    draws$def[, m$home_slot, drop = FALSE] - draws$ha_def[, m$home_team, drop = FALSE]
  if (!is.null(draws$g_goal)) {
    g <- draws$g_goal[, m$cell_nr, drop = FALSE]
    h <- draws$g_ha[, m$cell_nr, drop = FALSE]
    eta_home <- eta_home + g + 0.5 * h
    eta_away <- eta_away + g - 0.5 * h
  }
  if (!is.null(draws$lvl_goal)) {
    g <- draws$lvl_goal[, m$week_nr, drop = FALSE]
    h <- draws$lvl_ha[, m$week_nr, drop = FALSE]
    eta_home <- eta_home + g + 0.5 * h
    eta_away <- eta_away + g - 0.5 * h
  }
  list(lam_home = t(exp(eta_home)), lam_away = t(exp(eta_away)))
}

#' A league-wide level walk from its innovations, centred on zero over the
#' weeks: the R twin of the level walks in stan/fv2_rev.stan, and the same
#' prior as INLA's `rw1` on the week values with a sum-to-zero constraint.
#'
#' @param week_step sqrt(weeks since the previous week in the grid), 0 first.
fv2_level_walk <- function(week_step, z, sigma) {
  raw <- cumsum(sigma * week_step * z)
  raw - mean(raw)
}

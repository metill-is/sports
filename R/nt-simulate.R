#' @include nt-model.R
NULL

# Knockout ties and tournament simulation for the national-team model.
#
# A knockout tie is played out per simulation: 90 minutes; if level, 30 minutes
# of extra time at `et_factor` times the 90-minute rates; if still level, a
# shootout won with probability `shootout_p`. The production World Cup
# simulator instead split a 90-minute draw in proportion to the two sides'
# 90-minute win probabilities (equivalent to replaying the match until someone
# wins), which hands the stronger side more of the drawn ties than extra time
# and a coin-toss shootout do.

# P(A + lead > B) and P(A + lead == B) for independent Poisson goal counts A
# and B; `lead` is a goal advantage A carries in (an earlier leg's margin).
.nt_poisson_compare <- function(rate_a, rate_b, max_goals = 30L, lead = 0L) {
  win <- draw <- rate_a * 0
  for (g in 0:max_goals) {
    pb <- stats::dpois(g, rate_b)
    win <- win + pb * stats::ppois(g - lead, rate_a, lower.tail = FALSE)
    draw <- draw + pb * stats::dpois(g - lead, rate_a)
  }
  list(win = win, draw = draw)
}

#' Probability that a side advances from a knockout tie.
#'
#' Closed form (Poisson sums) of what [nt_simulate_knockout()] simulates.
#'
#' @param rate_a,rate_b 90-minute scoring rates of the two sides (vectors or
#'   matrices of the same shape).
#' @param method `"extra_time"` (extra time, then shootout) or `"replay"` (a
#'   draw is split in proportion to the 90-minute win probabilities).
#' @param et_factor Extra-time scoring rate as a fraction of the 90-minute
#'   rate (1/3 is the same per-minute rate over 30 minutes).
#' @param shootout_p Probability that side A wins a shootout.
#' @return P(A advances), same shape as the rates.
#' @export
nt_knockout_win_prob <- function(rate_a, rate_b,
                                 method = c("extra_time", "replay"),
                                 et_factor = 1 / 3, shootout_p = 0.5) {
  method <- match.arg(method)
  reg <- .nt_poisson_compare(rate_a, rate_b)
  if (method == "replay") {
    return(reg$win / (1 - reg$draw))
  }
  et <- .nt_poisson_compare(rate_a * et_factor, rate_b * et_factor)
  reg$win + reg$draw * (et$win + et$draw * shootout_p)
}

#' Simulate knockout ties.
#'
#' @inheritParams nt_knockout_win_prob
#' @return List: `a_wins` (logical) and `decided` (1 = in 90 minutes, 2 = in
#'   extra time, 3 = on penalties; always 1 for `method = "replay"`).
#' @export
nt_simulate_knockout <- function(rate_a, rate_b,
                                 method = c("extra_time", "replay"),
                                 et_factor = 1 / 3, shootout_p = 0.5) {
  method <- match.arg(method)
  n <- length(rate_a)
  goals_a <- stats::rpois(n, rate_a)
  goals_b <- stats::rpois(n, rate_b)
  a_wins <- goals_a > goals_b
  decided <- rep(1L, n)
  level <- which(goals_a == goals_b)
  if (method == "replay") {
    while (length(level) > 0L) {
      ga <- stats::rpois(length(level), rate_a[level])
      gb <- stats::rpois(length(level), rate_b[level])
      a_wins[level] <- ga > gb
      level <- level[ga == gb]
    }
    return(list(a_wins = a_wins, decided = decided))
  }
  if (length(level) > 0L) {
    ga <- stats::rpois(length(level), rate_a[level] * et_factor)
    gb <- stats::rpois(length(level), rate_b[level] * et_factor)
    a_wins[level] <- ga > gb
    decided[level] <- 2L
    pens <- level[ga == gb]
    a_wins[pens] <- stats::runif(length(pens)) < shootout_p
    decided[pens] <- 3L
  }
  list(a_wins = a_wins, decided = decided)
}

#' How a two-legged tie ends, in closed form.
#'
#' The side with more goals over the two legs goes through; away goals do not
#' count. Level after the second leg, the tie goes to extra time at the
#' second leg's venue (rates `et_factor` times the second leg's 90-minute
#' rates) and then to a shoot-out won by A with probability `shootout_p`. This
#' is the rule of the UEFA Women's European Qualifiers play-offs for the 2027
#' Women's World Cup (Articles 27.03 and 28.01 of their regulations) and of
#' most UEFA two-legged ties since 2021.
#'
#' Given the rates the four leg scores are independent Poisson counts, so each
#' side's aggregate is Poisson with the summed rate and the 180 minutes reduce
#' to one comparison. Pass per-draw rates (draws x ties matrices from
#' [nt_match_rates()]) and average the result over draws: the legs share the
#' uncertain strengths, so averaging the rates first would be wrong.
#'
#' @param rate_a1,rate_b1 First-leg 90-minute scoring rates of sides A and B.
#' @param rate_a2,rate_b2 Second-leg rates (extra time is played there).
#' @param et_factor,shootout_p See [nt_knockout_win_prob()].
#' @param lead Goals A leads by before the legs given here. To condition on a
#'   played first leg, pass zero first-leg rates and its margin as `lead`.
#' @return List of arrays shaped like the rates: `a_wins` and the six ways the
#'   tie ends, which sum to one: `a_regulation`, `a_extra_time`, `a_shootout`,
#'   `b_regulation`, `b_extra_time`, `b_shootout`.
#' @export
nt_two_leg_win_prob <- function(rate_a1, rate_b1, rate_a2, rate_b2,
                                et_factor = 1 / 3, shootout_p = 0.5, lead = 0L) {
  reg <- .nt_poisson_compare(rate_a1 + rate_a2, rate_b1 + rate_b2, max_goals = 40L, lead = lead)
  et <- .nt_poisson_compare(rate_a2 * et_factor, rate_b2 * et_factor)
  out <- list(
    a_regulation = reg$win,
    a_extra_time = reg$draw * et$win,
    a_shootout = reg$draw * et$draw * shootout_p,
    b_regulation = 1 - reg$win - reg$draw,
    b_extra_time = reg$draw * (1 - et$win - et$draw),
    b_shootout = reg$draw * et$draw * (1 - shootout_p)
  )
  c(list(a_wins = out$a_regulation + out$a_extra_time + out$a_shootout), out)
}

#' Simulate two-legged ties.
#'
#' Plays out what [nt_two_leg_win_prob()] computes: both legs over 90
#' minutes, then, with the aggregate level, extra time at the second leg's
#' rates and a shoot-out.
#'
#' @inheritParams nt_two_leg_win_prob
#' @return List: 90-minute goals `leg1_a`, `leg1_b`, `leg2_a`, `leg2_b`;
#'   `a_wins` (logical); `decided` (1 = on aggregate after 180 minutes, 2 = in
#'   extra time, 3 = on penalties).
#' @export
nt_simulate_two_leg <- function(rate_a1, rate_b1, rate_a2, rate_b2,
                                et_factor = 1 / 3, shootout_p = 0.5) {
  n <- length(rate_a1)
  leg1_a <- stats::rpois(n, rate_a1)
  leg1_b <- stats::rpois(n, rate_b1)
  leg2_a <- stats::rpois(n, rate_a2)
  leg2_b <- stats::rpois(n, rate_b2)
  margin <- leg1_a + leg2_a - leg1_b - leg2_b
  a_wins <- margin > 0
  decided <- rep(1L, n)
  level <- which(margin == 0)
  if (length(level) > 0L) {
    ga <- stats::rpois(length(level), rate_a2[level] * et_factor)
    gb <- stats::rpois(length(level), rate_b2[level] * et_factor)
    a_wins[level] <- ga > gb
    decided[level] <- 2L
    pens <- level[ga == gb]
    a_wins[pens] <- stats::runif(length(pens)) < shootout_p
    decided[pens] <- 3L
  }
  list(
    leg1_a = leg1_a, leg1_b = leg1_b, leg2_a = leg2_a, leg2_b = leg2_b,
    a_wins = a_wins, decided = decided
  )
}

#' Places won from a round of play-off ties ranked on a table.
#'
#' Of the ties' winners the `n_direct` best-ranked take the direct places and
#' the rest the other places (for the 2027 Women's World Cup UEFA play-offs:
#' eight winners, the seven best-ranked on the 2026 Women's European
#' Qualifiers overall league ranking qualify, the last goes to the
#' inter-confederation play-off; Article 27.08). Every combination of results
#' is weighed exactly, per draw, with the ties independent given the draw.
#'
#' @param p_win Draws x ties matrix: P(`team_a` wins its tie) in each draw.
#' @param ties Tibble `team_a, team_b, rank_a, rank_b` (smaller is better),
#'   one row per column of `p_win`.
#' @param n_direct Number of direct places.
#' @return Tibble `team, rank, p_win, p_direct, p_other`, by rank.
#' @export
nt_playoff_places <- function(p_win, ties, n_direct) {
  p_win <- as.matrix(p_win)
  k <- nrow(ties)
  if (ncol(p_win) != k) {
    cli::cli_abort("{.arg p_win} has {ncol(p_win)} column{?s} for {k} tie{?s}.")
  }
  if (k > 16L) {
    cli::cli_abort("{k} ties is too many to enumerate.")
  }
  a_wins <- as.matrix(expand.grid(rep(list(c(TRUE, FALSE)), k)))
  prob <- matrix(1, nrow(p_win), nrow(a_wins))
  for (j in seq_len(k)) {
    prob <- prob * outer(p_win[, j], a_wins[, j], function(p, a) ifelse(a, p, 1 - p))
  }
  winner_rank <- ifelse(a_wins, rep(ties$rank_a, each = nrow(a_wins)),
    rep(ties$rank_b, each = nrow(a_wins)))
  direct <- t(apply(winner_rank, 1L, rank, ties.method = "first")) <= n_direct
  p_combo <- colMeans(prob)
  side <- function(won, place) {
    vapply(seq_len(k), function(j) sum(p_combo[won[, j] & place[, j]]), numeric(1))
  }
  out <- tibble::tibble(
    team = c(ties$team_a, ties$team_b),
    rank = c(ties$rank_a, ties$rank_b),
    p_win = c(colMeans(p_win), 1 - colMeans(p_win)),
    p_direct = c(side(a_wins, direct), side(!a_wins, direct)),
    p_other = c(side(a_wins, !direct), side(!a_wins, !direct))
  )
  out[order(out$rank), , drop = FALSE]
}

#' Head-to-head advancement probabilities at a neutral venue.
#'
#' Shootouts are a coin toss here: a tilted shootout needs a rule for which
#' side of each pair it favours, which a symmetric matrix cannot carry.
#'
#' @param fit A fit from [nt_fit_inla()].
#' @param teams Team names.
#' @param method,et_factor See [nt_knockout_win_prob()].
#' @return Matrix teams x teams; `[a, b]` is P(a advances against b), averaged
#'   over posterior draws, with `[a, b] + [b, a] = 1`.
#' @export
nt_head_to_head <- function(fit, teams, method = c("extra_time", "replay"),
                            et_factor = 1 / 3) {
  method <- match.arg(method)
  d <- fit$draws
  acc <- matrix(0, length(teams), length(teams), dimnames = list(teams, teams))
  for (i in seq_along(d$mu)) {
    th <- d$theta[i, teams]
    dl <- d$delta[i, teams]
    rate <- exp(d$mu[i] + outer(th, th, "-") + outer(dl, dl, "+"))
    acc <- acc + nt_knockout_win_prob(rate, t(rate), method, et_factor, 0.5)
  }
  acc <- acc / length(d$mu)
  diag(acc) <- NA_real_
  acc
}

# Rank simulated World Cup group tables. FIFA World Cup 2026 Regulations,
# Article 13 (https://digitalhub.fifa.com/m/636f5c9c6f29771f/original/FWC2026_regulations_EN.pdf):
# points; then head-to-head points, goal difference and goals among the teams
# level (step 1), re-applied to the teams still level (step 2); then goal
# difference and goals in all group matches. `tiebreak` stands in for team
# conduct and the FIFA ranking. "goal_difference" is the order used before
# 2026 (points, goal difference, goals), kept to reproduce runs made with it.
.nt_wc_rank_group <- function(home, away, teams, goals_h, goals_a, tiebreak,
                              rule = "head_to_head", h2h_reapply = "repeat") {
  x <- .nt_group_arrays(home, away, teams, goals_h, goals_a)
  total <- function(v) apply(v, c(1, 2), sum)
  points <- total(x$points)
  gd <- total(x$gd)
  gf <- total(x$gf)
  rank <- if (rule == "head_to_head") {
    .nt_rank_group_vec(x$points, x$gd, x$gf, list(gd, gf), tiebreak, h2h_reapply)
  } else {
    .nt_lexi_rank(list(points, gd, gf, tiebreak))
  }
  list(rank = rank, points = points, gd = gd, gf = gf)
}

#' Simulate the 48-team World Cup from a national-team fit.
#'
#' Group stage and knockouts are simulated jointly: each simulation uses one
#' posterior draw of the strengths for all 104 matches, so a team's uncertain
#' strength carries through its whole run. Hosts play their group matches at
#' home; knockout matches are neutral.
#'
#' @param fit A fit from [nt_fit_inla()].
#' @param structure Output of [wc_structure()].
#' @param n_sims Number of simulated tournaments.
#' @param knockout How a level knockout match is settled; see
#'   [nt_knockout_win_prob()].
#' @param et_factor,shootout_p Passed to [nt_simulate_knockout()].
#' @param group_rule How teams level on points are ranked within a group:
#'   `"head_to_head"` (FIFA World Cup 2026 Regulations, Article 13: results
#'   between the teams level first, then goal difference and goals in all
#'   group matches) or `"goal_difference"` (the earlier order: goal difference,
#'   then goals). Team conduct and the FIFA ranking are not modelled; a random
#'   draw settles what is still level. Third-placed teams are compared on
#'   points, goal difference and goals under either rule.
#' @param h2h_reapply How often step 2 re-applies the head-to-head criteria to
#'   teams still level: `"repeat"` (until nothing changes) or `"once"`. With
#'   groups of four the two always give the same table: a third pass needs all
#'   four teams level on points, and then the last two can never be separated
#'   by their own match.
#' @param seed Optional seed.
#' @return List: `teams` (tibble: `team`, `group`, group-position and
#'   round-reached probabilities), `r32` (list `a`, `b`: simulations x 16 team
#'   indices of the round-of-32 pairings, in bracket order), `decided`
#'   (share of knockout ties settled in 90 minutes, extra time, on penalties)
#'   and `team_names`.
#' @export
nt_simulate_world_cup <- function(fit, structure, n_sims = 20000L,
                                  knockout = c("extra_time", "replay"),
                                  et_factor = 1 / 3, shootout_p = 0.5,
                                  group_rule = c("head_to_head", "goal_difference"),
                                  h2h_reapply = c("repeat", "once"),
                                  seed = NULL) {
  knockout <- match.arg(knockout)
  group_rule <- match.arg(group_rule)
  h2h_reapply <- match.arg(h2h_reapply)
  if (!is.null(seed)) set.seed(seed)
  teams <- unlist(structure$groups, use.names = FALSE)
  wc_validate_teams(structure, fit$teams)
  nt <- length(teams)
  n_groups <- length(structure$groups)
  group_names <- names(structure$groups)
  d <- fit$draws
  pick <- rep_len(sample.int(length(d$mu)), n_sims)
  th <- d$theta[pick, teams, drop = FALSE]
  dl <- d$delta[pick, teams, drop = FALSE]
  mu <- d$mu[pick]
  home_for <- d$home_for[pick]
  home_against <- d$home_against[pick]
  rates <- function(a, b, venue = 0) {
    ia <- cbind(seq_len(n_sims), a)
    ib <- cbind(seq_len(n_sims), b)
    base <- mu + dl[ia] + dl[ib]
    gap <- th[ia] - th[ib]
    list(
      a = exp(base + gap + home_for * venue),
      b = exp(base - gap + home_against * venue)
    )
  }

  games <- vector("list", n_groups)
  for (g in seq_len(n_groups)) {
    idx <- match(structure$groups[[g]], teams)
    pairs <- utils::combn(idx, 2L)
    home <- away <- integer(ncol(pairs))
    goals_h <- goals_a <- matrix(0L, n_sims, ncol(pairs))
    for (p in seq_len(ncol(pairs))) {
      h <- pairs[1, p]
      a <- pairs[2, p]
      if (teams[a] %in% structure$hosts) {
        h <- pairs[2, p]
        a <- pairs[1, p]
      }
      r <- rates(rep(h, n_sims), rep(a, n_sims), venue = teams[h] %in% structure$hosts)
      goals_h[, p] <- stats::rpois(n_sims, r$a)
      goals_a[, p] <- stats::rpois(n_sims, r$b)
      home[p] <- h
      away[p] <- a
    }
    games[[g]] <- list(home = teams[home], away = teams[away], goals_h = goals_h, goals_a = goals_a)
  }

  finish <- matrix(0L, n_sims, nt)
  first <- second <- third <- matrix(0L, n_sims, n_groups)
  third_key <- matrix(0, n_sims, n_groups)
  for (g in seq_len(n_groups)) {
    tg <- structure$groups[[g]]
    idx <- match(tg, teams)
    tiebreak <- matrix(stats::runif(n_sims * length(tg)), n_sims, length(tg))
    rk <- .nt_wc_rank_group(
      games[[g]]$home, games[[g]]$away, tg, games[[g]]$goals_h, games[[g]]$goals_a,
      tiebreak, group_rule, h2h_reapply
    )
    finish[, idx] <- rk$rank
    first[, g] <- idx[max.col(rk$rank == 1, ties.method = "first")]
    second[, g] <- idx[max.col(rk$rank == 2, ties.method = "first")]
    pos3 <- max.col(rk$rank == 3, ties.method = "first")
    third[, g] <- idx[pos3]
    at3 <- cbind(seq_len(n_sims), pos3)
    third_key[, g] <- rk$points[at3] * 1e6 + (rk$gd[at3] + 500) * 1e3 + rk$gf[at3] +
      tiebreak[at3] * 0.5
  }
  third_rank <- third_key * 0
  for (g in seq_len(n_groups)) third_rank <- third_rank + (third_key[, g] > third_key)
  third_in <- third_rank < 8
  combo <- apply(third_in, 1L, function(q) paste(group_names[q], collapse = ""))
  combos <- unique(combo)
  slots <- as.character(structure$third_slots$match_no)
  alloc <- vapply(combos, function(cb) {
    a <- .wc_allocate_thirds(strsplit(cb, "")[[1]], structure)
    match(unlist(a[slots]), group_names)
  }, integer(length(slots)))
  third_slot_group <- t(alloc[, match(combo, combos), drop = FALSE])
  colnames(third_slot_group) <- slots

  winner <- loser <- list()
  slot_team <- function(label) {
    tag <- substr(label, 1L, 1L)
    if (tag == "W") {
      return(winner[[substring(label, 2L)]])
    }
    if (tag == "L") {
      return(loser[[substring(label, 2L)]])
    }
    if (tag == "3") {
      g <- third_slot_group[, substring(label, 3L)]
      return(third[cbind(seq_len(n_sims), g)])
    }
    g <- match(substr(label, 2L, 2L), group_names)
    if (tag == "1") first[, g] else second[, g]
  }

  rounds <- c("R32", "R16", "QF", "SF", "Final")
  reach <- matrix(0, nt, length(rounds), dimnames = list(teams, rounds))
  decided <- integer(3L)
  r32_a <- r32_b <- matrix(0L, n_sims, sum(structure$bracket$round == "R32"))
  r32_i <- 0L
  play <- function(a, b) {
    r <- rates(a, b)
    res <- nt_simulate_knockout(r$a, r$b, knockout, et_factor, shootout_p)
    decided <<- decided + tabulate(res$decided, nbins = 3L)
    list(winner = ifelse(res$a_wins, a, b), loser = ifelse(res$a_wins, b, a))
  }
  for (i in seq_len(nrow(structure$bracket))) {
    row <- structure$bracket[i, ]
    a <- slot_team(row$feeder_a)
    b <- slot_team(row$feeder_b)
    if (row$round == "R32") {
      r32_i <- r32_i + 1L
      r32_a[, r32_i] <- a
      r32_b[, r32_i] <- b
    }
    reach[, row$round] <- reach[, row$round] +
      tabulate(a, nbins = nt) + tabulate(b, nbins = nt)
    res <- play(a, b)
    winner[[as.character(row$match_no)]] <- res$winner
    loser[[as.character(row$match_no)]] <- res$loser
  }
  final_no <- as.character(structure$bracket$match_no[structure$bracket$round == "Final"])
  champion <- tabulate(winner[[final_no]], nbins = nt)
  bronze <- play(
    slot_team(structure$third_place$feeder_a),
    slot_team(structure$third_place$feeder_b)
  )

  list(
    teams = tibble::tibble(
      team = teams,
      group = unname(structure$group_of[teams]),
      p_first = colMeans(finish == 1), p_second = colMeans(finish == 2),
      p_third = colMeans(finish == 3), p_fourth = colMeans(finish == 4),
      p_advance = reach[, "R32"] / n_sims, p_r16 = reach[, "R16"] / n_sims,
      p_qf = reach[, "QF"] / n_sims, p_sf = reach[, "SF"] / n_sims,
      p_final = reach[, "Final"] / n_sims, p_champion = champion / n_sims,
      p_bronze = tabulate(bronze$winner, nbins = nt) / n_sims
    ),
    r32 = list(a = r32_a, b = r32_b),
    decided = stats::setNames(
      decided / sum(decided), c("regulation", "extra_time", "shootout")
    ),
    team_names = teams
  )
}

#' @include nt-model.R
NULL

# UEFA European Qualifiers: group ranking rules and group-stage simulation.
#
# Source: Regulations of the UEFA European Football Championship 2026-28
# (edition 2026-28, in force from 29 July 2026), read 2026-11-22 at
# https://documents.uefa.com/r/Regulations-of-the-UEFA-European-Football-Championship-2026-28
#   Article 14 Match system - qualifying group stage: 12 groups of four or five,
#     double round robin, 3/1/0 points; the 12 group winners and the 8 best
#     runners-up by Article 23 qualify directly, the other 4 runners-up go to
#     the play-offs (14.03). Two places are reserved for host associations
#     that do not qualify directly (14.04).
#   Article 15 Equality of points - qualifying group stage (15.01):
#     a) points, b) goal difference, c) goals scored, all in the matches
#        among the teams in question;
#     d) if teams are still level, a) to c) are re-applied exclusively to the
#        matches between those teams; if that does not decide,
#     e) goal difference, f) goals scored, g) away goals scored, h) wins,
#     i) away wins, all in all group matches; j) lower disciplinary points;
#     k) higher position in the interim overall 2026/27 Nations League ranking.
#   Article 23 Overall European Qualifiers rankings (23.01): position in the
#     group, then points, goal difference, goals scored, away goals scored,
#     wins, away wins, disciplinary points and Nations League position, where
#     for groups of five the results against the fifth-placed team are not
#     taken into account (fifth-placed teams keep all their matches).
# The Euro 2024 qualifying rules (2022-24 regulations) used the same criteria
# with groups of five and six, discarding results against sixth place; they
# are reproduced in tests/testthat/test-nt-qualifying.R.
#
# Re-application (d): the regulation's wording describes one re-application
# of a) to c), after which e) to k) apply. Published explanations of the
# Euro 2024 qualifying rules instead resume the procedure "from the
# beginning for those teams that were still tied", which repeats it on any
# subset still level. The two readings differ only when four or more teams
# are level on points and the head-to-head criteria split them twice without
# deciding. Every function here takes `h2h_reapply`: "repeat" (default, the
# published reading) or "once" (the literal wording).
#
# The reserved host places (14.04) and the play-offs (Articles 16 and 17) are
# in R/nt-euro2028.R, which takes nt_simulate_qualifying()'s group positions,
# runners-up ranking and per-team Article 23 statistics.

.nt_uefa_overall_criteria <- c(
  e_goal_difference = "gd", f_goals_for = "gf", g_away_goals = "away_gf",
  h_wins = "won", i_away_wins = "away_won"
)

#' Group-stage statistics from played matches.
#'
#' @param matches Data frame with `home_team`, `away_team`, `home_score` and
#'   `away_score`. Rows with a missing score are ignored. The listed home
#'   team counts as the home side for away goals and away wins, also when the
#'   match was moved to a neutral venue.
#' @param teams Teams to report, in any order. Defaults to every team in
#'   `matches`.
#' @return Data frame `team, played, won, drawn, lost, gf, ga, gd, points,
#'   away_gf, away_won`.
#' @export
nt_uefa_group_stats <- function(matches, teams = NULL) {
  m <- matches[!is.na(matches$home_score) & !is.na(matches$away_score), , drop = FALSE]
  if (is.null(teams)) teams <- sort(unique(c(m$home_team, m$away_team)))
  side <- data.frame(
    team = c(m$home_team, m$away_team),
    gf = c(m$home_score, m$away_score),
    ga = c(m$away_score, m$home_score),
    away = rep(c(FALSE, TRUE), each = nrow(m))
  )
  side <- side[side$team %in% teams, , drop = FALSE]
  agg <- function(x) {
    out <- tapply(x, factor(side$team, levels = teams), sum)
    out[is.na(out)] <- 0
    as.vector(out)
  }
  won <- agg(side$gf > side$ga)
  drawn <- agg(side$gf == side$ga)
  lost <- agg(side$gf < side$ga)
  gf <- agg(side$gf)
  ga <- agg(side$ga)
  data.frame(
    team = teams, played = won + drawn + lost, won = won, drawn = drawn,
    lost = lost, gf = gf, ga = ga, gd = gf - ga, points = 3 * won + drawn,
    away_gf = agg(ifelse(side$away, side$gf, 0)),
    away_won = agg(side$away & side$gf > side$ga)
  )
}

# Order `teams` lexicographically by the columns of `keys` (larger is better),
# returning blocks of teams that are level on all of them. `labels` names the
# criterion of each column; each block records the label that separated it
# from the block above.
.nt_order_blocks <- function(teams, keys, labels, first_split) {
  ord <- do.call(order, c(lapply(keys, function(k) -k), list(method = "radix")))
  teams <- teams[ord]
  keys <- keys[ord, , drop = FALSE]
  blocks <- list(list(teams = teams[1], split = first_split))
  for (i in seq_along(teams)[-1]) {
    differs <- which(as.numeric(keys[i, ]) != as.numeric(keys[i - 1L, ]))
    if (length(differs) == 0L) {
      blocks[[length(blocks)]]$teams <- c(blocks[[length(blocks)]]$teams, teams[i])
    } else {
      blocks[[length(blocks) + 1L]] <- list(teams = teams[i], split = labels[differs[1]])
    }
  }
  blocks
}

# Split each block of level teams by one more criterion (larger is better).
# A block that reaches a criterion without a value for every team in it is
# marked stuck: later criteria apply only if this one does not decide, so it
# stays level (unresolved) instead of skipping ahead.
.nt_refine_blocks <- function(blocks, values, label) {
  do.call(c, lapply(blocks, function(b) {
    if (length(b$teams) == 1L || isTRUE(b$stuck)) {
      return(list(b))
    }
    v <- if (is.null(values)) rep(NA_real_, length(b$teams)) else unname(values[b$teams])
    if (anyNA(v)) {
      b$stuck <- TRUE
      return(list(b))
    }
    .nt_order_blocks(b$teams, data.frame(v = v), label, b$split)
  }))
}

# Positions, `decided_by` and `unresolved` from ordered blocks of level teams.
.nt_blocks_to_table <- function(blocks, stats) {
  ordered <- unlist(lapply(blocks, `[[`, "teams"))
  block_id <- rep(seq_along(blocks), lengths(lapply(blocks, `[[`, "teams")))
  split <- vapply(blocks, `[[`, "", "split")
  out <- stats[match(ordered, stats$team), , drop = FALSE]
  nxt <- c(block_id[-1], NA)
  out$unresolved <- !is.na(nxt) & nxt == block_id
  out$decided_by <- ifelse(
    is.na(nxt) | out$unresolved, "", split[pmin(nxt, length(split))]
  )
  rownames(out) <- NULL
  out
}

#' Rank one qualifying group under UEFA's rules.
#'
#' Implements Article 15 of the Euro 2026-28 regulations (see the header of
#' `R/nt-qualifying.R`): points; then points, goal difference and goals in the
#' matches among the teams level on points, re-applied to any subset still
#' level; then goal difference, goals, away goals, wins and away wins in all
#' group matches; then disciplinary points and the Nations League ranking.
#'
#' @param matches Played group matches (see [nt_uefa_group_stats()]).
#' @param teams The group's teams. Defaults to every team in `matches`.
#' @param disciplinary Optional named numeric vector of disciplinary points
#'   (lower ranks higher), used only if criterion j) is reached and every
#'   team still level has a value.
#' @param nl_rank Optional named numeric vector of Nations League overall
#'   positions (lower ranks higher), used the same way for criterion k).
#'   A tie that reaches j) or k) without a value for every team in it is left
#'   unresolved: the next criterion applies only if this one does not decide.
#' @param h2h_reapply How often criterion d) re-applies a) to c): `"repeat"`
#'   (to any subset still level, until nothing splits) or `"once"` (one
#'   re-application, then e) to k)). See the header of `R/nt-qualifying.R`.
#' @return Data frame: the columns of [nt_uefa_group_stats()] plus `position`,
#'   `decided_by` (the criterion that ranks the team above the next one,
#'   empty for the last team) and `unresolved` (`TRUE` when the team is level
#'   with the team below on every criterion available).
#' @export
nt_rank_uefa_group <- function(matches, teams = NULL, disciplinary = NULL,
                               nl_rank = NULL,
                               h2h_reapply = c("repeat", "once")) {
  h2h_reapply <- match.arg(h2h_reapply)
  max_level <- if (h2h_reapply == "once") 2L else Inf
  m <- matches[!is.na(matches$home_score) & !is.na(matches$away_score), , drop = FALSE]
  if (is.null(teams)) teams <- sort(unique(c(m$home_team, m$away_team)))
  stats <- nt_uefa_group_stats(m, teams)
  rownames(stats) <- stats$team
  neg <- function(v) if (is.null(v)) NULL else -v

  overall <- function(set, split) {
    keys <- stats[set, unname(.nt_uefa_overall_criteria), drop = FALSE]
    blocks <- .nt_order_blocks(set, keys, names(.nt_uefa_overall_criteria), split)
    blocks <- .nt_refine_blocks(blocks, neg(disciplinary), "j_disciplinary")
    .nt_refine_blocks(blocks, neg(nl_rank), "k_nations_league_rank")
  }

  head_to_head <- function(set, split, level) {
    if (level > max_level) {
      return(overall(set, split))
    }
    sub <- m[m$home_team %in% set & m$away_team %in% set, , drop = FALSE]
    h <- nt_uefa_group_stats(sub, set)
    keys <- h[, c("points", "gd", "gf")]
    tag <- if (level == 1L) "" else " (re-applied)"
    labels <- paste0(c("a_h2h_points", "b_h2h_goal_difference", "c_h2h_goals_for"), tag)
    blocks <- .nt_order_blocks(set, keys, labels, split)
    if (length(blocks) == 1L) {
      return(overall(set, split))
    }
    do.call(c, lapply(blocks, function(b) {
      if (length(b$teams) == 1L) list(b) else head_to_head(b$teams, b$split, level + 1L)
    }))
  }

  by_points <- .nt_order_blocks(teams, stats[teams, "points", drop = FALSE], "points", "")
  blocks <- do.call(c, lapply(by_points, function(b) {
    if (length(b$teams) == 1L) list(b) else head_to_head(b$teams, b$split, 1L)
  }))

  out <- .nt_blocks_to_table(blocks, stats)
  out$position <- seq_len(nrow(out))
  out[, c("position", setdiff(names(out), "position"))]
}

#' Overall ranking of all teams across qualifying groups.
#'
#' Implements Article 23 of the Euro 2026-28 regulations: teams are ordered by
#' group position, then by points, goal difference, goals scored, away goals,
#' wins and away wins, then disciplinary points and Nations League position,
#' counting only results against teams placed within `counted_positions`
#' (for groups of four and five: results against the fifth-placed team are
#' discarded; for Euro 2024's groups of five and six, those against the
#' sixth). Teams placed below that position keep all their matches.
#'
#' A group whose table is left unresolved (teams level on every criterion
#' available) has no determined positions, so the overall ranking of its
#' teams, and the matches discarded for them, are not determined either:
#' the function warns and flags them in `group_position_unresolved`.
#'
#' @param matches Played matches with a `group` column (see
#'   [nt_uefa_group_stats()]).
#' @param groups Named list of the teams in each group.
#' @param disciplinary Disciplinary points over all group matches, for the
#'   group tables (Article 15 j); see [nt_rank_uefa_group()].
#' @param nl_rank Nations League overall positions, used in the group tables
#'   (Article 15 k) and in this ranking (Article 23).
#' @param disciplinary_overall Named numeric vector of disciplinary points in
#'   the matches this ranking takes into account (Article 23 counts cards
#'   only in those matches, so the values differ from `disciplinary` for a
#'   team whose group had a discarded team).
#' @param counted_positions Results against teams placed below this position
#'   are discarded. Defaults to the size of the smallest group.
#' @param h2h_reapply Passed to [nt_rank_uefa_group()].
#' @return Data frame: `rank`, `team`, `group`, `group_position`,
#'   `group_position_unresolved`, the statistics of the matches taken into
#'   account, `decided_by` and `unresolved` (as in [nt_rank_uefa_group()]).
#' @export
nt_uefa_overall_ranking <- function(matches, groups, disciplinary = NULL,
                                    nl_rank = NULL,
                                    disciplinary_overall = NULL,
                                    counted_positions = NULL,
                                    h2h_reapply = c("repeat", "once")) {
  h2h_reapply <- match.arg(h2h_reapply)
  comparable <- counted_positions %||% min(lengths(groups))
  rows <- lapply(names(groups), function(g) {
    gm <- matches[matches$group == g, , drop = FALSE]
    tab <- nt_rank_uefa_group(gm, groups[[g]], disciplinary, nl_rank, h2h_reapply)
    open <- tab$unresolved | c(FALSE, utils::head(tab$unresolved, -1L))
    if (any(open)) {
      cli::cli_warn(c(
        "Group {g}: {.val {tab$team[open]}} are level on every criterion available.",
        "i" = "Their group positions, and the matches discarded for them, are not determined."
      ))
    }
    keep <- tab$team[tab$position <= comparable]
    do.call(rbind, lapply(seq_len(nrow(tab)), function(i) {
      team <- tab$team[i]
      used <- if (tab$position[i] <= comparable) {
        gm[gm$home_team %in% keep & gm$away_team %in% keep, , drop = FALSE]
      } else {
        gm
      }
      data.frame(
        group = g, group_position = tab$position[i],
        group_position_unresolved = open[i], nt_uefa_group_stats(used, team)
      )
    }))
  })
  tab <- do.call(rbind, rows)
  keys <- data.frame(
    position = -tab$group_position,
    tab[, c("points", "gd", "gf", "away_gf", "won", "away_won")]
  )
  labels <- c(
    "group_position", "points", "goal_difference", "goals_for", "away_goals",
    "wins", "away_wins"
  )
  neg <- function(v) if (is.null(v)) NULL else -v
  blocks <- .nt_order_blocks(tab$team, keys, labels, "")
  blocks <- .nt_refine_blocks(blocks, neg(disciplinary_overall), "disciplinary")
  blocks <- .nt_refine_blocks(blocks, neg(nl_rank), "nations_league_rank")
  out <- .nt_blocks_to_table(blocks, tab)
  out$rank <- seq_len(nrow(out))
  out[, c("rank", "team", setdiff(names(out), c("rank", "team")))]
}

# ---- Vectorised ranking for simulation -------------------------------------

# Lexicographic ranks within each row of a set of n x k key matrices (larger is
# better). Returns an n x k matrix of positions (1 = best); ties after the
# last key share the better position.
.nt_lexi_rank <- function(keys) {
  n <- nrow(keys[[1]])
  k <- ncol(keys[[1]])
  above <- array(FALSE, c(n, k, k))
  open <- array(TRUE, c(n, k, k))
  for (v in keys) {
    vi <- array(v, c(n, k, k))
    vj <- aperm(vi, c(1, 3, 2))
    d <- vj - vi
    hit <- open & d != 0
    above[hit] <- d[hit] > 0
    open <- open & d == 0
  }
  1L + apply(above, c(1, 2), sum)
}

# Rank simulated tables of one group: points, then head-to-head points, goal
# difference and goals among the teams level on everything so far, re-applied
# to the teams still level (UEFA Article 15 a) to d); FIFA World Cup 2026
# Regulations, Article 13 steps 1 and 2). Inputs are n x k x k arrays of what
# team i obtained against team j over their matches (points, goal difference,
# goals); `overall` is a list of n x k matrices applied in order once the
# head-to-head criteria are exhausted; `tiebreak` (n x k, larger is better)
# settles whatever is still level after them. Pass 1 of the loop is a) to c),
# later passes re-apply them: `"repeat"` until nothing splits, `"once"` one
# more pass, `"none"` no re-application (the head-to-head table among all the
# teams level on points, then straight on to `overall`; the literal reading of
# the EHF and IHF handball rules, R/ih-simulate.R).
# `level` (a list of n x k matrices, default the points) is what teams must be
# level on before the head-to-head step: the FIFA Women's World Cup 2023 order
# passes points, goal difference and goals in all group matches.
.nt_rank_group_vec <- function(pts_vs, gd_vs, gf_vs, overall, tiebreak,
                               h2h_reapply = "repeat", level = NULL) {
  n <- dim(pts_vs)[1]
  k <- dim(pts_vs)[2]
  keys <- if (is.null(level)) list(apply(pts_vs, c(1, 2), sum)) else level
  same_class <- function(cls) {
    ci <- array(cls, c(n, k, k))
    eq <- ci == aperm(ci, c(1, 3, 2))
    for (i in seq_len(k)) eq[, i, i] <- FALSE
    eq
  }
  class_of <- function(key_list) {
    lab <- do.call(paste, c(lapply(key_list, function(x) format(x, digits = 15)), sep = "|"))
    matrix(match(lab, unique(lab)), n, k)
  }
  cls <- class_of(keys)
  max_pass <- switch(h2h_reapply,
    none = 1L,
    once = 2L,
    k
  )
  for (pass in seq_len(max_pass)) {
    eq <- same_class(cls)
    h_pts <- apply(eq * pts_vs, c(1, 2), sum)
    h_gd <- apply(eq * gd_vs, c(1, 2), sum)
    h_gf <- apply(eq * gf_vs, c(1, 2), sum)
    new_cls <- class_of(c(keys, list(h_pts, h_gd, h_gf)))
    split <- any(eq & !same_class(new_cls))
    keys <- c(keys, list(h_pts, h_gd, h_gf))
    cls <- new_cls
    if (!split) break
  }
  keys <- c(keys, unname(overall), list(tiebreak))
  .nt_lexi_rank(keys)
}

# Per-pair records of one group across simulations: n x k x k arrays of what
# team i obtained against team j (points, goal difference, goals, away goals,
# wins, away wins), from n x m matrices of home and away goals of the group's
# m fixtures.
.nt_group_arrays <- function(home, away, teams, goals_h, goals_a) {
  n <- nrow(goals_h)
  k <- length(teams)
  arr <- function() array(0, c(n, k, k))
  out <- list(
    points = arr(), gd = arr(), gf = arr(), away_gf = arr(), won = arr(),
    away_won = arr()
  )
  for (r in seq_along(home)) {
    h <- match(home[r], teams)
    a <- match(away[r], teams)
    gh <- goals_h[, r]
    ga <- goals_a[, r]
    out$points[, h, a] <- out$points[, h, a] + 3 * (gh > ga) + (gh == ga)
    out$points[, a, h] <- out$points[, a, h] + 3 * (ga > gh) + (gh == ga)
    out$gd[, h, a] <- out$gd[, h, a] + gh - ga
    out$gd[, a, h] <- out$gd[, a, h] + ga - gh
    out$gf[, h, a] <- out$gf[, h, a] + gh
    out$gf[, a, h] <- out$gf[, a, h] + ga
    out$away_gf[, a, h] <- out$away_gf[, a, h] + ga
    out$won[, h, a] <- out$won[, h, a] + (gh > ga)
    out$won[, a, h] <- out$won[, a, h] + (ga > gh)
    out$away_won[, a, h] <- out$away_won[, a, h] + (ga > gh)
  }
  out
}

# Rank simulated tables of one group from the arrays of .nt_group_arrays().
.nt_rank_group_arrays <- function(x, tiebreak, h2h_reapply = "repeat") {
  total <- function(v) apply(v, c(1, 2), sum)
  overall <- lapply(x[unname(.nt_uefa_overall_criteria)], total)
  .nt_rank_group_vec(x$points, x$gd, x$gf, overall, tiebreak, h2h_reapply)
}

#' Simulate UEFA qualifying groups from a national-team fit.
#'
#' Each simulation uses one posterior draw of the strengths for every match,
#' so a team's uncertain strength carries through its whole group. Played
#' matches keep their results; the rest are simulated as independent Poisson
#' scores with [nt_match_rates()]. Each group is ranked under Article 15 of
#' the Euro 2026-28 regulations and the runners-up are ranked across groups
#' under Article 23, discarding results against teams placed below the size
#' of the smallest group.
#'
#' Groups may hold placeholders for the two sides of a two-legged tie that is
#' played before the qualifiers (the 2026/27 Nations League quarter-finals
#' enter the EURO 2028 draw as "QFk winner" and "QFk defeated"). Each tie is
#' simulated per simulation with [nt_simulate_two_leg()], each side at home
#' in one leg, and the placeholders take its winner and defeated team.
#'
#' Disciplinary points (criterion j) are not modelled: a uniform random draw
#' per team and simulation stands in for them, which settles every tie that
#' reaches j), so k) is never needed.
#'
#' @param fit A fit from [nt_fit_inla()] (or any list with the same `draws`,
#'   `teams` and `kappa` elements).
#' @param groups Named list of character vectors: the teams (or placeholder
#'   slots) of each group.
#' @param fixtures Optional data frame of group matches with `group`,
#'   `home_team`, `away_team`, `home_score`, `away_score` (`NA` for unplayed)
#'   and `venue` (1 when the home side plays at home, 0 at a neutral venue;
#'   default 1), in the labels of `groups`. Each group's fixtures must be its
#'   double round robin: every ordered pair of its teams exactly once
#'   (Article 14.01). `NULL` plays every pair home and away, all unplayed.
#' @param n_sims Number of simulated group stages.
#' @param n_best_runners_up Runners-up that qualify directly.
#' @param counted_positions Results against teams placed below this position
#'   are discarded from the runners-up ranking. Defaults to the size of the
#'   smallest group (4 for Euro 2028: results against fifth place).
#' @param h2h_reapply Passed to [nt_rank_uefa_group()].
#' @param quarter_finals Optional data frame of ties that decide placeholder
#'   slots: `winner_slot`, `defeated_slot` (labels used in `groups`),
#'   `first_leg_home`, `second_leg_home` (teams in the fit). Rows whose slots
#'   are not drawn are ignored.
#' @param et_factor,shootout_p Passed to [nt_simulate_two_leg()].
#' @param seed Optional seed.
#' @return List:
#'   * `teams`: tibble per team (`team`, `group` (for a team behind a
#'     placeholder, the winner's and defeated team's groups as `"A/G"`),
#'     `p_pos1` ... `p_posK`, `p_winner`, `p_runner_up_direct`,
#'     `p_runner_up_other`, `p_qualify_direct`, `exp_points`);
#'   * `slots`: the same per slot of `groups`;
#'   * `positions` (n_sims x teams) and `slot_positions` (n_sims x slots):
#'     group positions;
#'   * `runner_up` (n_sims x groups): each group's runner-up (team);
#'   * `runner_up_rank` (n_sims x groups): its rank among the runners-up;
#'   * `overall`: list of n_sims x teams matrices, each team's Article 23
#'     statistics (`points`, `gd`, `gf`, `away_gf`, `won`, `away_won`) over
#'     the matches that ranking counts for it, plus a uniform `tiebreak`
#'     standing in for disciplinary points;
#'   * `qf_winner` (n_sims x ties, `NULL` without placeholders);
#'   * `draw`: the posterior draw behind each simulation; and `n_sims`.
#'   These are the inputs a play-off and host-place step needs.
#' @export
nt_simulate_qualifying <- function(fit, groups, fixtures = NULL, n_sims = 10000L,
                                   n_best_runners_up = 8L,
                                   counted_positions = NULL,
                                   h2h_reapply = c("repeat", "once"),
                                   quarter_finals = NULL,
                                   et_factor = 1 / 3, shootout_p = 0.5,
                                   seed = NULL) {
  h2h_reapply <- match.arg(h2h_reapply)
  if (!is.null(seed)) set.seed(seed)
  groups <- lapply(groups, as.character)
  slots <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(slots)) cli::cli_abort("A team appears in more than one group.")
  if (is.null(fixtures)) fixtures <- nt_qualifying_fixtures(groups)
  fixtures <- .nt_check_qualifying_fixtures(fixtures, groups)
  if (length(groups) < n_best_runners_up) {
    cli::cli_abort("{n_best_runners_up} best runners-up need at least that many groups.")
  }
  qf <- .nt_check_quarter_finals(quarter_finals, slots)
  placeholders <- c(qf$winner_slot, qf$defeated_slot)
  teams <- c(setdiff(slots, placeholders), as.vector(rbind(qf$first_leg_home, qf$second_leg_home)))
  unknown <- setdiff(teams, fit$teams)
  if (length(unknown) > 0L) cli::cli_abort("Team{?s} not in the fit: {.val {unknown}}.")
  comparable <- counted_positions %||% min(lengths(groups))
  sims <- seq_len(n_sims)

  draw <- sample.int(length(fit$draws$mu), n_sims, replace = TRUE)
  slot_team <- matrix(slots, n_sims, length(slots), byrow = TRUE, dimnames = list(NULL, slots))
  qf_winner <- NULL
  if (nrow(qf) > 0L) {
    qf_winner <- matrix(NA_character_, n_sims, nrow(qf), dimnames = list(NULL, paste0("QF", qf$qf)))
    for (i in seq_len(nrow(qf))) {
      a <- qf$first_leg_home[i]
      b <- qf$second_leg_home[i]
      legs <- function(x) matrix(rep(match(x, fit$teams), each = n_sims), n_sims)
      r <- .nt_sim_rates(fit, draw, legs(c(a, b)), legs(c(b, a)), c(1, 1))
      res <- nt_simulate_two_leg(
        r$home[, 1], r$away[, 1], r$away[, 2], r$home[, 2], et_factor, shootout_p
      )
      qf_winner[, i] <- ifelse(res$a_wins, a, b)
      slot_team[, qf$winner_slot[i]] <- qf_winner[, i]
      slot_team[, qf$defeated_slot[i]] <- ifelse(res$a_wins, b, a)
    }
  }

  open <- which(is.na(fixtures$home_score) | is.na(fixtures$away_score))
  goals_h <- matrix(fixtures$home_score, n_sims, nrow(fixtures), byrow = TRUE)
  goals_a <- matrix(fixtures$away_score, n_sims, nrow(fixtures), byrow = TRUE)
  if (length(open) > 0L) {
    idx <- function(lab) matrix(match(slot_team[, lab, drop = FALSE], fit$teams), n_sims)
    rates <- .nt_sim_rates(
      fit, draw, idx(fixtures$home_team[open]), idx(fixtures$away_team[open]),
      .nt_venue_values(fixtures$venue[open])
    )
    goals_h[, open] <- stats::rpois(length(rates$home), rates$home)
    goals_a[, open] <- stats::rpois(length(rates$away), rates$away)
  }

  crit <- unname(c("points", .nt_uefa_overall_criteria))
  slot_positions <- matrix(0L, n_sims, length(slots), dimnames = list(NULL, slots))
  slot_points <- matrix(0, n_sims, length(slots), dimnames = list(NULL, slots))
  slot_overall <- stats::setNames(rep(list(slot_points), length(crit)), crit)
  ru_keys <- list()
  runner_up_slot <- matrix(NA_character_, n_sims, length(groups), dimnames = list(NULL, names(groups)))
  for (g in names(groups)) {
    tg <- groups[[g]]
    k <- length(tg)
    rows <- which(fixtures$group == g)
    x <- .nt_group_arrays(
      fixtures$home_team[rows], fixtures$away_team[rows], tg,
      goals_h[, rows, drop = FALSE], goals_a[, rows, drop = FALSE]
    )
    pos <- .nt_rank_group_arrays(
      x, matrix(stats::runif(n_sims * k), n_sims, k), h2h_reapply
    )
    slot_positions[, tg] <- pos
    slot_points[, tg] <- apply(x$points, c(1, 2), sum)
    # Article 23: a team placed within `comparable` counts only its matches
    # against teams also placed within it; a team placed below keeps all.
    within <- array(pos <= comparable, c(n_sims, k, k))
    counted <- aperm(within, c(1, 3, 2)) | !within
    for (nm in crit) slot_overall[[nm]][, tg] <- apply(x[[nm]] * counted, c(1, 2), sum)
    second <- max.col(pos == 2L, ties.method = "first")
    at <- cbind(sims, match(tg[second], slots))
    ru_keys[[g]] <- lapply(slot_overall, function(m) m[at])
    runner_up_slot[, g] <- tg[second]
  }

  key_mat <- function(name) {
    matrix(
      vapply(names(groups), function(g) ru_keys[[g]][[name]], numeric(n_sims)),
      nrow = n_sims
    )
  }
  ru_rank <- .nt_lexi_rank(list(
    key_mat("points"), key_mat("gd"), key_mat("gf"), key_mat("away_gf"),
    key_mat("won"), key_mat("away_won"),
    matrix(stats::runif(n_sims * length(groups)), n_sims, length(groups))
  ))
  dimnames(ru_rank) <- list(NULL, names(groups))
  slot_overall$tiebreak <- matrix(
    stats::runif(n_sims * length(slots)), n_sims, length(slots), dimnames = list(NULL, slots)
  )

  # Per team: the column of the slot each team occupies in each simulation.
  slot_of <- matrix(match(teams, slots), n_sims, length(teams), byrow = TRUE, dimnames = list(NULL, teams))
  for (i in seq_len(nrow(qf))) {
    for (t in c(qf$first_leg_home[i], qf$second_leg_home[i])) {
      slot_of[, t] <- match(ifelse(qf_winner[, i] == t, qf$winner_slot[i], qf$defeated_slot[i]), slots)
    }
  }
  by_team <- function(m) {
    out <- m[cbind(rep(sims, length(teams)), c(slot_of))]
    matrix(out, n_sims, length(teams), dimnames = list(NULL, teams))
  }
  runner_up <- matrix(
    slot_team[cbind(rep(sims, length(groups)), c(match(runner_up_slot, slots)))],
    n_sims, length(groups), dimnames = list(NULL, names(groups))
  )
  direct <- ru_rank <= n_best_runners_up
  group_of_slot <- stats::setNames(rep(names(groups), lengths(groups)), slots)
  team_group <- stats::setNames(group_of_slot[teams], teams)
  for (i in seq_len(nrow(qf))) {
    both <- paste(group_of_slot[qf$winner_slot[i]], group_of_slot[qf$defeated_slot[i]], sep = "/")
    team_group[c(qf$first_leg_home[i], qf$second_leg_home[i])] <- both
  }
  max_k <- max(lengths(groups))
  summarise <- function(ids, group, positions, points, holder) {
    pos_cols <- vapply(seq_len(max_k), function(p) colMeans(positions == p), numeric(length(ids)))
    pos_cols <- matrix(pos_cols, ncol = max_k, dimnames = list(ids, paste0("p_pos", seq_len(max_k))))
    # A team sits in one group per simulation, so each row holds it at most once.
    ru_direct <- vapply(ids, function(t) mean(rowSums(holder == t & direct) > 0), numeric(1))
    ru_other <- vapply(ids, function(t) mean(rowSums(holder == t & !direct) > 0), numeric(1))
    tibble::tibble(
      team = ids, group = unname(group),
      tibble::as_tibble(pos_cols),
      p_winner = pos_cols[, "p_pos1"],
      p_runner_up_direct = unname(ru_direct),
      p_runner_up_other = unname(ru_other),
      p_qualify_direct = pos_cols[, "p_pos1"] + unname(ru_direct),
      exp_points = colMeans(points)
    )
  }
  team_tab <- summarise(teams, team_group, by_team(slot_positions), by_team(slot_points), runner_up)
  slot_tab <- summarise(slots, group_of_slot, slot_positions, slot_points, runner_up_slot)
  names(slot_tab)[1] <- "slot"
  list(
    teams = team_tab, slots = slot_tab,
    positions = by_team(slot_positions), slot_positions = slot_positions,
    runner_up = runner_up, runner_up_rank = ru_rank,
    overall = lapply(slot_overall, by_team),
    qf_winner = qf_winner, draw = draw, n_sims = n_sims
  )
}

# Check the ties behind placeholder slots: keep those whose slots are drawn,
# refuse half-drawn ties and quarter-finalists drawn as themselves.
.nt_check_quarter_finals <- function(quarter_finals, slots) {
  empty <- data.frame(
    qf = integer(), winner_slot = character(), defeated_slot = character(),
    first_leg_home = character(), second_leg_home = character(), stringsAsFactors = FALSE
  )
  if (is.null(quarter_finals) || nrow(quarter_finals) == 0L) {
    stray <- grep("^QF[0-9]+ (winner|defeated)$", slots, value = TRUE)
    if (length(stray) > 0L) {
      cli::cli_abort("Placeholder{?s} {.val {stray}} need {.arg quarter_finals}.")
    }
    return(empty)
  }
  need <- c("winner_slot", "defeated_slot", "first_leg_home", "second_leg_home")
  if (!all(need %in% names(quarter_finals))) {
    cli::cli_abort("{.arg quarter_finals} needs columns {.field {need}}.")
  }
  q <- as.data.frame(quarter_finals, stringsAsFactors = FALSE)
  if (is.null(q$qf)) q$qf <- seq_len(nrow(q))
  for (col in need) q[[col]] <- as.character(q[[col]])
  drawn_w <- q$winner_slot %in% slots
  drawn_d <- q$defeated_slot %in% slots
  half <- xor(drawn_w, drawn_d)
  if (any(half)) {
    cli::cli_abort("Only one placeholder of tie{?s} {.val {paste0('QF', q$qf[half])}} is in the groups.")
  }
  q <- q[drawn_w & drawn_d, , drop = FALSE]
  direct <- intersect(c(q$first_leg_home, q$second_leg_home), slots)
  if (length(direct) > 0L) {
    cli::cli_abort("{.val {direct}} {?is/are} drawn directly and behind a placeholder.")
  }
  stray <- setdiff(grep("^QF[0-9]+ (winner|defeated)$", slots, value = TRUE), c(q$winner_slot, q$defeated_slot))
  if (length(stray) > 0L) cli::cli_abort("Placeholder{?s} {.val {stray}} match no tie.")
  q[, c("qf", need)]
}

# Check that the fixtures are each group's double round robin (every ordered
# pair of its teams exactly once) and return them with character team and
# group columns and a venue column.
.nt_check_qualifying_fixtures <- function(fixtures, groups) {
  need <- c("group", "home_team", "away_team", "home_score", "away_score")
  missing <- setdiff(need, names(fixtures))
  if (length(missing) > 0L) {
    cli::cli_abort("Fixtures need the column{?s} {.field {missing}}.")
  }
  for (col in c("group", "home_team", "away_team")) {
    fixtures[[col]] <- as.character(fixtures[[col]])
  }
  if (is.null(fixtures$venue)) fixtures$venue <- 1L
  unknown <- setdiff(unique(fixtures$group), names(groups))
  if (length(unknown) > 0L) {
    cli::cli_abort("Fixtures name group{?s} not in {.arg groups}: {.val {unknown}}.")
  }
  for (g in names(groups)) {
    f <- fixtures[fixtures$group == g, , drop = FALSE]
    want <- nt_qualifying_fixtures(groups[g])
    got_key <- paste(f$home_team, f$away_team, sep = "\r")
    want_key <- paste(want$home_team, want$away_team, sep = "\r")
    problems <- c(
      if (length(setdiff(want_key, got_key)) > 0L) "missing pairings",
      if (anyDuplicated(got_key)) "repeated pairings",
      if (length(setdiff(got_key, want_key)) > 0L) "pairings that are not two different group teams"
    )
    if (length(problems) > 0L) {
      cli::cli_abort(
        "Group {g} fixtures are not its double round robin: {paste(problems, collapse = ', ')}."
      )
    }
  }
  fixtures
}

#' Double round-robin fixtures for qualifying groups.
#'
#' @param groups Named list of the teams in each group.
#' @return Data frame `group, home_team, away_team, home_score, away_score,
#'   venue`, every pair home and away, all unplayed, `venue` 1.
#' @export
nt_qualifying_fixtures <- function(groups) {
  do.call(rbind, lapply(names(groups), function(g) {
    tg <- groups[[g]]
    pairs <- expand.grid(home = tg, away = tg, stringsAsFactors = FALSE)
    pairs <- pairs[pairs$home != pairs$away, , drop = FALSE]
    data.frame(
      group = g, home_team = pairs$home, away_team = pairs$away,
      home_score = NA_integer_, away_score = NA_integer_, venue = 1L
    )
  }))
}

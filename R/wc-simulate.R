#' @include wc-structure.R
NULL

# World Cup tournament simulator.
#
# Group stage: for each posterior draw, play the 12 round-robins (played matches
# fixed, unplayed simulated), rank, pick the 8 best thirds, allocate them to the
# Round-of-32 via the FIFA Annex-C table. This yields, per draw, who fills each
# of the 32 knockout entry slots.
#
# Knockout: a forward "bracket model". From the posterior we form a 48x48
# head-to-head win matrix W[a,b] = P(a beats b) in one neutral knockout match,
# and the R32 slot-occupancy (P each team fills each entry). The bracket is then
# propagated forward round by round. This is light, smooth, and supports
# client-side what-if (pin a winner -> re-propagate). It is the standard
# bracket-independence approximation: each round uses marginal matchup odds, so
# it slightly understates favourites' deep runs vs full-joint conditioning.
#
# Win-probability shortcut: under the trivariate-reduction bivariate Poisson the
# shared component cancels in the margin (g_a - g_b = X1 - X2), so P(a beats b)
# is a Skellam (difference-of-Poissons) probability of the two independent rates
# exp(mu_a), exp(mu_b) and does not depend on the correlation parameter.

# ---- Match-level primitives (group stage) ----------------------------------

.wc_match_lambdas <- function(home, away, venue,
                              off, def, ha_off, ha_def, mlg, amu3, bmu3) {
  off_h <- off[[home]]
  def_h <- def[[home]]
  off_a <- off[[away]]
  def_a <- def[[away]]
  if (identical(venue, "home")) {
    off_h <- off_h + ha_off[[home]]
    def_h <- def_h + ha_def[[home]]
  } else if (identical(venue, "away")) {
    off_a <- off_a + ha_off[[away]]
    def_a <- def_a + ha_def[[away]]
  }
  mu_h <- mlg + off_h - def_a
  mu_a <- mlg + off_a - def_h
  strength_diff <- abs(off_h + def_h - off_a - def_a)
  logit_rho <- amu3 + bmu3 * strength_diff
  mu3 <- stats::plogis(logit_rho, log.p = TRUE) + 0.5 * (mu_h + mu_a)
  c(exp(mu_h), exp(mu_a), exp(mu3))
}

.wc_rbvpois <- function(l) {
  x3 <- stats::rpois(1L, l[3])
  c(stats::rpois(1L, l[1]) + x3, stats::rpois(1L, l[2]) + x3)
}

# Elementwise P(X1 > X2) for X1 ~ Pois(la), X2 ~ Pois(lb); la, lb matrices.
.wc_skellam_pwin <- function(la, lb, kmax = 20L) {
  P <- matrix(0, nrow(la), ncol(la))
  for (k in 0:kmax) P <- P + stats::dpois(k, lb) * (1 - stats::ppois(k, la))
  P
}

# ---- Group table -----------------------------------------------------------

.wc_group_table <- function(group_teams, matches) {
  pts <- stats::setNames(numeric(length(group_teams)), group_teams)
  gf <- pts
  ga <- pts
  for (i in seq_len(nrow(matches))) {
    h <- matches$home_team[i]
    a <- matches$away_team[i]
    hs <- matches$home_score[i]
    as_ <- matches$away_score[i]
    gf[h] <- gf[h] + hs
    ga[h] <- ga[h] + as_
    gf[a] <- gf[a] + as_
    ga[a] <- ga[a] + hs
    if (hs > as_) {
      pts[h] <- pts[h] + 3
    } else if (as_ > hs) {
      pts[a] <- pts[a] + 3
    } else {
      pts[h] <- pts[h] + 1
      pts[a] <- pts[a] + 1
    }
  }
  gd <- gf - ga
  ord <- order(-pts, -gd, -gf, stats::runif(length(group_teams)))
  ranked <- group_teams[ord]
  tibble::tibble(
    team = ranked, rank = seq_along(ranked),
    pts = unname(pts[ranked]), gd = unname(gd[ranked]), gf = unname(gf[ranked])
  )
}

# ---- Best-third allocation -------------------------------------------------

.wc_allocate_thirds <- function(qual_groups, structure) {
  combo <- paste(sort(qual_groups), collapse = "")
  ta <- structure$third_allocation
  if (!is.null(ta)) {
    row <- ta[ta$combo == combo, , drop = FALSE]
    if (nrow(row) == 1L) {
      slots <- as.character(structure$third_slots$match_no)
      return(stats::setNames(
        lapply(slots, function(m) row[[paste0("m", m)]]), slots
      ))
    }
  }
  .wc_bipartite_thirds(qual_groups, structure$third_slots)
}

.wc_bipartite_thirds <- function(qual_groups, third_slots) {
  slots <- as.character(third_slots$match_no)
  elig <- stats::setNames(third_slots$eligible, slots)
  cand <- lapply(slots, function(s) intersect(elig[[s]], qual_groups))
  names(cand) <- slots
  ord <- slots[order(lengths(cand))]
  assignment <- stats::setNames(rep(NA_character_, length(slots)), slots)
  used <- character(0)
  rec <- function(i) {
    if (i > length(ord)) {
      return(TRUE)
    }
    s <- ord[i]
    for (g in cand[[s]]) {
      if (!(g %in% used)) {
        assignment[[s]] <<- g
        used <<- c(used, g)
        if (rec(i + 1L)) {
          return(TRUE)
        }
        used <<- setdiff(used, g)
        assignment[[s]] <<- NA_character_
      }
    }
    FALSE
  }
  if (!rec(1L)) {
    cli::cli_abort("No valid third-placed assignment for combo {paste(sort(qual_groups), collapse = '')}.")
  }
  as.list(assignment)
}

# ---- Forward bracket model -------------------------------------------------

#' Propagate the knockout bracket forward from a win matrix + R32 occupancy.
#'
#' @param W `nt x nt` win matrix; `W[a, b]` = P(a beats b) in one neutral
#'   knockout match (rows/cols indexed by team, 1..nt).
#' @param occ_a,occ_b `16 x nt` matrices; row i = the team-occupancy of slot
#'   a / b of the i-th Round-of-32 match (in `bracket` order).
#' @param bracket The `structure$bracket` tibble (31 rows, ordered R32->Final).
#' @param teams Character vector of team names (length nt).
#' @param pins Optional named list `match_no -> team index` forcing a winner.
#' @return List: `placement` (tibble team/round_name/probability, cumulative),
#'   `reach` (per-round occupancy), `winner` (per-match winner distribution).
#' @export
wc_forward_bracket <- function(W, occ_a, occ_b, bracket, teams, pins = list()) {
  nt <- length(teams)
  Wm <- W
  diag(Wm) <- 0
  rounds <- c("R32", "R16", "QF", "SF", "Final")
  reach <- stats::setNames(lapply(rounds, function(.) numeric(nt)), rounds)
  winner <- vector("list", max(bracket$match_no))
  r32_i <- 0L
  for (i in seq_len(nrow(bracket))) {
    m <- bracket$match_no[i]
    r <- bracket$round[i]
    if (r == "R32") {
      r32_i <- r32_i + 1L
      dA <- occ_a[r32_i, ]
      dB <- occ_b[r32_i, ]
    } else {
      dA <- winner[[as.integer(sub("W", "", bracket$feeder_a[i]))]]
      dB <- winner[[as.integer(sub("W", "", bracket$feeder_b[i]))]]
    }
    reach[[r]] <- reach[[r]] + dA + dB
    # P(t wins) = P(t in A)*P(beats B's team) + P(t in B)*P(beats A's team),
    # diag(Wm)=0 excludes the impossible t-vs-t. The two feeder distributions can
    # overlap (a team can reach a deep match from either half), so the excluded
    # self-play mass leaves a leak; renormalise to redistribute it (standard
    # bracket-independence treatment).
    wd <- dA * as.vector(Wm %*% dB) + dB * as.vector(Wm %*% dA)
    sw <- sum(wd)
    if (sw > 0) wd <- wd / sw
    pin <- pins[[as.character(m)]]
    if (!is.null(pin)) {
      wd <- numeric(nt)
      wd[pin] <- 1
    }
    winner[[m]] <- wd
  }
  final_m <- bracket$match_no[bracket$round == "Final"]
  champ <- winner[[final_m]]

  placement <- do.call(rbind, c(
    lapply(rounds, function(r) {
      tibble::tibble(team = teams, round_name = r, probability = reach[[r]])
    }),
    list(tibble::tibble(team = teams, round_name = "Champion", probability = champ))
  ))
  placement$round_name <- factor(placement$round_name,
    levels = c(rounds, "Champion")
  )
  placement <- placement[order(placement$team, placement$round_name), ]
  list(placement = tibble::as_tibble(placement), reach = reach, winner = winner)
}

# ---- One-draw group + R32 seeding ------------------------------------------

.wc_sim_one_draw <- function(off, def, ha_off, ha_def, mlg, amu3, bmu3, ctx) {
  fx <- ctx$fixtures
  nf <- nrow(fx)
  lh <- numeric(nf)
  lg <- numeric(nf)
  mh <- integer(nf)
  mg <- integer(nf)
  gh <- integer(nf)
  ga <- integer(nf)
  for (i in seq_len(nf)) {
    l <- .wc_match_lambdas(
      fx$home_team[i], fx$away_team[i], fx$venue[i],
      off, def, ha_off, ha_def, mlg, amu3, bmu3
    )
    lh[i] <- l[1]
    lg[i] <- l[2]
    s <- .wc_rbvpois(l)
    mh[i] <- s[1]
    mg[i] <- s[2]
    if (fx$played[i]) {
      gh[i] <- fx$home_score[i]
      ga[i] <- fx$away_score[i]
    } else {
      gh[i] <- s[1]
      ga[i] <- s[2]
    }
  }

  teams <- ctx$teams
  finish <- stats::setNames(integer(length(teams)), teams)
  pts <- stats::setNames(numeric(length(teams)), teams)
  tgf <- pts
  tga <- pts
  winners <- character(0)
  runners <- character(0)
  third_rows <- vector("list", length(ctx$groups))
  names(third_rows) <- names(ctx$groups)
  for (g in names(ctx$groups)) {
    sel <- ctx$fix_group_idx[[g]]
    gm <- tibble::tibble(
      home_team = fx$home_team[sel], away_team = fx$away_team[sel],
      home_score = gh[sel], away_score = ga[sel]
    )
    tbl <- .wc_group_table(ctx$groups[[g]], gm)
    finish[tbl$team] <- tbl$rank
    pts[tbl$team] <- tbl$pts
    tgf[tbl$team] <- tbl$gf
    tga[tbl$team] <- tbl$gf - tbl$gd
    winners[[g]] <- tbl$team[1]
    runners[[g]] <- tbl$team[2]
    tr <- tbl[3, , drop = FALSE]
    tr$group <- g
    third_rows[[g]] <- tr
  }

  thirds_df <- do.call(rbind, third_rows)
  ord <- order(-thirds_df$pts, -thirds_df$gd, -thirds_df$gf, stats::runif(nrow(thirds_df)))
  thirds_df <- thirds_df[ord, , drop = FALSE]
  qualifying <- thirds_df[1:8, , drop = FALSE]
  third_team_of_group <- stats::setNames(thirds_df$team, thirds_df$group)
  alloc <- .wc_allocate_thirds(qualifying$group, structure = list(
    third_allocation = ctx$third_allocation, third_slots = ctx$third_slots
  ))
  resolve_slot <- function(slot) {
    tag <- substr(slot, 1L, 1L)
    if (tag == "1") {
      winners[[substr(slot, 2L, 2L)]]
    } else if (tag == "2") {
      runners[[substr(slot, 2L, 2L)]]
    } else {
      third_team_of_group[[alloc[[substr(slot, 3L, nchar(slot))]]]]
    }
  }

  # Resolve the 16 Round-of-32 matchups (bracket rows where round == "R32").
  r32 <- ctx$bracket[ctx$bracket$round == "R32", , drop = FALSE]
  r32_a <- vapply(r32$feeder_a, function(f) ctx$tidx[[resolve_slot(f)]], integer(1))
  r32_b <- vapply(r32$feeder_b, function(f) ctx$tidx[[resolve_slot(f)]], integer(1))

  list(
    finish = finish[teams], pts = pts[teams], gf = tgf[teams], ga = tga[teams],
    lh = lh, lg = lg, mh = mh, mg = mg,
    r32_a = unname(r32_a), r32_b = unname(r32_b),
    off = unname(off[teams]), def = unname(def[teams])
  )
}

# ---- Public API ------------------------------------------------------------

#' Build the 72 group-stage fixtures from the facts store.
#'
#' @param structure Output of [wc_structure()].
#' @param root Data root.
#' @return Tibble: `group`, `home_team`, `away_team`, `home_score`,
#'   `away_score`, `played`, `venue`, `match_date`.
#' @export
wc_group_fixtures <- function(structure, root = here::here("data")) {
  flt <- list(sport = "football", country = "world", sex = "male")
  res <- read_table("results", root = root, filter = flt)
  res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
  sch <- read_table("schedules", root = root, filter = flt)
  sch <- sch[sch$division == "FIFA World Cup" & sch$season == 2026L, , drop = FALSE]

  played <- tibble::tibble(
    match_date = res$match_date, home_team = res$home_team, away_team = res$away_team,
    home_score = res$home_score, away_score = res$away_score, played = TRUE
  )
  upcoming <- tibble::tibble(
    match_date = sch$match_date, home_team = sch$home_team, away_team = sch$away_team,
    home_score = NA_integer_, away_score = NA_integer_, played = FALSE
  )
  fx <- rbind(played, upcoming)
  fx$group <- structure$group_of[fx$home_team]
  away_group <- structure$group_of[fx$away_team]
  fx <- fx[!is.na(fx$group) & !is.na(away_group) & fx$group == away_group, , drop = FALSE]
  fx$venue <- ifelse(fx$home_team %in% structure$hosts, "home",
    ifelse(fx$away_team %in% structure$hosts, "away", "neutral")
  )
  fx <- fx[order(fx$match_date, fx$group), , drop = FALSE]
  fx
}

# Knockout round name from the number of knockout matches already played.
# 16 R32 + 8 R16 + 4 QF + 2 SF + 1 Final = 31; cumulative thresholds.
.wc_knockout_round_of <- function(n_played) {
  if (n_played < 16L) {
    "R32"
  } else if (n_played < 24L) {
    "R16"
  } else if (n_played < 28L) {
    "QF"
  } else if (n_played < 30L) {
    "SF"
  } else {
    "Final"
  }
}

# Pure core of [wc_knockout_fixtures()]: from schedule (upcoming) and results
# (played) fixture tibbles, return the scheduled-but-unplayed *knockout* fixtures
# with a `round` label. A knockout fixture is any pairing that is not one of the
# 72 within-group pairings (and whose teams are both known). The round is derived
# from the number of knockout matches already in `results` — so it rolls forward
# (R32 -> R16 -> ...) for free as martj42 fills each round into the schedule.
.wc_knockout_fixtures_from <- function(schedule, results, structure) {
  go <- structure$group_of
  group_pairs <- character(0)
  for (g in names(structure$groups)) {
    cmb <- utils::combn(structure$groups[[g]], 2L)
    group_pairs <- c(group_pairs, .wc_pair_key(cmb[1, ], cmb[2, ]))
  }
  is_knockout <- function(home, away) {
    !is.na(go[home]) & !is.na(go[away]) &
      !(.wc_pair_key(home, away) %in% group_pairs)
  }
  n_ko_played <- if (nrow(results) > 0L) {
    sum(is_knockout(results$home_team, results$away_team))
  } else {
    0L
  }
  round_name <- .wc_knockout_round_of(n_ko_played)

  empty <- tibble::tibble(
    round = character(0), match_date = as.Date(character(0)),
    home_team = character(0), away_team = character(0),
    played = logical(0), venue = character(0)
  )
  if (nrow(schedule) == 0L) {
    return(empty)
  }
  keep <- is_knockout(schedule$home_team, schedule$away_team)
  played_keys <- if (nrow(results) > 0L) {
    .wc_pair_key(results$home_team, results$away_team)
  } else {
    character(0)
  }
  keep <- keep & !(.wc_pair_key(schedule$home_team, schedule$away_team) %in% played_keys)
  k <- schedule[keep, , drop = FALSE]
  if (nrow(k) == 0L) {
    return(empty)
  }
  tibble::tibble(
    round = rep(round_name, nrow(k)),
    match_date = k$match_date,
    home_team = k$home_team, away_team = k$away_team,
    played = FALSE, venue = "neutral"
  )
}

#' Build the upcoming knockout-stage fixtures from the facts store.
#'
#' The mirror of [wc_group_fixtures()] for the knockout rounds: the schedule's
#' cross-group fixtures with both teams known and not yet played, labelled by
#' round. Empty during the group stage (no knockout teams known yet).
#'
#' @param structure Output of [wc_structure()].
#' @param root Data root.
#' @return Tibble: `round`, `match_date`, `home_team`, `away_team`, `played`
#'   (always FALSE), `venue` (always "neutral").
#' @export
wc_knockout_fixtures <- function(structure, root = here::here("data")) {
  flt <- list(sport = "football", country = "world", sex = "male")
  res <- read_table("results", root = root, filter = flt)
  res <- res[res$division == "FIFA World Cup" & res$season == 2026L, , drop = FALSE]
  sch <- read_table("schedules", root = root, filter = flt)
  sch <- sch[sch$division == "FIFA World Cup" & sch$season == 2026L, , drop = FALSE]
  .wc_knockout_fixtures_from(sch, res, structure)
}

# Shared fixture-card row builder. From per-fixture posterior score matrices
# (`mh`/`mg` integer goals, `lh`/`lg` expected goals; all `draws x fixtures`)
# build the league next_games card contract. Used for BOTH group and knockout
# fixtures so the platform/reels render the two identically; callers add their
# own identity column (`group` or `round`/`p_advance`).
.wc_prediction_rows <- function(meta, mh, mg, lh, lg, nd) {
  ff <- seq_len(ncol(mh))
  tibble::tibble(
    match_date = meta$match_date,
    home = meta$home, away = meta$away,
    p_home = colMeans(mh > mg),
    p_draw = colMeans(mh == mg),
    p_away = colMeans(mh < mg),
    eg_home = colMeans(lh),
    eg_away = colMeans(lg),
    goal_diff_distribution = lapply(ff, function(f) {
      tb <- table(mh[, f] - mg[, f])
      tibble::tibble(diff = as.integer(names(tb)), p = as.numeric(tb) / nd)
    }),
    home_goal_distribution = lapply(ff, function(f) {
      tb <- table(mh[, f])
      tibble::tibble(goals = as.integer(names(tb)), p = as.numeric(tb) / nd)
    }),
    away_goal_distribution = lapply(ff, function(f) {
      tb <- table(mg[, f])
      tibble::tibble(goals = as.integer(names(tb)), p = as.numeric(tb) / nd)
    }),
    total_goal_distribution = lapply(ff, function(f) {
      tb <- table(mh[, f] + mg[, f])
      tibble::tibble(total = as.integer(names(tb)), p = as.numeric(tb) / nd)
    })
  )
}

#' Simulate the World Cup across posterior draws.
#'
#' @param sim_inputs_team Tibble with `team`, `.draw`, `cur_offense`,
#'   `cur_defense`, `home_advantage_off`, `home_advantage_def`.
#' @param sim_inputs_scalar Tibble with `.draw`, `mean_log_goals`, `alpha_mu3`,
#'   `beta_mu3_strength_diff`.
#' @param group_fixtures Output of [wc_group_fixtures()].
#' @param structure Output of [wc_structure()].
#' @param pairing_seed Optional integer for reproducibility.
#' @param knockout_results Optional played-knockout-results tibble from
#'   [wc_knockout_results()]. When supplied, decided matches are pinned so the
#'   forward bracket collapses onto reality (winner advances w.p. 1, loser 0);
#'   the per-match records also surface as `bracket_model$played`. `NULL` (the
#'   default) leaves the forecast unconditioned — its behaviour is unchanged.
#' @param shootout_winners Optional `pair_key -> winner` map from
#'   [wc_shootout_winners()] resolving knockouts level on score.
#' @return List of aggregated views: `group_probs`, `placement_probs` (from the
#'   forward bracket model), `predictions` (per unplayed fixture: 1X2
#'   probabilities, expected goals, and a `goal_diff_distribution` list-column
#'   of P(home - away = d) tibbles — the league next_games contract),
#'   `performance`, `bracket_model` (win matrix + R32 occupancy + structure
#'   for the interactive what-if).
#' @importFrom rlang .data
#' @export
simulate_world_cup <- function(sim_inputs_team, sim_inputs_scalar,
                               group_fixtures, structure, pairing_seed = NULL,
                               knockout_results = NULL, shootout_winners = NULL) {
  if (!is.null(pairing_seed)) set.seed(pairing_seed)

  teams <- unlist(structure$groups, use.names = FALSE)
  tidx <- stats::setNames(seq_along(teams), teams)
  nt <- length(teams)
  ctx <- list(
    teams = teams, tidx = tidx, groups = structure$groups,
    group_of = structure$group_of, fixtures = group_fixtures,
    fix_group_idx = split(seq_len(nrow(group_fixtures)), group_fixtures$group),
    bracket = structure$bracket, third_slots = structure$third_slots,
    third_allocation = structure$third_allocation
  )

  sit <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  draws_team <- split(sit, sit$.draw)
  draws_scalar <- split(sim_inputs_scalar, sim_inputs_scalar$.draw)
  keys <- intersect(names(draws_team), names(draws_scalar))
  if (length(keys) == 0L) cli::cli_abort("simulate_world_cup: no overlapping .draw keys.")
  nd <- length(keys)
  nf <- nrow(group_fixtures)

  finish_m <- matrix(0L, nd, nt)
  pts_m <- matrix(0, nd, nt)
  gf_m <- matrix(0, nd, nt)
  ga_m <- matrix(0, nd, nt)
  lh_m <- matrix(0, nd, nf)
  lg_m <- matrix(0, nd, nf)
  mh_m <- matrix(0L, nd, nf)
  mg_m <- matrix(0L, nd, nf)
  r32a_m <- matrix(0L, nd, 16L)
  r32b_m <- matrix(0L, nd, 16L)
  w_sum <- matrix(0, nt, nt)

  for (i in seq_along(keys)) {
    dt <- draws_team[[keys[i]]]
    off <- stats::setNames(dt$cur_offense, dt$team)
    def <- stats::setNames(dt$cur_defense, dt$team)
    ha_off <- stats::setNames(dt$home_advantage_off, dt$team)
    ha_def <- stats::setNames(dt$home_advantage_def, dt$team)
    ds <- draws_scalar[[keys[i]]]
    r <- .wc_sim_one_draw(
      off, def, ha_off, ha_def,
      ds$mean_log_goals, ds$alpha_mu3, ds$beta_mu3_strength_diff, ctx
    )
    finish_m[i, ] <- r$finish
    pts_m[i, ] <- r$pts
    gf_m[i, ] <- r$gf
    ga_m[i, ] <- r$ga
    lh_m[i, ] <- r$lh
    lg_m[i, ] <- r$lg
    mh_m[i, ] <- r$mh
    mg_m[i, ] <- r$mg
    r32a_m[i, ] <- r$r32_a
    r32b_m[i, ] <- r$r32_b
    # neutral-venue win matrix contribution (Skellam; correlation cancels)
    l1 <- exp(ds$mean_log_goals + outer(r$off, r$def, "-"))
    pw <- .wc_skellam_pwin(l1, t(l1))
    denom <- pw + t(pw)
    cw <- pw / denom
    cw[!is.finite(cw)] <- 0.5
    w_sum <- w_sum + cw
  }
  W <- w_sum / nd

  # R32 slot occupancy (16 matches x nt)
  occ_a <- matrix(0, 16L, nt)
  occ_b <- matrix(0, 16L, nt)
  for (m in 1:16) {
    occ_a[m, ] <- tabulate(r32a_m[, m], nbins = nt) / nd
    occ_b[m, ] <- tabulate(r32b_m[, m], nbins = nt) / nd
  }

  # Condition on played knockout results: pin each decided match's winner so the
  # forward bracket collapses onto reality. Empty pins (no results / group stage)
  # leave the call identical to before.
  kp <- .wc_knockout_pins(
    structure$bracket, teams, occ_a, occ_b, knockout_results, shootout_winners
  )
  fwd <- wc_forward_bracket(W, occ_a, occ_b, structure$bracket, teams, pins = kp$pins)
  reach_r32 <- fwd$reach$R32

  group_probs <- tibble::tibble(
    team = teams,
    group = unname(structure$group_of[teams]),
    p_first = colMeans(finish_m == 1L), p_second = colMeans(finish_m == 2L),
    p_third = colMeans(finish_m == 3L), p_fourth = colMeans(finish_m == 4L),
    p_advance = reach_r32,
    proj_points = colMeans(pts_m), proj_gf = colMeans(gf_m), proj_ga = colMeans(ga_m)
  )

  # ---- upcoming-match predictions (unplayed fixtures) ----
  # Group cards; knockout cards are produced separately by
  # [wc_knockout_predictions()] (same contract via .wc_prediction_rows) and
  # appended downstream, since their teams come from the schedule, not the
  # group-oriented per-draw matrices above.
  up <- which(!group_fixtures$played)
  predictions <- NULL
  if (length(up) > 0L) {
    meta <- tibble::tibble(
      match_date = group_fixtures$match_date[up],
      home = group_fixtures$home_team[up], away = group_fixtures$away_team[up]
    )
    predictions <- .wc_prediction_rows(
      meta, mh_m[, up, drop = FALSE], mg_m[, up, drop = FALSE],
      lh_m[, up, drop = FALSE], lg_m[, up, drop = FALSE], nd
    )
    predictions$group <- group_fixtures$group[up]
  }

  # ---- played-match performance: xG / xPts vs actual ----
  pl <- which(group_fixtures$played)
  performance <- tibble::tibble(
    team = teams, group = unname(structure$group_of[teams]),
    n_played = 0L, gf = 0, ga = 0, pts = 0, xg_for = 0, xg_against = 0, xpts = 0
  )
  if (length(pl) > 0L) {
    eg_h <- colMeans(lh_m[, pl, drop = FALSE])
    eg_a <- colMeans(lg_m[, pl, drop = FALSE])
    xp_h <- colMeans(3 * (mh_m[, pl, drop = FALSE] > mg_m[, pl, drop = FALSE]) +
      (mh_m[, pl, drop = FALSE] == mg_m[, pl, drop = FALSE]))
    xp_a <- colMeans(3 * (mh_m[, pl, drop = FALSE] < mg_m[, pl, drop = FALSE]) +
      (mh_m[, pl, drop = FALSE] == mg_m[, pl, drop = FALSE]))
    acc <- stats::setNames(
      replicate(nt, list(n = 0L, gf = 0, ga = 0, pts = 0, xgf = 0, xga = 0, xpts = 0),
        simplify = FALSE
      ), teams
    )
    for (j in seq_along(pl)) {
      f <- pl[j]
      h <- group_fixtures$home_team[f]
      a <- group_fixtures$away_team[f]
      hs <- group_fixtures$home_score[f]
      as_ <- group_fixtures$away_score[f]
      hp <- if (hs > as_) 3 else if (hs == as_) 1 else 0
      ap <- if (as_ > hs) 3 else if (hs == as_) 1 else 0
      acc[[h]]$n <- acc[[h]]$n + 1L
      acc[[a]]$n <- acc[[a]]$n + 1L
      acc[[h]]$gf <- acc[[h]]$gf + hs
      acc[[h]]$ga <- acc[[h]]$ga + as_
      acc[[a]]$gf <- acc[[a]]$gf + as_
      acc[[a]]$ga <- acc[[a]]$ga + hs
      acc[[h]]$pts <- acc[[h]]$pts + hp
      acc[[a]]$pts <- acc[[a]]$pts + ap
      acc[[h]]$xgf <- acc[[h]]$xgf + eg_h[j]
      acc[[h]]$xga <- acc[[h]]$xga + eg_a[j]
      acc[[a]]$xgf <- acc[[a]]$xgf + eg_a[j]
      acc[[a]]$xga <- acc[[a]]$xga + eg_h[j]
      acc[[h]]$xpts <- acc[[h]]$xpts + xp_h[j]
      acc[[a]]$xpts <- acc[[a]]$xpts + xp_a[j]
    }
    performance$n_played <- vapply(teams, function(t) acc[[t]]$n, integer(1))
    performance$gf <- vapply(teams, function(t) acc[[t]]$gf, numeric(1))
    performance$ga <- vapply(teams, function(t) acc[[t]]$ga, numeric(1))
    performance$pts <- vapply(teams, function(t) acc[[t]]$pts, numeric(1))
    performance$xg_for <- vapply(teams, function(t) acc[[t]]$xgf, numeric(1))
    performance$xg_against <- vapply(teams, function(t) acc[[t]]$xga, numeric(1))
    performance$xpts <- vapply(teams, function(t) acc[[t]]$xpts, numeric(1))
  }

  bracket_model <- list(
    teams = teams, W = W, occ_a = occ_a, occ_b = occ_b,
    bracket = structure$bracket, played = kp$played
  )

  list(
    group_probs = group_probs,
    placement_probs = fwd$placement,
    predictions = predictions,
    performance = performance,
    bracket_model = bracket_model,
    n_draws = nd
  )
}

#' Predict the upcoming knockout fixtures (per-match cards).
#'
#' For each scheduled-but-unplayed knockout fixture (known teams), simulate the
#' match per posterior draw at a neutral venue and emit the same fixture-card
#' contract as the group predictions, plus `round` and `p_advance` (the
#' tie-resolved progression probability for the home team, taken from the neutral
#' win matrix `W` so the card and the bracket model agree exactly).
#'
#' @param knockout_fixtures Output of [wc_knockout_fixtures()].
#' @param sim_inputs_team,sim_inputs_scalar As for [simulate_world_cup()].
#' @param structure Output of [wc_structure()].
#' @param W The `nt x nt` neutral win matrix (`bracket_model$W`).
#' @return Tibble in the [.wc_prediction_rows()] contract plus `group` (NA),
#'   `round`, and `p_advance`; or `NULL` if there are no knockout fixtures.
#' @export
wc_knockout_predictions <- function(knockout_fixtures, sim_inputs_team,
                                    sim_inputs_scalar, structure, W) {
  if (is.null(knockout_fixtures) || nrow(knockout_fixtures) == 0L) {
    return(NULL)
  }
  teams <- unlist(structure$groups, use.names = FALSE)
  tidx <- stats::setNames(seq_along(teams), teams)

  sit <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  draws_team <- split(sit, sit$.draw)
  draws_scalar <- split(sim_inputs_scalar, sim_inputs_scalar$.draw)
  keys <- intersect(names(draws_team), names(draws_scalar))
  if (length(keys) == 0L) {
    cli::cli_abort("wc_knockout_predictions: no overlapping .draw keys.")
  }
  nd <- length(keys)
  nf <- nrow(knockout_fixtures)

  lh_m <- matrix(0, nd, nf)
  lg_m <- matrix(0, nd, nf)
  mh_m <- matrix(0L, nd, nf)
  mg_m <- matrix(0L, nd, nf)
  for (i in seq_along(keys)) {
    dt <- draws_team[[keys[i]]]
    off <- stats::setNames(dt$cur_offense, dt$team)
    def <- stats::setNames(dt$cur_defense, dt$team)
    ha_off <- stats::setNames(dt$home_advantage_off, dt$team)
    ha_def <- stats::setNames(dt$home_advantage_def, dt$team)
    ds <- draws_scalar[[keys[i]]]
    for (f in seq_len(nf)) {
      l <- .wc_match_lambdas(
        knockout_fixtures$home_team[f], knockout_fixtures$away_team[f],
        knockout_fixtures$venue[f], off, def, ha_off, ha_def,
        ds$mean_log_goals, ds$alpha_mu3, ds$beta_mu3_strength_diff
      )
      lh_m[i, f] <- l[1]
      lg_m[i, f] <- l[2]
      s <- .wc_rbvpois(l)
      mh_m[i, f] <- s[1]
      mg_m[i, f] <- s[2]
    }
  }

  meta <- tibble::tibble(
    match_date = knockout_fixtures$match_date,
    home = knockout_fixtures$home_team, away = knockout_fixtures$away_team
  )
  rows <- .wc_prediction_rows(meta, mh_m, mg_m, lh_m, lg_m, nd)
  rows$group <- NA_character_
  rows$round <- knockout_fixtures$round
  # p_advance = P(home advances). W is the neutral knockout win matrix with draws
  # already redistributed (W[i, j] + W[j, i] = 1), i.e. tie-resolved via the
  # ET/penalties shortcut — so it IS the progression probability, and is the same
  # W the bracket model propagates, so card and bracket agree exactly.
  rows$p_advance <- vapply(seq_len(nf), function(f) {
    W[tidx[[knockout_fixtures$home_team[f]]], tidx[[knockout_fixtures$away_team[f]]]]
  }, numeric(1))
  rows
}

# ---- Team-vs-team head-to-head (joint Monte Carlo) -------------------------

#' All-pairs head-to-head progression probabilities
#'
#' The published `placement_probs` (and the page's bracket model) are MARGINAL:
#' [wc_forward_bracket()] is an analytic independence propagation over the
#' *averaged* win matrix, so it cannot answer a JOINT pair question like
#' "P(team A goes farther than team B)". That answer depends on two
#' correlations that only live in the posterior draws: within-draw strength
#' correlation (a draw where A is strong makes A both advance AND beat B
#' head-to-head) and the bracket collision (A can eliminate B).
#'
#' This runs a nested per-draw Monte Carlo — outer loop over posterior strength
#' draws (reusing [.wc_sim_one_draw()] for the *true joint* R32 bracket), inner
#' loop of `k_replays` knockout playthroughs per draw using THIS draw's win
#' matrix — and accumulates, for every one of the 1128 unordered team pairs:
#' P(i farther), P(tie), P(they meet), P(i wins | meet), and the meeting-round
#' distribution. Deliberately a *separate* pass from [simulate_world_cup()]
#' (re-simulating the group stage) so the production forecast path is untouched;
#' the extra group-stage pass costs ~1.5 min on the daily cron.
#'
#' @param sim_inputs_team,sim_inputs_scalar Per-draw strengths + scalars, as
#'   consumed by [simulate_world_cup()].
#' @param group_fixtures,structure As for [simulate_world_cup()].
#' @param k_replays Knockout playthroughs per posterior draw (default 400).
#' @param pairing_seed Seed for the group-stage tiebreak + third allocation RNG.
#' @return A list: `teams` (English, index order), `n_draws`, `k_replays`,
#'   `pairs` (1128 rows, 0-based `i < j`), `team_marginals` (48x7 furthest-round
#'   pmf, levels 0 group-exit .. 6 champion). Shape it for publishing with
#'   [wc_head_to_head_payload()].
#' @export
wc_head_to_head <- function(sim_inputs_team, sim_inputs_scalar, group_fixtures,
                            structure, k_replays = 400L, pairing_seed = 2026L) {
  if (!is.null(pairing_seed)) set.seed(pairing_seed)
  K <- as.integer(k_replays)

  teams <- unlist(structure$groups, use.names = FALSE)
  tidx <- stats::setNames(seq_along(teams), teams)
  nt <- length(teams)
  ctx <- list(
    teams = teams, tidx = tidx, groups = structure$groups,
    group_of = structure$group_of, fixtures = group_fixtures,
    fix_group_idx = split(seq_len(nrow(group_fixtures)), group_fixtures$group),
    bracket = structure$bracket, third_slots = structure$third_slots,
    third_allocation = structure$third_allocation
  )

  br <- structure$bracket
  round_level <- c(R32 = 1L, R16 = 2L, QF = 3L, SF = 4L, Final = 5L)
  r32_rows <- which(br$round == "R32")
  max_m <- max(br$match_no)
  final_m <- br$match_no[br$round == "Final"]

  sit <- sim_inputs_team[sim_inputs_team$team %in% teams, , drop = FALSE]
  draws_team <- split(sit, sit$.draw)
  draws_scalar <- split(sim_inputs_scalar, sim_inputs_scalar$.draw)
  keys <- intersect(names(draws_team), names(draws_scalar))
  if (length(keys) == 0L) cli::cli_abort("wc_head_to_head: no overlapping .draw keys.")
  nd <- length(keys)
  N <- nd * K

  cnt_farther <- matrix(0, nt, nt) # [i,j] = #replays exit_i > exit_j
  cnt_iwin <- matrix(0, nt, nt) # [i,j] = #replays i knocked j out (any round)
  cnt_iwin_r <- replicate(5L, matrix(0, nt, nt), simplify = FALSE) # by round 1..5
  cnt_exit <- matrix(0, nt, 7L) # [i, level+1] = #replays exit_i == level (0..6)
  kidx <- seq_len(K)

  for (d in seq_along(keys)) {
    dt <- draws_team[[keys[d]]]
    off <- stats::setNames(dt$cur_offense, dt$team)
    def <- stats::setNames(dt$cur_defense, dt$team)
    ha_off <- stats::setNames(dt$home_advantage_off, dt$team)
    ha_def <- stats::setNames(dt$home_advantage_def, dt$team)
    ds <- draws_scalar[[keys[d]]]
    r <- .wc_sim_one_draw(
      off, def, ha_off, ha_def,
      ds$mean_log_goals, ds$alpha_mu3, ds$beta_mu3_strength_diff, ctx
    )

    # per-draw neutral-venue conditional (decisive) win matrix — same recipe as
    # simulate_world_cup() but kept PER DRAW (not averaged), so within-draw
    # strength correlation is preserved.
    l1 <- exp(ds$mean_log_goals + outer(r$off, r$def, "-"))
    pw <- .wc_skellam_pwin(l1, t(l1))
    cw <- pw / (pw + t(pw))
    cw[!is.finite(cw)] <- 0.5

    # K-replay knockout for THIS draw's fixed R32 bracket.
    E <- matrix(0L, K, nt) # furthest round reached (0 = group exit)
    winner <- vector("list", max_m)
    for (rr in seq_len(nrow(br))) {
      m <- br$match_no[rr]
      rd <- br$round[rr]
      if (rd == "R32") {
        k32 <- match(rr, r32_rows)
        A <- rep.int(r$r32_a[k32], K)
        B <- rep.int(r$r32_b[k32], K)
      } else {
        A <- winner[[as.integer(sub("W", "", br$feeder_a[rr]))]]
        B <- winner[[as.integer(sub("W", "", br$feeder_b[rr]))]]
      }
      a_wins <- stats::runif(K) < cw[cbind(A, B)]
      w <- ifelse(a_wins, A, B)
      l <- ifelse(a_wins, B, A)
      winner[[m]] <- w
      lvl <- round_level[[rd]]
      E[cbind(kidx, l)] <- lvl
      # winner-vs-loser counts for this round (one-hot crossprod, 48x48)
      ow <- matrix(0, K, nt)
      ow[cbind(kidx, w)] <- 1
      ol <- matrix(0, K, nt)
      ol[cbind(kidx, l)] <- 1
      mm <- crossprod(ow, ol)
      cnt_iwin <- cnt_iwin + mm
      cnt_iwin_r[[lvl]] <- cnt_iwin_r[[lvl]] + mm
    }
    E[cbind(kidx, winner[[final_m]])] <- 6L # champion

    # cnt_farther[i,j] = Σ_k 1(exit_i > exit_j) = Σ_{s=0..5} #(exit_i>s & exit_j==s)
    for (s in 0:5) cnt_farther <- cnt_farther + crossprod(E > s, E == s)
    for (lv in 0:6) cnt_exit[, lv + 1L] <- cnt_exit[, lv + 1L] + colSums(E == lv)
  }

  # ---- shape per-pair output (1128 unordered pairs, i < j, 0-based) ----
  pairs <- vector("list", nt * (nt - 1L) / 2L)
  p <- 0L
  for (i in seq_len(nt - 1L)) {
    for (j in (i + 1L):nt) {
      cf_ij <- cnt_farther[i, j]
      cf_ji <- cnt_farther[j, i]
      meet <- cnt_iwin[i, j] + cnt_iwin[j, i]
      mbr <- vapply(1:5, function(lv) {
        (cnt_iwin_r[[lv]][i, j] + cnt_iwin_r[[lv]][j, i]) / N
      }, numeric(1))
      p <- p + 1L
      pairs[[p]] <- list(
        i = i - 1L, j = j - 1L,
        p_i_farther = round(cf_ij / N, 4),
        p_tie = round((N - cf_ij - cf_ji) / N, 4),
        p_meet = round(meet / N, 4),
        p_i_wins_if_meet = if (meet > 0) round(cnt_iwin[i, j] / meet, 4) else NULL,
        meet_by_round = list(
          R32 = round(mbr[1], 4), R16 = round(mbr[2], 4), QF = round(mbr[3], 4),
          SF = round(mbr[4], 4), Final = round(mbr[5], 4)
        )
      )
    }
  }

  team_marginals <- lapply(seq_len(nt), function(i) round(cnt_exit[i, ] / N, 4))

  list(
    teams = teams, n_draws = nd, k_replays = K,
    pairs = pairs, team_marginals = team_marginals
  )
}

#' Shape a [wc_head_to_head()] result into the published JSON payload
#'
#' Adds the Icelandic display names + timestamps. Shared by
#' [publish_world_cup()] and the local `scripts/wc/gen_head_to_head.R` so the
#' on-disk contract has a single source of truth.
#'
#' @param h2h Output of [wc_head_to_head()].
#' @param is_name Namer fn (English -> Icelandic), e.g. `.wc_country_namer()`.
#' @param generated_at,fit_date Stamps.
#' @return A list ready for `jsonlite::write_json(auto_unbox = TRUE)`.
#' @export
wc_head_to_head_payload <- function(h2h, is_name, generated_at, fit_date) {
  list(
    generated_at = generated_at, fit_date = as.character(fit_date),
    n_draws = h2h$n_draws, k_replays = h2h$k_replays,
    teams = h2h$teams,
    teams_is = unname(vapply(h2h$teams, is_name, character(1))),
    pairs = h2h$pairs,
    team_marginals = h2h$team_marginals
  )
}

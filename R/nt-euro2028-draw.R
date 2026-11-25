#' @include nt-euro2028.R
NULL

# The EURO 2028 qualifying draw of 6 December 2026: conditions and procedure
# from UEFA's draw procedure (pages 3, 5-8) and Article 13.02 of the 2026-28
# regulations (see R/nt-euro2028.R for the sources).
# - Groups A-F have four teams (pots 1-4), G-L five (pots 1-5); pot 5 goes to
#   G-L only.
# - The four quarter-final winner placeholders go to groups of four.
# - Each group of five holds at most one quarter-final defeated placeholder or
#   League A/B or B/C play-off team.
# - Hosts and Northern Ireland: one per group. Winter venues: at most two per
#   group. Prohibited pairs: never together. Excessive-travel pairs: at most
#   one such pair per group. A condition on a quarter-finalist applies to both
#   placeholders of its tie.
# - Pots are emptied in turn; each drawn team goes to the first group in
#   alphabetical order that has no team from its pot yet, keeps every
#   condition and leaves the rest of the draw possible ("the computer
#   calculations need to anticipate all possible scenarios and prevent any
#   deadlock situation").

.nt_draw_groups <- LETTERS[1:12]
.nt_draw_five <- LETTERS[7:12]

# Draw slots (teams and quarter-final placeholders) with what the conditions
# need: pot, allowed groups, flags and pair matrices.
.nt_draw_context <- function(facts) {
  s <- facts$seeding
  q <- facts$quarter_finals
  cond <- facts$conditions
  qf_teams <- c(q$first_leg_home, q$second_leg_home)
  plain <- s[!s$team %in% qf_teams, , drop = FALSE]
  pair <- lapply(seq_len(nrow(q)), function(i) c(q$first_leg_home[i], q$second_leg_home[i]))
  slots <- c(q$winner_slot, q$defeated_slot, plain$team)
  tags <- c(pair, pair, as.list(plain$team))
  pot <- c(rep(1L, 2L * nrow(q)), plain$pot)
  ord <- order(pot, seq_along(pot))
  slots <- slots[ord]
  tags <- tags[ord]
  pot <- pot[ord]
  n <- length(slots)
  qf_winner <- slots %in% q$winner_slot
  po <- plain$team[plain$nl_knockout %in% c("AB_PO", "BC_PO")]
  limited <- slots %in% c(q$defeated_slot, po)
  has_tag <- function(set) vapply(tags, function(t) any(t %in% set), logical(1))
  pair_matrix <- function(rule) {
    m <- matrix(0, n, n, dimnames = list(slots, slots))
    rows <- cond[cond$rule == rule, , drop = FALSE]
    for (r in seq_len(nrow(rows))) {
      a <- which(has_tag(rows$team_a[r]))
      b <- which(has_tag(rows$team_b[r]))
      m[a, b] <- 1
      m[b, a] <- 1
    }
    m
  }
  allowed <- matrix(TRUE, n, 12L, dimnames = list(slots, .nt_draw_groups))
  allowed[pot == 5L, ] <- rep(.nt_draw_groups %in% .nt_draw_five, each = sum(pot == 5L))
  allowed[qf_winner, ] <- rep(!.nt_draw_groups %in% .nt_draw_five, each = sum(qf_winner))
  list(
    slots = slots, tags = tags, pot = pot, qf_winner = qf_winner, limited = limited,
    winter = as.numeric(has_tag(cond$team_a[cond$rule == "winter"])),
    separate = as.numeric(has_tag(facts$separate)),
    prohibited = pair_matrix("prohibited"), travel = pair_matrix("travel"),
    allowed = allowed, five = .nt_draw_groups %in% .nt_draw_five
  )
}

.nt_draw_empty <- function(ctx) {
  list(
    mem = matrix(0, 12L, length(ctx$slots)),
    has_pot = matrix(FALSE, 12L, 5L),
    cnt_w = numeric(12L), cnt_s = numeric(12L), cnt_l = numeric(12L), cnt_t = numeric(12L)
  )
}

# Groups x slots logical: where each slot in `rem` could go next.
.nt_draw_options <- function(st, rem, ctx) {
  ok <- t(ctx$allowed[rem, , drop = FALSE]) & !st$has_pot[, ctx$pot[rem], drop = FALSE]
  ok <- ok & (st$mem %*% ctx$prohibited[, rem, drop = FALSE]) == 0
  ok <- ok & outer(st$cnt_w, ctx$winter[rem], "+") <= 2
  ok <- ok & outer(st$cnt_s, ctx$separate[rem], "+") <= 1
  ok <- ok & (!ctx$five | outer(st$cnt_l, ctx$limited[rem], "+") <= 1)
  ok & (st$cnt_t + st$mem %*% ctx$travel[, rem, drop = FALSE]) <= 1
}

.nt_draw_place <- function(st, u, g, ctx) {
  st$cnt_t[g] <- st$cnt_t[g] + sum(st$mem[g, ] * ctx$travel[, u])
  st$mem[g, u] <- 1
  st$has_pot[g, ctx$pot[u]] <- TRUE
  st$cnt_w[g] <- st$cnt_w[g] + ctx$winter[u]
  st$cnt_s[g] <- st$cnt_s[g] + ctx$separate[u]
  st$cnt_l[g] <- st$cnt_l[g] + ctx$limited[u]
  st
}

# Counting conditions that per-slot options miss: every pot's remaining slots
# need as many groups; the quarter-final winners need free groups of four;
# limited slots that cannot all fit in groups of four need groups of five
# still without one, at most one each.
.nt_draw_counts_ok <- function(st, rem, opts, ctx) {
  for (p in unique(ctx$pot[rem])) {
    in_p <- ctx$pot[rem] == p
    if (sum(rowSums(opts[, in_p, drop = FALSE]) > 0) < sum(in_p)) {
      return(FALSE)
    }
  }
  w <- ctx$qf_winner[rem]
  if (any(w) && sum(rowSums(opts[, w, drop = FALSE]) > 0) < sum(w)) {
    return(FALSE)
  }
  overflow <- 0
  for (p in unique(ctx$pot[rem])) {
    lim <- ctx$pot[rem] == p & ctx$limited[rem]
    if (!any(lim)) next
    free_four <- sum(!ctx$five & !st$has_pot[, p])
    overflow <- overflow + max(0, sum(lim) - free_four)
  }
  overflow <= sum(ctx$five & st$cnt_l == 0)
}

.nt_draw_feasible <- function(st, rem, ctx, budget) {
  if (length(rem) == 0L) {
    return(TRUE)
  }
  opts <- .nt_draw_options(st, rem, ctx)
  n_opt <- colSums(opts)
  if (any(n_opt == 0L) || !.nt_draw_counts_ok(st, rem, opts, ctx)) {
    return(FALSE)
  }
  j <- which.min(n_opt)
  for (g in which(opts[, j])) {
    budget$nodes <- budget$nodes + 1L
    if (budget$nodes > budget$limit) {
      cli::cli_abort("The draw search passed {budget$limit} nodes without settling.")
    }
    if (.nt_draw_feasible(.nt_draw_place(st, rem[j], g, ctx), rem[-j], ctx, budget)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Simulate the EURO 2028 qualifying draw.
#'
#' Draws the 54 entrants (the eight Nations League quarter-finalists as four
#' winner and four defeated placeholders) into groups A to L following UEFA's
#' published procedure: pots 1 to 5 in turn, teams in random order within a
#' pot, each going to the first group in alphabetical order that has no team
#' from its pot, keeps every draw condition and leaves the rest of the draw
#' possible (a depth-first search over the teams still to be drawn). See the
#' header of `R/nt-euro2028-draw.R` for the conditions.
#'
#' @param facts Output of [nt_euro2028_facts()].
#' @param n_draws Number of draws.
#' @param draw_order Optional list of character vectors, the order in which
#'   the slots of pots 1, 2, ... are drawn (each a permutation of its pot);
#'   pots it does not cover are drawn in random order.
#' @param node_limit Search nodes allowed per drawn team before giving up.
#' @param seed Optional seed.
#' @return List of `n_draws` draws, each a named list (A to L) of slot labels
#'   in pot order.
#' @export
nt_simulate_uefa_draw <- function(facts = nt_euro2028_facts(), n_draws = 1L,
                                  draw_order = NULL, node_limit = 200000L, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ctx <- .nt_draw_context(facts)
  lapply(seq_len(n_draws), function(i) .nt_draw_once(ctx, draw_order, node_limit))
}

.nt_draw_once <- function(ctx, draw_order, node_limit) {
  st <- .nt_draw_empty(ctx)
  unplaced <- seq_along(ctx$slots)
  for (p in 1:5) {
    in_pot <- which(ctx$pot == p)
    ord <- if (length(draw_order) >= p) {
      given <- match(draw_order[[p]], ctx$slots)
      if (anyNA(given) || !setequal(given, in_pot) || anyDuplicated(given)) {
        cli::cli_abort("{.arg draw_order} for pot {p} must be a permutation of its slots.")
      }
      given
    } else {
      in_pot[sample.int(length(in_pot))]
    }
    for (u in ord) {
      unplaced <- setdiff(unplaced, u)
      placed <- FALSE
      for (g in which(.nt_draw_options(st, u, ctx)[, 1])) {
        trial <- .nt_draw_place(st, u, g, ctx)
        budget <- new.env()
        budget$nodes <- 0L
        budget$limit <- node_limit
        if (.nt_draw_feasible(trial, unplaced, ctx, budget)) {
          st <- trial
          placed <- TRUE
          break
        }
      }
      if (!placed) cli::cli_abort("No group keeps the draw possible for {.val {ctx$slots[u]}}.")
    }
  }
  out <- lapply(seq_len(12L), function(g) {
    i <- which(st$mem[g, ] == 1)
    ctx$slots[i[order(ctx$pot[i])]]
  })
  stats::setNames(out, .nt_draw_groups)
}

#' Check groups against the EURO 2028 draw conditions.
#'
#' @param groups Named list (A to L) of slot labels, as returned by
#'   [nt_read_euro2028_draw()] or [nt_simulate_uefa_draw()].
#' @param facts Output of [nt_euro2028_facts()].
#' @return Character vector describing each condition broken; empty when the
#'   groups keep them all.
#' @export
nt_check_uefa_draw <- function(groups, facts = nt_euro2028_facts()) {
  ctx <- .nt_draw_context(facts)
  out <- character()
  if (!identical(sort(names(groups)), .nt_draw_groups)) {
    return("The groups are not named A to L.")
  }
  all_slots <- unlist(groups, use.names = FALSE)
  if (!setequal(all_slots, ctx$slots) || anyDuplicated(all_slots)) {
    out <- c(out, "The groups do not hold each of the 54 entrants once.")
  }
  pair_names <- function(m, i) {
    hit <- which(m[i, i, drop = FALSE] > 0 & upper.tri(m[i, i, drop = FALSE]), arr.ind = TRUE)
    paste(ctx$slots[i[hit[, 1]]], ctx$slots[i[hit[, 2]]], sep = " / ")
  }
  for (g in .nt_draw_groups) {
    i <- match(groups[[g]], ctx$slots)
    i <- i[!is.na(i)]
    five <- g %in% .nt_draw_five
    want <- if (five) 1:5 else 1:4
    if (!identical(sort(ctx$pot[i]), want)) {
      out <- c(out, sprintf("Group %s does not hold one team from each of pots %s.", g, paste(range(want), collapse = "-")))
    }
    if (five && any(ctx$qf_winner[i])) {
      out <- c(out, sprintf("%s is in group %s, a group of five.", paste(ctx$slots[i][ctx$qf_winner[i]], collapse = ", "), g))
    }
    if (five && sum(ctx$limited[i]) > 1) {
      out <- c(out, sprintf(
        "Group %s, a group of five, holds more than one play-off team or quarter-final defeated team: %s.",
        g, paste(ctx$slots[i][ctx$limited[i]], collapse = ", ")
      ))
    }
    if (sum(ctx$separate[i]) > 1) {
      out <- c(out, sprintf("Group %s holds more than one host or Northern Ireland: %s.", g, paste(ctx$slots[i][ctx$separate[i] > 0], collapse = ", ")))
    }
    if (sum(ctx$winter[i]) > 2) {
      out <- c(out, sprintf("Group %s holds more than two winter venues: %s.", g, paste(ctx$slots[i][ctx$winter[i] > 0], collapse = ", ")))
    }
    for (p in pair_names(ctx$prohibited, i)) out <- c(out, sprintf("Group %s holds the prohibited pair %s.", g, p))
    travel <- pair_names(ctx$travel, i)
    if (length(travel) > 1) {
      out <- c(out, sprintf("Group %s holds more than one excessive-travel pair: %s.", g, paste(travel, collapse = "; ")))
    }
  }
  out
}

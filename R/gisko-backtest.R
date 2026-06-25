# GISKO retrospective backtest: score the model's leak-free pre-match
# predictions against actual results, as if we had been participating from the
# start. Read-only; never on CI. The per-match predictions come from the frozen
# accountability log (data/wc/accountability/prediction_log.json), whose
# fit_date <= match_date for every match -- no look-ahead.

#' Marginals from a `prediction_log.json` match entry
#'
#' Same `marg` shape as [gisko_marginals_from_pbar()], adapted from the
#' accountability log's `dist_home` / `dist_away` / `dist_diff` list-columns
#' (each a list of `{goals|diff, p}`).
#'
#' @param m One element of `prediction_log.json$matches`.
#' @return A `marg` list.
#' @export
gisko_marginals_from_log <- function(m) {
  pull <- function(dist, key) {
    stats::setNames(
      vapply(dist, function(e) as.numeric(e$p), numeric(1)),
      vapply(dist, function(e) as.numeric(e[[key]]), numeric(1))
    )
  }
  list(
    p_outcome = c(
      home = as.numeric(m$p_home), draw = as.numeric(m$p_draw),
      away = as.numeric(m$p_away)
    ),
    home_pmf = pull(m$dist_home, "goals"),
    away_pmf = pull(m$dist_away, "goals"),
    gd_pmf = pull(m$dist_diff, "diff")
  )
}

# All permutations of 1..n as rows (n! x n). Used for the size-4 group
# assignment; not meant for large n.
.gisko_perms <- function(n) {
  if (n == 1L) {
    return(matrix(1L, 1L, 1L))
  }
  sub <- .gisko_perms(n - 1L)
  do.call(rbind, lapply(seq_len(n), function(i) {
    cbind(i, matrix(ifelse(sub >= i, sub + 1L, sub), nrow(sub)))
  }))
}

#' Optimal group ranking: maximise expected correctly-placed teams
#'
#' GISKO awards 1 point per team placed in its correct final group position.
#' The optimal entry assigns teams to positions to maximise the expected number
#' of correct placements -- a linear assignment on the placement-probability
#' matrix (brute-forced; groups are size 4). The top two of the returned order
#' double as the qualification (reach-R32) prediction.
#'
#' @param p_matrix Numeric matrix, rows = teams (rownamed), cols = positions
#'   `1..n`, with `p_matrix[i, j]` = P(team i finishes j-th).
#' @return Character vector of team names ordered 1st..last.
#' @export
gisko_optimal_group_order <- function(p_matrix) {
  teams <- rownames(p_matrix)
  n <- nrow(p_matrix)
  perms <- .gisko_perms(n)
  scores <- apply(perms, 1L, function(pi) sum(p_matrix[cbind(pi, seq_len(n))]))
  teams[perms[which.max(scores), ]]
}

#' Score played matches under GISKO with a per-round joker
#'
#' For each played match: take the leak-free pre-match marginals, pick the
#' expected-points-optimal scoreline (Lemma 1), and score it against the actual
#' result. Per round, the joker doubles the match with the highest *pre-match*
#' expected points (Lemma 2 -- a leak-free choice); its bonus is that match's
#' realised points.
#'
#' @param played Tibble with `round`, `label`, `marg` (list-column of marginals),
#'   `act_home`, `act_away`.
#' @param max_goals Candidate-grid upper bound.
#' @return list `picks` (per-match tibble), `by_round` (per-round base + joker),
#'   `base_total`, `joker_total`, `grand_total`.
#' @export
gisko_backtest_score <- function(played, max_goals = 8L) {
  rows <- lapply(seq_len(nrow(played)), function(i) {
    opt <- gisko_optimal_scoreline_marginal(played$marg[[i]], max_goals)
    pts <- gisko_match_points(
      opt$home, opt$away, played$act_home[i], played$act_away[i]
    )
    tibble::tibble(
      round = played$round[i], label = played$label[i],
      pick = paste0(opt$home, "-", opt$away),
      actual = paste0(played$act_home[i], "-", played$act_away[i]),
      exp_points = opt$exp_points, points = pts
    )
  })
  picks <- do.call(rbind, rows)
  by_round <- do.call(rbind, lapply(
    split(seq_len(nrow(picks)), picks$round),
    function(idx) {
      sub <- picks[idx, , drop = FALSE]
      j <- which.max(sub$exp_points)
      tibble::tibble(
        round = sub$round[1], n = nrow(sub), base = sum(sub$points),
        joker_match = sub$label[j], joker_bonus = sub$points[j]
      )
    }
  ))
  list(
    picks = picks, by_round = by_round,
    base_total = sum(picks$points),
    joker_total = sum(by_round$joker_bonus),
    grand_total = sum(picks$points) + sum(by_round$joker_bonus)
  )
}

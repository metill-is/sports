# GISKO (gisko.is) optimal-prediction engine. Posterior-first: the substrate is
# a per-draw bivariate-Poisson scoreline matrix from the WC model's own rate
# function. See docs/superpowers/specs/2026-06-25-gisko-optimal-predictions-design.md
#
# Scoreline matrices are oriented rows = home goals, cols = away goals:
# P[h + 1, a + 1] = P(home = h, away = a).

#' GISKO points for one predicted scoreline against an actual result
#'
#' Scoring (rules section 06): 2 for correct outcome, 1 for exact home goals,
#' 1 for exact away goals, 1 for correct goal difference. Max 5; a score of 4
#' is unreachable. Vectorised over equal-length arguments.
#'
#' @param pred_home,pred_away Predicted goals.
#' @param act_home,act_away Actual goals.
#' @return Integer vector of scores in {0,1,2,3,5}.
#' @export
gisko_match_points <- function(pred_home, pred_away, act_home, act_away) {
  outcome <- 2L * (sign(pred_home - pred_away) == sign(act_home - act_away))
  gh <- 1L * (pred_home == act_home)
  ga <- 1L * (pred_away == act_away)
  gd <- 1L * ((pred_home - pred_away) == (act_home - act_away))
  as.integer(outcome + gh + ga + gd)
}

#' Score of a fixed prediction against every actual scoreline on a grid
#'
#' @param pred_home,pred_away Predicted goals.
#' @param max_goals Grid upper bound.
#' @return `(max+1) x (max+1)` integer matrix; `[h+1, a+1]` is the score when
#'   the actual result is home `h`, away `a`.
#' @export
gisko_score_matrix <- function(pred_home, pred_away, max_goals = 8L) {
  g <- 0:max_goals
  ah <- matrix(g, length(g), length(g))
  aa <- matrix(g, length(g), length(g), byrow = TRUE)
  matrix(
    gisko_match_points(pred_home, pred_away, as.vector(ah), as.vector(aa)),
    length(g), length(g)
  )
}

#' Analytic bivariate-Poisson PMF on a goal grid
#'
#' P(H=h, A=a) = sum_k dpois(h-k, l1) dpois(a-k, l2) dpois(k, l3), the
#' trivariate-reduction bivariate Poisson the WC model samples from
#' (`.wc_rbvpois`). Truncated to `0..max_goals` and renormalised. The realised
#' goals are H = Pois(l1) + X3, so E[H] = l1 + l3.
#'
#' @param lambdas `c(lambda_h, lambda_a, lambda3)`.
#' @param max_goals Grid upper bound.
#' @return `(max+1) x (max+1)` matrix; rows = home, cols = away.
#' @noRd
.gisko_bvpois_pmf <- function(lambdas, max_goals = 8L) {
  l1 <- lambdas[1]
  l2 <- lambdas[2]
  l3 <- lambdas[3]
  g <- 0:max_goals
  P <- matrix(0, length(g), length(g))
  for (k in 0:max_goals) {
    pk <- stats::dpois(k, l3)
    if (pk == 0) next
    vh <- ifelse(g - k >= 0, stats::dpois(g - k, l1), 0)
    va <- ifelse(g - k >= 0, stats::dpois(g - k, l2), 0)
    P <- P + pk * outer(vh, va)
  }
  P / sum(P)
}

#' Marginal summaries of an integrated scoreline PMF
#'
#' @param pbar `n x n` matrix, rows = home, cols = away.
#' @return list `p_outcome` (named home/draw/away), `home_pmf`, `away_pmf`
#'   (named by goals), `gd_pmf` (named by signed goal difference).
#' @export
gisko_marginals_from_pbar <- function(pbar) {
  n <- nrow(pbar)
  g <- 0:(n - 1L)
  hh <- matrix(g, n, n)
  aa <- matrix(g, n, n, byrow = TRUE)
  gd <- tapply(as.vector(pbar), as.vector(hh - aa), sum)
  list(
    p_outcome = c(
      home = sum(pbar[lower.tri(pbar)]),
      draw = sum(diag(pbar)),
      away = sum(pbar[upper.tri(pbar)])
    ),
    home_pmf = stats::setNames(rowSums(pbar), g),
    away_pmf = stats::setNames(colSums(pbar), g),
    gd_pmf = stats::setNames(as.numeric(gd), names(gd))
  )
}

#' Expected GISKO points for a predicted scoreline over an integrated PMF
#'
#' @param pred_home,pred_away Predicted goals.
#' @param pbar Integrated scoreline PMF (rows = home, cols = away).
#' @return Scalar expected points.
#' @export
gisko_expected_points <- function(pred_home, pred_away, pbar) {
  sum(pbar * gisko_score_matrix(pred_home, pred_away, nrow(pbar) - 1L))
}

.gisko_lookup <- function(v, nm) {
  x <- v[as.character(nm)]
  if (length(x) == 0L || is.na(x)) 0 else unname(x)
}

#' Expected GISKO points via the marginal (Lemma 1) decomposition
#'
#' Equivalent to [gisko_expected_points()] but takes only marginals -- used to
#' cross-check and to score from `predictions.json` (which ships marginals).
#'
#' @param pred_home,pred_away Predicted goals.
#' @param marg Marginals list as from [gisko_marginals_from_pbar()].
#' @return Scalar expected points.
#' @export
gisko_expected_points_marginal <- function(pred_home, pred_away, marg) {
  out <- if (pred_home > pred_away) {
    "home"
  } else if (pred_home < pred_away) {
    "away"
  } else {
    "draw"
  }
  2 * unname(marg$p_outcome[[out]]) +
    .gisko_lookup(marg$home_pmf, pred_home) +
    .gisko_lookup(marg$away_pmf, pred_away) +
    .gisko_lookup(marg$gd_pmf, pred_home - pred_away)
}

#' Bayes-optimal GISKO scoreline (maximise expected points)
#'
#' @param pbar Integrated scoreline PMF (rows = home, cols = away).
#' @param max_goals Candidate-grid upper bound.
#' @return list `home`, `away`, `exp_points`, `p_exact`, `modal_home`,
#'   `modal_away`, `optimal_differs_from_modal`, `top` (top-6 candidate tibble).
#' @export
gisko_optimal_scoreline <- function(pbar, max_goals = nrow(pbar) - 1L) {
  g <- 0:max_goals
  cand <- expand.grid(home = g, away = g)
  cand$exp_points <- mapply(
    function(h, a) gisko_expected_points(h, a, pbar), cand$home, cand$away
  )
  cand <- cand[order(-cand$exp_points), , drop = FALSE]
  best <- cand[1, ]
  modal <- which(pbar == max(pbar), arr.ind = TRUE)[1, ]
  modal_home <- unname(modal[["row"]]) - 1L
  modal_away <- unname(modal[["col"]]) - 1L
  list(
    home = best$home, away = best$away,
    exp_points = best$exp_points,
    p_exact = pbar[best$home + 1L, best$away + 1L],
    modal_home = modal_home, modal_away = modal_away,
    optimal_differs_from_modal =
      !(best$home == modal_home && best$away == modal_away),
    top = tibble::as_tibble(utils::head(cand, 6L))
  )
}

#' Integrated posterior-predictive scoreline matrix for a pairing
#'
#' Reuses the WC model's rate function `.wc_match_lambdas()` (single source of
#' truth) so this can never drift from the forecast. Builds the analytic
#' bivariate-Poisson PMF per posterior draw and averages.
#'
#' @param home,away Team names (martj42 convention, as in `sim_inputs`).
#' @param sim_inputs `list(team=, scalar=)` from `data/wc/fit/sim_inputs.rds`.
#' @param venue One of "neutral" (all knockouts), "home", "away".
#' @param max_goals Grid upper bound.
#' @return list `pbar`, `lambdas` (`n_draws x 3`), `n_draws`.
#' @export
gisko_predictive_matrix <- function(home, away, sim_inputs,
                                    venue = "neutral", max_goals = 8L) {
  sit <- sim_inputs$team
  sca <- sim_inputs$scalar
  missing_teams <- setdiff(c(home, away), sit$team)
  if (length(missing_teams) > 0L) {
    cli::cli_abort("gisko_predictive_matrix: team(s) not in sim_inputs: {.val {missing_teams}}")
  }
  dt_by <- split(sit, sit$.draw)
  ds_by <- split(sca, sca$.draw)
  keys <- intersect(names(dt_by), names(ds_by))
  if (length(keys) == 0L) {
    cli::cli_abort("gisko_predictive_matrix: no overlapping .draw keys.")
  }
  nd <- length(keys)
  n <- max_goals + 1L
  pbar <- matrix(0, n, n)
  lambdas <- matrix(0, nd, 3L)
  for (i in seq_along(keys)) {
    dt <- dt_by[[keys[i]]]
    off <- stats::setNames(dt$cur_offense, dt$team)
    def <- stats::setNames(dt$cur_defense, dt$team)
    ha_off <- stats::setNames(dt$home_advantage_off, dt$team)
    ha_def <- stats::setNames(dt$home_advantage_def, dt$team)
    ds <- ds_by[[keys[i]]]
    l <- .wc_match_lambdas(
      home, away, venue, off, def, ha_off, ha_def,
      ds$mean_log_goals, ds$alpha_mu3, ds$beta_mu3_strength_diff
    )
    lambdas[i, ] <- l
    pbar <- pbar + .gisko_bvpois_pmf(l, max_goals)
  }
  list(pbar = pbar / nd, lambdas = lambdas, n_draws = nd)
}

#' Assemble marginals from a published `predictions.json` match (cross-check)
#'
#' @param match One element of `predictions.json$matches`, parsed by jsonlite.
#' @return A `marg` list as produced by [gisko_marginals_from_pbar()].
#' @export
gisko_marginals_from_predictions <- function(match) {
  pull <- function(dist, value_key) {
    vals <- vapply(dist, function(e) as.numeric(e[[value_key]]), numeric(1))
    ps <- vapply(dist, function(e) as.numeric(e$p), numeric(1))
    stats::setNames(ps, vals)
  }
  list(
    p_outcome = c(
      home = as.numeric(match$p_home),
      draw = as.numeric(match$p_draw),
      away = as.numeric(match$p_away)
    ),
    home_pmf = pull(match$home_goal_distribution, "goals"),
    away_pmf = pull(match$away_goal_distribution, "goals"),
    gd_pmf = pull(match$goal_diff_distribution, "diff")
  )
}

.gisko_score_support <- c(0L, 1L, 2L, 3L, 5L)

.gisko_conv <- function(p, q) {
  r <- numeric(length(p) + length(q) - 1L)
  for (i in seq_along(p)) {
    r[i:(i + length(q) - 1L)] <- r[i:(i + length(q) - 1L)] + p[i] * q
  }
  r
}

.gisko_support_vec <- function(pmf_on_support) {
  v <- numeric(6L)
  v[.gisko_score_support + 1L] <- pmf_on_support
  v
}

.gisko_dist_summary <- function(total_pmf, scenario) {
  support <- seq_along(total_pmf) - 1L
  cdf <- cumsum(total_pmf)
  q <- function(p) support[which(cdf >= p)[1]]
  mu <- sum(support * total_pmf)
  tibble::tibble(
    scenario = scenario,
    mean = mu,
    sd = sqrt(sum((support - mu)^2 * total_pmf)),
    q05 = q(0.05), q50 = q(0.5), q95 = q(0.95)
  )
}

#' Optimise one GISKO round: scorelines, joker, and the round-total distribution
#'
#' Expected-points optimal per match (Lemma 1); joker doubles the highest-
#' expected-points match (Lemma 2). The round-total distribution is the
#' posterior-predictive mixture over draws of the within-draw convolution of
#' per-match score PMFs -- matches are conditionally independent given a draw.
#'
#' @param matches Tibble `home`, `away`, `venue`, `label` (one round).
#' @param sim_inputs `list(team=, scalar=)` from the WC fit cache.
#' @param max_goals Grid upper bound.
#' @return list `picks`, `joker` (row index), `total_pmf`, `total_pmf_joker`,
#'   `summary`.
#' @export
gisko_optimise_round <- function(matches, sim_inputs, max_goals = 8L) {
  nm <- nrow(matches)
  if (nm == 0L) {
    empty <- tibble::tibble(
      label = character(), home = character(), away = character(),
      opt_home = integer(), opt_away = integer(), exp_points = numeric(),
      p_exact = numeric(), differs_from_modal = logical()
    )
    return(list(
      picks = empty, joker = integer(0),
      total_pmf = 1, total_pmf_joker = 1,
      summary = rbind(
        .gisko_dist_summary(1, "base"), .gisko_dist_summary(1, "joker")
      )
    ))
  }
  picks <- vector("list", nm)
  per_draw <- vector("list", nm)
  nd <- NULL
  for (m in seq_len(nm)) {
    pm <- gisko_predictive_matrix(
      matches$home[m], matches$away[m], sim_inputs,
      venue = matches$venue[m], max_goals = max_goals
    )
    opt <- gisko_optimal_scoreline(pm$pbar, max_goals)
    picks[[m]] <- tibble::tibble(
      label = matches$label[m], home = matches$home[m], away = matches$away[m],
      opt_home = opt$home, opt_away = opt$away,
      exp_points = opt$exp_points, p_exact = opt$p_exact,
      differs_from_modal = opt$optimal_differs_from_modal
    )
    nd <- pm$n_draws
    smv <- as.vector(gisko_score_matrix(opt$home, opt$away, max_goals))
    fac <- factor(smv, levels = .gisko_score_support)
    mat <- matrix(0, nd, 6L)
    for (d in seq_len(nd)) {
      Pd <- .gisko_bvpois_pmf(pm$lambdas[d, ], max_goals)
      agg <- tapply(as.vector(Pd), fac, sum)
      agg[is.na(agg)] <- 0
      mat[d, ] <- .gisko_support_vec(as.numeric(agg))
    }
    per_draw[[m]] <- mat
  }
  picks <- do.call(rbind, picks)
  joker <- which.max(picks$exp_points)

  total_for <- function(joker_idx) {
    total <- 0
    for (d in seq_len(nd)) {
      acc <- 1
      for (m in seq_len(nm)) {
        sp <- per_draw[[m]][d, ]
        acc <- .gisko_conv(acc, sp)
        if (!is.na(joker_idx) && joker_idx == m) acc <- .gisko_conv(acc, sp)
      }
      if (length(total) < length(acc)) {
        total <- c(total, numeric(length(acc) - length(total)))
      }
      total[seq_along(acc)] <- total[seq_along(acc)] + acc
    }
    total / nd
  }
  total_pmf <- total_for(NA_integer_)
  total_pmf_joker <- total_for(joker)

  list(
    picks = picks, joker = joker,
    total_pmf = total_pmf, total_pmf_joker = total_pmf_joker,
    summary = rbind(
      .gisko_dist_summary(total_pmf, "base"),
      .gisko_dist_summary(total_pmf_joker, "joker")
    )
  )
}

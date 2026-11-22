#' Score 1X2 forecasts.
#'
#' @param p_home,p_draw,p_away Outcome probabilities (renormalised to sum to 1).
#' @param home_score,away_score Observed 90-minute scores.
#' @return Tibble `log_1x2` (negative log probability of the outcome) and `rps`
#'   (ranked probability score); lower is better for both.
#' @export
nt_score_outcomes <- function(p_home, p_draw, p_away, home_score, away_score) {
  s <- p_home + p_draw + p_away
  p_home <- p_home / s
  p_draw <- p_draw / s
  p_away <- p_away / s
  y_home <- as.numeric(home_score > away_score)
  y_draw <- as.numeric(home_score == away_score)
  p_obs <- ifelse(y_home == 1, p_home, ifelse(y_draw == 1, p_draw, p_away))
  tibble::tibble(
    log_1x2 = -log(p_obs),
    rps = 0.5 * ((p_home - y_home)^2 + (p_home + p_draw - y_home - y_draw)^2)
  )
}

#' Score exact-scoreline forecasts.
#'
#' @param grid Scoreline probabilities, fixtures x goals x goals (see
#'   [nt_scoreline_probs()]).
#' @param home_score,away_score Observed 90-minute scores.
#' @return Numeric vector: negative log probability of the observed scoreline.
#' @export
nt_score_scorelines <- function(grid, home_score, away_score) {
  top <- dim(grid)[2] - 1L
  if (any(home_score > top | away_score > top)) {
    cli::cli_abort("An observed score exceeds the scoreline grid ({top} goals).")
  }
  p <- grid[cbind(seq_along(home_score), home_score + 1L, away_score + 1L)]
  -log(p)
}

#' Paired comparison of two per-match score vectors.
#'
#' @param a,b Per-match scores of two forecasters on the same matches.
#' @param level Confidence level of the interval.
#' @return One-row tibble: `mean_a`, `mean_b`, `diff` (a - b), `se`, `lower`,
#'   `upper`, `n`. The interval is a paired t interval that treats matches as
#'   independent.
#' @export
nt_paired_diff <- function(a, b, level = 0.95) {
  d <- a - b
  n <- length(d)
  se <- stats::sd(d) / sqrt(n)
  half <- stats::qt(1 - (1 - level) / 2, df = n - 1L) * se
  tibble::tibble(
    mean_a = mean(a), mean_b = mean(b), diff = mean(d), se = se,
    lower = mean(d) - half, upper = mean(d) + half, n = n
  )
}

#' Mismatch calibration by predicted edge.
#'
#' Bins matches by the favourite's expected goal margin and compares, per bin,
#' the predicted and observed favourite-win share, draw share and goal margin.
#' A model that over-extrapolates lopsided matches shows observed margins and
#' favourite wins falling short of predicted in the top bins.
#'
#' @param pred Tibble with `p_home, p_draw, p_away, eg_home, eg_away` (see
#'   [nt_outcome_probs()]).
#' @param home_score,away_score Observed 90-minute scores.
#' @param breaks Bin edges for the absolute expected goal margin.
#' @return Tibble, one row per bin: `edge_bin`, `n`, predicted and observed
#'   `fav_win`, `draw` and `margin` (favourite's goals minus underdog's).
#' @export
nt_edge_calibration <- function(pred, home_score, away_score,
                                breaks = c(0, 0.35, 0.7, 1.05, 1.75, Inf)) {
  home_fav <- pred$eg_home >= pred$eg_away
  edge <- abs(pred$eg_home - pred$eg_away)
  p_fav <- ifelse(home_fav, pred$p_home, pred$p_away)
  obs_margin <- ifelse(home_fav, home_score - away_score, away_score - home_score)
  bin <- cut(edge, breaks = breaks, right = FALSE, include.lowest = TRUE)
  rows <- lapply(split(seq_along(edge), bin, drop = TRUE), function(i) {
    tibble::tibble(
      n = length(i),
      pred_fav_win = mean(p_fav[i]), obs_fav_win = mean(obs_margin[i] > 0),
      pred_draw = mean(pred$p_draw[i]), obs_draw = mean(obs_margin[i] == 0),
      pred_margin = mean(edge[i]), obs_margin = mean(obs_margin[i])
    )
  })
  out <- do.call(rbind, rows)
  tibble::tibble(edge_bin = names(rows), out)
}

#' Outcome probabilities, expected goals and all three scores for a forecast.
#'
#' @param grid Scoreline probabilities, fixtures x goals x goals.
#' @param home_score,away_score Observed 90-minute scores.
#' @return Tibble: the columns of [nt_outcome_probs()] plus `log_1x2`, `rps`
#'   and `log_score` (exact scoreline), one row per fixture.
#' @export
nt_evaluate_grid <- function(grid, home_score, away_score) {
  probs <- nt_outcome_probs(grid)
  sc <- nt_score_outcomes(
    probs$p_home, probs$p_draw, probs$p_away, home_score, away_score
  )
  tibble::tibble(
    probs, sc,
    log_score = nt_score_scorelines(grid, home_score, away_score)
  )
}

#' Inflate the draw probability of 1X2 forecasts.
#'
#' Multiplies the odds of a draw against a decided match by `1 + phi` and
#' leaves the home-to-away ratio unchanged. `phi = 0` returns the input.
#'
#' @param p_home,p_draw,p_away Outcome probabilities.
#' @param phi Inflation; positive values move mass onto the draw.
#' @return Tibble `p_home, p_draw, p_away`.
#' @export
nt_inflate_draws <- function(p_home, p_draw, p_away, phi) {
  s <- p_home + p_draw + p_away
  p_home <- p_home / s
  p_draw <- p_draw / s
  p_away <- p_away / s
  w <- 1 + phi * p_draw
  tibble::tibble(
    p_home = p_home / w, p_draw = (1 + phi) * p_draw / w, p_away = p_away / w
  )
}

#' Estimate a draw inflation by maximum likelihood.
#'
#' One-parameter calibration for a model that predicts too few (or too many)
#' draws; see [nt_inflate_draws()]. Fit it on forecasts that were made out of
#' sample, never on the matches it will be applied to.
#'
#' @inheritParams nt_inflate_draws
#' @param home_score,away_score Observed scores.
#' @return The estimated `phi`.
#' @export
nt_fit_draw_inflation <- function(p_home, p_draw, p_away,
                                  home_score, away_score) {
  s <- p_home + p_draw + p_away
  p_draw <- p_draw / s
  drawn <- home_score == away_score
  nll <- function(phi) {
    -(sum(drawn) * log1p(phi) - sum(log1p(phi * p_draw)))
  }
  stats::optimize(nll, interval = c(-0.9, 5))$minimum
}

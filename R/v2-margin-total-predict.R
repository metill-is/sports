#' Predictive CDF of a location-scale Student-t or Gaussian, averaged over
#' posterior draws.
#'
#' `loc` and `scale` are either vectors (one predictive per match) or
#' matrices with one row per posterior draw and one column per match.
#'
#' @param x Points to evaluate, one per match.
#' @param loc,scale Location and scale, vector or draws-by-matches matrix.
#'   With a matrix `loc`, a vector `scale` is one value per draw (or a scalar);
#'   with a vector `loc`, one per match (or a scalar). Any other length is
#'   refused rather than recycled.
#' @param df Degrees of freedom; `Inf` (default) is the Gaussian. A scalar or,
#'   with a matrix `loc`, one value per draw.
#' @return One probability per match.
#' @noRd
v2_cdf <- function(x, loc, scale, df = Inf) {
  colMeans(v2_cdf_draws(x, loc, scale, df))
}

#' Predictive CDF per posterior draw, before averaging.
#'
#' @inheritParams v2_cdf
#' @return A draws-by-matches matrix of probabilities (one row when `loc` is a
#'   vector).
#' @noRd
v2_cdf_draws <- function(x, loc, scale, df = Inf) {
  if (!is.matrix(loc)) loc <- matrix(loc, nrow = 1L)
  # WHY the checks: matrix() recycles, so a scale given per match with a draws-by-matches
  # location would be spread over the draws in a cycle without a word.
  per_draw <- nrow(loc) > 1L
  if (!is.matrix(scale) && !length(scale) %in% c(1L, if (per_draw) nrow(loc) else ncol(loc))) {
    cli::cli_abort("{.arg scale} has {length(scale)} value{?s}; it needs 1 or one per {if (per_draw) 'draw' else 'match'} ({if (per_draw) nrow(loc) else ncol(loc)}).")
  }
  if (!length(df) %in% c(1L, nrow(loc))) {
    cli::cli_abort("{.arg df} has {length(df)} value{?s}; it needs 1 or one per draw ({nrow(loc)}).")
  }
  if (!is.matrix(scale)) scale <- matrix(scale, nrow = nrow(loc), ncol = ncol(loc), byrow = !per_draw)
  stopifnot(length(x) == ncol(loc), identical(dim(loc), dim(scale)))
  z <- (matrix(x, nrow(loc), ncol(loc), byrow = TRUE) - loc) / scale
  df <- rep_len(df, nrow(loc))
  p <- if (all(is.infinite(df))) stats::pnorm(z) else stats::pt(z, df = df)
  matrix(p, nrow(loc), ncol(loc))
}

#' Probability that an integer margin or total equals `k`.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2).
#'
#' Scores are integers, the model's predictive is continuous. Rounding the
#' continuous value to the nearest integer gives
#' `P(X = k) = F(k + 0.5) - F(k - 0.5)`. For `k = 0` this is the production
#' "draw band" (`|margin| <= 0.5`), so the band is not wrong in itself; what
#' it lacks is a way to say that level scores are more (or less) common than a
#' smooth curve implies. `zero_shift` supplies that: it moves the probability
#' of a level score on the logit scale and rescales every other margin so the
#' total stays one.
#'
#' @param k Integer values, one per match.
#' @param loc,scale,df Predictive, see [v2_cdf()].
#' @param zero_shift Logit shift applied to `P(X = 0)`, a scalar or one value
#'   per match; `0` leaves the rounded distribution as it is.
#' @param ties Can the value be zero? `FALSE` is a margin that cannot be level
#'   (a final basketball score): the rounded mass on zero is removed and every
#'   other margin is divided by what remains, so the distribution still sums
#'   to one. Without this, a log score of such a margin is charged for mass
#'   placed on an outcome that cannot happen.
#' @return One probability per match.
#' @noRd
v2_integer_pmf <- function(k, loc, scale, df = Inf, zero_shift = 0, ties = TRUE) {
  p <- v2_cdf(k + 0.5, loc, scale, df) - v2_cdf(k - 0.5, loc, scale, df)
  if (ties && all(zero_shift == 0)) {
    return(p)
  }
  zero <- rep(0, length(k))
  p0 <- v2_cdf(zero + 0.5, loc, scale, df) - v2_cdf(zero - 0.5, loc, scale, df)
  if (!ties) {
    if (any(zero_shift != 0)) {
      cli::cli_abort("{.arg zero_shift} moves the level-score probability, which {.code ties = FALSE} sets to zero.")
    }
    return(ifelse(k == 0, 0, p / (1 - p0)))
  }
  p0_new <- stats::plogis(stats::qlogis(p0) + zero_shift)
  ifelse(k == 0, p0_new, p * (1 - p0_new) / (1 - p0))
}

#' Home / draw / away probabilities from a margin predictive.
#'
#' With `ties = TRUE` (handball, basketball regulation time) a draw is a
#' level score: the rounded mass on zero, moved by `zero_shift`. With
#' `ties = FALSE` (a result that cannot be level) the home side wins when the
#' continuous margin is positive.
#'
#' Three functions here give a result that cannot be level, each by its own
#' rule, and they differ by up to about 0.7 percentage points for one game:
#' this one (`P(X > 0)`), `v2_integer_pmf(ties = FALSE)` (`P(X > 0.5)`
#' renormalised over the non-zero margins) and [v2_basketball_result_probs()]
#' (regulation time plus overtime). A harness that scores the final margin
#' and the final result of one game should use the same rule for both.
#'
#' @inheritParams v2_integer_pmf
#' @param ties Can the result be level?
#' @return Data frame with `p_home`, `p_draw`, `p_away`.
#' @noRd
v2_outcome_probs <- function(loc, scale, df = Inf, ties = TRUE, zero_shift = 0) {
  n <- if (is.matrix(loc)) ncol(loc) else length(loc)
  zero <- rep(0, n)
  if (!ties) {
    p_away <- v2_cdf(zero, loc, scale, df)
    return(data.frame(p_home = 1 - p_away, p_draw = 0, p_away = p_away))
  }
  below <- v2_cdf(zero - 0.5, loc, scale, df)
  above <- 1 - v2_cdf(zero + 0.5, loc, scale, df)
  p0 <- 1 - below - above
  p0_new <- stats::plogis(stats::qlogis(p0) + zero_shift)
  rescale <- (1 - p0_new) / (1 - p0)
  data.frame(p_home = above * rescale, p_draw = p0_new, p_away = below * rescale)
}

#' Estimate the logit shift of the level-score probability.
#'
#' One parameter, fitted by maximum likelihood: a logistic regression of
#' "the match ended level" on an intercept with the rounded probability's
#' logit as offset. A positive shift means level scores are more common than
#' the smooth predictive implies.
#'
#' @param p0 Rounded probability of a level score, one per match.
#' @param is_tie Logical, did the match end level.
#' @return Named vector: `shift`, `se`, `n`, `ties`.
#' @noRd
v2_fit_zero_shift <- function(p0, is_tie) {
  stopifnot(length(p0) == length(is_tie), all(p0 > 0 & p0 < 1))
  fit <- stats::glm(as.integer(is_tie) ~ 1, offset = stats::qlogis(p0), family = stats::binomial())
  c(
    shift = unname(stats::coef(fit)[1]),
    se = unname(sqrt(diag(stats::vcov(fit)))[1]),
    n = length(p0), ties = sum(is_tie)
  )
}

#' Final-result probabilities for basketball from a regulation-time margin.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2).
#'
#' The bookmaker settles on regulation time, where a level score is a third
#' outcome; the league table needs the final result, which cannot be level.
#' Both come from one regulation-time margin predictive. Overtime is modelled
#' as a short continuation of the same game: a five-minute period is
#' `ot_share = 5 / 40` of regulation, so its margin has mean `ot_share * loc`
#' and variance `ot_share * scale^2`. A level overtime is replayed, so the
#' winner of overtime is the sign of the first non-level period.
#'
#' With posterior draws, a game goes to overtime mostly in the draws where it
#' is close, and those are the draws where overtime is closest to a coin flip.
#' So the overtime share is averaged over draws with each draw weighted by its
#' own probability of a level regulation score; multiplying the averaged
#' level probability by an averaged overtime share would overstate the
#' favourite.
#'
#' @inheritParams v2_outcome_probs
#' @param ot_share Length of an overtime period as a share of regulation.
#' @return Data frame: regulation-time `p_home_reg`, `p_draw_reg`,
#'   `p_away_reg`; overtime `p_home_ot` (home wins given overtime); final
#'   `p_home`, `p_away`.
#' @noRd
v2_basketball_result_probs <- function(loc, scale, df = Inf, zero_shift = 0, ot_share = 5 / 40) {
  reg <- v2_outcome_probs(loc, scale, df, ties = TRUE, zero_shift = zero_shift)
  zero <- rep(0, nrow(reg))
  level <- v2_cdf_draws(zero + 0.5, loc, scale, df) - v2_cdf_draws(zero - 0.5, loc, scale, df)
  ot_away <- v2_cdf_draws(zero - 0.5, loc * ot_share, scale * sqrt(ot_share), df)
  ot_home <- 1 - v2_cdf_draws(zero + 0.5, loc * ot_share, scale * sqrt(ot_share), df)
  ot_share_home <- ot_home / (ot_home + ot_away)
  weight <- colSums(level)
  # WHY the fallback: in a game so lopsided that a level score has probability zero in
  # every draw there is no weight to share out. Overtime then never happens, and the
  # plain average keeps the result defined instead of 0 / 0.
  p_home_ot <- ifelse(weight > 0, colSums(level * ot_share_home) / weight, colMeans(ot_share_home))
  data.frame(
    p_home_reg = reg$p_home, p_draw_reg = reg$p_draw, p_away_reg = reg$p_away,
    p_home_ot = p_home_ot,
    p_home = reg$p_home + reg$p_draw * p_home_ot,
    p_away = reg$p_away + reg$p_draw * (1 - p_home_ot)
  )
}

#' Probability of an observed margin from posterior score draws.
#'
#' The archived production forecasts are draws of continuous scores, not a
#' predictive in closed form. The probability that the margin rounds to `k` is
#' the share of draws within half a point of `k`, with half a count added so
#' that a margin no draw happened to hit does not score minus infinity.
#'
#' @param draws Continuous margin draws for one match.
#' @param k The observed whole-number margin.
#' @param ties Can the margin be zero? With `FALSE` (a final basketball
#'   score) the draws within half a point of a level score are set aside and
#'   the share is taken among the rest, the same conditioning
#'   [v2_integer_pmf()] applies with `ties = FALSE`.
#' @return One probability.
#' @noRd
v2_draws_pmf <- function(draws, k, ties = TRUE) {
  hit <- abs(draws - k) < 0.5
  if (ties) {
    return((sum(hit) + 0.5) / (length(draws) + 1))
  }
  if (k == 0) {
    return(0)
  }
  not_level <- abs(draws) >= 0.5
  (sum(hit & not_level) + 0.5) / (sum(not_level) + 1)
}

#' De-vig decimal odds proportionally.
#'
#' @param odds Matrix or data frame of decimal odds, one row per market, one
#'   column per outcome.
#' @return Matrix of probabilities whose rows sum to one, with the
#'   bookmaker's overround as attribute `"overround"`.
#' @noRd
v2_devig <- function(odds) {
  inv <- 1 / as.matrix(odds)
  over <- rowSums(inv)
  structure(inv / over, overround = over)
}

#' Draw integer home and away scores from the margin and total predictives.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2).
#'
#' Everything downstream of the fit (the league-table simulation, the decide
#' layer, the published score distributions) consumes `(home_goals,
#' away_goals)` per posterior draw. The v2 model predicts a margin and a total
#' instead, and the two are tied by arithmetic: `home = (total + margin) / 2`
#' is an integer only when margin and total are both even or both odd. So the
#' scores are drawn in that order:
#'
#' 1. a continuous margin and total per draw, correlated by `rho_mt` (the
#'    residual correlation of margin and total; it is zero exactly when the
#'    two sides' scores have equal variance);
#' 2. the margin is rounded to the nearest integer and the level-score
#'    probability is moved by `zero_shift`, in the way [v2_integer_pmf()]
#'    defines it (other margins keep their relative shares);
#' 3. the total is rounded to the nearest integer with the margin's parity.
#'
#' Uses R's random number generator; set the seed outside.
#'
#' @param loc_m,loc_t Draws-by-matches matrices of the margin and total
#'   locations (posterior draws of the linear predictors).
#' @param scale_m,scale_t Observation sds: one per draw, or a matrix shaped
#'   like `loc_m`.
#' @param rho_mt Correlation between the margin and total noise.
#' @param zero_shift Logit shift of the level-score probability, a scalar or
#'   one value per match.
#' @return List of two integer draws-by-matches matrices, `home` and `away`.
#' @noRd
v2_sample_scores <- function(loc_m, scale_m, loc_t, scale_t, rho_mt = 0, zero_shift = 0) {
  stopifnot(is.matrix(loc_m), is.matrix(loc_t), identical(dim(loc_m), dim(loc_t)))
  n_draw <- nrow(loc_m)
  n_match <- ncol(loc_m)
  e1 <- matrix(stats::rnorm(n_draw * n_match), n_draw, n_match)
  e2 <- matrix(stats::rnorm(n_draw * n_match), n_draw, n_match)
  margin <- round(loc_m + scale_m * e1)
  total_raw <- loc_t + scale_t * (rho_mt * e1 + sqrt(1 - rho_mt^2) * e2)

  zero_shift <- rep_len(zero_shift, n_match)
  for (j in which(zero_shift != 0)) {
    is_zero <- margin[, j] == 0
    p0 <- mean(is_zero)
    if (p0 <= 0 || p0 >= 1) next
    target <- stats::plogis(stats::qlogis(p0) + zero_shift[j])
    if (target > p0) {
      flip <- !is_zero & stats::runif(n_draw) < (target - p0) / (1 - p0)
      margin[flip, j] <- 0
    } else {
      flip <- is_zero & stats::runif(n_draw) < (p0 - target) / p0
      pool <- margin[!is_zero, j]
      margin[flip, j] <- pool[sample.int(length(pool), sum(flip), replace = TRUE)]
    }
  }

  total <- margin + 2 * round((total_raw - margin) / 2)
  # WHY: a total smaller than the margin would mean a negative score. It needs a
  # predictive far outside any real match; lift the total to keep the margin intact.
  total <- pmax(total, abs(margin))
  storage.mode(margin) <- "integer"
  storage.mode(total) <- "integer"
  list(home = (total + margin) %/% 2L, away = (total - margin) %/% 2L)
}

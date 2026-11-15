# Football v2 prototype: scoring, de-vigging, season phase, paired intervals.

FV2_MAX_GOALS <- 12L

#' Score predictive Poisson rates against results.
#'
#' @param lam_home,lam_away Matches in rows, posterior draws in columns.
#' @return One row per match: exact-scoreline log score, 1X2 probabilities,
#'   1X2 log score (higher is better for both), RPS, expected goals.
fv2_score <- function(lam_home, lam_away, home_score, away_score) {
  g <- 0:FV2_MAX_GOALS
  n <- nrow(lam_home)
  stopifnot(nrow(lam_away) == n, length(home_score) == n, length(away_score) == n)
  out <- matrix(NA_real_, n, 8L, dimnames = list(NULL, c(
    "ls_score", "p_home", "p_draw", "p_away", "ls_1x2", "rps", "xg_home", "xg_away"
  )))
  for (i in seq_len(n)) {
    ph <- outer(g, lam_home[i, ], stats::dpois)
    pa <- outer(g, lam_away[i, ], stats::dpois)
    grid <- tcrossprod(ph, pa) / ncol(ph)
    grid <- grid / sum(grid)
    p <- c(sum(grid[lower.tri(grid)]), sum(diag(grid)), sum(grid[upper.tri(grid)]))
    hs <- min(home_score[i], FV2_MAX_GOALS)
    as_ <- min(away_score[i], FV2_MAX_GOALS)
    o <- c(home_score[i] > away_score[i], home_score[i] == away_score[i], home_score[i] < away_score[i])
    rps <- 0.5 * ((p[1L] - o[1L])^2 + (p[1L] + p[2L] - o[1L] - o[2L])^2)
    out[i, ] <- c(
      log(grid[hs + 1L, as_ + 1L]), p, log(p[o]), rps,
      mean(lam_home[i, ]), mean(lam_away[i, ])
    )
  }
  as.data.frame(out)
}

fv2_log_score_1x2 <- function(p_home, p_draw, p_away, home_score, away_score) {
  log(ifelse(home_score > away_score, p_home, ifelse(home_score == away_score, p_draw, p_away)))
}

#' Remove the bookmaker margin from decimal 1X2 odds.
#'
#' @param odds Matrix or data frame with columns home, draw, away.
#' @param method "proportional" divides the inverse odds by their sum; "power"
#'   raises them to the exponent that makes them sum to one.
fv2_devig <- function(odds, method = c("proportional", "power")) {
  method <- match.arg(method)
  inv <- 1 / as.matrix(odds)
  stopifnot(ncol(inv) == 3L, all(is.finite(inv)), all(inv > 0))
  if (method == "proportional") {
    q <- inv / rowSums(inv)
  } else {
    q <- t(apply(inv, 1L, function(r) {
      if (abs(sum(r) - 1) < 1e-12) {
        return(r)
      }
      k <- stats::uniroot(function(k) sum(r^k) - 1, c(0.2, 5), tol = 1e-12)$root
      r^k
    }))
  }
  colnames(q) <- c("home", "draw", "away")
  q
}

#' Season phase of each match: first five rounds, middle, last third.
#'
#' League matches are classed by their round within the division's regular
#' season; split-phase and play-off matches are "late". Matches without a
#' league (cups) take the phase most league matches of the same season had in
#' that week.
#'
#' @param matches Data frame with match_date, season, round, division and
#'   league_division (NA for cups; differs from `division` for play-offs).
fv2_phase <- function(matches) {
  base <- matches$league_division
  is_league <- !is.na(base) & matches$division == base
  is_post <- !is.na(base) & matches$division != base
  grp <- paste(matches$season, base)
  max_round <- stats::ave(ifelse(is_league, matches$round, NA_real_), grp, FUN = function(x) {
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  })
  phase <- rep(NA_character_, nrow(matches))
  phase[is_league] <- ifelse(
    matches$round[is_league] <= 5, "early",
    ifelse(matches$round[is_league] > (2 / 3) * max_round[is_league], "late", "middle")
  )
  phase[is_post] <- "late"
  todo <- which(is.na(phase))
  if (length(todo)) {
    wk <- fv2_week(matches$match_date)
    known <- !is.na(phase)
    for (i in todo) {
      same <- known & matches$season == matches$season[i]
      if (!any(same)) next
      d <- abs(wk[same] - wk[i])
      near <- phase[same][d == min(d)]
      phase[i] <- names(which.max(table(factor(near, c("early", "middle", "late")))))
    }
  }
  factor(phase, c("early", "middle", "late"))
}

#' Mean paired difference with a cluster-robust standard error.
#'
#' @param diff One paired score difference per match.
#' @param cluster Cluster label per match; defaults to one cluster per match.
fv2_paired <- function(diff, cluster = seq_along(diff), level = 0.95) {
  keep <- !is.na(diff)
  diff <- diff[keep]
  cluster <- cluster[keep]
  n <- length(diff)
  if (n < 2L) {
    return(data.frame(
      n = n, n_cluster = length(unique(cluster)), mean = mean(diff),
      se = NA_real_, lo = NA_real_, hi = NA_real_
    ))
  }
  m <- mean(diff)
  sums <- tapply(diff - m, cluster, sum)
  n_cl <- length(sums)
  se <- sqrt(n_cl / (n_cl - 1) * sum(sums^2)) / n
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(n = n, n_cluster = n_cl, mean = m, se = se, lo = m - z * se, hi = m + z * se)
}

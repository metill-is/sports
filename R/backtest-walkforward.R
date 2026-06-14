#' Out-of-sample probability scores: Brier and log-loss over frozen as-of `p`.
#'
#' The PRIMARY walk-forward verdict. Rows with `NA` win (bets that never
#' settled within the horizon) are dropped before scoring. `p` is clamped to
#' `[eps, 1-eps]` so a degenerate 0/1 forecast cannot send log-loss to Inf.
#' @param settled Tibble with numeric `p` and logical `win`.
#' @param eps Clamp bound. Default `1e-6`.
#' @return One-row tibble `(n, brier, log_loss)`; `n = 0` -> NA scores.
#' @export
bt_oos_scores <- function(settled, eps = 1e-6) {
  d <- settled[!is.na(settled$win), , drop = FALSE]
  if (nrow(d) == 0L) {
    return(tibble::tibble(n = 0L, brier = NA_real_, log_loss = NA_real_))
  }
  p <- pmin(pmax(d$p, eps), 1 - eps)
  y <- as.numeric(d$win)
  tibble::tibble(
    n = nrow(d),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(p) + (1 - y) * log(1 - p))
  )
}

#' Sport-specific generous upper bound on a single team's score.
#'
#' Roughly 1.3-2x the observed maxima in `data/facts/results/` (football 24,
#' handball 51, basketball 193 as of 2026-05). Generous on purpose: a value
#' validator that rejects a real record-breaking scoreline is worse than the
#' garbage it guards against. Bump with a `# WHY` note if a genuine game ever
#' trips it.
#' @noRd
score_caps <- function() c(football = 40L, handball = 80L, basketball = 250L)

#' @noRd
check_result_values <- function(df) {
  v <- character(0)
  caps <- score_caps()[df$sport]
  caps[is.na(caps)] <- Inf
  for (col in c("home_score", "away_score")) {
    x <- df[[col]]
    neg <- which(!is.na(x) & x < 0L)
    if (length(neg) > 0L) {
      v <- c(v, sprintf(
        "%s has %d negative value(s) (first: row %d = %s)",
        col, length(neg), neg[[1L]], x[[neg[[1L]]]]
      ))
    }
    hi <- which(!is.na(x) & x > caps)
    if (length(hi) > 0L) {
      v <- c(v, sprintf(
        "%s exceeds the sport cap (first: row %d sport=%s score=%s)",
        col, hi[[1L]], df$sport[[hi[[1L]]]], x[[hi[[1L]]]]
      ))
    }
  }
  c(v, check_regulation_values(df))
}

#' Regulation-time scores must be possible beside the final score.
#'
#' Overtime only adds points, and only after a level regulation: a regulation
#' score is never above the final; when regulation was not level the final is
#' the regulation score; when it was level the final is not (overtime cannot
#' end level). A row without regulation scores is not checked.
#' @noRd
check_regulation_values <- function(df) {
  if (!all(c("home_score_reg", "away_score_reg") %in% names(df))) {
    return(character(0))
  }
  v <- character(0)
  hr <- df$home_score_reg
  ar <- df$away_score_reg
  hf <- df$home_score
  af <- df$away_score
  first <- function(bad) which(bad)[[1L]]
  for (col in c("home_score_reg", "away_score_reg")) {
    neg <- !is.na(df[[col]]) & df[[col]] < 0L
    if (any(neg)) v <- c(v, sprintf("%s has %d negative value(s) (first: row %d)", col, sum(neg), first(neg)))
  }
  both <- !is.na(hr) & !is.na(ar) & !is.na(hf) & !is.na(af)
  above_h <- both & hr > hf
  above_a <- both & ar > af
  if (any(above_h)) v <- c(v, sprintf("home_score_reg is above home_score (first: row %d)", first(above_h)))
  if (any(above_a)) v <- c(v, sprintf("away_score_reg is above away_score (first: row %d)", first(above_a)))
  no_ot_but_changed <- both & hr != ar & (hf != hr | af != ar)
  ot_but_level <- both & hr == ar & hf == af
  if (any(no_ot_but_changed)) {
    v <- c(v, sprintf("final score differs from a regulation score that was not level, so there was no overtime to add points (first: row %d)", first(no_ot_but_changed)))
  }
  if (any(ot_but_level)) {
    v <- c(v, sprintf("final score is level after a level regulation score, which overtime cannot leave (first: row %d)", first(ot_but_level)))
  }
  v
}

#' @noRd
check_odds_values <- function(df) {
  x <- df$odds
  bad <- which(!(is.finite(x) & x > 1))
  if (length(bad) == 0L) {
    return(character(0))
  }
  sprintf(
    "odds must be finite and > 1; %d violation(s) (first: row %d = %s)",
    length(bad), bad[[1L]], format(x[[bad[[1L]]]])
  )
}

#' @noRd
check_recommendation_values <- function(df) {
  p <- df$p
  bad <- which(!(is.finite(p) & p >= 0 & p <= 1))
  if (length(bad) == 0L) {
    return(character(0))
  }
  sprintf(
    "p must be a finite probability in [0, 1]; %d violation(s) (first: row %d = %s)",
    length(bad), bad[[1L]], format(p[[bad[[1L]]]])
  )
}

#' Value-level validation layered on top of the Arrow type check.
#'
#' `validate_against_schema()` only checks columns + types. This catches
#' impossible *values* that are structurally valid int32/float64 but would
#' poison the Kelly stake or the Stan likelihood: negative / absurd scores,
#' odds <= 1 or non-finite, recommendation `p` outside `[0, 1]`. Tables that
#' do not feed the money path or the likelihood (schedules, beliefs_*,
#' candidates, ledger, fit_diagnostics) have no value rules and pass through.
#' The ledger is deliberately exempt: the placer writes real Lengjan figures
#' and a guard that rejects a legitimate placement is worse than the hole.
#' NA scores are allowed (an unplayed fixture row); NA odds / NA rec-p are not.
#' @noRd
validate_values <- function(df, table) {
  violations <- switch(table,
    results = check_result_values(df),
    odds = check_odds_values(df),
    recommendations = check_recommendation_values(df),
    character(0)
  )
  if (length(violations) > 0L) {
    cli::cli_abort(
      c(
        "Value validation failed for table {.val {table}}:",
        stats::setNames(violations, rep_len("x", length(violations)))
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}

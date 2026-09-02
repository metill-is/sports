# A cmdstanr-fit-like object for tests. The 2DT extractors and publishers touch
# exactly one method -- `fit$draws(var)` -- so a list carrying that closure is a
# complete substitute for a 300-600 MB gitignored fit.rds, which is why the
# extract tests have never once executed.
#
# Deliberately NOT class CmdStanMCMC. Grepped across
# R/extract-iceland-2dt-shared.R, R/publish-iceland-2dt-helpers.R and the four
# per-sport extract/publish files: the only `fit$` usage is `fit$draws` (6
# occurrences), every CmdStanMCMC mention is a roxygen @param, and the only
# inherits() call is on fit_date. A fake class would be a lie that hides a real
# dispatch dependency if one is ever introduced.

# Coerce one variable's draws (matrix, draws_matrix or draws_array) to a plain
# numeric matrix, n_draws x n_elements, with element names as colnames.
.stub_draws_matrix <- function(x, var) {
  if (posterior::is_draws(x)) {
    x <- posterior::as_draws_matrix(x)
    nms <- posterior::variables(x)
    m <- matrix(as.numeric(x), nrow = posterior::ndraws(x))
    colnames(m) <- nms
    return(m)
  }
  m <- as.matrix(x)
  if (is.null(colnames(m))) {
    colnames(m) <- if (ncol(m) == 1L) var else paste0(var, "[", seq_len(ncol(m)), "]")
  }
  m
}

#' Minimal CmdStanMCMC substitute.
#'
#' @param draws_list Named list: Stan variable name -> draws_array / matrix of
#'   `n_draws` rows. `lp__` is required, because
#'   `publish_{basketball,handball}_iceland` read `posterior::ndraws(
#'   fit$draws("lp__"))`.
#' @return `list(draws = function(variables = NULL, ...))`. `$draws(v)` returns
#'   a `posterior::draws_array` holding every element of the named variables,
#'   in the order requested.
stub_fit <- function(draws_list) {
  stopifnot(
    is.list(draws_list),
    !is.null(names(draws_list)),
    all(nzchar(names(draws_list)))
  )
  if (!"lp__" %in% names(draws_list)) {
    stop("stub_fit: draws_list must include lp__", call. = FALSE)
  }
  mats <- Map(.stub_draws_matrix, draws_list, names(draws_list))
  n_draws <- unique(vapply(mats, nrow, integer(1)))
  stopifnot(length(n_draws) == 1L)
  force(mats)

  list(
    draws = function(variables = NULL, ...) {
      vars <- if (is.null(variables)) names(mats) else as.character(variables)
      absent <- setdiff(vars, names(mats))
      if (length(absent) > 0L) {
        stop(
          "stub_fit: no draws for variable(s): ",
          paste(absent, collapse = ", "),
          call. = FALSE
        )
      }
      m <- do.call(cbind, mats[vars])
      posterior::as_draws_array(posterior::as_draws_matrix(m))
    }
  )
}

#' Build the full 2DT posterior surface as a named list of draws matrices.
#'
#' @param teams Character vector of team names (length K).
#' @param n_pred Number of prediction rows -- MUST equal `nrow(pred_d)` from the
#'   same `prepare_data()` call the code under test will make.
#' @param n_draws Posterior draws. 50 keeps fixtures small and quantiles stable.
#' @param seed Integer seed; the same seed always yields the same draws.
#' @param constants Named list of `variable = value`; every element of that
#'   variable is pinned to `value` (used by callers that need an exactly-known
#'   posterior, e.g. a units assertion).
#' @return Named list suitable for `stub_fit()`.
stub_2dt_draws <- function(teams, n_pred, n_draws = 50L, seed = 2100L,
                           constants = list()) {
  stopifnot(is.character(teams), length(teams) >= 2L, n_pred >= 1L)
  k <- length(teams)
  n_draws <- as.integer(n_draws)
  set.seed(seed)

  block <- function(prefix, n, centre, spread, indexed = TRUE) {
    m <- matrix(
      rep(centre, each = n_draws) + stats::rnorm(n_draws * n, 0, spread),
      nrow = n_draws, ncol = n
    )
    if (!is.null(constants[[prefix]])) {
      m[] <- constants[[prefix]]
    }
    colnames(m) <- if (indexed) {
      paste0(prefix, "[", seq_len(n), "]")
    } else {
      prefix
    }
    m
  }

  # Team-ordered strengths: team 1 strongest, so the simulated table matches the
  # facts fixture's deterministic results ordering.
  off <- seq(from = 1.5, to = -1.5, length.out = k)
  def <- seq(from = -1.2, to = 1.2, length.out = k)

  list(
    cur_offense_home   = block("cur_offense_home", k, off + 0.3, 0.4),
    cur_defense_home   = block("cur_defense_home", k, def - 0.2, 0.4),
    cur_strength_home  = block("cur_strength_home", k, off - def + 0.5, 0.5),
    cur_offense_away   = block("cur_offense_away", k, off, 0.4),
    cur_defense_away   = block("cur_defense_away", k, def, 0.4),
    cur_strength_away  = block("cur_strength_away", k, off - def, 0.5),
    home_advantage_off = block("home_advantage_off", k, rep(1.1, k), 0.2),
    home_advantage_def = block("home_advantage_def", k, rep(0.7, k), 0.2),
    home_advantage_tot = block("home_advantage_tot", k, rep(1.8, k), 0.3),
    goals1_pred        = block("goals1_pred", n_pred, rep(24, n_pred), 4),
    goals2_pred        = block("goals2_pred", n_pred, rep(22, n_pred), 4),
    lp__               = block("lp__", 1L, -1234, 5, indexed = FALSE)
  )
}

#' Prepare data once, then build a stub sized from that exact `pred_d`.
#'
#' @return `list(fit =, prep =)`.
local_stub_2dt <- function(league, sex, end_date = FIXTURE_END_DATE, root,
                           n_draws = FIXTURE_N_DRAWS, constants = list()) {
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  stopifnot(nrow(prep$pred_d) > 0L)
  list(
    fit = stub_fit(stub_2dt_draws(
      teams = prep$teams$team,
      n_pred = nrow(prep$pred_d),
      n_draws = n_draws,
      constants = constants
    )),
    prep = prep
  )
}

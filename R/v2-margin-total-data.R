#' Index data for the v2 (margin, total) model.
#'
#' PROTOTYPE (2026-11-15, handball / basketball v2): not called by the
#' production fit path.
#'
#' The v2 model writes a team's strength on a given date as
#' `b[team] + u[team, season] + w[team-season, week]`: a long-run club level, a
#' season level that follows an AR(1) across seasons, and a within-season
#' random walk on calendar weeks that sums to zero over the season. This
#' function turns match rows into the integer indices both engines (Stan and
#' R-INLA) consume, for training rows and prediction rows together, so a
#' fixture in a week or a season with no played match still has a state to
#' read.
#'
#' Weeks are counted from each season's first match date. Pass `season_start`
#' built from the full fixture calendar when comparing fits at different
#' cutoffs, so a week keeps the same index at every cutoff; fixture dates are
#' public before the matches are played, so that leaks nothing.
#'
#' @param train Played matches: `match_date`, `season`, `division`,
#'   `home_team`, `away_team`, `home_score`, `away_score`.
#' @param pred Matches to predict (same columns; scores are not read). May
#'   have zero rows.
#' @param teams Character vector of team names; its order defines the team
#'   index. Every team in `train` and `pred` must be in it.
#' @param season_start Optional named Date vector, one entry per season
#'   (names are the season labels). Seasons not named fall back to the first
#'   match date found in `train` and `pred`.
#' @param n_weeks Optional week-grid length. Defaults to the largest week
#'   index found.
#' @param division_map Optional named character vector that recodes divisions
#'   before they are indexed (for example `c(PO = "OD")` so that handball's
#'   play-off rows share the top division's scoring level).
#' @return A list of dimensions (`K`, `NS`, `TS`, `W`, `ND`, `N`, `N_pred`),
#'   level vectors (`seasons`, `divisions`, `ts_team`, `ts_season`) and, for
#'   the training rows and with a `_pred` suffix for the prediction rows, the
#'   per-match indices `home`, `away`, `season`, `div`, `wk`, `ts_home`,
#'   `ts_away`, plus `y_home` / `y_away` for the training rows.
#' @noRd
build_v2_data <- function(train, pred, teams, season_start = NULL,
                          n_weeks = NULL, division_map = NULL) {
  cols <- c("match_date", "season", "division", "home_team", "away_team")
  stopifnot(all(cols %in% names(train)), all(cols %in% names(pred)))
  both <- rbind(
    as.data.frame(train)[, cols, drop = FALSE],
    as.data.frame(pred)[, cols, drop = FALSE]
  )
  if (nrow(both) == 0L) {
    cli::cli_abort("{.fn build_v2_data} needs at least one match.")
  }
  unknown <- setdiff(c(both$home_team, both$away_team), teams)
  if (length(unknown) > 0L) {
    cli::cli_abort("Teams missing from {.arg teams}: {.val {unknown}}.")
  }

  seasons <- seq(min(both$season), max(both$season))
  found <- tapply(as.numeric(both$match_date), both$season, min)
  start <- stats::setNames(rep(NA_real_, length(seasons)), seasons)
  start[names(found)] <- found
  if (!is.null(season_start)) {
    given <- intersect(names(season_start), names(start))
    start[given] <- as.numeric(season_start[given])
  }

  division <- as.character(both$division)
  if (!is.null(division_map)) {
    hit <- division %in% names(division_map)
    division[hit] <- unname(division_map[division[hit]])
  }
  divisions <- sort(unique(division))

  wk <- as.integer(
    floor((as.numeric(both$match_date) - start[as.character(both$season)]) / 7)
  ) + 1L
  if (any(wk < 1L)) {
    cli::cli_abort("A match is dated before its season's {.arg season_start}.")
  }
  n_weeks <- if (is.null(n_weeks)) max(wk) else as.integer(n_weeks)
  if (max(wk) > n_weeks) {
    cli::cli_abort("Week {max(wk)} exceeds {.arg n_weeks} = {n_weeks}.")
  }

  home <- match(both$home_team, teams)
  away <- match(both$away_team, teams)
  season <- match(both$season, seasons)
  n_seasons <- length(seasons)
  ts_key <- sort(unique(c(
    (home - 1L) * n_seasons + season,
    (away - 1L) * n_seasons + season
  )))
  ts_home <- match((home - 1L) * n_seasons + season, ts_key)
  ts_away <- match((away - 1L) * n_seasons + season, ts_key)

  n <- nrow(train)
  is_train <- seq_len(nrow(both)) <= n
  pick <- function(x, keep) as.integer(x[keep])
  list(
    K = length(teams), NS = n_seasons, TS = length(ts_key), W = n_weeks,
    ND = length(divisions), N = n, N_pred = nrow(pred),
    seasons = seasons, divisions = divisions,
    season_start = as.Date(start, origin = "1970-01-01"),
    ts_team = as.integer((ts_key - 1L) %/% n_seasons + 1L),
    ts_season = as.integer((ts_key - 1L) %% n_seasons + 1L),
    home = pick(home, is_train), away = pick(away, is_train),
    season = pick(season, is_train),
    div = pick(match(division, divisions), is_train),
    wk = pick(wk, is_train),
    ts_home = pick(ts_home, is_train), ts_away = pick(ts_away, is_train),
    y_home = as.numeric(train$home_score), y_away = as.numeric(train$away_score),
    home_pred = pick(home, !is_train), away_pred = pick(away, !is_train),
    season_pred = pick(season, !is_train),
    div_pred = pick(match(division, divisions), !is_train),
    wk_pred = pick(wk, !is_train),
    ts_home_pred = pick(ts_home, !is_train),
    ts_away_pred = pick(ts_away, !is_train)
  )
}

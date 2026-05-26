#' @include extract-iceland-2dt-shared.R config.R
NULL

#' Extract per-fit basketball iceland summaries to a Parquet partition.
#'
#' Writes 5 Parquet files under
#' `<extracts_root>/sport=basketball/country=iceland/sex=<sex>/fit_date=<D>/`:
#'
#' * `predicted_matches.parquet` — per-match posterior summaries (mean
#'   home / away score, mean goal-diff, p_home_win / p_away_win, plus a
#'   binned `goal_diff_distribution` list-column).
#' * `team_strengths_quantiles.parquet` — 9-cell strength grid
#'   (component × location) quantile bands per team in the current top
#'   division.
#' * `home_advantage_quantiles.parquet` — per-team home-advantage
#'   quantile bands by component (offence / defence / total).
#' * `final_positions.parquet` — per-team placement probability
#'   (1..n_teams) at season end.
#' * `points_distribution.parquet` — per-team discrete points-total
#'   distribution.
#'
#' The 4 football-specific extracts (`round_strengths_quantiles`,
#' `tournament_placements`, `sim_inputs_team`, `sim_inputs_scalar`) are
#' intentionally skipped — basketball doesn't currently model a knockout
#' cup, and the per-round strength projection is football-specific.
#'
#' Basketball-specific configuration vs the shared 2DT extractor:
#' top division `"BD"` (Bónusdeild), no draws (`has_ties = FALSE`),
#' goal-diff binned in 5-point buckets across [-50, +50] to match the
#' score scale.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with sport == "basketball" and country == "iceland".
#' @param sex `"male"` or `"female"`.
#' @param fit_date Date stamp written into the partition path. Default
#'   `Sys.Date()`; pass an explicit `as_of` when re-running historically.
#' @param end_date Date for filtering results / schedule. Default `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param extracts_root Optional override for the extracts root.
#'   Defaults to `file.path(root, "beliefs", "extracts")`. Tests can
#'   point at a tempdir.
#' @param prep Optional pre-built result of `prepare_data()`. When
#'   passed, the function skips its own `prepare_data()` call.
#' @return `invisible(NULL)`. Writes 5 Parquet files to the partition.
#' @export
extract_basketball_iceland <- function(fit, league, sex,
                                       fit_date = Sys.Date(),
                                       end_date = fit_date,
                                       root = here::here("data"),
                                       extracts_root = NULL,
                                       prep = NULL) {
  stopifnot(league$sport == "basketball", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))

  .extract_2dt_iceland_pfi(
    fit = fit,
    league = league,
    sex = sex,
    sport = "basketball",
    top_div = "BD",
    bucket_width = 5L,
    bucket_low = -50L,
    bucket_high = 50L,
    has_ties = isTRUE(league$betting$scoring$has_ties),
    tie_threshold = if (is.null(league$betting$scoring$tie_threshold)) {
      0
    } else {
      league$betting$scoring$tie_threshold
    },
    fit_date = fit_date,
    end_date = end_date,
    root = root,
    extracts_root = extracts_root,
    prep = prep
  )
}

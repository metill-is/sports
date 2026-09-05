#' @include extract-iceland-2dt-shared.R config.R publish-profile.R
NULL

#' Extract per-fit basketball iceland summaries to a Parquet partition.
#'
#' Writes one Parquet file per file type under
#' `<extracts_root>/sport=basketball/country=iceland/sex=<sex>/fit_date=<D>/`.
#' Every file below carries a `division` payload column spanning
#' `config/leagues.yml::basketball_iceland.publish_divisions[[sex]]`
#' (`BD` + `1D`); the reader splits on it.
#'
#' * `predicted_matches.parquet` — per-match posterior summaries (mean
#'   home / away score, mean goal-diff, p_home_win / p_away_win, plus a
#'   binned `goal_diff_distribution` list-column).
#' * `team_strengths_quantiles.parquet` — 9-cell strength grid
#'   (component × location) quantile bands per team in the division.
#' * `round_strengths_quantiles.parquet` — the same grid per division
#'   matchweek, from the model's `offense`/`defense` random walk. NOT
#'   football-specific: the 2DT models declare the identical
#'   `array[N_rounds] vector[K]` surface
#'   (Stan/basketball_iceland/2d_student_t_scalarsigma.stan:157,164).
#' * `home_advantage_quantiles.parquet` — per-team home-advantage
#'   quantile bands by component (offence / defence / total).
#' * `final_positions.parquet` — per-team placement probability
#'   (1..n_teams) at season end.
#' * `points_distribution.parquet` — per-team discrete points-total
#'   distribution.
#'
#' `tournament_placements`, `sim_inputs_team` and `sim_inputs_scalar` stay
#' football-only — basketball models no knockout cup.
#'
#' The league table and the trajectory are scoped to the REGULAR season.
#' KKI packages urslitakeppni as extra rounds inside the same `division`
#' and the same `season_id`, so without that cut the published table is
#' simulated on post-season points. See R/publish-format.R.
#'
#' Basketball-specific configuration vs the shared 2DT extractor: no draws
#' (`has_ties = FALSE`), goal-diff binned in 5-point buckets across
#' [-50, +50] to match the score scale.
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
#' @return `invisible(NULL)`. Writes the partition's Parquet files.
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
    key = "basketball_iceland",
    # The bin width comes from the publish profile, not a literal: it is also
    # published as meta.units.diff_bin_width, and two copies of the number
    # drift. See tests/testthat/test-publish-profile-units.R.
    bucket_width = sport_publish_profile("basketball")$units$diff_bin_width,
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

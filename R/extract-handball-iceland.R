#' @include extract-iceland-2dt-shared.R config.R publish-profile.R
NULL

#' Extract per-fit handball iceland summaries to a Parquet partition.
#'
#' Writes 5 Parquet files under
#' `<extracts_root>/sport=handball/country=iceland/sex=<sex>/fit_date=<D>/`,
#' same shape as `extract_basketball_iceland()`. See that function's
#' roxygen for the file-by-file contract; this entry point differs only
#' in the per-sport config it passes through to the shared 2DT
#' extractor.
#'
#' Handball-specific configuration vs the shared 2DT extractor:
#' top division `"BD"`, draws permitted (`has_ties = TRUE` if the league
#' config sets it; default per `config/leagues.yml::handball_iceland.
#' betting.scoring`), goal-diff binned in 2-point buckets across
#' [-20, +20] to match the score scale.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with sport == "handball" and country == "iceland".
#' @param sex `"male"` or `"female"`.
#' @param fit_date Date stamp written into the partition path. Default
#'   `Sys.Date()`.
#' @param end_date Date for filtering results / schedule. Default `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param extracts_root Optional override; defaults to
#'   `file.path(root, "beliefs", "extracts")`.
#' @param prep Optional pre-built `prepare_data()` result.
#' @return `invisible(NULL)`. Writes 5 Parquet files to the partition.
#' @export
extract_handball_iceland <- function(fit, league, sex,
                                     fit_date = Sys.Date(),
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     extracts_root = NULL,
                                     prep = NULL) {
  stopifnot(league$sport == "handball", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))

  .extract_2dt_iceland_pfi(
    fit = fit,
    league = league,
    sex = sex,
    sport = "handball",
    top_div = "OD",
    # The bin width comes from the publish profile, not a literal: it is also
    # published as meta.units.diff_bin_width, and two copies of the number
    # drift. See tests/testthat/test-publish-profile-units.R.
    bucket_width = sport_publish_profile("handball")$units$diff_bin_width,
    bucket_low = -20L,
    bucket_high = 20L,
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

#' @include extract-iceland-2dt-shared.R config.R publish-profile.R
NULL

#' Extract per-fit handball iceland summaries to a Parquet partition.
#'
#' Writes one Parquet file per file type under
#' `<extracts_root>/sport=handball/country=iceland/sex=<sex>/fit_date=<D>/`,
#' same shape as `extract_basketball_iceland()`. See that function's
#' roxygen for the file-by-file contract; this entry point differs only
#' in the per-sport config it passes through to the shared 2DT
#' extractor. The `division` column spans
#' `config/leagues.yml::handball_iceland.publish_divisions[[sex]]`
#' (`OD` + `G66`).
#'
#' Handball-specific configuration vs the shared 2DT extractor: draws
#' permitted (`has_ties = TRUE` if the league config sets it; default per
#' `config/leagues.yml::handball_iceland.betting.scoring`), goal-diff binned
#' in 2-point buckets across [-20, +20] to match the score scale.
#'
#' Handball's post-season is a SEPARATE division (`PO`), not extra rounds
#' inside `OD`, so unlike basketball it needs no regular-season round cut --
#' the division filter already excludes it.
#'
#' @param fit CmdStanMCMC fit object.
#' @param league League list with sport == "handball" and country == "iceland".
#' @param sex `"male"` or `"female"`.
#' @param fit_date Required; there is deliberately no `Sys.Date()` fallback.
#'   Date stamp written into the partition path, which also seeds the season
#'   simulation and, through `end_date`, cuts the results/schedule. Pass the
#'   fit's own date when re-extracting a historical fit.
#' @param end_date Date for filtering results / schedule. Default `fit_date`.
#' @param root Data root. Default `here::here("data")`.
#' @param extracts_root Optional override; defaults to
#'   `file.path(root, "beliefs", "extracts")`.
#' @param prep Optional pre-built `prepare_data()` result.
#' @return `invisible(NULL)`. Writes the partition's Parquet files.
#' @export
extract_handball_iceland <- function(fit, league, sex,
                                     fit_date,
                                     end_date = fit_date,
                                     root = here::here("data"),
                                     extracts_root = NULL,
                                     prep = NULL) {
  stopifnot(league$sport == "handball", league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))

  # `fit_date` is required HERE, not only in `.extract_2dt_iceland_pfi()`. That
  # helper's missing() guard could never fire while this entry point defaulted
  # to `Sys.Date()`, because the default was always passed down as a value --
  # so re-extracting an August fit still landed in a today-stamped partition,
  # seeded on today and cut at today. Checked explicitly rather than left to
  # the raw missing-argument error the `fit_date = fit_date` promise would
  # raise, so the failure names the entry point the caller actually used; the
  # shape checks (length, NA, NULL) stay in the shared extractor.
  if (missing(fit_date)) {
    cli::cli_abort(
      c(
        "{.arg fit_date} is required.",
        "i" = "It keys the partition, seeds the simulation and cuts the data.",
        "x" = "No {.code Sys.Date()} fallback: pass the fit's own date."
      )
    )
  }

  .extract_2dt_iceland_pfi(
    fit = fit,
    league = league,
    sex = sex,
    sport = "handball",
    key = "handball_iceland",
    # The bin width comes from the publish profile, not a literal: it is also
    # published as meta.units.diff_bin_width, and two copies of the number
    # drift. See tests/testthat/test-publish-profile-units.R.
    bucket_width = sport_publish_profile("handball")$units$diff_bin_width,
    bucket_low = -20L,
    bucket_high = 20L,
    has_ties = .tie_params_pfi("handball")$has_ties,
    tie_threshold = .tie_params_pfi("handball")$tie_threshold,
    fit_date = fit_date,
    end_date = end_date,
    root = root,
    extracts_root = extracts_root,
    prep = prep
  )
}

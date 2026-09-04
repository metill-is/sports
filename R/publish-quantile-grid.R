#' Quantile grid stored in the extract parquets.
#'
#' Both extractors used to write all 99 percentiles per
#' (team, round, component, location). The only consumer,
#' `.intervals_from_quantiles_pfi()`, filters to nine of them on its first
#' line and discards the rest -- so 90% of the largest artefact in the repo
#' was computed, written, committed to git and shallow-cloned by nine CI
#' workflows in order to be thrown away. Football's
#' `round_strengths_quantiles.parquet` alone was 8.0 MB of a 22 MB partition.
#'
#' It does not compress its way out either: `value` held 1,001,475 distinct
#' doubles across 1,001,484 rows, so parquet's dictionary and run-length
#' encodings have nothing to exploit -- it is ~8 bytes per row of irreducible
#' payload, and the only lever is writing fewer rows.
#'
#' WHY THIS GRID AND NOT THE NINE THAT ARE USED. Storing exactly what today's
#' publisher wants would bake a presentation choice into the stored data, and
#' changing a coverage band later would need a refit rather than a republish.
#' That is the same class of mistake as B5, where football's `exp()` was
#' carried into a model whose parameter was additive. Every 5th percentile
#' plus the 95% band's interpolated tails costs about 1 MB per partition over
#' the minimal set and makes any 5%-granular band expressible without
#' refitting.
#'
#' Quantile 1 and 99 are deliberately excluded: they are the noisiest tails of
#' a 4000-draw posterior and nothing publishes them.
#'
#' Composition:
#' - `seq(5, 95, by = 5)` -- median, and symmetric bands at 50 / 80 / 90%
#' - `2, 3, 97, 98` -- the 95% band, whose exact 2.5 / 97.5 points are not
#'   representable on an integer 1..99 grid and are interpolated from these
#'
#' @format Integer vector, sorted, length 23.
#' @export
PUBLISH_QUANTILE_GRID <- sort(unique(as.integer(c(
  seq(5L, 95L, by = 5L),
  2L, 3L, 97L, 98L
))))

#' Abort when a consumer needs a quantile the grid does not carry.
#'
#' Without this a missing quantile silently becomes NA after the
#' `pivot_wider()` in `.intervals_from_quantiles_pfi()`, and the published
#' interval is an empty band rather than an error. Adding a coverage band
#' whose tails are unstored must fail loudly, at test time.
#'
#' @param have Quantiles present in the data.
#' @param need Quantiles the caller requires.
#' @param what Human label for the surface, used in the message.
#' @return `invisible(TRUE)`; aborts otherwise.
#' @keywords internal
#' @noRd
.assert_quantiles_available <- function(have, need, what = "quantile surface") {
  missing <- setdiff(as.integer(need), as.integer(have))
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "{what}: needs {length(missing)} quantile{?s} the stored grid does not carry: {missing}.",
        "i" = "Stored grid is PUBLISH_QUANTILE_GRID (R/publish-quantile-grid.R).",
        "i" = "Add them there and re-run the fit -- a republish alone cannot
               recover a quantile that was never written."
      ),
      call = NULL
    )
  }
  invisible(TRUE)
}

#' @include publish-football-iceland.R publish-basketball-iceland.R publish-handball-iceland.R validate-publish.R
NULL

#' Does a football-iceland extract partition exist for this cell?
#'
#' Distinguishes "no fit yet" (no partition -> a legitimate skip) from "a
#' fit_date partition exists but won't read" (corrupt / half-written extract ->
#' a loud failure) in publish_one().
#' @noRd
football_extract_partition_exists <- function(extracts_root, sport, country, sex) {
  cell_dir <- file.path(
    extracts_root,
    paste0("sport=", sport),
    paste0("country=", country),
    paste0("sex=", sex)
  )
  dir.exists(cell_dir) && length(list.dirs(cell_dir, recursive = FALSE)) > 0L
}

#' Publish JSONs for a single (league x sex).
#'
#' Football iceland reads the per-fit extraction tree
#' (`data/beliefs/extracts/sport=football/country=iceland/sex=Z/fit_date=*/`,
#' the per-cell Parquets emitted by `extract_football_iceland()`) and
#' dispatches to `publish_football_iceland(extracted, ...)`. Basketball and
#' handball still read the fit RDS directly from
#' `data/beliefs/fits/sport=X/country=Y/sex=Z/fit.rds` -- their migration
#' to the extraction layer is deferred to the autumn 2026 cutover.
#'
#' Takes the static + betting slices separately because basketball/handball
#' publishers branch on `betting.scoring` (tie thresholds); a `lengjan`
#' change shouldn't trigger a republish.
#'
#' After a successful publish, the resulting JSONs are validated against
#' `config/publish-schemas/<sport>/*.schema.json` via `validate_publish_dir()`.
#' Failures abort with the list of (file, error) entries -- the cron pipeline
#' surfaces the message in its run log so drift between producer and
#' platform-side consumer is caught locally. Set `validate = FALSE` for
#' synthetic-data tests where the schema would reject the fixture by design.
#'
#' @param static Per-league static slice (sport, country, ...).
#' @param betting Per-league `betting` slice.
#' @param key League key (used only to dispatch to the per-sport publisher).
#' @param sex `"male"` or `"female"`.
#' @param root Storage root.
#' @param validate Logical. When `TRUE` (default) the published JSONs are
#'   validated against `config/publish-schemas/<sport>/`. Failures abort.
#' @return invisible(NULL).
#' @export
publish_one <- function(static, betting, key, sex,
                        root = here::here("data"),
                        validate = TRUE) {
  league <- static
  league$betting <- betting

  output_root <- file.path(root, "publish")

  if (identical(key, "football_iceland")) {
    extracts_root <- file.path(root, "beliefs", "extracts")
    archive_root <- file.path(root, "beliefs", "archive")
    extracted <- tryCatch(
      read_extracted_football(
        league = league,
        sex = sex,
        extracts_root = extracts_root
      ),
      error = function(e) {
        # Re-raise loudly when a fit_date partition EXISTS but won't read (a
        # corrupt / half-written extract) so the publish step fails rather than
        # silently republishing yesterday's JSON. Stay quiet when there is no
        # fit yet (no partition) -- a legitimate skip.
        if (football_extract_partition_exists(
          extracts_root, league$sport, league$country, sex
        )) {
          cli::cli_abort(
            c(
              "publish_one(football_iceland/{sex}): extract read failed despite an existing fit_date partition.",
              "x" = conditionMessage(e),
              "i" = "Likely a corrupt or incomplete extract write under {.path {extracts_root}}."
            ),
            call = NULL
          )
        }
        cli::cli_alert_warning(
          "publish_one(football_iceland/{sex}): {conditionMessage(e)} (no extract partition yet \u2014 skipping)"
        )
        NULL
      }
    )
    if (is.null(extracted)) {
      return(invisible(NULL))
    }
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = sex,
      root = root,
      output_root = output_root,
      extracts_root = extracts_root,
      archive_root = archive_root
    )
    if (isTRUE(validate)) {
      .validate_or_abort(output_root, sport = "football", key = key, sex = sex)
    }
    return(invisible(NULL))
  }

  fit_path <- file.path(
    root, "beliefs", "fits",
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex),
    "fit.rds"
  )
  if (!file.exists(fit_path)) {
    cli::cli_alert_warning(
      "No fit at {fit_path} -- skipping publish_{key}_{sex}"
    )
    return(invisible(NULL))
  }

  dispatch <- list(
    basketball_iceland = publish_basketball_iceland,
    handball_iceland   = publish_handball_iceland
  )
  pub_fn <- dispatch[[key]]
  if (is.null(pub_fn)) {
    cli::cli_alert_info("No publish dispatcher for {key} -- skipping")
    return(invisible(NULL))
  }

  fit <- readRDS(fit_path)
  pub_fn(fit, league, sex = sex)
  if (isTRUE(validate)) {
    .validate_or_abort(
      output_root,
      sport = league$sport,
      key = key,
      sex = sex
    )
  }
  invisible(NULL)
}

.validate_or_abort <- function(output_root, sport, key, sex) {
  schema_dir <- here::here("config", "publish-schemas")
  sport_dir <- file.path(output_root, sport)
  if (!dir.exists(sport_dir) || !dir.exists(schema_dir)) {
    return(invisible(NULL))
  }
  if (!dir.exists(file.path(schema_dir, sport))) {
    # No schemas for this sport yet (e.g. basketball / handball pre-F6).
    # Skip with an informational note.
    cli::cli_alert_info(
      "publish_one({key}/{sex}): no schemas at {schema_dir}/{sport}/ -- skipping validation"
    )
    return(invisible(NULL))
  }

  result <- validate_publish_dir(output_root, schema_dir = schema_dir)
  if (isTRUE(result$ok)) {
    cli::cli_alert_success(
      "publish_one({key}/{sex}): schema validation passed ({result$n_passed}/{result$n_files} files)"
    )
    return(invisible(NULL))
  }

  cli::cli_alert_danger(
    "publish_one({key}/{sex}): schema validation FAILED ({result$n_failed}/{result$n_files} files)"
  )
  for (e in result$errors) {
    cli::cli_alert_warning(e)
  }
  cli::cli_abort(
    paste0(
      "publish_one({key}/{sex}) produced JSONs that do not match ",
      "config/publish-schemas/{sport}/*.schema.json. See errors above. ",
      "Either fix the publisher or, if the schema is genuinely stale, ",
      "update the schema with a corresponding test."
    )
  )
}

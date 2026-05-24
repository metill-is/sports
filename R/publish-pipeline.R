#' @include publish-football-iceland.R publish-basketball-iceland.R publish-handball-iceland.R
NULL

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
#' @param static Per-league static slice (sport, country, ...).
#' @param betting Per-league `betting` slice.
#' @param key League key (used only to dispatch to the per-sport publisher).
#' @param sex `"male"` or `"female"`.
#' @param root Storage root.
#' @return invisible(NULL).
#' @export
publish_one <- function(static, betting, key, sex,
                        root = here::here("data")) {
  league <- static
  league$betting <- betting

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
        cli::cli_alert_warning(
          "publish_one(football_iceland/{sex}): {conditionMessage(e)}"
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
      output_root = file.path(root, "publish"),
      extracts_root = extracts_root,
      archive_root = archive_root
    )
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
  invisible(NULL)
}

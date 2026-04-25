#' @include publish-football-iceland.R publish-basketball-iceland.R publish-handball-iceland.R
NULL

#' tar_target wrapper: publish JSONs for a single (league x sex).
#'
#' Reads the fit RDS from `data/beliefs/fits/sport=X/country=Y/sex=Z/fit.rds`
#' (saved by `fit_league` as a side-effect) and dispatches to the appropriate
#' `publish_<sport>_iceland()`. Plan 6 cutover: replaces SPORTS_BACKUP_ROOT
#' with a canonical in-tree path.
#'
#' @param leagues Output of `load_leagues()`.
#' @param key League key.
#' @param sex `"male"` or `"female"`.
#' @param fit_dep,decide_dep DAG-only dependency declarations; ignored.
#' @param root Storage root.
#' @return invisible(NULL).
#' @export
publish_one <- function(leagues, key, sex, fit_dep = NULL, decide_dep = NULL,
                        root = here::here("data")) {
  league <- leagues[[key]]
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
    football_iceland   = publish_football_iceland,
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

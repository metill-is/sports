#' @include model-league.R extract-football-iceland.R publish-football-iceland.R
NULL

#' Replay the football iceland publisher for a historical date.
#'
#' Orchestrator behind `scripts/0Nr_replay.R`. Wraps the existing
#' fit + extract + publish chain with two replay-specific behaviours:
#'
#'   * Stan seed is derived from `as_of` (`as.integer(format(as_of, "%Y%m%d"))`)
#'     so a re-run on the same date reproduces the same posterior draws.
#'     Matches the bracket-simulator convention.
#'   * `meta.json::fit_date` reflects the extracts partition (F2 honesty
#'     fix from the 2026-05-25 audit) — so a stale republish reports its
#'     own age, not today's date.
#'
#' Three call shapes:
#'
#'   * Default (`fit = TRUE`): refit at `end_date = as_of`, then publish.
#'     Wall-clock ~hours (full Stan fit). Use for backfilling missing
#'     historical fits or for true "as-of" reconstruction.
#'   * `fit = FALSE`: republish from existing extracts at `fit_date = as_of`.
#'     Errors if no such partition exists. Wall-clock seconds. Use for
#'     schema-only iteration on the publisher.
#'   * `output_root` overridden to a non-default path: isolated what-if
#'     publish that won't touch the live `data/publish/` tree. Pair with
#'     `fit = TRUE` or `fit = FALSE` either way.
#'
#' Basketball + handball not yet supported (their publishers still consume
#' the fit RDS directly — F6 in the audit roadmap).
#'
#' @param sex `"male"` or `"female"`.
#' @param as_of Target date. Date, or a string parseable by `as.Date()`.
#' @param fit Logical. `TRUE` (default) re-fits at `as_of` before publish.
#'   `FALSE` requires existing extracts at `as_of` and skips the fit.
#' @param output_root Where to write the publish JSONs. Default
#'   `here::here("data", "publish")` — the live production tree. Override
#'   to a tempdir or `data/publish_replay/<date>/` for what-if work.
#' @param root Data root. Default `here::here("data")`.
#' @param extracts_root Football extracts root.
#' @param archive_root Beliefs archive root.
#' @param round_predictions_history_root Optional override for the
#'   publisher-internal `round_predictions_history.json` tree. Defaults
#'   to a sibling of `output_root` via `publish_football_iceland()`.
#' @param schedule_horizon_days Days ahead of `as_of` to include in
#'   predictions. Default `200L` to mirror `03b_backfill_*`.
#' @return `invisible(NULL)`.
#' @export
replay_football_iceland <- function(sex,
                                    as_of,
                                    fit = TRUE,
                                    output_root = here::here(
                                      "data", "publish"
                                    ),
                                    root = here::here("data"),
                                    extracts_root = file.path(
                                      root, "beliefs", "extracts"
                                    ),
                                    archive_root = file.path(
                                      root, "beliefs", "archive"
                                    ),
                                    round_predictions_history_root = NULL,
                                    schedule_horizon_days = 200L) {
  stopifnot(sex %in% c("male", "female"))
  as_of <- as.Date(as_of)
  stopifnot(!is.na(as_of))

  leagues <- load_leagues()
  league <- leagues[["football_iceland"]]

  if (isTRUE(fit)) {
    seed_int <- as.integer(format(as_of, "%Y%m%d"))
    cli::cli_alert_info(
      "replay({sex}, as_of={format(as_of)}): refit with seed={seed_int}"
    )
    fit_league(
      league_key = "football_iceland",
      sex = sex,
      fit_date = as_of,
      end_date = as_of,
      seed = seed_int,
      schedule_horizon_days = schedule_horizon_days,
      root = root
    )
  }

  partition <- file.path(
    extracts_root,
    "sport=football",
    "country=iceland",
    paste0("sex=", sex),
    paste0("fit_date=", format(as_of, "%Y-%m-%d"))
  )
  if (!dir.exists(partition)) {
    stop(
      "replay_football_iceland(", sex, ", as_of=", format(as_of),
      "): no extracts at ", partition,
      ". Re-run with `fit = TRUE` to generate them, or pick an `as_of` ",
      "that has an existing partition.",
      call. = FALSE
    )
  }

  extracted <- read_extracted_football(
    league,
    sex = sex,
    fit_date = as_of,
    extracts_root = extracts_root
  )

  publish_args <- list(
    extracted = extracted,
    league = league,
    sex = sex,
    end_date = as_of,
    root = root,
    output_root = output_root,
    extracts_root = extracts_root,
    archive_root = archive_root
  )
  if (!is.null(round_predictions_history_root)) {
    publish_args$round_predictions_history_root <- round_predictions_history_root
  }
  do.call(publish_football_iceland, publish_args)

  invisible(NULL)
}

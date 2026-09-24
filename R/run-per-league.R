#' Run one pipeline step per league, containing each league's failure.
#'
#' The odds scrape (`scripts/02_scrape_odds.R`) and the decide layer
#' (`scripts/04_decide.R`) walk `config/leagues.yml` order -- basketball,
#' handball, football -- so a bare loop let one plain error in an earlier
#' league (an unexpected Lengjan API shape, an HTML body where JSON was due,
#' a paper league's decide) abort the loop before football, the live money
#' path, and the scrape workflow then committed nothing from the run (final
#' review F1, 2026-09-23). This runs `fn` for every work item inside
#' `tryCatch()`, alerts loudly naming the league and the error, and carries on.
#'
#' Failures are collected, not swallowed: the caller turns a non-empty
#' `failed` frame into `quit(status = 1L)`, on ANY failure (INT-2, as in
#' [run_fit_targets()] and [run_publish_targets()]). Classed conditions that
#' `fn` already handles itself -- `ingest_one_lengjan()`'s `lengjan_fetch_error`
#' soft-fail to 0 rows -- never reach this handler, so they stay successes.
#'
#' @param keys Vector of work items, each passed to `fn` as `fn(keys[[i]])`.
#'   `names(keys)`, when set, label the items in alerts and in `failed` (the
#'   decide script passes row indices named `"<key> (<sex>)"`); otherwise the
#'   items label themselves.
#' @param fn Function of one work item.
#' @param what Short name of the step, for the alert (e.g. `"odds scrape"`).
#' @return `list(results, failed)`: `results` is a list named by label holding
#'   `fn`'s value per item (`NULL` for a failed item or an `fn` that returned
#'   `NULL`); `failed` is a tibble with columns `key` (label) and `message`.
#' @keywords internal
#' @noRd
run_per_league <- function(keys, fn, what = "step") {
  labels <- names(keys)
  if (is.null(labels)) labels <- as.character(keys)
  results <- stats::setNames(vector("list", length(keys)), labels)
  failed <- list()

  for (i in seq_along(keys)) {
    label <- labels[[i]]
    out <- tryCatch(
      list(value = fn(keys[[i]])),
      error = function(e) {
        cli::cli_alert_danger(
          "{what} failed for {label}: {conditionMessage(e)}"
        )
        failed[[length(failed) + 1L]] <<- tibble::tibble(
          key = label, message = conditionMessage(e)
        )
        NULL
      }
    )
    if (!is.null(out) && !is.null(out$value)) results[i] <- list(out$value)
  }

  list(
    results = results,
    failed = if (length(failed) == 0L) {
      tibble::tibble(key = character(), message = character())
    } else {
      dplyr::bind_rows(failed)
    }
  )
}

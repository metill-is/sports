#' @include storage.R
NULL

#' Append a placed-bet row to the ledger.
#'
#' Writes to `data/decisions/ledger/` Parquet by reading the existing
#' partition, row-binding the new row, and calling [write_table()] — preserving
#' all existing rows. When `dual_write_csv = TRUE`, also appends to
#' `<legacy_root>/<sport>/<country>/history/bets_log.csv` for cutover safety.
#'
#' This is the only writer to the ledger.
#'
#' @param bet_row Single-row tibble matching `schemas()$ledger`.
#' @param root Data root for the Parquet ledger.
#' @param dual_write_csv Default `FALSE` (Plan 6 cutover from Plan 5's
#'   dual-write). Set `TRUE` only for opt-in regression-testing -- Parquet
#'   is the canonical store from Plan 6 onward.
#' @param legacy_root Root path for legacy CSV writes (default
#'   `<root>/../_legacy/sports`).
#' @return invisible(NULL)
#' @export
append_to_ledger <- function(bet_row, root,
                             dual_write_csv = FALSE,
                             legacy_root = normalizePath(
                               file.path(root, "..", "_legacy", "sports"),
                               mustWork = FALSE
                             )) {
  stopifnot(nrow(bet_row) == 1L)

  # Pull the column list from the canonical Arrow schema so this guard tracks
  # any future ledger-schema additions automatically. write_table() validates
  # types in the same call; this hand-rolled check is purely for a clearer
  # "missing column(s)" error message before the Arrow conversion.
  required <- schemas()$ledger$names
  missing_cols <- setdiff(required, names(bet_row))
  if (length(missing_cols) > 0L) {
    stop(
      "append_to_ledger: bet_row missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Read existing rows for this partition (sport + country), bind, re-write.
  # The ledger has no natural dedup key — every placed bet is a unique event —
  # so we simply accumulate without deduplication. Sequential placer only:
  # concurrent calls on the same partition would race on the read-modify-write.
  part_filter <- list(
    sport = bet_row$sport[[1]],
    country = bet_row$country[[1]]
  )
  existing <- tryCatch(
    read_table("ledger", root = root, filter = part_filter),
    error = function(e) tibble::tibble()
  )

  combined <- if (nrow(existing) > 0L) {
    dplyr::bind_rows(existing, bet_row)
  } else {
    bet_row
  }

  write_table(combined, "ledger", root = root)

  if (isTRUE(dual_write_csv)) {
    csv_dir <- file.path(
      legacy_root,
      bet_row$sport[[1]], bet_row$country[[1]],
      "history"
    )
    dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
    csv_path <- file.path(csv_dir, "bets_log.csv")

    # No dedup on CSV — a retry would produce a duplicate row. Parquet is the
    # canonical store; CSV dual-write is opt-in post Plan 6 cutover.
    if (file.exists(csv_path)) {
      existing_csv <- readr::read_csv(csv_path, show_col_types = FALSE)
      combined_csv <- dplyr::bind_rows(existing_csv, bet_row)
    } else {
      combined_csv <- bet_row
    }
    readr::write_csv(combined_csv, csv_path)
  }

  invisible(NULL)
}

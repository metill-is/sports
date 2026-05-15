#' @include storage-schemas.R
NULL

#' Partitioning rules per table (spec §3.2)
#' @noRd
table_partitions <- function() {
  list(
    results = c("sport", "country", "sex", "season"),
    schedules = c("sport", "country", "sex", "season"),
    odds = c("sport", "country", "scraped_date"),
    beliefs_latest = c("sport", "country", "sex"),
    beliefs_archive = c("sport", "country", "sex", "fit_date"),
    beliefs_by_round = c("sport", "country", "sex", "season", "round_cutoff"),
    candidates = c("sport", "country", "run_date"),
    recommendations = c("sport", "country", "run_date"),
    ledger = c("sport", "country")
  )
}

#' Map each table to its subdirectory under `root` (spec §3.1)
#' @noRd
table_subdir <- function(table) {
  switch(table,
    results = c("facts", "results"),
    schedules = c("facts", "schedules"),
    odds = c("facts", "odds"),
    beliefs_latest = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    beliefs_by_round = c("beliefs", "by_round"),
    candidates = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger = c("decisions", "ledger"),
    stop("Unknown table: ", table, call. = FALSE)
  )
}

#' Derive any virtual partition columns the table needs (e.g. scraped_date from
#' scraped_at, run_date from run_id) before validation.
#' @noRd
add_virtual_partitions <- function(df, table) {
  if (table == "odds" && !("scraped_date" %in% names(df)) && "scraped_at" %in% names(df)) {
    df$scraped_date <- as.Date(df$scraped_at)
  }
  if (table %in% c("candidates", "recommendations") &&
    !("run_date" %in% names(df)) && "run_id" %in% names(df)) {
    df$run_date <- as.Date(df$run_id)
  }
  if (table == "beliefs_archive" && !("fit_date" %in% names(df))) {
    stop("beliefs_archive requires fit_date", call. = FALSE)
  }
  df
}

#' Validate a data frame against a schema.
#' Raises a diagnostic error on the first problem found; returns `invisible(TRUE)`
#' on success so the caller can keep using the original (augmented) data frame
#' and does not have to round-trip through an Arrow Table that drops virtual
#' partition columns.
#' @noRd
validate_against_schema <- function(df, table) {
  s <- schemas()[[table]]
  if (is.null(s)) stop("Unknown table: ", table, call. = FALSE)

  required_cols <- s$names
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Table '%s' missing column(s): %s",
        table, paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Try materialising an Arrow Table; Arrow raises on type mismatch. The result
  # is discarded — this function is validation-only, and the caller keeps the
  # original augmented data frame (which may carry virtual partition columns
  # that are not in the schema).
  df_ordered <- df[, required_cols]
  tryCatch(
    arrow::as_arrow_table(df_ordered, schema = s),
    error = function(e) {
      msg <- conditionMessage(e)
      # Attempt to identify which column(s) failed by trying each one individually
      # so the error message always names the offending column.
      offenders <- character(0)
      for (col in required_cols) {
        field <- s$GetFieldByName(col)
        col_df <- df_ordered[, col, drop = FALSE]
        col_schema <- arrow::schema(stats::setNames(list(field$type), col))
        ok <- tryCatch(
          {
            arrow::as_arrow_table(col_df, schema = col_schema)
            TRUE
          },
          error = function(e2) FALSE
        )
        if (!ok) offenders <- c(offenders, col)
      }
      detail <- if (length(offenders) > 0) {
        sprintf(" (offending column(s): %s)", paste(offenders, collapse = ", "))
      } else {
        ""
      }
      stop(
        sprintf(
          "Schema validation failed for table '%s'%s: %s",
          table, detail, msg
        ),
        call. = FALSE
      )
    }
  )
  invisible(TRUE)
}

#' Write a data frame to the store as hive-partitioned Parquet.
#'
#' @param df data frame or tibble.
#' @param table one of names(schemas()).
#' @param root filesystem root (defaults to here::here("data")).
#' @return invisible(NULL)
#' @details
#' Writes are hive-partitioned. Each call overwrites the existing `part-*.parquet`
#' files for matching partitions — it does NOT append. Callers that need to grow
#' a partition (notably the `ledger` table which has genuine append semantics)
#' must read the existing partition, row-bind the new rows, and pass the full
#' combined frame to `write_table()`.
#'
#' Some tables define virtual partition columns that are derived from a source
#' column (e.g. `odds` partitions on `scraped_date` derived from `scraped_at`;
#' `candidates` and `recommendations` partition on `run_date` derived from
#' `run_id`). These are added by `add_virtual_partitions()` before validation
#' and passed through to `arrow::write_dataset()` alongside the schema columns.
#' @export
write_table <- function(df, table, root = here::here("data")) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  df <- add_virtual_partitions(df, table)
  validate_against_schema(df, table)
  partitions <- table_partitions()[[table]]

  dest <- do.call(fs::path, c(list(root), table_subdir(table)))
  fs::dir_create(dest, recurse = TRUE)

  arrow::write_dataset(
    df,
    path = dest,
    format = "parquet",
    partitioning = partitions,
    existing_data_behavior = "overwrite"
  )

  invisible(NULL)
}

#' Natural match-key columns per table (for upsert dedup).
#'
#' Unlike the partition columns, which identify which Parquet files to write,
#' the natural key identifies one row within a partition. Re-ingests that
#' return the same key with differing metadata (e.g. corrected scores) keep
#' the newer row; re-ingests that return a strict subset of an earlier fetch
#' keep the full union.
#'
#' For \code{odds}, `scraped_at` is part of the natural key so each cron
#' snapshot accretes within the day's partition rather than overwriting
#' earlier snapshots. Same-second re-runs (typically only in tests) dedup
#' against the bet's full identity.
#' @noRd
natural_key_for <- function(table) {
  switch(table,
    results = c(
      "sport", "country", "sex", "season", "match_date",
      "home_team", "away_team"
    ),
    schedules = c(
      "sport", "country", "sex", "season", "match_date",
      "home_team", "away_team"
    ),
    odds = c(
      "sport", "country", "scraped_at", "match_date",
      "home_team", "away_team", "market", "outcome", "line"
    ),
    stop("upsert_table not supported for table: ", table, call. = FALSE)
  )
}

#' Write a frame into the store with per-partition merge semantics.
#'
#' Unlike [write_table()], which overwrites all Parquet files that match the
#' partition columns of the incoming frame, `upsert_table()` reads the existing
#' rows of each touched partition, row-binds the new rows, and deduplicates on
#' the natural match key (see [natural_key_for()]). Partitions that the new
#' frame does not touch are left on disk untouched.
#'
#' When the new frame collides with an existing row on the natural match key,
#' the NEW row wins — this lets corrections (e.g. finalised scores) propagate.
#'
#' Currently supports `"results"` and `"schedules"`. Other tables fall through
#' to `natural_key_for()` which raises a clear error.
#'
#' @param df A tibble or data frame matching the schema for `table`.
#' @param table One of `"results"`, `"schedules"`.
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @return invisible(NULL)
#' @export
upsert_table <- function(df, table, root = here::here("data")) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  df <- add_virtual_partitions(df, table)
  partitions <- table_partitions()[[table]]
  nat_key <- natural_key_for(table)

  missing <- setdiff(partitions, names(df))
  if (length(missing) > 0) {
    stop(
      sprintf(
        "upsert_table: frame missing partition column(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Distinct partition keys in the incoming frame determine which partitions
  # we need to read, merge, and re-write.
  partition_keys <- dplyr::distinct(df[, partitions, drop = FALSE])

  for (i in seq_len(nrow(partition_keys))) {
    part_filter <- as.list(partition_keys[i, , drop = FALSE])

    # Mask: rows of df that belong to this partition.
    mask <- rep(TRUE, nrow(df))
    for (col in partitions) {
      mask <- mask & (df[[col]] == partition_keys[[col]][i])
    }
    new_rows <- df[mask, , drop = FALSE]

    existing <- tryCatch(
      read_table(table, root = root, filter = part_filter),
      error = function(e) tibble::tibble()
    )

    combined <- if (nrow(existing) > 0 &&
      all(nat_key %in% names(existing))) {
      # Arrow returns hive-partition columns as character (parsed from the
      # directory name) regardless of the in-memory type they had at write
      # time. Coerce existing's partition columns back to the source type
      # before bind_rows so dplyr doesn't error on type-mismatch.
      for (col in intersect(partitions, names(new_rows))) {
        target_class <- class(new_rows[[col]])[[1L]]
        if (!identical(class(existing[[col]])[[1L]], target_class)) {
          existing[[col]] <- switch(target_class,
            "Date"      = as.Date(existing[[col]]),
            "integer"   = as.integer(existing[[col]]),
            "numeric"   = as.numeric(existing[[col]]),
            "double"    = as.numeric(existing[[col]]),
            "character" = as.character(existing[[col]]),
            existing[[col]]
          )
        }
      }
      # Put new rows first so row-wise dedup keeps the newer version on
      # natural-key collisions.
      bound <- dplyr::bind_rows(new_rows, existing)
      dup <- duplicated(bound[, nat_key, drop = FALSE])
      bound[!dup, , drop = FALSE]
    } else {
      new_rows
    }

    write_table(combined, table, root = root)
  }

  invisible(NULL)
}

#' Read a table back as a tibble.
#'
#' @param table one of names(schemas()).
#' @param root filesystem root.
#' @param filter optional named list of column=value filters pushed down to Arrow.
#' @return tibble
#' @importFrom rlang .data
#' @export
read_table <- function(table, root = here::here("data"), filter = list()) {
  src <- do.call(fs::path, c(list(root), table_subdir(table)))
  if (!fs::dir_exists(src)) {
    return(tibble::tibble())
  }

  ds <- arrow::open_dataset(src, partitioning = table_partitions()[[table]])

  if (length(filter) > 0) {
    for (col in names(filter)) {
      val <- filter[[col]]
      ds <- dplyr::filter(ds, .data[[col]] == val)
    }
  }

  dplyr::collect(ds) |> tibble::as_tibble()
}

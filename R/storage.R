#' Partitioning rules per table (spec §3.2)
table_partitions <- function() {
  list(
    results         = c("sport", "country", "sex", "season"),
    schedules       = c("sport", "country", "sex", "season"),
    odds            = c("sport", "country", "scraped_date"),
    beliefs_latest  = c("sport", "country", "sex"),
    beliefs_archive = c("sport", "country", "sex", "fit_date"),
    candidates      = c("sport", "country", "run_date"),
    recommendations = c("sport", "country", "run_date"),
    ledger          = c("sport", "country")
  )
}

#' Map each table to its subdirectory under `root` (spec §3.1)
table_subdir <- function(table) {
  switch(table,
    results = c("facts", "results"),
    schedules = c("facts", "schedules"),
    odds = c("facts", "odds"),
    beliefs_latest = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    candidates = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger = c("decisions", "ledger"),
    stop("Unknown table: ", table, call. = FALSE)
  )
}

#' Derive any virtual partition columns the table needs (e.g. scraped_date from
#' scraped_at, run_date from run_id) before validation.
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
#' Raises a diagnostic error on the first problem found.
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

  # Try materialising an Arrow Table; Arrow raises on type mismatch.
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
}

#' Write a data frame to the store as hive-partitioned Parquet.
#'
#' @param df data frame or tibble.
#' @param table one of names(schemas()).
#' @param root filesystem root (defaults to here::here("data")).
#' @return invisible(NULL)
#' @export
write_table <- function(df, table, root = here::here("data")) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  df <- add_virtual_partitions(df, table)
  tbl <- validate_against_schema(df, table)
  partitions <- table_partitions()[[table]]

  dest <- do.call(fs::path, c(list(root), table_subdir(table)))
  fs::dir_create(dest, recurse = TRUE)

  arrow::write_dataset(
    tbl,
    path = dest,
    format = "parquet",
    partitioning = partitions,
    existing_data_behavior = "overwrite"
  )

  invisible(NULL)
}

#' Read a table back as a tibble.
#'
#' @param table one of names(schemas()).
#' @param root filesystem root.
#' @param filter optional named list of column=value filters pushed down to Arrow.
#' @return tibble
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

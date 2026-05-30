#' Regenerate sports.duckdb with views over every Parquet store.
#'
#' Non-destructive: the old file is deleted and a fresh one written.
#' Because views are just SQL over Parquet paths, the resulting DB file
#' is small (<1 MB) and fully regenerable.
#'
#' @param root Filesystem root containing data/.
#' @param db_path Where to write the DuckDB file.
#' @return invisible(db_path)
#' @export
rebuild_duckdb <- function(root = here::here("data"),
                           db_path = here::here("sports.duckdb")) {
  if (file.exists(db_path)) fs::file_delete(db_path)
  wal <- paste0(db_path, ".wal")
  if (file.exists(wal)) fs::file_delete(wal)

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  subdirs <- list(
    results         = c("facts", "results"),
    schedules       = c("facts", "schedules"),
    odds            = c("facts", "odds"),
    beliefs_latest  = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    candidates      = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger          = c("decisions", "ledger"),
    fit_diagnostics = c("beliefs", "diagnostics")
  )

  for (view_name in names(subdirs)) {
    dir <- do.call(fs::path, c(list(root), subdirs[[view_name]]))
    if (!fs::dir_exists(dir)) next

    glob <- sprintf("%s/**/*.parquet", dir)
    # union_by_name reconciles schema drift across partitions (e.g. a column
    # added to a store after older partitions were written, like
    # schedules.kickoff_time) by name, null-filling absentees -- without it
    # read_parquet adopts the first file's schema and silently drops the new
    # column. Verified safe on the odds store's mixed scraped_at tz partitions.
    sql <- sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s', hive_partitioning = TRUE, union_by_name = TRUE)",
      view_name, glob
    )
    DBI::dbExecute(con, sql)
  }

  cli::cli_inform("Rebuilt DuckDB at {.path {db_path}}")
  invisible(db_path)
}

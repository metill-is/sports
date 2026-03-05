#' Centralised Parquet storage layer for Sports pipeline
#'
#' Hive-partitioned Parquet store for cross-league queries.
#' All writes are wrapped in tryCatch — failures log warnings, never stop the pipeline.
#'
#' Layout:
#'   store/predictions/sport={X}/country={Y}/sex={Z}/predictions.parquet
#'   store/bets/sport={X}/country={Y}/sex={Z}/bets.parquet
#'
#' Load via source() — not box::use() — to avoid path resolution issues.

# ---------------------------------------------------------------------------
# Write functions
# ---------------------------------------------------------------------------

#' Write posterior predictions to the Parquet store
#'
#' Overwrites the partition each run (latest predictions only).
#'
#' @param df Data frame of posterior draws (posterior_goals.csv schema)
#' @param sport Sport name (e.g., "basketball")
#' @param country Country name (e.g., "iceland")
#' @param sex Sex label (e.g., "male")
#' @param sports_dir Absolute path to Sports/ root
store_predictions <- function(df, sport, country, sex, sports_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    warning("arrow package not available — skipping Parquet store write")
    return(invisible(NULL))
  }

  tryCatch({
    partition_dir <- file.path(
      sports_dir, "store", "predictions",
      paste0("sport=", sport),
      paste0("country=", country),
      paste0("sex=", sex)
    )
    if (!dir.exists(partition_dir)) dir.create(partition_dir, recursive = TRUE)

    out_path <- file.path(partition_dir, "predictions.parquet")
    arrow::write_parquet(df, out_path)
    cat("  Store: wrote", nrow(df), "prediction rows to", out_path, "\n")
  }, error = function(e) {
    warning("Store prediction write failed: ", e$message)
  })

  invisible(NULL)
}

#' Write bet history to the Parquet store
#'
#' Full overwrite of partition (caller provides complete data).
#'
#' @param df Data frame of bet rows (bets_log.csv schema)
#' @param sport Sport name
#' @param country Country name
#' @param sex Sex label (use "all" for settle which writes all sexes)
#' @param sports_dir Absolute path to Sports/ root
store_bets <- function(df, sport, country, sex, sports_dir) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    warning("arrow package not available — skipping Parquet store write")
    return(invisible(NULL))
  }

  tryCatch({
    partition_dir <- file.path(
      sports_dir, "store", "bets",
      paste0("sport=", sport),
      paste0("country=", country),
      paste0("sex=", sex)
    )
    if (!dir.exists(partition_dir)) dir.create(partition_dir, recursive = TRUE)

    out_path <- file.path(partition_dir, "bets.parquet")
    arrow::write_parquet(df, out_path)
    cat("  Store: wrote", nrow(df), "bet rows to", out_path, "\n")
  }, error = function(e) {
    warning("Store bet write failed: ", e$message)
  })

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Read functions
# ---------------------------------------------------------------------------

#' Read predictions from the Parquet store
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Filter by sport (NULL = all)
#' @param country Filter by country (NULL = all)
#' @param sex Filter by sex (NULL = all)
#' @return Tibble of predictions, or NULL if store doesn't exist
read_predictions <- function(sports_dir, sport = NULL, country = NULL, sex = NULL) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(NULL)

  store_path <- file.path(sports_dir, "store", "predictions")
  if (!dir.exists(store_path)) return(NULL)

  tryCatch({
    ds <- arrow::open_dataset(store_path)
    if (!is.null(sport))   ds <- ds |> dplyr::filter(sport == !!sport)
    if (!is.null(country)) ds <- ds |> dplyr::filter(country == !!country)
    if (!is.null(sex))     ds <- ds |> dplyr::filter(sex == !!sex)
    dplyr::collect(ds)
  }, error = function(e) {
    warning("Store prediction read failed: ", e$message)
    NULL
  })
}

#' Read bets from the Parquet store
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Filter by sport (NULL = all)
#' @param country Filter by country (NULL = all)
#' @param sex Filter by sex (NULL = all)
#' @return Tibble of bets, or NULL if store doesn't exist
read_bets <- function(sports_dir, sport = NULL, country = NULL, sex = NULL) {
  if (!requireNamespace("arrow", quietly = TRUE)) return(NULL)

  store_path <- file.path(sports_dir, "store", "bets")
  if (!dir.exists(store_path)) return(NULL)

  tryCatch({
    ds <- arrow::open_dataset(store_path)
    if (!is.null(sport))   ds <- ds |> dplyr::filter(sport == !!sport)
    if (!is.null(country)) ds <- ds |> dplyr::filter(country == !!country)
    if (!is.null(sex))     ds <- ds |> dplyr::filter(sex == !!sex)
    dplyr::collect(ds)
  }, error = function(e) {
    warning("Store bet read failed: ", e$message)
    NULL
  })
}

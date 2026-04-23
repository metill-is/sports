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

  tryCatch(
    {
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
    },
    error = function(e) {
      warning("Store prediction write failed: ", e$message)
    }
  )

  invisible(NULL)
}

#' Archive a posterior prediction snapshot keyed by (variant, fit_date).
#'
#' Writes a zstd-compressed Parquet under
#'   store/predictions_archive/sport={X}/country={Y}/sex={Z}/variant={V}/fit_date={YYYY-MM-DD}/predictions.parquet
#'
#' Unlike `store_predictions()` — which overwrites each run so the "current
#' posterior" is always a single file per bucket — this function preserves
#' every vintage. It is the only hook we have for reconstructing posteriors
#' as they were at a given historical decision time; without it, back-test
#' counterfactuals become lookahead-biased (using today's posterior against
#' yesterday's odds). Fit retroactively cannot be recreated, so start
#' writing these now even if you are not yet querying them.
#'
#' The `variant` partition was added 2026-04-23 for the betting-PnL audit
#' (see Knowledge/Sports Models/betting-pnl-harness-design-2026-04-23.md).
#' Production writes default to variant = "bvp_noinflation" for football_iceland
#' (matching the active Stan file); shadow audit variants share the partition
#' prefix and coexist on disk.
#'
#' @param df Data frame of posterior draws (posterior_goals.csv schema)
#' @param sport Sport name (e.g., "basketball")
#' @param country Country name (e.g., "iceland")
#' @param sex Sex label (e.g., "male")
#' @param sports_dir Absolute path to Sports/ root
#' @param variant Model variant tag (default "bvp_noinflation" - the current production
#'                model for football_iceland). Audit variants use e.g. "v3_free_nu".
#' @param fit_date Snapshot date (default: today). ISO-format string or Date.
#'
#' Starting Phase 2 (2026-04-23), df SHOULD include a `scope` column with
#' values in {"in_sample", "oos"}. Legacy callers that omit the column produce
#' files without it; read_predictions_archive() treats such files as all-OOS
#' for backwards compatibility.
archive_predictions <- function(df, sport, country, sex, sports_dir,
                                variant = "bvp_noinflation",
                                fit_date = Sys.Date()) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    warning("arrow package not available — skipping archive write")
    return(invisible(NULL))
  }

  fit_date <- as.character(as.Date(fit_date))

  tryCatch(
    {
      partition_dir <- file.path(
        sports_dir, "store", "predictions_archive",
        paste0("sport=", sport),
        paste0("country=", country),
        paste0("sex=", sex),
        paste0("variant=", variant),
        paste0("fit_date=", fit_date)
      )
      if (!dir.exists(partition_dir)) dir.create(partition_dir, recursive = TRUE)

      out_path <- file.path(partition_dir, "predictions.parquet")
      # zstd gives ~3x smaller files than snappy on posterior draws (lots of
      # repeated team names, integer goals). Level 9 is a good
      # compression/speed sweet spot for write-once/read-many.
      arrow::write_parquet(df, out_path, compression = "zstd", compression_level = 9)
      cat("  Archive: wrote", nrow(df), "rows to", out_path, "\n")
    },
    error = function(e) {
      warning("Archive prediction write failed: ", e$message)
    }
  )

  invisible(NULL)
}

#' Read archived predictions across one or more fit dates, optionally filtered by variant.
#'
#' Returns a long data frame with fit_date and variant columns. Useful for
#' "as-of" back-tests — filter to the latest fit_date <= decision_time, and
#' optionally to a specific model variant.
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Filter by sport (NULL = all)
#' @param country Filter by country (NULL = all)
#' @param sex Filter by sex (NULL = all)
#' @param variant Filter by model variant (NULL = all; e.g. "v3_free_nu" or "bvp_noinflation")
#' @param scope Filter by scope ("in_sample" or "oos"). NULL returns all. Legacy
#'              files without a scope column are treated as "oos" (Phase 1
#'              backfills all predate the column).
#' @param fit_date_from Keep fits from this date onwards (Date or string)
#' @param fit_date_to Keep fits up to this date (Date or string)
#' @return Tibble or NULL if store does not exist
read_predictions_archive <- function(sports_dir,
                                     sport = NULL, country = NULL, sex = NULL,
                                     variant = NULL,
                                     scope = NULL,
                                     fit_date_from = NULL, fit_date_to = NULL) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(NULL)
  }

  store_path <- file.path(sports_dir, "store", "predictions_archive")
  if (!dir.exists(store_path)) {
    return(NULL)
  }

  tryCatch(
    {
      ds <- arrow::open_dataset(store_path)
      if (!is.null(sport)) ds <- ds |> dplyr::filter(sport == !!sport)
      if (!is.null(country)) ds <- ds |> dplyr::filter(country == !!country)
      if (!is.null(sex)) ds <- ds |> dplyr::filter(sex == !!sex)
      if (!is.null(variant)) ds <- ds |> dplyr::filter(variant == !!variant)
      if (!is.null(fit_date_from)) {
        ds <- ds |> dplyr::filter(fit_date >= !!as.character(as.Date(fit_date_from)))
      }
      if (!is.null(fit_date_to)) {
        ds <- ds |> dplyr::filter(fit_date <= !!as.character(as.Date(fit_date_to)))
      }
      df <- dplyr::collect(ds)

      # Scope filter applied post-collect so legacy files (without a scope column)
      # can be treated as "oos". arrow's filter on a column that doesn't exist in
      # some underlying files is fragile; post-collect guarantees correct NA
      # handling across the schema union.
      if (!is.null(scope)) {
        if (!"scope" %in% names(df)) {
          df$scope <- NA_character_
        }
        if (scope == "oos") {
          df <- df |> dplyr::filter(scope == "oos" | is.na(scope))
        } else {
          df <- df |> dplyr::filter(scope == !!scope)
        }
      }

      df
    },
    error = function(e) {
      warning("Archive read failed: ", e$message)
      NULL
    }
  )
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

  tryCatch(
    {
      partition_dir <- file.path(
        sports_dir, "store", "bets",
        paste0("sport=", sport),
        paste0("country=", country),
        paste0("sex=", sex)
      )
      if (!dir.exists(partition_dir)) dir.create(partition_dir, recursive = TRUE)

      # Enforce consistent types to avoid schema conflicts across partitions
      if ("win" %in% names(df)) df$win <- as.logical(df$win)
      if ("pnl" %in% names(df)) df$pnl <- as.numeric(df$pnl)
      if ("info" %in% names(df)) df$info <- as.character(df$info)

      out_path <- file.path(partition_dir, "bets.parquet")
      arrow::write_parquet(df, out_path)
      cat("  Store: wrote", nrow(df), "bet rows to", out_path, "\n")
    },
    error = function(e) {
      warning("Store bet write failed: ", e$message)
    }
  )

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
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(NULL)
  }

  store_path <- file.path(sports_dir, "store", "predictions")
  if (!dir.exists(store_path)) {
    return(NULL)
  }

  tryCatch(
    {
      ds <- arrow::open_dataset(store_path)
      if (!is.null(sport)) ds <- ds |> dplyr::filter(sport == !!sport)
      if (!is.null(country)) ds <- ds |> dplyr::filter(country == !!country)
      if (!is.null(sex)) ds <- ds |> dplyr::filter(sex == !!sex)
      dplyr::collect(ds)
    },
    error = function(e) {
      warning("Store prediction read failed: ", e$message)
      NULL
    }
  )
}

#' Read bets from the Parquet store
#'
#' @param sports_dir Absolute path to Sports/ root
#' @param sport Filter by sport (NULL = all)
#' @param country Filter by country (NULL = all)
#' @param sex Filter by sex (NULL = all)
#' @return Tibble of bets, or NULL if store doesn't exist
read_bets <- function(sports_dir, sport = NULL, country = NULL, sex = NULL) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    return(NULL)
  }

  store_path <- file.path(sports_dir, "store", "bets")
  if (!dir.exists(store_path)) {
    return(NULL)
  }

  tryCatch(
    {
      ds <- arrow::open_dataset(store_path)
      if (!is.null(sport)) ds <- ds |> dplyr::filter(sport == !!sport)
      if (!is.null(country)) ds <- ds |> dplyr::filter(country == !!country)
      if (!is.null(sex)) ds <- ds |> dplyr::filter(sex == !!sex)
      dplyr::collect(ds)
    },
    error = function(e) {
      warning("Store bet read failed: ", e$message)
      NULL
    }
  )
}

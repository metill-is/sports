#' @include storage-schemas.R
NULL

#' Partitioning rules per table (spec §3.2)
#' @noRd
table_partitions <- function() {
  list(
    results = c("sport", "country", "sex", "season"),
    schedules = c("sport", "country", "sex", "season"),
    odds = c("sport", "country", "scraped_date"),
    odds_markets = c("sport", "country", "scraped_date"),
    beliefs_latest = c("sport", "country", "sex"),
    beliefs_archive = c("sport", "country", "sex", "fit_date"),
    beliefs_by_round = c("sport", "country", "sex", "season", "round_cutoff"),
    candidates = c("sport", "country", "run_date"),
    recommendations = c("sport", "country", "run_date"),
    ledger = c("sport", "country"),
    fit_diagnostics = c("sport", "country", "sex", "fit_date"),
    # `batch` is the write: every write is a new partition, so no file is ever
    # rewritten (append_only_tables()).
    forecast_archive = c("sport", "country", "sex", "fit_date", "batch"),
    engine_reference = c("sport", "country", "sex", "run_date")
  )
}

#' Tables whose stored rows may never change.
#'
#' Each is written one new partition at a time: its last partition column
#' names the write. `write_table()` refuses a write into a partition that
#' already exists, and a row whose `key` is stored under the same parent
#' partition (every partition column but the last) with different `compare`
#' columns. Appending works; changing a stored row is refused, whoever the
#' caller is. Two rows with one key and the same `compare` values may both be
#' stored (two writers that could not see each other); readers keep one.
#' @return Named list: table -> `list(key, compare)`.
#' @noRd
append_only_tables <- function() {
  list(
    forecast_archive = list(
      key = c(
        "sport", "country", "sex", "fit_date", "fit_id", "published_to",
        "match_date", "home_team", "away_team"
      ),
      compare = c(
        "division", "p_home_win", "p_draw", "p_away_win", "xg_home", "xg_away",
        "engine", "stan_model", "model_version"
      )
    )
  )
}

#' Map each table to its subdirectory under `root` (spec §3.1)
#' @noRd
table_subdir <- function(table) {
  switch(table,
    results = c("facts", "results"),
    schedules = c("facts", "schedules"),
    odds = c("facts", "odds"),
    odds_markets = c("facts", "odds_markets"),
    beliefs_latest = c("beliefs", "latest"),
    beliefs_archive = c("beliefs", "archive"),
    beliefs_by_round = c("beliefs", "by_round"),
    candidates = c("decisions", "candidates"),
    recommendations = c("decisions", "recommendations"),
    ledger = c("decisions", "ledger"),
    fit_diagnostics = c("beliefs", "diagnostics"),
    forecast_archive = c("archive", "forecasts"),
    engine_reference = c("health", "engine_reference"),
    stop("Unknown table: ", table, call. = FALSE)
  )
}

#' Derive any virtual partition columns the table needs (e.g. scraped_date from
#' scraped_at, run_date from run_id) before validation.
#'
#' Virtual-partition derivation pins timezone to UTC: cron runners are UTC-only
#' today but a developer running locally near midnight in a non-UTC locale
#' would otherwise push rows into the wrong scraped_date / run_date partition.
#' Audit 2026-05-15 Important §I3 / Data M.
#' @noRd
add_virtual_partitions <- function(df, table) {
  if (table %in% c("odds", "odds_markets") && !("scraped_date" %in% names(df)) &&
    "scraped_at" %in% names(df)) {
    df$scraped_date <- as.Date(df$scraped_at, tz = "UTC")
  }
  if (table %in% c("candidates", "recommendations") &&
    !("run_date" %in% names(df)) && "run_id" %in% names(df)) {
    df$run_date <- as.Date(df$run_id, tz = "UTC")
  }
  if (table == "beliefs_archive" && !("fit_date" %in% names(df))) {
    stop("beliefs_archive requires fit_date", call. = FALSE)
  }
  df
}

#' Add any missing optional columns (see [optional_columns()]) as typed NA.
#' @noRd
fill_optional_columns <- function(df, table) {
  opt <- optional_columns(table)
  for (nm in setdiff(names(opt), names(df))) {
    df[[nm]] <- rep(opt[[nm]], nrow(df))
  }
  df
}

#' Explicit read schema for a table whose fragments cannot be unified.
#'
#' The odds store mixes `scraped_at` as timestamp[us, tz=UTC] (148 files) and
#' tz-naive timestamp[us] (47 files; counted 2026-09-23), so
#' `unify_schemas = TRUE` fails and Arrow's default open takes the first
#' fragment's schema -- silently dropping any column added later (odds.sex,
#' odds.event_id, ...). Casting every fragment to this schema null-fills absent
#' columns and reads the tz-naive values as the same UTC instants (verified
#' 2026-09-23: 66,336 rows either way). Timestamps are microsecond because that
#' is what arrow writes from POSIXct; a second-unit schema fails with "would
#' lose data". Hive partition columns are strings, as the default open infers.
#' @noRd
read_schema <- function(table) {
  s <- schemas()[[table]]
  fields <- lapply(s$names, function(nm) {
    type <- s$GetFieldByName(nm)$type
    if (grepl("^timestamp", type$ToString())) {
      type <- arrow::timestamp(unit = "us", timezone = "UTC")
    }
    arrow::field(nm, type)
  })
  parts <- setdiff(table_partitions()[[table]], s$names)
  fields <- c(fields, lapply(parts, function(p) arrow::field(p, arrow::string())))
  do.call(arrow::schema, fields)
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
#' combined frame to `write_table()`. The decide layer does the same per sex
#' (`write_decide_table()`): `candidates` and `recommendations` partitions
#' carry no `sex`, so one sex's run would otherwise wipe the other's.
#'
#' Some tables define virtual partition columns that are derived from a source
#' column (e.g. `odds` partitions on `scraped_date` derived from `scraped_at`;
#' `candidates` and `recommendations` partition on `run_date` derived from
#' `run_id`). These are added by `add_virtual_partitions()` before validation
#' and passed through to `arrow::write_dataset()` alongside the schema columns.
#'
#' Atomicity: each call stages the new files in a sibling directory and then
#' POSIX-renames each touched partition into place. Arrow's bare
#' `existing_data_behavior = "overwrite"` first deletes the old partition's
#' files and *then* writes the new ones — a process kill in that window
#' leaves the partition empty, which has bitten `beliefs/latest/` after a
#' SIGKILL'd Stan fit. The staged-rename pattern narrows the failure window:
#' if the arrow write fails, the on-disk state is unchanged; on success the
#' visible state flips per partition via a single rename. See audit
#' 2026-05-15 §H.
#'
#' Append-only tables ([append_only_tables()], the forecast archive): the write
#' is refused, and nothing is written, when a touched partition already exists
#' or a row contradicts a stored row with the same key.
#' @export
write_table <- function(df, table, root = here::here("data")) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  df <- fill_optional_columns(add_virtual_partitions(df, table), table)
  validate_against_schema(df, table)
  validate_values(df, table)
  partitions <- table_partitions()[[table]]

  dest <- do.call(fs::path, c(list(root), table_subdir(table)))
  if (table %in% names(append_only_tables())) {
    .refuse_changed_rows(df, table, dest, partitions)
  }
  fs::dir_create(dest, recurse = TRUE)

  # Stage into a sibling directory adjacent to dest, then rename per-partition
  # into place. The staging tree mirrors the partition structure so each
  # partition leaf is a single atomic-rename unit.
  # macOS strftime doesn't support `%N`, so build a microsecond-unique
  # suffix from numeric Sys.time() instead of relying on locale strftime.
  stage_root <- paste0(
    dest, ".stage.", Sys.getpid(), ".",
    formatC(as.numeric(Sys.time()) * 1e6, format = "f", digits = 0)
  )
  on.exit(
    if (fs::dir_exists(stage_root)) {
      tryCatch(fs::dir_delete(stage_root), error = function(e) NULL)
    },
    add = TRUE
  )
  fs::dir_create(stage_root, recurse = TRUE)

  arrow::write_dataset(
    df,
    path = stage_root,
    format = "parquet",
    partitioning = partitions,
    existing_data_behavior = "overwrite"
  )

  partition_keys <- dplyr::distinct(df[, partitions, drop = FALSE])
  for (i in seq_len(nrow(partition_keys))) {
    keys <- partition_keys[i, , drop = FALSE]
    partition_segs <- vapply(partitions, function(p) {
      paste0(p, "=", as.character(keys[[p]]))
    }, character(1))
    src <- do.call(fs::path, c(list(stage_root), as.list(partition_segs)))
    dst <- do.call(fs::path, c(list(dest), as.list(partition_segs)))
    fs::dir_create(fs::path_dir(dst), recurse = TRUE)
    if (fs::dir_exists(dst)) {
      tryCatch(fs::dir_delete(dst), error = function(e) NULL)
    }
    fs::file_move(src, dst)
  }

  invisible(NULL)
}

#' Rows stored under one partition of a table, with its partition columns.
#'
#' `keys` gives values for the first partition columns (all of them, or a
#' leading subset); every partition below is read, its values parsed from the
#' directory names. The given values keep the caller's types rather than
#' Arrow's hive inference. Nothing stored reads as `NULL`.
#' @noRd
.read_partition_rows <- function(dest, partitions, keys) {
  known <- intersect(partitions, names(keys))
  stopifnot(identical(known, partitions[seq_along(known)]))
  segs <- vapply(known, function(p) paste0(p, "=", as.character(keys[[p]])), character(1))
  leaves <- do.call(fs::path, c(list(dest), as.list(segs)))
  for (p in setdiff(partitions, known)) {
    leaves <- as.character(unlist(lapply(leaves[fs::dir_exists(leaves)], function(d) {
      sub_dirs <- list.dirs(d, recursive = FALSE, full.names = TRUE)
      sub_dirs[startsWith(basename(sub_dirs), paste0(p, "="))]
    })))
  }
  leaves <- leaves[fs::dir_exists(leaves)]
  rows <- lapply(sort(leaves), function(leaf) {
    files <- sort(list.files(leaf, pattern = "[.]parquet$", full.names = TRUE))
    if (length(files) == 0L) {
      return(NULL)
    }
    r <- dplyr::bind_rows(lapply(files, function(f) tibble::as_tibble(arrow::read_parquet(f))))
    for (p in known) {
      r[[p]] <- rep(keys[[p]], nrow(r))
    }
    rel <- strsplit(sub(paste0("^", dest, "/?"), "", leaf), "/", fixed = TRUE)[[1]]
    for (p in setdiff(partitions, known)) {
      seg <- rel[startsWith(rel, paste0(p, "="))]
      r[[p]] <- rep(sub(paste0("^", p, "="), "", seg), nrow(r))
    }
    r
  })
  rows <- dplyr::bind_rows(rows)
  if (nrow(rows) == 0L) NULL else rows
}

#' One string per row that is equal exactly when the rows are.
#'
#' Doubles are written with 17 significant digits, so two values print alike
#' only when they are the same double; timestamps as whole seconds.
#' @noRd
.row_signature <- function(df) {
  canon <- lapply(df, function(x) {
    out <- if (inherits(x, "POSIXct")) {
      sprintf("%.0f", as.numeric(x))
    } else if (inherits(x, "Date")) {
      format(x, "%Y-%m-%d")
    } else if (is.double(x)) {
      sprintf("%.17g", x)
    } else {
      as.character(x)
    }
    out[is.na(x)] <- "<NA>"
    out
  })
  if (length(canon) == 0L) {
    return(character())
  }
  do.call(paste, c(unname(canon), sep = "\x1f"))
}

#' Refuse a write that would change a stored row of an append-only table. See
#' [append_only_tables()].
#' @noRd
.refuse_changed_rows <- function(df, table, dest, partitions) {
  rule <- append_only_tables()[[table]]
  parent <- partitions[-length(partitions)]
  keys <- dplyr::distinct(df[, partitions, drop = FALSE])
  where <- function(k, cols) {
    paste(paste0(cols, "=", vapply(cols, function(p) as.character(k[[p]]), character(1))), collapse = "/")
  }
  for (i in seq_len(nrow(keys))) {
    k <- keys[i, , drop = FALSE]
    segs <- vapply(partitions, function(p) paste0(p, "=", as.character(k[[p]])), character(1))
    if (fs::dir_exists(do.call(fs::path, c(list(dest), as.list(segs))))) {
      stop(
        sprintf(
          "Write to append-only table '%s' refused: partition %s already exists. Stored rows are never rewritten; write new rows to a new partition.",
          table, where(k, partitions)
        ),
        call. = FALSE
      )
    }
  }
  incoming_key <- .row_signature(df[rule$key])
  incoming_val <- .row_signature(df[rule$compare])
  clash <- tapply(incoming_val, incoming_key, function(v) length(unique(v)) > 1L)
  if (any(clash)) {
    stop(sprintf("Write to append-only table '%s' refused: %d key(s) appear with different values in the incoming rows.", table, sum(clash)), call. = FALSE)
  }
  parents <- dplyr::distinct(df[, parent, drop = FALSE])
  for (i in seq_len(nrow(parents))) {
    k <- parents[i, , drop = FALSE]
    stored <- .read_partition_rows(dest, partitions, k)
    if (is.null(stored) || nrow(stored) == 0L) {
      next
    }
    stored <- fill_optional_columns(stored, table)
    mine <- dplyr::semi_join(df, k, by = parent)
    at <- match(.row_signature(mine[rule$key]), .row_signature(stored[rule$key]))
    hit <- !is.na(at)
    differ <- .row_signature(mine[hit, rule$compare]) != .row_signature(stored[at[hit], rule$compare])
    if (any(differ)) {
      stop(
        sprintf(
          "Write to append-only table '%s' refused: %d row(s) contradict stored rows with the same key under %s. Stored rows are never rewritten.",
          table, sum(differ), where(k, parent)
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

#' Tables of scraped snapshots, which cannot be fetched again.
#'
#' `upsert_table()` lets a read error on one of their partitions propagate
#' rather than overwrite the partition.
#' @noRd
snapshot_tables <- function() c("odds", "odds_markets")

#' Natural match-key columns per table (for upsert dedup).
#'
#' Unlike the partition columns, which identify which Parquet files to write,
#' the natural key identifies one row within a partition. Re-ingests that
#' return the same key with differing metadata (e.g. corrected scores) keep
#' the newer row; re-ingests that return a strict subset of an earlier fetch
#' keep the full union.
#'
#' For \code{odds} and \code{odds_markets}, `scraped_at` is part of the
#' natural key so each cron snapshot accretes within the day's partition
#' rather than overwriting earlier snapshots. Same-second re-runs (typically
#' only in tests) dedup against the bet's full identity.
#' @noRd
natural_key_for <- function(table) {
  switch(table,
    # `division` joins the key for results+schedules so cup-vs-league same-day
    # pairings of the same teams don't collide (the audit's Data §I2 finding).
    # Currently this combination is empty in the data, but a same-day cup tie
    # plus league fixture between the same teams is physically possible.
    results = c(
      "sport", "country", "sex", "season", "division", "match_date",
      "home_team", "away_team"
    ),
    schedules = c(
      "sport", "country", "sex", "season", "division", "match_date",
      "home_team", "away_team"
    ),
    odds = c(
      "sport", "country", "sex", "scraped_at", "match_date",
      "home_team", "away_team", "market", "outcome", "line"
    ),
    # One selection of one market in one scrape. Sport, country and the
    # scrape's date are fixed within a partition.
    odds_markets = c("scraped_at", "event_id", "market_id", "selection"),
    stop("upsert_table not supported for table: ", table, call. = FALSE)
  )
}

#' Columns a source fills in a second step, which a plain re-fetch lacks.
#'
#' The KKI export carries no regulation-time scores; they are read from each
#' game's card once, after the game. The daily ingest re-fetches the season from
#' the export, and the new row wins on the natural key, so these columns would
#' be blanked every morning without [carry_enrichment()].
#' @noRd
enrichment_columns <- function(table) {
  switch(table,
    results = c("game_id", "home_score_reg", "away_score_reg"),
    character(0)
  )
}

#' Keep a stored row's enrichment where the winning new row has none.
#'
#' Only while the final score is unchanged: a corrected final score makes a
#' stored regulation score suspect, so it is dropped and fetched again.
#' @param kept Rows that won the natural-key dedup (new rows first).
#' @param dropped The stored rows they replaced.
#' @noRd
carry_enrichment <- function(kept, dropped, table, nat_key) {
  cols <- intersect(enrichment_columns(table), intersect(names(kept), names(dropped)))
  if (length(cols) == 0L || nrow(dropped) == 0L) {
    return(kept)
  }
  key <- function(x) do.call(paste, c(lapply(x[, nat_key, drop = FALSE], as.character), sep = "\r"))
  at <- match(key(kept), key(dropped))
  same_final <- !is.na(at)
  for (col in c("home_score", "away_score")) {
    if (col %in% names(kept)) same_final <- same_final & (kept[[col]] %in% NA & dropped[[col]][at] %in% NA | kept[[col]] == dropped[[col]][at])
  }
  same_final[is.na(same_final)] <- FALSE
  # WHY "" too: an export cell left blank reads as an empty string, which says no
  # more about the game than NA does.
  missing <- function(x) is.na(x) | (is.character(x) & !nzchar(x))
  for (col in cols) {
    fill <- same_final & missing(kept[[col]]) & !missing(dropped[[col]][at])
    kept[[col]][fill] <- dropped[[col]][at][fill]
  }
  kept
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
#' Currently supports `"results"`, `"schedules"`, `"odds"` and
#' `"odds_markets"`. Other tables fall through to `natural_key_for()` which
#' raises a clear error.
#'
#' A partition that exists but cannot be read is treated as empty (and so
#' overwritten) for `"results"` and `"schedules"`, which can be re-ingested.
#' For `"odds"` and `"odds_markets"` the read error propagates instead and
#' nothing is written: snapshots cannot be re-scraped. An absent table directory is not an error
#' ([read_table()] returns an empty tibble), so a first write still works.
#'
#' @param df A tibble or data frame matching the schema for `table`.
#' @param table One of `"results"`, `"schedules"`, `"odds"`, `"odds_markets"`.
#' @param root Filesystem root (defaults to `here::here("data")`).
#' @param snapshot_future_by Optional character vector of scope columns (e.g.
#'   `"division"`). When set, the incoming frame is treated as the
#'   authoritative snapshot of FUTURE rows for every scope-combo it contains:
#'   within each touched partition, existing rows whose scope columns match a
#'   combo present in the new frame and whose `match_date >=
#'   snapshot_cutoff` are retracted before the union. This is how rescheduled
#'   fixtures lose their old-dated ghost row — the natural key includes
#'   `match_date`, so a date change is otherwise a new identity that plain
#'   upsert semantics keep forever. Scope-combos absent from the new frame
#'   (e.g. a division whose scrape failed) are left untouched. Default `NULL`
#'   preserves the legacy union semantics.
#' @param snapshot_cutoff Date; rows at or after this date are eligible for
#'   retraction when `snapshot_future_by` is set. Defaults to `Sys.Date()`.
#' @return invisible(NULL)
#' @export
upsert_table <- function(df, table, root = here::here("data"),
                         snapshot_future_by = NULL,
                         snapshot_cutoff = Sys.Date()) {
  if (nrow(df) == 0) {
    return(invisible(NULL))
  }

  df <- fill_optional_columns(add_virtual_partitions(df, table), table)
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

    # Odds snapshots cannot be re-scraped, so for odds a read error must
    # propagate: swallowing it as "no existing rows" would overwrite the
    # partition and silently drop the day's earlier snapshots (final review
    # F4). Other tables keep the swallow: their rows are re-ingestable.
    existing <- if (table %in% snapshot_tables()) {
      read_table(table, root = root, filter = part_filter)
    } else {
      tryCatch(
        read_table(table, root = root, filter = part_filter),
        error = function(e) tibble::tibble()
      )
    }
    # A partition written before a column became optional lacks it; fill it so
    # the natural-key check below merges instead of overwriting the partition
    # (spec 2026-09-23 Review Focus 1: odds snapshots cannot be re-scraped).
    if (nrow(existing) > 0L) existing <- fill_optional_columns(existing, table)

    if (!is.null(snapshot_future_by) && nrow(existing) > 0) {
      missing_scope <- setdiff(
        snapshot_future_by, intersect(names(new_rows), names(existing))
      )
      if (length(missing_scope) > 0 || !("match_date" %in% names(existing))) {
        stop(
          sprintf(
            "upsert_table: snapshot_future_by column(s) unavailable: %s",
            paste(c(missing_scope, "match_date"), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      scope_keys <- dplyr::distinct(
        new_rows[, snapshot_future_by, drop = FALSE]
      )
      in_scope <- do.call(
        paste, c(existing[, snapshot_future_by, drop = FALSE], sep = "\r")
      ) %in% do.call(paste, c(scope_keys, sep = "\r"))
      md <- as.Date(existing$match_date)
      retract <- in_scope & !is.na(md) & md >= as.Date(snapshot_cutoff)
      existing <- existing[!retract, , drop = FALSE]
    }

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
      carry_enrichment(bound[!dup, , drop = FALSE], bound[dup, , drop = FALSE], table, nat_key)
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

  partitioning <- table_partitions()[[table]]
  # Unify fragment schemas so a column added to the schema after some partitions
  # were written (schema evolution, e.g. schedules.kickoff_time) is surfaced and
  # null-filled for the older partitions, instead of being silently dropped by
  # Arrow's default first-fragment-wins inference. The odds store carries a
  # pre-existing scraped_at tz inconsistency (timestamp[us, tz=UTC] vs
  # timestamp[us]) that unification cannot merge; there we fall back to an
  # explicit read schema (read_schema()), which reads every fragment and keeps
  # columns added after the first. Arrow raises the merge error at
  # open time, so this tryCatch catches it before any collect.
  ds <- tryCatch(
    arrow::open_dataset(src, partitioning = partitioning, unify_schemas = TRUE),
    error = function(e) {
      arrow::open_dataset(src, partitioning = partitioning, schema = read_schema(table))
    }
  )

  if (length(filter) > 0) {
    for (col in names(filter)) {
      val <- filter[[col]]
      ds <- dplyr::filter(ds, .data[[col]] == val)
    }
  }

  dplyr::collect(ds) |> tibble::as_tibble()
}

#' Write JSON with consistent NA serialisation.
#'
#' Thin wrapper over `jsonlite::write_json()` that forces `na = "null"`.
#' Pre-2026-05-16 about half the publisher's `write_json` call sites set
#' `na = "null"` and half didn't, producing inconsistent JSON shapes
#' across snapshots (sometimes NA dropped, sometimes serialised as "NA"
#' string). Audit 2026-05-15 publish §I1. Callers can still override
#' `na` explicitly — wrapper-supplied default loses to caller-supplied
#' value.
#'
#' @param x R object to serialise.
#' @param path Destination path.
#' @param ... Forwarded to `jsonlite::write_json`.
#' @keywords internal
#' @noRd
write_json_consistent <- function(x, path, ...) {
  dots <- list(...)
  if (!"na" %in% names(dots)) dots$na <- "null"
  do.call(jsonlite::write_json, c(list(x = x, path = path), dots))
}

# Sports MCP tools — headless artefact inspection.
#
# Loaded via:
#   claude mcp add -s project r-sports -- Rscript -e \
#     "mcptools::mcp_server(tools = '/Users/brynjolfurjonsson/sports/.claude/r-tools.R')"

PROJECT_ROOT <- "/Users/brynjolfurjonsson/sports"

tool_query_duckdb <- ellmer::tool(
  fun = function(sql) {
    db_path <- file.path(PROJECT_ROOT, "sports.duckdb")
    if (!file.exists(db_path)) {
      return(sprintf(
        "DuckDB not found at %s. Run `Rscript -e 'sports::rebuild_duckdb()'` first.",
        db_path
      ))
    }
    con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
    res <- tryCatch(
      DBI::dbGetQuery(con, sql),
      error = function(e) sprintf("SQL error: %s", conditionMessage(e))
    )
    if (is.character(res)) {
      return(res)
    }
    btw::btw_this(res, format = "skim")
  },
  description = paste(
    "Run a read-only SQL query against the Sports DuckDB",
    "(SQL view layer over data/facts/, data/beliefs/, data/decisions/).",
    "Discover tables via `SELECT table_name FROM INFORMATION_SCHEMA.TABLES`.",
    "Typical tables: results, odds, schedules, beliefs_latest, beliefs_archive,",
    "candidates, recommendations, ledger.",
    "Returns a skim of the result frame. Read-only."
  ),
  name = "sports_query_duckdb",
  arguments = list(
    sql = ellmer::type_string("Read-only SQL query.")
  )
)

tool_inspect_target <- ellmer::tool(
  fun = function(target_name) {
    store <- file.path(PROJECT_ROOT, "_targets")
    if (!dir.exists(store)) {
      return(sprintf("targets store not found at %s. Run pipeline first.", store))
    }
    obj <- tryCatch(
      targets::tar_read_raw(target_name, store = store),
      error = function(e) {
        sprintf("Could not read target '%s': %s", target_name, conditionMessage(e))
      }
    )
    if (is.character(obj) && length(obj) == 1L && grepl("^Could not read", obj)) {
      return(obj)
    }
    if (inherits(obj, "data.frame")) {
      btw::btw_this(obj, format = "skim")
    } else {
      paste(
        utils::capture.output({
          cat(sprintf("Class: %s\n\n", paste(class(obj), collapse = ", ")))
          utils::str(obj, max.level = 2L)
        }),
        collapse = "\n"
      )
    }
  },
  description = paste(
    "Load a saved {targets} object from the Sports pipeline by name and skim it",
    "(or str() it for non-data-frame outputs like Stan fits, lists).",
    "Use to inspect what an existing target produced without re-running the DAG.",
    "Run sports_list_targets first if you don't know exact names."
  ),
  name = "sports_inspect_target",
  arguments = list(
    target_name = ellmer::type_string(
      "Target name, e.g. 'fit_football_iceland_male' or 'odds_basketball_iceland'."
    )
  )
)

tool_list_targets <- ellmer::tool(
  fun = function() {
    store <- file.path(PROJECT_ROOT, "_targets")
    if (!dir.exists(store)) {
      return(sprintf("targets store not found at %s.", store))
    }
    meta <- targets::tar_meta(
      fields = c("type", "size", "time", "warnings", "error"),
      complete_only = TRUE,
      store = store
    )
    btw::btw_this(meta[order(meta$name), ], format = "skim")
  },
  description = paste(
    "List all completed {targets} objects in the Sports pipeline with type, size,",
    "build time, warnings, and errors. Run before sports_inspect_target."
  ),
  name = "sports_list_targets"
)

tool_inspect_parquet <- ellmer::tool(
  fun = function(relative_path) {
    full_path <- file.path(PROJECT_ROOT, relative_path)
    if (!file.exists(full_path) && !dir.exists(full_path)) {
      return(sprintf("Path not found: %s", full_path))
    }
    df <- tryCatch(
      {
        ds <- arrow::open_dataset(full_path)
        dplyr::collect(head(ds, 2000L))
      },
      error = function(e) sprintf("Could not read %s: %s", full_path, conditionMessage(e))
    )
    if (is.character(df)) {
      return(df)
    }
    btw::btw_this(df, format = "skim")
  },
  description = paste(
    "Read a Parquet file or hive-partitioned dataset under ~/sports and skim the first 2000 rows.",
    "Path is relative to project root.",
    "Examples: 'data/facts/results', 'data/beliefs/latest/sport=football/country=iceland/sex=male/beliefs.parquet'."
  ),
  name = "sports_inspect_parquet",
  arguments = list(
    relative_path = ellmer::type_string("Path relative to ~/sports.")
  )
)

list(tool_query_duckdb, tool_inspect_target, tool_list_targets, tool_inspect_parquet)

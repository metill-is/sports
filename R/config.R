#' Load and validate leagues.yml
#'
#' @param path Path to leagues.yml.
#' @param schema_path Path to leagues.schema.json. Defaults to here().
#' @param validate If TRUE (default), validate the loaded YAML against the schema.
#' @return Named list keyed by league_key (e.g. "basketball_iceland").
#' @export
load_leagues <- function(path = here::here("config", "leagues.yml"),
                         schema_path = here::here("config", "leagues.schema.json"),
                         validate = TRUE) {
  raw <- readr::read_file(path)
  leagues <- yaml::yaml.load(raw)

  if (is.null(leagues)) {
    stop(
      "leagues.yml parsed to NULL (empty or malformed file): ", path,
      call. = FALSE
    )
  }

  if (isTRUE(validate)) {
    if (!file.exists(schema_path)) {
      stop("leagues.schema.json not found: ", schema_path, call. = FALSE)
    }
    validate_leagues(leagues, schema_path)
  }

  leagues
}

# Coerce known-array fields to lists so that jsonlite::toJSON(auto_unbox = TRUE)
# does not flatten single-element arrays into scalars (which would trip the
# schema's "must be array" checks on e.g. `sexes: [male]`).
# Note: betting$markets is now an object (boolean toggles), not an array —
# do NOT wrap it here.
coerce_array_fields <- function(leagues) {
  for (key in names(leagues)) {
    l <- leagues[[key]]
    if (!is.null(l$sexes) && !is.list(l$sexes)) {
      l$sexes <- as.list(l$sexes)
    }
    # lengjan$competitions is already a list-of-lists in yaml.load output; leave alone.
    leagues[[key]] <- l
  }
  leagues
}

validate_leagues <- function(leagues, schema_path) {
  leagues <- coerce_array_fields(leagues)
  json_text <- jsonlite::toJSON(leagues, auto_unbox = TRUE, null = "null", na = "null")
  schema_text <- readr::read_file(schema_path)

  result <- jsonvalidate::json_validate(json_text, schema_text, verbose = TRUE, engine = "ajv")
  if (!isTRUE(result)) {
    errors <- attr(result, "errors")
    err_lines <- if (!is.null(errors) && nrow(errors) > 0) {
      paste(sprintf("  %s: %s", errors$instancePath, errors$message), collapse = "\n")
    } else {
      "  (no detailed errors returned by validator)"
    }
    stop(paste0("leagues.yml failed schema validation:\n", err_lines), call. = FALSE)
  }
  invisible(TRUE)
}

#' Load + validate the global bankroll config.
#'
#' Reads `config/bankroll.yml`. If `current_pool` is missing from the YAML
#' (the usual case), derives it as `initial_pool + sum(ledger$pnl[settled])`
#' so the bankroll evolves with realised PnL.
#'
#' @param path Path to bankroll.yml.
#' @param ledger_root Root directory for Parquet stores (parent of `decisions/ledger/`).
#' @return List with `initial_pool`, `current_pool`, `daily_budget_frac`,
#'   `daily_budget_min_isk`.
#' @export
load_bankroll <- function(path = here::here("config", "bankroll.yml"),
                          ledger_root = here::here("data")) {
  cfg <- yaml::yaml.load(readr::read_file(path))
  if (is.null(cfg$current_pool)) {
    led <- tryCatch(
      read_table("ledger", root = ledger_root),
      error = function(e) tibble::tibble(pnl = numeric(0), settled = logical(0))
    )
    if (!all(c("pnl", "settled") %in% names(led))) {
      led <- tibble::tibble(pnl = numeric(0), settled = logical(0))
    }
    settled_pnl <- led$pnl[!is.na(led$settled) & led$settled]
    realised_pnl <- sum(settled_pnl, na.rm = TRUE)
    cfg$current_pool <- cfg$initial_pool + realised_pnl
  }
  cfg
}

#' Filter a loaded leagues list by selector.
#'
#' @param leagues Named list from `load_leagues()`.
#' @param sport,country,league Optional filters.
#' @param active_only If TRUE, keep only leagues with `active = TRUE`.
#' @return Filtered named list.
#' @export
filter_leagues <- function(leagues, sport = NULL, country = NULL,
                           league = NULL, active_only = FALSE) {
  keep <- rep(TRUE, length(leagues))
  names(keep) <- names(leagues)

  if (!is.null(sport)) {
    keep <- keep & vapply(leagues, function(l) identical(l$sport, sport), logical(1))
  }
  if (!is.null(country)) {
    keep <- keep & vapply(leagues, function(l) identical(l$country, country), logical(1))
  }
  if (!is.null(league)) {
    keep <- keep & (names(leagues) == league)
  }
  if (isTRUE(active_only)) {
    keep <- keep & vapply(leagues, function(l) isTRUE(l$active), logical(1))
  }

  leagues[keep]
}

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

  if (isTRUE(validate) && file.exists(schema_path)) {
    validate_leagues(leagues, schema_path)
  }

  leagues
}

validate_leagues <- function(leagues, schema_path) {
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

#' Validate every JSON in a publish tree against config/publish-schemas/*.
#'
#' Walks `dir` for `*.json` files. For each file at relative path
#' `<sport>/.../<basename>.json` the validator looks for a schema at:
#'
#'   1. `schema_dir/<sport>/<basename>.schema.json` (sport-namespaced; preferred)
#'   2. `schema_dir/<basename>.schema.json`         (sport-agnostic; fallback)
#'
#' If neither is found the file is added to `unmatched` (not an error — it
#' lets future producer-side artefacts ship before their schema is written).
#'
#' Used at the end of `publish_one()` to catch producer-side drift before
#' a fresh publish lands. Also used by `tests/testthat/test-publish-schemas.R`
#' as a real-output regression. The metill-platform side runs an equivalent
#' Python validator after rsync; both fail closed.
#'
#' Sport namespacing is what lets football iceland have strict schemas
#' (cumulative xPts fields, is_cup marker, trend arrays) while basketball
#' and handball's legacy fit-based publish output remains schema-less
#' until the F6 cutover migrates them onto the football shape.
#'
#' @param dir Directory containing publish JSONs. Walked recursively.
#' @param schema_dir Directory containing `<sport>/*.schema.json` files.
#'   Defaults to `config/publish-schemas/` under the project root.
#' @return List with:
#'   * `ok` — TRUE iff every matched JSON validated
#'   * `n_files` — count of JSONs that matched a schema and were validated
#'   * `n_passed`/`n_failed` — pass/fail counts
#'   * `errors` — character vector of "<rel_path>: <error>" entries
#'   * `unmatched` — character vector of paths with no schema (informational)
#' @export
validate_publish_dir <- function(dir,
                                 schema_dir = here::here("config", "publish-schemas"),
                                 sport = NULL) {
  stopifnot(dir.exists(dir))
  stopifnot(dir.exists(schema_dir))
  stopifnot(is.null(sport) || (is.character(sport) && length(sport) == 1L))

  files <- list.files(dir, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)

  errors <- character()
  unmatched <- character()
  n_passed <- 0L
  n_failed <- 0L

  for (f in files) {
    rel <- sub(paste0("^", normalizePath(dir, mustWork = FALSE), "/?"), "", normalizePath(f, mustWork = FALSE))
    rel_parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    base <- basename(f)
    # When `dir` is the whole publish tree the first path segment IS the
    # sport. When it is a single sport's subtree that segment is "iceland",
    # no schema resolves, every file lands in `unmatched` and ok stays TRUE --
    # validation silently doing nothing. So a caller narrowing `dir` to one
    # sport MUST name it; there is a test pinning both behaviours.
    file_sport <- if (is.null(sport)) rel_parts[1] else sport

    schema_path <- .resolve_schema_path(schema_dir, file_sport, base)

    if (is.null(schema_path)) {
      unmatched <- c(unmatched, rel)
      next
    }

    ok <- tryCatch(
      jsonvalidate::json_validate(
        json = f,
        schema = schema_path,
        engine = "ajv",
        verbose = TRUE
      ),
      error = function(e) {
        errors <<- c(errors, sprintf("%s: %s", rel, conditionMessage(e)))
        FALSE
      }
    )

    if (isTRUE(ok)) {
      n_passed <- n_passed + 1L
    } else {
      n_failed <- n_failed + 1L
      err_msg <- .format_validation_errors(ok)
      errors <- c(errors, sprintf("%s: %s", rel, err_msg))
    }
  }

  list(
    ok        = (length(errors) == 0L && n_failed == 0L),
    n_files   = n_passed + n_failed,
    n_passed  = n_passed,
    n_failed  = n_failed,
    errors    = errors,
    unmatched = unmatched
  )
}

.resolve_schema_path <- function(schema_dir, sport, base) {
  candidates <- c(
    file.path(schema_dir, sport, sub("\\.json$", ".schema.json", base)),
    file.path(schema_dir, sub("\\.json$", ".schema.json", base))
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) NULL else hit[[1L]]
}

.format_validation_errors <- function(ok) {
  attrs <- attr(ok, "errors")
  if (is.data.frame(attrs) && nrow(attrs) > 0L) {
    msgs <- vapply(
      seq_len(nrow(attrs)),
      function(i) {
        row <- attrs[i, , drop = FALSE]
        field <- if ("instancePath" %in% names(row) &&
          !is.null(row$instancePath) && !is.na(row$instancePath) &&
          nzchar(row$instancePath)) {
          row$instancePath
        } else {
          "(root)"
        }
        msg <- if ("message" %in% names(row) && !is.na(row$message)) {
          row$message
        } else {
          "validation error"
        }
        sprintf("%s %s", field, msg)
      },
      character(1)
    )
    paste(msgs, collapse = "; ")
  } else {
    "validation failed (no error detail available)"
  }
}

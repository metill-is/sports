#' Long-form analytical tibble of standings_history across cells.
#'
#' Walks `output_root` for all `standings_history.json` files (one per
#' per cell), parses them into a single long tibble with `(sport,
#' country, sex, division)` columns prepended. Each row is a per-fit
#' per-team snapshot of standings; `xpts`/`xg_for`/`xg_against` are
#' cumulative across the played matches that have a pre-round fit
#' (`n_predicted_matches` discloses partial coverage).
#'
#' Companion to `replay_football_iceland()`: produces the analytical
#' shape the user asked for in the 2026-05-25 review ("re-export the
#' data for cumulative xP calculations"). Once a `--per-round --no-fit`
#' replay run has populated `data/publish_replay/` (or you point this
#' at the live `data/publish/`), one call returns the entire trajectory
#' for plotting, residual analyses, or calibration backtests.
#'
#' Currently football-iceland-shaped: assumes cell directories are
#' `{sex_slug}-{division_slug}` (e.g. `karla-bd`). Basketball/handball
#' cells (`{sex_slug}/` only, no division suffix) are skipped --
#' migration to the same per-division layout is F6.
#'
#' @param output_root Where to find `standings_history.json` files.
#'   Default `here::here("data", "publish")` -- the live publish tree.
#'   Pass the directory used by `replay_football_iceland(--publish-to)`
#'   for what-if analyses against an isolated tree.
#' @return Tibble; empty (with all expected columns) if no files
#'   present or if all are empty.
#' @export
cumulative_xpts_long <- function(output_root = here::here("data", "publish")) {
  empty_result <- tibble::tibble(
    sport = character(),
    country = character(),
    sex = character(),
    division = character(),
    as_of = character(),
    generated_at = character(),
    round = integer(),
    season = integer(),
    team = character(),
    short = character(),
    played = integer(),
    wins = integer(),
    draws = integer(),
    losses = integer(),
    goals_for = integer(),
    goals_against = integer(),
    goal_diff = integer(),
    points = integer(),
    xg_for = numeric(),
    xg_against = numeric(),
    xpts = numeric(),
    n_predicted_matches = integer(),
    n_played_matches = integer(),
    rank = integer()
  )

  files <- list.files(
    output_root,
    pattern = "^standings_history\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(files) == 0L) {
    return(empty_result)
  }

  # WHY: don't use normalizePath() for the prefix -- on macOS it resolves
  # /var/... to /private/var/... while list.files() returns the original
  # /var/... form, so the sub() never matches and rel stays absolute.
  # Strip the literal output_root prefix (allowing 1+ trailing slashes).
  prefix_rx <- paste0(
    "^\\Q", sub("/+$", "", output_root), "\\E/+"
  )

  rows <- lapply(files, function(f) {
    rel <- sub(prefix_rx, "", f)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    if (length(parts) < 4L) {
      return(NULL)
    }
    sport <- parts[1]
    country <- parts[2]
    cell <- parts[3]
    cell_parts <- strsplit(cell, "-", fixed = TRUE)[[1]]
    if (length(cell_parts) < 2L) {
      return(NULL)
    }
    sex_slug <- cell_parts[1]
    division_slug <- paste(cell_parts[-1], collapse = "-")
    sex <- switch(sex_slug,
      "karla" = "male",
      "kvenna" = "female",
      sex_slug
    )

    parsed <- tryCatch(
      jsonlite::fromJSON(f, simplifyDataFrame = TRUE),
      error = function(e) NULL
    )
    if (is.null(parsed) || is.null(parsed$records)) {
      return(NULL)
    }
    recs <- parsed$records
    if ((is.data.frame(recs) && nrow(recs) == 0L) ||
      (is.list(recs) && !is.data.frame(recs) && length(recs) == 0L)) {
      return(NULL)
    }

    df <- tibble::as_tibble(recs)
    df$sport <- sport
    df$country <- country
    df$sex <- sex
    df$division <- division_slug
    df
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(empty_result)
  }

  combined <- dplyr::bind_rows(rows)
  lead_cols <- c("sport", "country", "sex", "division")
  combined[, c(lead_cols, setdiff(names(combined), lead_cols)), drop = FALSE]
}

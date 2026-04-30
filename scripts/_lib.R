# scripts/_lib.R -- Shared helpers for scripts/0N_*.R entry points.
# Sourced by each script via `source(here::here("scripts", "_lib.R"))`.

#' Parse the standard pipeline-script CLI: `--league`, `--sex`, `--force`.
#'
#' @return A list with `$league` (character or NULL), `$sex` (character or
#'   NULL), `$force` (logical).
parse_pipeline_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  get_flag <- function(name, default = NULL) {
    i <- which(args == paste0("--", name))
    if (length(i) == 0L) default else args[[i + 1L]]
  }
  has_flag <- function(name) paste0("--", name) %in% args

  list(
    league = get_flag("league"),
    sex = get_flag("sex"),
    force = has_flag("force")
  )
}

#' Resolve the (league, sex) pairs to operate on, respecting --league/--sex
#' filters. Reads active leagues from config/leagues.yml.
#'
#' @param opts Output of `parse_pipeline_args()`.
#' @param require_lengjan If TRUE, restricts to leagues that publish on Lengjan.
#' @return Tibble with one row per (key, sex) pair, plus the static slice.
resolve_targets <- function(opts, require_lengjan = FALSE) {
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE)
  if (require_lengjan) {
    active <- filter_leagues(active, has_lengjan = TRUE)
  }

  if (!is.null(opts$league)) {
    if (!opts$league %in% names(active)) {
      stop(
        "--league '", opts$league, "' not active. Active: ",
        paste(names(active), collapse = ", ")
      )
    }
    active <- active[opts$league]
  }

  rows <- list()
  for (key in names(active)) {
    league_def <- active[[key]]
    sexes <- if (is.null(opts$sex) || opts$sex == "all") {
      league_def$sexes
    } else {
      if (!opts$sex %in% league_def$sexes) {
        next
      }
      opts$sex
    }
    for (sx in sexes) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        key = key,
        sex = sx,
        sport = league_def$sport,
        country = league_def$country
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      key = character(), sex = character(),
      sport = character(), country = character()
    ))
  }
  dplyr::bind_rows(rows)
}

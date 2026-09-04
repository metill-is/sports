#' @include storage.R
NULL

# Retention for data/beliefs/extracts/ (Plan B follow-on).
#
# The extracts tree is the SOLE publish input and is git-tracked, committed by
# fit.yml on every run. Nothing pruned it, so it reached 1.3 GB over 99
# partitions inside a 1.7 GB .git that CI full-clones on every workflow run --
# roughly 22 MB per football fit at ~24 fits a month.
#
# What makes pruning safe is that publish reads the NEWEST partition per cell
# (read_extracted_iceland() with fit_date = NULL). What makes it dangerous is
# that a seasonal sport's newest partition can be months old: basketball and
# handball are dormant all summer, so a pure date cutoff would delete every
# partition they own and break their publish silently the moment the season
# restarts. Hence keep_min, applied per cell and independent of age.
#
# Replay degrades LOUDLY, not silently: replay_football_iceland() asks for a
# specific fit_date and, when it is gone, aborts with "no extracts at ... pick
# an `as_of` that has an existing partition". So bounded replay depth is a
# visible, actionable limit rather than a wrong answer.

#' Prune old `fit_date=` partitions from an extracts tree.
#'
#' @param root Extracts root (`data/beliefs/extracts`).
#' @param keep_days Keep every partition whose `fit_date` is within this many
#'   days of `now`.
#' @param keep_min Additionally keep this many newest partitions per
#'   (sport, country, sex) cell REGARDLESS of age. Must be >= 1: publish reads
#'   the newest, so dropping to zero would break a dormant cell.
#' @param now Reference date.
#' @param dry_run When `TRUE` (default) nothing is deleted; the caller gets the
#'   list of partitions that would go.
#' @return Tibble of pruned (or prunable) partitions: `sport`, `country`,
#'   `sex`, `fit_date`, `path`, `bytes`.
#' @export
prune_extracts <- function(root = here::here("data", "beliefs", "extracts"),
                           keep_days = 14L,
                           keep_min = 3L,
                           now = Sys.Date(),
                           dry_run = TRUE) {
  stopifnot(keep_min >= 1L)
  empty <- tibble::tibble(
    sport = character(), country = character(), sex = character(),
    fit_date = as.Date(character()), path = character(), bytes = numeric()
  )
  if (!dir.exists(root)) {
    return(empty)
  }

  parts <- list.dirs(root, recursive = TRUE, full.names = TRUE)
  parts <- parts[grepl("/fit_date=[0-9]{4}-[0-9]{2}-[0-9]{2}$", parts)]
  if (length(parts) == 0L) {
    return(empty)
  }

  meta <- tibble::tibble(
    path = parts,
    cell = sub("/fit_date=.*$", "", parts),
    fit_date = as.Date(sub(".*/fit_date=", "", parts))
  )
  meta$sport <- sub(".*sport=([^/]+).*", "\\1", meta$path)
  meta$country <- sub(".*country=([^/]+).*", "\\1", meta$path)
  meta$sex <- sub(".*sex=([^/]+).*", "\\1", meta$path)

  cutoff <- as.Date(now) - as.integer(keep_days)

  drop <- meta |>
    dplyr::group_by(.data$cell) |>
    # rank 1 is the newest. Keep it and the next keep_min - 1 whatever their
    # age -- this is the clause that protects a dormant seasonal cell.
    dplyr::mutate(rank = rank(-as.numeric(.data$fit_date), ties.method = "first")) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$rank > as.integer(keep_min), .data$fit_date < cutoff)

  if (nrow(drop) == 0L) {
    return(empty)
  }

  drop$bytes <- vapply(drop$path, function(p) {
    sum(file.info(list.files(p, recursive = TRUE, full.names = TRUE))$size,
        na.rm = TRUE)
  }, numeric(1))

  if (!isTRUE(dry_run)) {
    unlink(drop$path, recursive = TRUE, force = TRUE)
  }

  drop |>
    dplyr::select("sport", "country", "sex", "fit_date", "path", "bytes") |>
    dplyr::arrange(.data$sport, .data$sex, .data$fit_date)
}

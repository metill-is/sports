#' @include ingest.R
NULL

#' Column order and types of the federation-season provenance cache.
#' @keywords internal
#' @noRd
.federation_seasons_empty <- function() {
  tibble::tibble(
    federation = character(),
    sex = character(),
    division = character(),
    season = integer(),
    id = integer(),
    title = character(),
    source = character(),
    discovered_at = character(),
    verified = logical(),
    note = character()
  )
}

#' Trust ranking of provenance sources, highest first.
#'
#' `inferred-verified` outranks `hand-verified` because it is the only source
#' that has been checked against three independent properties of the fetched
#' page (title pattern, season stamp, roster intersection); a hand-verified id
#' was only ever eyeballed.
#' @keywords internal
#' @noRd
FEDERATION_SOURCE_TRUST <- c(
  "inferred-verified" = 5L,
  "hand-verified" = 4L,
  "live-nav" = 3L,
  "live" = 2L,
  "inferred-candidate" = 1L,
  "live-nav-unattributed" = 0L
)

#' Coerce a possibly list-shaped JSON column to an atomic vector.
#'
#' `jsonlite` returns an all-`null` column as a list of NULLs rather than a
#' typed NA vector, which then poisons every downstream comparison. Flattening
#' NULL to NA first keeps the cache's column types stable regardless of how
#' many fields happened to be unset when it was written.
#' @keywords internal
#' @noRd
.fs_coerce <- function(x, caster) {
  if (is.null(x)) {
    return(caster(NA))
  }
  if (is.list(x)) {
    x <- vapply(
      x,
      function(el) if (is.null(el) || length(el) == 0L) NA_character_ else as.character(el)[[1L]],
      character(1)
    )
  }
  suppressWarnings(caster(x))
}

#' Path to the git-tracked federation-season provenance cache.
#' @keywords internal
#' @noRd
federation_seasons_path <- function() {
  here::here("config", "federation-seasons.json")
}

#' Read the federation-season provenance cache.
#'
#' A missing file is not an error -- it means nothing has been discovered yet,
#' and every lookup falls through to NULL, which is the fail-safe direction.
#'
#' @param path Path to the cache JSON.
#' @return Tibble with the columns of [.federation_seasons_empty()].
#' @keywords internal
#' @noRd
read_federation_seasons <- function(path = federation_seasons_path()) {
  empty <- .federation_seasons_empty()
  if (!file.exists(path)) {
    return(empty)
  }
  payload <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  entries <- payload$entries
  if (is.null(entries) || length(entries) == 0L || nrow(entries) == 0L) {
    return(empty)
  }
  out <- tibble::as_tibble(entries)
  for (nm in names(empty)) {
    if (!nm %in% names(out)) out[[nm]] <- empty[[nm]][NA_integer_]
  }
  out$season <- .fs_coerce(out$season, as.integer)
  out$id <- .fs_coerce(out$id, as.integer)
  out$verified <- .fs_coerce(out$verified, as.logical)
  for (nm in c("federation", "sex", "division", "title", "source",
               "discovered_at", "note")) {
    out[[nm]] <- .fs_coerce(out[[nm]], as.character)
  }
  out[, names(empty), drop = FALSE]
}

#' Resolve one federation id from the provenance cache.
#'
#' Only `verified` entries carrying a season attribution resolve. An
#' observation whose season is unknown (`season = NA`) is deliberately
#' unresolvable: recording that an id exists is not the same as knowing which
#' season it belongs to.
#' @keywords internal
#' @noRd
federation_season_id <- function(federation, sex, division, season,
                                 path = federation_seasons_path()) {
  cache <- read_federation_seasons(path)
  if (nrow(cache) == 0L) {
    return(NULL)
  }
  hit <- cache[
    cache$federation == federation &
      cache$sex == sex &
      cache$division == division &
      !is.na(cache$season) & cache$season == as.integer(season) &
      !is.na(cache$verified) & cache$verified, ,
    drop = FALSE
  ]
  if (nrow(hit) == 0L) {
    return(NULL)
  }
  as.integer(hit$id[[1L]])
}

#' Merge discovered entries into an existing provenance cache.
#'
#' Keyed on (federation, sex, division, season) -- NA seasons key on the id
#' instead, since an unattributed observation is about the id, not a season.
#' Two `verified` rows disagreeing on `id` for the same key is a hard abort:
#' that is a federation renumbering or a bad discovery pass, and silently
#' picking one is exactly the class of quiet wrongness this workstream exists
#' to remove.
#' @keywords internal
#' @noRd
merge_federation_seasons <- function(new_entries,
                                     existing = .federation_seasons_empty()) {
  if (nrow(new_entries) == 0L) {
    return(existing)
  }
  combined <- dplyr::bind_rows(existing, new_entries)
  combined$.key <- ifelse(
    is.na(combined$season),
    paste(combined$federation, combined$sex, combined$division, "id", combined$id, sep = "/"),
    paste(combined$federation, combined$sex, combined$division, combined$season, sep = "/")
  )

  for (k in unique(combined$.key)) {
    grp <- combined[combined$.key == k, , drop = FALSE]
    ver <- grp[!is.na(grp$verified) & grp$verified, , drop = FALSE]
    if (nrow(ver) > 1L && length(unique(ver$id)) > 1L) {
      cli::cli_abort(
        c(
          "Conflicting verified federation ids for {k}.",
          "x" = "Ids seen: {paste(sort(unique(ver$id)), collapse = ', ')}.",
          "i" = "Resolve by hand before merging -- do not let discovery pick a winner."
        ),
        class = "sports_federation_id_conflict"
      )
    }
  }

  combined$.trust <- unname(FEDERATION_SOURCE_TRUST[combined$source])
  combined$.trust[is.na(combined$.trust)] <- -1L

  combined |>
    dplyr::arrange(.data$.key, dplyr::desc(.data$.trust)) |>
    dplyr::distinct(.data$.key, .keep_all = TRUE) |>
    dplyr::select(-".key", -".trust") |>
    dplyr::arrange(
      .data$federation, .data$sex, .data$division, .data$season, .data$id
    )
}

#' Merge entries into the provenance cache and rewrite it.
#'
#' The maintenance entry point: `hsi_discover_tournaments()` (and WS5's
#' `kki_discover_season_ids()`) produce rows, this persists them with their
#' provenance so the next session can tell a live-read id from an inferred one.
#'
#' @param entries Tibble shaped like [.federation_seasons_empty()].
#' @param path Path to the cache JSON.
#' @return The merged cache, invisibly.
#' @importFrom rlang .data
#' @export
refresh_federation_seasons <- function(entries,
                                       path = federation_seasons_path()) {
  merged <- merge_federation_seasons(entries, read_federation_seasons(path))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(schema_version = 1L, entries = merged),
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
  invisible(merged)
}

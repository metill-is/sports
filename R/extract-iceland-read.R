#' @include publish-profile.R publish-divisions.R
NULL

#' Load a per-fit Icelandic-league extraction partition
#'
#' Reads the Parquet files written by the sport's extractor
#' (`extract_football_iceland()`, `extract_basketball_iceland()`,
#' `extract_handball_iceland()`) from
#' `data/beliefs/extracts/sport=X/country=iceland/sex=Z/fit_date=D/`, splits
#' each by the in-payload `division` column, and returns a named list keyed by
#' division code.
#'
#' Generalised from the football-only reader it replaces by PARAMETERISATION,
#' not by copying: which files are required, which are optional and what an
#' absent one degrades to all come from [`sport_publish_profile()`], and the division set
#' comes from `config/leagues.yml::<key>.publish_divisions[[sex]]`. The
#' descending `fit_date` scan, the completeness check, the per-division split,
#' `sim_inputs` and `cup_bracket` are sport-agnostic and exist exactly once.
#'
#' Auto-discovery (default `fit_date = NULL`) walks the `fit_date=*` partitions
#' in descending order and returns the first one containing every file in
#' `profile$required_extracts`. Files in `profile$optional_extracts` degrade to
#' the 0-row tibble from `profile$empty_extracts`, so a partition written
#' before a file type existed still reads.
#'
#' @param league League list with `country == "iceland"`. Its
#'   `<sport>_<country>` key must exist in `config/leagues.yml`.
#' @param sex `"male"` or `"female"`.
#' @param fit_date `Date` or `NULL`. When `NULL` (default), reads the latest
#'   partition containing the full required set.
#' @param extracts_root Beliefs extracts root.
#'   Default `here::here("data", "beliefs", "extracts")`.
#' @param target_divs Character vector of divisions to load. When `NULL`
#'   (default), resolves to the per-sex publish set from
#'   `config/leagues.yml::<key>.publish_divisions[[sex]]`. The returned list
#'   always includes a slot per requested division (with empty-tibble parquets
#'   when the `division` filter yields no rows).
#' @param profile Per-sport publish profile; see [`sport_publish_profile()`].
#' @return Named list. Each requested division key (e.g. `"BD"`, `"1D"`) maps to
#'   a list of the required + optional tibbles, `division` column dropped after
#'   filtering. Plus `fit_date` (the `Date` of the partition that was loaded),
#'   `sim_inputs` (list or `NULL`) and `cup_bracket` (list or `NULL`).
#' @export
read_extracted_iceland <- function(league, sex, fit_date = NULL,
                                   extracts_root = here::here(
                                     "data", "beliefs", "extracts"
                                   ),
                                   target_divs = NULL,
                                   profile = sport_publish_profile(
                                     league$sport
                                   )) {
  stopifnot(league$country == "iceland")
  stopifnot(sex %in% c("male", "female"))
  # The league key is DERIVED, then asserted -- the `<sport>_<country>`
  # convention holds for all three Icelandic leagues today, but a future
  # league that breaks it must fail here rather than silently read the
  # wrong cell's publish_divisions block.
  league_key <- paste0(league$sport, "_", league$country)
  stopifnot(league_key %in% names(load_leagues()))
  if (is.null(target_divs)) {
    target_divs <- .iceland_division_codes(league_key, sex)
  }
  stopifnot(
    is.character(target_divs),
    length(target_divs) >= 1L,
    all(target_divs %in% .iceland_division_codes(league_key, sex))
  )

  file_types <- profile$required_extracts
  expected <- paste0(file_types, ".parquet")

  base <- file.path(
    extracts_root,
    paste0("sport=", league$sport),
    paste0("country=", league$country),
    paste0("sex=", sex)
  )
  if (!dir.exists(base)) {
    stop("No extracts directory at ", base, call. = FALSE)
  }

  .partition_is_complete <- function(fit_dir) {
    all(file.exists(file.path(fit_dir, expected)))
  }

  if (is.null(fit_date)) {
    parts <- list.dirs(base, full.names = TRUE, recursive = FALSE)
    fit_dirs <- parts[grepl("/fit_date=", parts)]
    if (length(fit_dirs) == 0L) {
      stop("No fit_date partitions under ", base, call. = FALSE)
    }
    fit_dates_chr <- sub(".*fit_date=", "", fit_dirs)
    ord <- order(as.Date(fit_dates_chr), decreasing = TRUE)
    fit_dir <- NULL
    for (i in ord) {
      d <- fit_dirs[i]
      if (.partition_is_complete(d)) {
        fit_dir <- d
        break
      }
    }
    if (is.null(fit_dir)) {
      stop(
        "No fit_date partition under ", base,
        " contains a complete extracted set. ",
        "Force-trigger fit.yml or run the sport's extractor locally.",
        call. = FALSE
      )
    }
    fit_date_out <- as.Date(sub(".*fit_date=", "", fit_dir))
  } else {
    fit_date_out <- as.Date(fit_date)
    fit_dir <- file.path(
      base, paste0("fit_date=", format(fit_date_out, "%Y-%m-%d"))
    )
    if (!dir.exists(fit_dir)) {
      stop("Extracts partition not found: ", fit_dir, call. = FALSE)
    }
    if (!.partition_is_complete(fit_dir)) {
      stop(
        "Extracts partition ", fit_dir,
        " is incomplete (one or more of: ",
        paste(expected, collapse = ", "), "). ",
        "Re-run the sport's extractor against this fit.",
        call. = FALSE
      )
    }
  }

  empty_tibbles <- profile$empty_extracts

  # Optional file types (football's `tournament_placements`, the 2DT sports'
  # `round_strengths_quantiles`, and `fit_meta` on both) degrade to a 0-row
  # tibble when absent: they are read but never gate partition completeness.
  per_division_file_types <- c(file_types, profile$optional_extracts)
  parquets <- lapply(per_division_file_types, function(ft) {
    p <- file.path(fit_dir, paste0(ft, ".parquet"))
    if (file.exists(p)) {
      arrow::read_parquet(p)
    } else {
      empty_tibbles[[ft]]
    }
  })
  names(parquets) <- per_division_file_types

  read_one_division <- function(target_div) {
    out <- lapply(per_division_file_types, function(ft) {
      df <- parquets[[ft]]
      if (!"division" %in% names(df) || nrow(df) == 0L) {
        return(empty_tibbles[[ft]])
      }
      df <- df[df$division == target_div, , drop = FALSE]
      df$division <- NULL
      tibble::as_tibble(df)
    })
    names(out) <- per_division_file_types
    out
  }

  out <- lapply(target_divs, read_one_division)
  names(out) <- target_divs

  # Optional shared sim_inputs (per-draw model parameters; not per-division).
  # Absent for pre-simulator partitions; the publisher only reads these when
  # it needs to re-run the simulator with non-default tiebreak / pairing opts.
  sim_inputs_team_path <- file.path(fit_dir, "sim_inputs_team.parquet")
  sim_inputs_scalar_path <- file.path(fit_dir, "sim_inputs_scalar.parquet")
  out$sim_inputs <- if (file.exists(sim_inputs_team_path) &&
    file.exists(sim_inputs_scalar_path)) {
    list(
      team   = tibble::as_tibble(arrow::read_parquet(sim_inputs_team_path)),
      scalar = tibble::as_tibble(arrow::read_parquet(sim_inputs_scalar_path))
    )
  } else {
    NULL
  }

  # Optional pre-built cup `bracket.json` payload (a single JSON-string cell;
  # written by extract_football_iceland() only when a live cup frontier
  # exists). Parsed back to the nested list the publisher serialises verbatim.
  # Absent for non-cup fits and for cup fits with no live frontier.
  cup_bracket_path <- file.path(fit_dir, "cup_bracket.parquet")
  out$cup_bracket <- if (file.exists(cup_bracket_path)) {
    pj <- arrow::read_parquet(cup_bracket_path)$payload_json
    if (length(pj) >= 1L && !is.na(pj[[1]])) {
      jsonlite::fromJSON(pj[[1]], simplifyVector = FALSE)
    } else {
      NULL
    }
  } else {
    NULL
  }

  out$fit_date <- fit_date_out
  out
}

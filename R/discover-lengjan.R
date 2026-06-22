# R/discover-lengjan.R
#' @include ingest-lengjan-odds.R storage.R config.R
NULL

#' Parse the Lengjan "Veldu deild" competition dropdown.
#'
#' The parent listing page (no `competition=` query param) renders three
#' `<select>`s: sport, country, and league ("Veldu deild"). The league select's
#' `<option value>` is the competition ID and the text is Lengjan's display name.
#' We identify the league select by its placeholder option text rather than
#' position, so a layout reorder does not silently pick the wrong dropdown.
#'
#' @param html Parsed HTML (from `rvest::read_html()`).
#' @return Tibble `{comp_id, lengjan_name}`; empty-with-columns if absent.
#' @export
parse_competition_dropdown <- function(html) {
  empty <- tibble::tibble(comp_id = character(0), lengjan_name = character(0))
  selects <- rvest::html_elements(html, "select")
  if (length(selects) == 0L) {
    return(empty)
  }
  for (sel in selects) {
    opts <- rvest::html_elements(sel, "option")
    if (length(opts) == 0L) next
    txt <- rvest::html_text2(opts)
    Encoding(txt) <- "UTF-8"
    if (!any(trimws(txt) == "Veldu deild", na.rm = TRUE)) next
    vals <- rvest::html_attr(opts, "value")
    keep <- !is.na(vals) & nzchar(vals)
    return(tibble::tibble(
      comp_id = as.character(vals[keep]),
      lengjan_name = trimws(txt[keep])
    ))
  }
  empty
}

#' Classify a Lengjan competition name into (sex, division).
#'
#' Deterministic, advisory name match. Sex from a "kvenna"/"kv" marker;
#' division from the league-name pattern. A name that matches no division
#' pattern is `division = NA`, `confidence = "low"` -- surfaced for a human,
#' never auto-wired. Patterns are ASCII except the basketball "Bonusdeild" name,
#' written with a `ó` escape (R-source non-ASCII rule); the cup matches the
#' ASCII substring "bikar", so it needs no escape.
#'
#' @param lengjan_name Competition display name from the dropdown.
#' @param sport,country Pass-through context (reserved for sport-specific rules).
#' @return Tibble `{sex, division, confidence}` (one row).
#' @export
classify_competition <- function(lengjan_name, sport, country) {
  nm <- lengjan_name
  Encoding(nm) <- "UTF-8"
  female <- grepl("kvenna", nm, ignore.case = TRUE) ||
    grepl("(^| )kv\\.?( |$)", nm, ignore.case = TRUE)
  sex <- if (female) "female" else "male"

  division <- if (grepl("bikar", nm, ignore.case = TRUE)) {
    "CUP"
  } else if (grepl("3\\. *deild", nm, ignore.case = TRUE)) {
    "LD3"
  } else if (grepl("4\\. *deild", nm, ignore.case = TRUE)) {
    "LD4"
  } else if (grepl("2\\. *deild", nm, ignore.case = TRUE)) {
    "LD2"
  } else if (grepl("lengjudeild", nm, ignore.case = TRUE)) {
    "LD1"
  } else if (grepl("besta *deild|b\u00f3nusdeild", nm, ignore.case = TRUE)) {
    "BD"
  } else {
    NA_character_
  }
  tibble::tibble(
    sex = sex,
    division = division,
    confidence = if (is.na(division)) "low" else "high"
  )
}

#' Normalise a team name for fuzzy comparison (comparison key only).
#'
#' Lowercases, strips a trailing women's marker (" kv"/" kv."), transliterates
#' diacritics to ASCII, removes dots, collapses whitespace, and folds the common
#' "Rvk" -> "r" and (post-transliteration) "ol" -> "o" abbreviations so
#' "Víkingur Rvk kv" and "Víkingur R." collide. The original canonical
#' string is always what gets emitted -- this key is never shown.
#' @noRd
.norm_team <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("\\s*kv\\.?$", "", x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\.", "", x)
  x <- gsub("\\brvk\\b", "r", x)
  x <- gsub("\\bol\\b", "o", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Fuzzy-match Lengjan team renderings to our canonical team names.
#'
#' Exact normalised match -> "high"; nearest within Levenshtein distance 2 ->
#' "medium"; otherwise `canonical_guess = NA`, "low". Low/medium guesses are
#' fail-safe: a wrong `team_names` entry makes `decide_league` warn-skip the
#' match, never mis-bet (existing normaliser invariant).
#'
#' @param renderings Character vector of Lengjan display names.
#' @param known_teams Character vector of canonical (federation) team names.
#' @return Tibble `{lengjan, canonical_guess, confidence}`.
#' @export
match_team_names <- function(renderings, known_teams) {
  empty <- tibble::tibble(
    lengjan = character(0), canonical_guess = character(0), confidence = character(0)
  )
  if (length(renderings) == 0L) {
    return(empty)
  }
  kn <- unique(known_teams)
  if (length(kn) == 0L) {
    return(tibble::tibble(
      lengjan = renderings, canonical_guess = NA_character_, confidence = "low"
    ))
  }
  kn_norm <- vapply(kn, .norm_team, character(1))
  rows <- lapply(renderings, function(r) {
    rn <- .norm_team(r)
    hit <- which(kn_norm == rn)
    if (length(hit) >= 1L) {
      return(tibble::tibble(lengjan = r, canonical_guess = kn[[hit[[1L]]]], confidence = "high"))
    }
    d <- utils::adist(rn, kn_norm)[1L, ]
    j <- which.min(d)
    if (length(j) == 1L && is.finite(d[[j]]) && d[[j]] <= 2L) {
      tibble::tibble(lengjan = r, canonical_guess = kn[[j]], confidence = "medium")
    } else {
      tibble::tibble(lengjan = r, canonical_guess = NA_character_, confidence = "low")
    }
  })
  dplyr::bind_rows(rows)
}

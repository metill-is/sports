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

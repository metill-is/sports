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

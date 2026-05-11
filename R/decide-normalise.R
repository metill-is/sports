#' @include config.R
NULL

#' Rewrite Lengjan-side team names in an odds tibble to canonical form.
#'
#' `config/leagues.yml::*.lengjan.team_names` maps canonical (federation)
#' team names to Lengjan display names — direction
#' `{canonical: lengjan_display}` — because the placer needs to find a
#' Lengjan match ID from a recommendation's canonical name.
#'
#' For the decide-time join (odds Parquet vs beliefs Parquet) we need the
#' inverse map. This function inverts the per-sex sub-map and applies it to
#' the `home_team` + `away_team` columns. Unmapped names pass through with
#' a `cli::cli_alert_warning` so the caller's loud "no beliefs" warning at
#' [decide_league()] catches the resulting empty-beliefs join.
#'
#' @param odds Tibble with at least `home_team` and `away_team` columns.
#' @param league League list (as from [load_leagues()]). Reads
#'   `league$lengjan$team_names[[sex]]`.
#' @param sex `"male"` or `"female"`.
#' @return Same tibble with Lengjan-side names rewritten to canonical
#'   wherever the inverse map has an entry. Other rows unchanged.
#' @export
normalise_lengjan_team_names <- function(odds, league, sex) {
  stopifnot(sex %in% c("male", "female"))
  if (nrow(odds) == 0L) {
    return(odds)
  }

  tn_all <- league$lengjan$team_names
  tn <- if (is.null(tn_all)) NULL else tn_all[[sex]]

  if (is.null(tn) || length(tn) == 0L) {
    cli::cli_alert_warning(
      "normalise_lengjan_team_names: no team_names for {league$sport}/{league$country}/{sex}; passing through (odds rows={nrow(odds)})"
    )
    return(odds)
  }

  # WHY tag_utf8: R's list-name round-trip through symbols drops the UTF-8
  # Encoding tag (yaml.load output, or inline `list("Grindavík" = ...)`,
  # both lose it). Without re-tagging, character matching between
  # arrow-Parquet-sourced strings (Encoding "UTF-8") and config-sourced
  # strings (Encoding "unknown") fails under a C locale.
  #
  # Crucially we use `Encoding(x) <- "UTF-8"` (changes the tag only) rather
  # than `enc2utf8(x)`, because the latter byte-escapes already-correct
  # UTF-8 bytes when input is tagged "unknown" (e.g. "Grindavik" bytes
  # `c3 ad` turn into the literal "Grindav<c3><ad>k" placeholder).
  tag_utf8 <- function(x) {
    Encoding(x) <- "UTF-8"
    x
  }

  canon <- tag_utf8(names(tn))
  lengjan <- tag_utf8(unname(unlist(tn)))
  if (anyDuplicated(lengjan) > 0L) {
    dups <- unique(lengjan[duplicated(lengjan)])
    stop(
      "normalise_lengjan_team_names: non-injective team_names for ",
      league$sport, "/", league$country, "/", sex,
      "; multiple canonical names map to: ",
      paste(dups, collapse = ", "),
      call. = FALSE
    )
  }
  invmap <- stats::setNames(canon, lengjan)

  remap <- function(x) {
    x <- tag_utf8(x)
    hit <- invmap[x]
    unmapped <- is.na(hit)
    if (any(unmapped)) {
      bad <- unique(x[unmapped])
      cli::cli_alert_warning(
        "normalise_lengjan_team_names: no team_names mapping for {league$sport}/{league$country}/{sex}: {paste(bad, collapse = ', ')}"
      )
      hit[unmapped] <- x[unmapped]
    }
    tag_utf8(unname(hit))
  }

  odds$home_team <- remap(odds$home_team)
  odds$away_team <- remap(odds$away_team)
  odds
}

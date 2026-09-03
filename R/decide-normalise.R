#' @include config.R
NULL

#' Rewrite Lengjan-side team names in an odds tibble to canonical form.
#'
#' `config/leagues.yml::*.lengjan.team_names` maps canonical (federation)
#' team names to Lengjan display names — direction
#' `{canonical: lengjan_display}` — because the placer needs to find a
#' Lengjan match ID from a recommendation's canonical name. A value is either
#' a single rendering (scalar) or a list of acceptable renderings, used when
#' Lengjan shows the same team under more than one byte-distinct string (e.g.
#' "Grindavík / Njarðvík kv" vs "Grindavik/Njarðvík kv").
#'
#' For the decide-time join (odds Parquet vs beliefs Parquet) we need the
#' inverse map. This function inverts the per-sex sub-map — every rendering of
#' a canonical points back at that canonical — and applies it to the
#' `home_team` + `away_team` columns. Unmapped names pass through with a
#' `cli::cli_alert_warning` so the caller's loud "no beliefs" warning at
#' [decide_league()] catches the resulting empty-beliefs join. The inverse must
#' stay injective (each rendering from one canonical); a shared rendering across
#' two canonicals is a hard error.
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

  # `tag_utf8()` (R/config.R) is load-bearing here: config-sourced strings and
  # Parquet-sourced strings carry different Encoding tags and compare unequal
  # under a C locale. See its roxygen for why it is not `enc2utf8()`.

  # A canonical may map to several acceptable Lengjan renderings (list value);
  # the inverse must point every rendering at that one canonical. Repeat each
  # canonical by its rendering count so `canon` and `lengjan` stay aligned.
  renders <- tn_renderings(tn)
  canon <- tag_utf8(rep(names(renders), lengths(renders)))
  lengjan <- tag_utf8(unlist(renders, use.names = FALSE))
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

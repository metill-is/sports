#' @include health.R ingest-hsi-handball.R ingest-kki-basketball.R
NULL

# ---- The season-resolution check --------------------------------------------
#
# WHY THIS EXISTS. "The season is over" and "the scraper went blind in October"
# look identical from the results table: zero new rows, every freshness check
# PAUSED, overall OK. The one thing that distinguishes them is whether the
# federation's season id RESOLVED, and only the registry knows that. Before the
# season-keyed registries the ids were derived from whatever season happened to
# be current, so the failure could not even be named.

#' Federation registries, keyed by the `data_source$results` prefix.
#'
#' Injectable so the unit tests stay hermetic. Both shipped resolvers are pure
#' registry + provenance-cache lookups and make NO network call -- that is a
#' requirement, not an accident: this runs inside `healthcheck.yml`, and adding
#' live discovery to either resolver would start making HTTP calls from a
#' read-only health snapshot.
#'
#' `league_divisions` is the set the pipeline actually needs NOW. Everything
#' outside it (HSI's `cup` and `playoffs`) is federation-deferred and WARNs.
#' @noRd
.federation_resolvers <- function() {
  list(
    hsi = list(
      current = hsi_current_season,
      unresolved = hsi_unresolved_seasons,
      league_divisions = c("div1", "div2")
    ),
    kki = list(
      current = kki_current_season,
      unresolved = kki_unresolved_seasons,
      league_divisions = c("div1", "div2")
    )
  )
}

#' Can every federation season the pipeline needs actually be resolved?
#'
#' One FAIL row per unresolvable LEAGUE division, one WARN row per
#' federation-deferred cup/playoffs gap, and one OK row per federation with no
#' gaps at all.
#'
#' INT-4, and it is a measurement rather than a preference:
#' `hsi_unresolved_seasons(2027L)` returns exactly three rows today -- male cup,
#' male playoffs, female playoffs -- because HSI does not create the
#' urslitakeppni or the 2026-27 bikar tournaments until later in the season.
#' `R/ingest-hsi-handball.R` documents that deferral as correct behaviour and
#' `tests/testthat/test-ingest-hsi.R` pins it. FAILing on them would leave this
#' check permanently red from its first day, and the alert channel is a
#' twice-daily GitHub workflow-failure email -- signal, not a pager. On a
#' channel that low-bandwidth a permanently-WARN check is worse than no check,
#' so FAIL is scoped to the divisions the pipeline needs now.
#'
#' INT-5: football contributes no rows. Its `data_source$results` is
#' `ksi_football`, for which there is no unresolved-seasons resolver and no
#' season registry to go stale; a fake OK row would imply a guarantee that does
#' not exist.
#'
#' @param leagues Leagues list, read as passed in (INT-1).
#' @param root Data root (unused today; kept for signature parity with the
#'   other checks so `pipeline_health()` composes them uniformly).
#' @param now Reference time; its date drives each federation's current season.
#' @param resolvers Injectable federation registry map.
#' @return A health tibble.
#' @noRd
check_season_resolution <- function(leagues,
                                    root = here::here("data"),
                                    now = Sys.time(),
                                    resolvers = .federation_resolvers()) {
  feds <- character()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    if (!isTRUE(lg$active)) next
    src <- lg$data_source$results
    if (is.null(src) || length(src) != 1L) next
    prefix <- sub("_.*$", "", as.character(src))
    if (prefix %in% names(resolvers)) feds <- c(feds, prefix)
  }
  # Both sexes of one league share one registry, so de-duplicate: the resolver
  # already covers every sex in a single call.
  feds <- unique(feds)
  if (length(feds) == 0L) {
    return(health_empty())
  }

  rows <- list()
  for (fed in feds) {
    r <- resolvers[[fed]]
    season <- as.integer(r$current(as.Date(now)))
    gaps <- r$unresolved(season)

    if (is.null(gaps) || nrow(gaps) == 0L) {
      rows[[length(rows) + 1L]] <- health_row(
        "season_resolution", fed, "OK",
        sprintf("season %d resolves for every division", season),
        "no gaps"
      )
      next
    }
    for (i in seq_len(nrow(gaps))) {
      g <- gaps[i, ]
      is_league <- g$division %in% r$league_divisions
      rows[[length(rows) + 1L]] <- health_row(
        "season_resolution",
        paste(fed, g$sex, g$division),
        if (is_league) "FAIL" else "WARN",
        sprintf(
          "no resolvable id for season %d%s",
          g$season,
          if (is_league) {
            " -- the ingest skips this cell, which is indistinguishable from an off-season"
          } else {
            " (federation-deferred cup/playoffs; created later in the season)"
          }
        ),
        if (is_league) "must resolve" else "deferred ok"
      )
    }
  }
  dplyr::bind_rows(rows)
}

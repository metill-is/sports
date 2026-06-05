# R/derive-round.R
NULL

#' Derive the league round (matchweek / umferð) for results rows.
#'
#' The federation scrapers cannot supply a round number — KSÍ exposes no
#' umferð/round on its match-list pages (confirmed via live fetch + fixtures),
#' so the schema `round` column was written `NA_integer_` by every scraper. This
#' helper reconstructs it from data already captured.
#'
#' A round is **not** a dense rank of match dates: an Icelandic league round is
#' routinely played across several calendar dates (e.g. Besta deild round 1 in
#' 2026 ran 2026-04-10 + 2026-04-12), so date-ranking would over-count rounds.
#' Instead the round is each team's cumulative appearance index within its
#' `(sport, country, sex, season, division)` cell — a team's Nth match of the
#' season is round N. In a clean round-robin both sides of a fixture are on the
#' same appearance count, so the fixture round is well defined; when a
#' postponement leaves the two sides on different counts (~4% of fixtures), the
#' round is taken as the **max** of the two, i.e. the more-progressed side, which
#' recovers the scheduled round when one team is behind. This mirrors the
#' per-team appearance index the model layer already uses (`model-prepare.R`).
#'
#' Cup divisions (`division == "CUP"`) are left `NA` on purpose: their dates are
#' knockout bracket rounds, not weekly matchweeks (the publish/extract layer
#' infers bracket rounds where it needs them).
#'
#' Approximation: a postponed fixture played much later is labelled by *when it
#' was played* (appearance order), not its original scheduled round — acceptable
#' for league-trajectory use, and nothing in the model path depends on this
#' column.
#'
#' @param df A results tibble carrying at least `sport`, `country`, `sex`,
#'   `season`, `division`, `match_date`, `home_team`, `away_team`.
#' @return `df` with the `round` column populated (integer); cup rows stay `NA`.
#' @keywords internal
#' @noRd
derive_league_round <- function(df) {
  if (nrow(df) == 0L) {
    return(df)
  }
  df$round <- NA_integer_
  noncup <- which(df$division != "CUP")
  if (length(noncup) == 0L) {
    return(df)
  }

  d <- df[noncup, , drop = FALSE]
  d$.fid <- seq_along(noncup)

  cell <- c("sport", "country", "sex", "season", "division")
  long <- dplyr::bind_rows(
    dplyr::transmute(d,
      .fid = .data$.fid, dplyr::across(dplyr::all_of(cell)),
      match_date = .data$match_date, team = .data$home_team
    ),
    dplyr::transmute(d,
      .fid = .data$.fid, dplyr::across(dplyr::all_of(cell)),
      match_date = .data$match_date, team = .data$away_team
    )
  )

  long <- long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(cell, "team")))) |>
    dplyr::arrange(.data$match_date, .by_group = TRUE) |>
    dplyr::mutate(app = dplyr::row_number()) |>
    dplyr::ungroup()

  rnd <- long |>
    dplyr::group_by(.data$.fid) |>
    dplyr::summarise(round = max(.data$app), .groups = "drop")

  df$round[noncup] <- as.integer(rnd$round[match(d$.fid, rnd$.fid)])
  df
}

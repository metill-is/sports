# Football v2 prototype: data access shared by the scripts. Reads the facts
# store through the package and applies production's own training filter, so
# every engine and variant trains on what a production fit at that cutoff saw.

fv2_source <- function(dir = here::here("research", "football-v2")) {
  for (f in c("spec.R", "engines.R", "score.R", "evaluate.R", "adapter.R")) source(file.path(dir, f))
}

fv2_out_dir <- function() {
  d <- Sys.getenv("FV2_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/out")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' League division behind a facts-store division code: split-phase and
#' play-off codes fold into their league, cups have none.
fv2_league_division <- function(division, playoff_pattern = "_((UPPER|LOWER)_)?PO$",
                                cup_codes = "CUP") {
  out <- sub(playoff_pattern, "", division)
  out[division %in% cup_codes] <- NA_character_
  out
}

#' Team-by-season league division with its rank (1 = top).
#'
#' @param rank_order League divisions from the top down; a division not
#'   listed ranks below all of them.
fv2_division_table <- function(matches, rank_order) {
  d <- data.frame(
    team = c(matches$home_team, matches$away_team),
    season = as.integer(c(matches$season, matches$season)),
    division = c(matches$league_division, matches$league_division)
  )
  d <- d[!is.na(d$division), , drop = FALSE]
  if (!nrow(d)) {
    return(data.frame(team = character(), season = integer(), division = character(), rank = integer()))
  }
  tab <- stats::aggregate(list(n = rep(1L, nrow(d))), d[c("team", "season", "division")], sum)
  tab <- tab[order(tab$team, tab$season, -tab$n, tab$division), , drop = FALSE]
  tab <- tab[!duplicated(tab[c("team", "season")]), c("team", "season", "division"), drop = FALSE]
  tab$rank <- match(tab$division, rank_order, nomatch = length(rank_order) + 1L)
  rownames(tab) <- NULL
  tab
}

#' League divisions from the top down, read off the league's training filter.
fv2_rank_order <- function(league) {
  unique(stats::na.omit(fv2_league_division(unlist(league$training_filter$divisions))))
}

#' Played Icelandic football results with the two columns the spec reads:
#' `league_division` (NA for the cup) and the offset group `cell`.
#'
#' @param sort Order by date and home team. `FALSE` keeps the store's own
#'   order, which is the order `prepare_data()` trains in.
fv2_results <- function(sex, root = here::here("data"), sort = TRUE) {
  r <- read_table("results", root = root, filter = list(sport = "football", country = "iceland", sex = sex))
  r <- as.data.frame(r[!is.na(r$home_score) & !is.na(r$away_score), ])
  r$league_division <- fv2_league_division(r$division)
  r$cell <- fv2_cell(r$division, r$season)
  if (sort) r <- r[order(r$match_date, r$home_team), ]
  r
}

#' Offset group of a match: its league (play-offs folded in) or cup, by season.
fv2_cell <- function(division, season) {
  league_division <- fv2_league_division(division)
  paste(ifelse(is.na(league_division), division, league_division), season, sep = "@")
}

#' Training matches at a cutoff and the fixtures to predict after it.
#'
#' @param horizon_days Predict matches in (cutoff, cutoff + horizon_days].
#' @return list(train, pred, skipped): `skipped` counts fixtures dropped
#'   because a team has no match in the training set.
fv2_split <- function(results, league, cutoff, horizon_days = 7L, horizon_end = NULL) {
  cutoff <- as.Date(cutoff)
  if (is.null(horizon_end)) horizon_end <- cutoff + horizon_days
  train <- as.data.frame(model_training_results(results, league, end_date = cutoff))
  teams <- unique(c(train$home_team, train$away_team))
  ahead <- results[results$match_date > cutoff & results$match_date <= horizon_end, , drop = FALSE]
  known <- ahead$home_team %in% teams & ahead$away_team %in% teams
  list(train = train, pred = ahead[known, , drop = FALSE], skipped = sum(!known))
}

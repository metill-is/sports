# Football v2 research scripts: data access and the research-only helpers.
# The model itself (spec, INLA engine, adapter) lives in the package
# (R/model-v2-*.R) and is loaded with devtools::load_all(); what is sourced
# here is the Stan reference engine, scoring and evaluation. Reads the facts
# store through the package and applies production's own training filter, so
# every engine and variant trains on what a production fit at that cutoff saw.

fv2_source <- function(dir = here::here("research", "football-v2")) {
  for (f in c("engines.R", "score.R", "evaluate.R")) source(file.path(dir, f))
}

fv2_out_dir <- function() {
  d <- Sys.getenv("FV2_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-football-v2/out")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
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

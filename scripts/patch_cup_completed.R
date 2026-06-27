# patch_cup_completed.R
#
# One-off, deterministic injector for the additive `completed[]` block into the
# already-published men's cup bracket.json. No Stan re-fit, no posterior re-sim:
# `completed[]` is pure bracket bookkeeping computed from the played CUP results,
# so injecting it cannot drift the existing posterior-derived keys
# (teams/teams_is/matchup/matches/r32).
#
# It mirrors the live extract call site in R/extract-football-iceland.R exactly:
#   - prepare_data()            (:1218)  -> pred_d
#   - read_table("results")     (:1235)  filtered match_date <= end_date (:1240)
#   - current_season = max(season) (:1244)
#   - read_table("schedules")   (:1255)  filtered to current_season (:1264)
#   - .build_bracket_state_pfi()(:1368)  pure bookkeeping, no RNG
#   - .build_cup_completed_pfi() (A1, tested)
#
# NOTE on `root`: read_table()/prepare_data() default root = here::here("data")
# and resolve stores under it (results -> data/facts/results). We pass that same
# root, NOT ".", so the loaders find the stores.

devtools::load_all(".")

league <- list(sport = "football", country = "iceland")
sex <- "male"
root <- here::here("data")
end_date <- Sys.Date()

prep <- prepare_data(league, sex, end_date = end_date, root = root) # :1218
pred_d <- prep$pred_d # :1221

results <- read_table(
  "results",
  root   = root,
  filter = list(sport = league$sport, country = league$country, sex = sex)
) # :1235
results <- results[
  !is.na(results$match_date) & results$match_date <= end_date, ,
  drop = FALSE
] # :1240
current_season <- max(results$season, na.rm = TRUE) # :1244

season_schedule <- read_table(
  "schedules",
  root   = root,
  filter = list(sport = league$sport, country = league$country, sex = sex)
) # :1255
season_schedule <- season_schedule[
  !is.na(season_schedule$match_date) &
    season_schedule$season == current_season, ,
  drop = FALSE
] # :1264

bracket_state <- .build_bracket_state_pfi(
  pred_d,
  results        = results,
  current_season = current_season,
  schedule       = season_schedule
) # :1368

completed <- .build_cup_completed_pfi(bracket_state, results, current_season)
stopifnot(length(completed) >= 1L)

bj_path <- "data/publish/football/iceland/karla-bikar/bracket.json"
bj <- jsonlite::fromJSON(bj_path, simplifyVector = FALSE)
bj$completed <- completed
jsonlite::write_json(bj, bj_path, auto_unbox = TRUE, matrix = "rowmajor")

cat(
  "injected", length(completed), "completed matches; rounds:",
  paste(unique(vapply(completed, `[[`, "", "round")), collapse = ","), "\n"
)

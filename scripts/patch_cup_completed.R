# patch_cup_completed.R
#
# Deterministic injector for the additive `completed[]` block into the men's
# cup forecast. No Stan re-fit, no posterior re-sim: `completed[]` is pure
# bracket bookkeeping computed from the played CUP results, so injecting it
# cannot drift the existing posterior-derived keys (teams/teams_is/matchup/
# matches/r32).
#
# WHY THIS PATCHES BOTH THE PARQUET *AND* THE PUBLISHED JSON
# ---------------------------------------------------------
# `completed[]` is built in the extract layer (it needs the transient
# `bracket_state`) and round-tripped through `cup_bracket.parquet`; the
# publisher writes that parquet payload verbatim to `bracket.json`. So the
# parquet is the source of truth — patching only the published json is
# fragile: the next `decide-publish` / `republish` run re-reads the parquet
# and silently reverts the json.
#
# That fragility bit us on 2026-06-27: the `completed[]` feature (R/extract-
# football-iceland.R `05bdcaa4`, committed 09:30Z) landed *while* a fit job
# launched at 08:32Z was still running. CI uses `devtools::load_all()`, which
# freezes the code at job-checkout time, so that ~2h fit wrote a `completed`-
# less parquet at 10:23Z. A json-only patch was reverted by the next publish.
# Patching the parquet too makes every subsequent publish keep `completed[]`,
# until the next fresh fit (running post-`05bdcaa4` code) carries it natively.
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

# ---- 1. Patch the parquet (the source of truth the publisher reads) ---------
# Discover the latest fit_date partition that carries a cup_bracket.parquet,
# the same partition read_extracted_football() auto-selects at publish time.
extract_base <- here::here(
  "data", "beliefs", "extracts",
  "sport=football", "country=iceland", "sex=male"
)
fit_dirs <- sort(
  list.files(extract_base, pattern = "^fit_date=", full.names = TRUE),
  decreasing = TRUE
)
parquet_path <- NULL
for (d in fit_dirs) {
  cand <- file.path(d, "cup_bracket.parquet")
  if (file.exists(cand)) {
    parquet_path <- cand
    break
  }
}
stopifnot(!is.null(parquet_path))

payload <- jsonlite::fromJSON(
  arrow::read_parquet(parquet_path)$payload_json[[1]],
  simplifyVector = FALSE
)
keys_before <- setdiff(names(payload), "completed")
payload$completed <- completed
new_payload <- as.character(jsonlite::toJSON(
  payload,
  auto_unbox = TRUE, matrix = "rowmajor"
))
arrow::write_parquet(
  tibble::tibble(payload_json = new_payload),
  parquet_path
)

# ---- 2. Re-derive bracket.json FROM the patched parquet ----------------------
# Mirrors R/publish-football-iceland.R: read the parquet payload, write it
# verbatim. Deriving the json from the parquet (rather than patching the json
# independently) guarantees the two artifacts can never diverge.
bj_path <- here::here(
  "data", "publish", "football", "iceland", "karla-bikar", "bracket.json"
)
bj_before <- jsonlite::fromJSON(bj_path, simplifyVector = FALSE)
cup_bracket <- jsonlite::fromJSON(
  arrow::read_parquet(parquet_path)$payload_json[[1]],
  simplifyVector = FALSE
)
jsonlite::write_json(cup_bracket, bj_path, auto_unbox = TRUE, matrix = "rowmajor")

# ---- 3. Drift gate: posterior-derived keys must be unchanged -----------------
# `matchup` is a list of 4-dp doubles round-tripped through JSON, so compare it
# numerically (epsilon) rather than with strict identical(); every other key is
# strings/integers and must be byte-identical.
bj_after <- jsonlite::fromJSON(bj_path, simplifyVector = FALSE)
for (k in keys_before) {
  if (identical(k, "matchup")) {
    drift <- max(abs(unlist(bj_before[[k]]) - unlist(bj_after[[k]])))
    stopifnot(is.finite(drift), drift < 1e-9)
  } else {
    stopifnot(identical(bj_before[[k]], bj_after[[k]]))
  }
}
stopifnot(length(bj_after$completed) == length(completed))

cat(sprintf(
  "patched parquet + json: %s\n  injected %d completed matches; rounds: %s\n  posterior keys unchanged: %s\n",
  parquet_path,
  length(completed),
  paste(
    sprintf(
      "%s=%d", names(table(vapply(completed, `[[`, "", "round"))),
      as.integer(table(vapply(completed, `[[`, "", "round")))
    ),
    collapse = ", "
  ),
  paste(keys_before, collapse = ", ")
))

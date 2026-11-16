# Replay one football cell at a past date through the production fit path into
# an isolated tree. Used for the cutover's replay (step 2 of the shadow plan)
# and by the INLA-on-a-runner trial workflow.
#
#   Rscript research/football-v2/run_replay.R <sex> <as_of> <out_dir> [mode] [horizon_days]
#
# mode "inla" (default): fit_league() with `engine: inla` set on the league in
#   memory (config/leagues.yml is not touched), then the unchanged extract and
#   publish layers.
# mode "stored": no fit. Production's stored extract partition for that date
#   goes through the same publish code with the same inputs: the baseline the
#   INLA tree is compared with.
#
# Nothing under data/ is written. The facts, the earlier extract partitions
# and the round-predictions accumulator are copied into <out_dir>, and every
# read and write after that happens there. Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
sex <- args[[1]]
as_of <- as.Date(args[[2]])
out_dir <- args[[3]]
mode <- if (length(args) >= 4L) args[[4]] else "inla"
horizon <- if (length(args) >= 5L) as.integer(args[[5]]) else 14L
stopifnot(sex %in% c("male", "female"), !is.na(as_of), mode %in% c("inla", "stored"))

src <- normalizePath(here::here("data"))
data_status <- function() system2("git", c("status", "--porcelain", "--", "data"), stdout = TRUE)
dirty_before <- data_status()
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir)
if (startsWith(out_dir, src)) stop("out_dir must be outside data/", call. = FALSE)
if (length(list.files(out_dir))) stop("out_dir is not empty: ", out_dir, call. = FALSE)
iso <- file.path(out_dir, "data")

copy_tree <- function(rel, to_root = iso) {
  from <- file.path(src, rel)
  if (!dir.exists(from)) {
    return(invisible(FALSE))
  }
  to <- file.path(to_root, dirname(rel))
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(from, to, recursive = TRUE))
  invisible(TRUE)
}
cell <- file.path("sport=football", "country=iceland")
copy_tree(file.path("facts", "results", cell))
copy_tree(file.path("facts", "schedules", cell))
ext_rel <- file.path("beliefs", "extracts", cell, paste0("sex=", sex))
parts <- list.files(file.path(src, ext_rel), pattern = "^fit_date=")
part_date <- as.Date(sub("^fit_date=", "", parts))
if (mode == "stored" && !as_of %in% part_date) {
  stop("no stored extract partition for ", format(as_of), call. = FALSE)
}
# WHY: the publisher reads earlier fits (pre-round forecasts, the pre-season
# baseline). Both modes get the same earlier partitions, so the two trees
# differ only through the partition for `as_of` itself.
before <- function(d) if (mode == "stored") d <= as_of else d < as_of
earlier <- parts[before(part_date)]
for (p in earlier) copy_tree(file.path(ext_rel, p))
# WHY: the standings' expected goals and points are summed over the rounds for
# which a pre-round fit can be found, and the publisher looks for that fit in
# the extracts tree and the archive tree together. Without the archive a
# round resolves to an older fit, or to none, and the sums change.
arc_rel <- file.path("beliefs", "archive", cell, paste0("sex=", sex))
arc_parts <- list.files(file.path(src, arc_rel), pattern = "^fit_date=")
arc_parts <- arc_parts[before(as.Date(sub("^fit_date=", "", arc_parts)))]
for (p in arc_parts) copy_tree(file.path(arc_rel, p))

# WHY: the daily fit runs before that day's evening games, which are ingested
# later, dated on the fit day. On today's store a replay would train on them
# and forecast nothing for them. Production recorded how many matches its fit
# saw, so when that count is the store's without the fit day's matches, the
# isolated copy is put back as it was: those matches leave the results and
# return to the schedule.
key <- list(sport = "football", country = "iceland", sex = sex)
league <- load_leagues()[["football_iceland"]]
stopifnot(is.null(league$engine))
facts <- as.data.frame(read_table("results", root = iso, filter = key))
recorded <- tryCatch(
  as.data.frame(read_table("fit_diagnostics", root = src, filter = key)),
  error = function(e) NULL
)
production_n_obs <- if (is.null(recorded)) integer() else recorded$n_obs[recorded$fit_date == as_of]
production_n_obs <- production_n_obs[!is.na(production_n_obs)]
trained_on <- function(end) nrow(model_training_results(facts, league, end_date = end))
returned <- facts[0, ]
if (length(production_n_obs) == 1L && production_n_obs != trained_on(as_of)) {
  if (production_n_obs != trained_on(as_of - 1L)) {
    stop(
      "production's fit saw ", production_n_obs, " matches; today's store gives ", trained_on(as_of),
      " to the fit day and ", trained_on(as_of - 1L), " before it. Cannot rebuild the store as it was.",
      call. = FALSE
    )
  }
  on_day <- facts$match_date == as_of & !is.na(facts$home_score)
  returned <- facts[on_day, ]
  season <- facts$season %in% unique(returned$season)
  write_table(tibble::as_tibble(facts[season & !on_day, ]), "results", root = iso)
  fixture_cols <- c("sport", "country", "sex", "season", "match_date", "home_team", "away_team", "division", "round")
  schedule <- as.data.frame(read_table("schedules", root = iso, filter = key))
  schedule <- schedule[schedule$season %in% unique(returned$season), , drop = FALSE]
  back <- returned[fixture_cols]
  back$kickoff_time <- NA_character_
  write_table(tibble::as_tibble(rbind(schedule[names(back)], back)), "schedules", root = iso)
  stopifnot(trained_on_iso <- nrow(model_training_results(
    as.data.frame(read_table("results", root = iso, filter = key)), league,
    end_date = as_of
  )) == production_n_obs)
}

hist_root <- file.path(out_dir, "round_predictions_history")
dir.create(hist_root)
file.copy(file.path(src, "beliefs", "round_predictions_history", "football"), hist_root, recursive = TRUE)

cpu <- function() sum(proc.time()[c("user.self", "sys.self", "user.child", "sys.child")], na.rm = TRUE)
timing <- list()
timed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  c0 <- cpu()
  force(expr)
  timing[[label]] <<- c(elapsed = proc.time()[["elapsed"]] - t0, cpu = cpu() - c0)
  cat(sprintf("  %-10s %6.1f s elapsed, %6.1f s CPU\n", label, timing[[label]][["elapsed"]], timing[[label]][["cpu"]]))
}

extracts_root <- file.path(iso, "beliefs", "extracts")
cat(sprintf(
  "replay %s %s, mode %s, horizon %d days, %d earlier extract and %d archive partition(s)\n",
  sex, as_of, mode, horizon, length(earlier), length(arc_parts)
))
cat(sprintf(
  "  production's fit saw %s matches; %d played later on the fit day returned to the schedule\n",
  if (length(production_n_obs) == 1L) format(production_n_obs) else "an unrecorded number of", nrow(returned)
))

fit_meta <- NULL
diagnostics <- NULL
if (mode == "inla") {
  league$engine <- "inla"
  timed("fit_league", fit_league(
    league = league, sex = sex, fit_date = as_of, end_date = as_of, root = iso,
    schedule_horizon_days = horizon, seed = as.integer(format(as_of, "%Y%m%d"))
  ))
  fit_meta <- readRDS(file.path(iso, "beliefs", "fits", cell, paste0("sex=", sex), "fit.rds"))$meta
  diagnostics <- as.data.frame(read_table("fit_diagnostics", root = iso))
  league$engine <- NULL
}

timed("publish", {
  extracted <- read_extracted_iceland(league, sex = sex, fit_date = as_of, extracts_root = extracts_root)
  publish_iceland_league(
    extracted = extracted, league = league, sex = sex, end_date = as_of, root = iso,
    output_root = file.path(out_dir, "publish"), extracts_root = extracts_root,
    archive_root = file.path(iso, "beliefs", "archive"),
    round_predictions_history_root = hist_root
  )
})

publish_root <- file.path(out_dir, "publish")
json <- list.files(publish_root, recursive = TRUE, pattern = "\\.json$")
check <- validate_publish_dir(publish_root)
part <- file.path(extracts_root, cell, paste0("sex=", sex), paste0("fit_date=", as_of))
summary <- list(
  sex = sex, as_of = format(as_of), mode = mode, horizon_days = horizon,
  earlier_partitions = sub("^fit_date=", "", earlier),
  archive_partitions = length(arc_parts),
  production_n_obs = if (length(production_n_obs) == 1L) production_n_obs else NA_integer_,
  returned_to_schedule = sprintf("%s v %s", returned$home_team, returned$away_team),
  timing = lapply(timing, as.list),
  fit = if (!is.null(fit_meta)) {
    list(
      engine = fit_meta$engine, version = fit_meta$version, int_strategy = fit_meta$int_strategy,
      inla_version = fit_meta$inla_version, n_obs = fit_meta$n_obs, n_teams = fit_meta$n_teams,
      n_states = fit_meta$n_states, seconds = as.list(fit_meta$seconds),
      gate = fit_meta$gate, hyper = fit_meta$hyper
    )
  },
  diagnostics = diagnostics,
  extract_files = list.files(part),
  json_files = length(json),
  schema = check[intersect(names(check), c("n_passed", "n_failed", "errors", "unmatched"))],
  r_version = R.version.string, platform = R.version$platform,
  cores = parallel::detectCores(),
  data_changed = setdiff(data_status(), dirty_before)
)
jsonlite::write_json(summary, file.path(out_dir, "summary.json"), auto_unbox = TRUE, pretty = TRUE, digits = 6, na = "null", dataframe = "rows")
cat(sprintf(
  "  %d JSON files, schema: %s passed, %s failed; summary at %s\n",
  length(json), format(check$n_passed), format(check$n_failed), file.path(out_dir, "summary.json")
))
if (mode == "inla") {
  stopifnot(isTRUE(fit_meta$gate$passed), identical(fit_meta$engine, "inla"), nrow(diagnostics) == 1L)
  # The replay is only a like-for-like comparison if it trained on what
  # production trained on.
  if (length(production_n_obs) == 1L) stopifnot(fit_meta$n_obs == production_n_obs)
}
if (mode == "inla" && isTRUE(check$n_failed > 0L)) stop("published JSON failed schema validation", call. = FALSE)
if (length(summary$data_changed)) stop("the replay changed files under data/", call. = FALSE)
cat("REPLAY DONE\n")

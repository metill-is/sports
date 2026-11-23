# Replay of the handball / basketball v2 switch path for one date, in a
# temporary data root:
#
#   Rscript docs/evidence/hb-bb-v2-switch/replay.R <handball|basketball> <YYYY-MM-DD>
#
# Nothing under data/ is written. The sport's facts, its extracts and archive
# from before the date, and its published cells are copied into a temporary
# root; fit_league() runs there with `engine: inla` for each sex, then
# publish_one() publishes from that fit and validates the JSON against the
# publish schemas (it aborts when a file fails). The run then validates the
# whole sport tree once more, compares the v2 forecasts with production's
# stored forecasts of the same fit date, and checks with git that data/ is
# exactly as it was. Writes <sport>-<date>.md and <sport>-<date>-fixtures.csv
# next to this script.
options(width = 150, warn = 1)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, args[1] %in% c("handball", "basketball"))
sport <- args[1]
end_date <- as.Date(args[2])
suppressMessages(devtools::load_all(quiet = TRUE))

repo <- here::here()
out_dir <- file.path(repo, "docs", "evidence", "hb-bb-v2-switch")
git_data <- function() {
  system2("git", c("-C", shQuote(repo), "status", "--porcelain", "--untracked-files=all", "--", "data"), stdout = TRUE)
}
data_before <- git_data()

src <- file.path(repo, "data")
root <- tempfile("hbbb-replay-")
dir.create(root)
sp <- paste0("sport=", sport)
before_date <- function(path) {
  d <- regmatches(path, regexpr("fit_date=[0-9]{4}-[0-9]{2}-[0-9]{2}", path))
  !length(d) || as.Date(sub("fit_date=", "", d)) < end_date
}
copy_tree <- function(rel, keep = function(path) TRUE) {
  from <- file.path(src, rel)
  if (!dir.exists(from)) {
    return(0L)
  }
  files <- list.files(from, recursive = TRUE)
  files <- files[vapply(files, keep, logical(1))]
  for (f in files) {
    dst <- file.path(root, rel, f)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(from, f), dst))
  }
  length(files)
}
copied <- c(
  results = copy_tree(file.path("facts", "results", sp)),
  schedules = copy_tree(file.path("facts", "schedules", sp)),
  extracts = copy_tree(file.path("beliefs", "extracts", sp), before_date),
  archive = copy_tree(file.path("beliefs", "archive", sp), before_date),
  publish = copy_tree(file.path("publish", sport))
)

key <- paste0(sport, "_iceland")
league <- load_leagues()[[key]]
league$engine <- "inla"
static <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source")]
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)

cells <- list()
for (sex in league$sexes) {
  t0 <- Sys.time()
  fit_league(league = league, sex = sex, fit_date = end_date, end_date = end_date, root = root)
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  saved <- readRDS(file.path(root, "beliefs", "fits", sp, "country=iceland", paste0("sex=", sex), "fit.rds"))
  t1 <- Sys.time()
  publish_one(static, league$betting, key, sex, root = root, end_date = end_date)
  pub_secs <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
  diag <- as.data.frame(read_table("fit_diagnostics", root = root, filter = list(sport = sport, country = "iceland", sex = sex)))

  part <- function(base) {
    file.path(base, "beliefs", "extracts", sp, "country=iceland", paste0("sex=", sex), paste0("fit_date=", format(end_date)), "predicted_matches.parquet")
  }
  keys <- c("match_date", "division", "home_team", "away_team")
  cols <- c("p_home_win", "p_draw", "p_away_win", "mean_home_goals", "mean_away_goals")
  v2 <- as.data.frame(arrow::read_parquet(part(root)))[, c(keys, cols)]
  prod_file <- part(src)
  both <- NULL
  if (file.exists(prod_file)) {
    prod <- as.data.frame(arrow::read_parquet(prod_file))[, c(keys, cols)]
    both <- merge(v2, prod, by = keys, suffixes = c("_v2", "_stan"))
    both$sex <- sex
  }
  cells[[sex]] <- list(meta = saved$meta, diag = diag, secs = secs, pub_secs = pub_secs, v2 = v2, both = both, prod_file = file.exists(prod_file))
}

validation <- validate_publish_dir(file.path(root, "publish", sport), sport = sport)
published <- list.files(file.path(root, "publish", sport, "iceland"), recursive = TRUE)
data_after <- git_data()
data_unchanged <- identical(data_before, data_after)

lines <- c(
  sprintf("# %s: v2 switch-path replay, fit date %s", sport, format(end_date)),
  "",
  sprintf(
    "Run %s UTC at commit %s with `Rscript docs/evidence/hb-bb-v2-switch/replay.R %s %s`.",
    format(Sys.time(), tz = "UTC", "%Y-%m-%d %H:%M"),
    system2("git", c("-C", shQuote(repo), "rev-parse", "--short", "HEAD"), stdout = TRUE), sport, format(end_date)
  ),
  sprintf(
    "R-INLA %s. Temporary root, copied from data/: %s files.", as.character(utils::packageVersion("INLA")),
    paste(sprintf("%s %d", names(copied), copied), collapse = ", ")
  ),
  "",
  "## Result",
  "",
  sprintf("- data/ unchanged by the run (git status before and after identical): **%s**.", data_unchanged),
  sprintf(
    "- `validate_publish_dir()` on the sport's published tree: ok = **%s**, %d files validated, %d failed, %d unmatched.",
    validation$ok, validation$n_files, validation$n_failed, length(validation$unmatched)
  ),
  sprintf("- Published cells: %s.", paste(sort(unique(dirname(published))), collapse = ", ")),
  "",
  "## Fits",
  "",
  "| Sex | Gate | Checks (run / skipped) | Mode status (margin, total) | Matches in likelihood / n_obs | Current season | Fit + extract (s) | Publish (s) | Diagnostics row: n_obs, engine, version, passed |",
  "|---|---|---|---|---|---|---|---|---|"
)
for (sex in names(cells)) {
  m <- cells[[sex]]$meta
  d <- cells[[sex]]$diag
  lines <- c(lines, sprintf(
    "| %s | %s | %d / %d | %s | %d / %d | %s | %.0f | %.0f | %d, %s, %s, %s |",
    sex, if (isTRUE(m$gate$passed)) "passed" else "FAILED", m$gate$evaluated, m$gate$skipped,
    paste(m$mode_status, collapse = ", "), m$n_trained, m$n_obs, m$current_season,
    cells[[sex]]$secs, cells[[sex]]$pub_secs, d$n_obs, d$engine, d$model_version, d$passed
  ))
}
lines <- c(lines, "", "The hyperparameter-jump check is skipped: the temporary root has no earlier INLA fit of the cell, and the run log says so.", "")
for (sex in names(cells)) {
  m <- cells[[sex]]$meta
  h <- m$hyper
  lines <- c(
    lines, sprintf("### %s: hyperparameters (posterior mean, 90 %% interval)", sex), "",
    "| Model | Parameter | Mean | 5 % | 95 % |", "|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s |", h$response, h$parameter, fmt(h$mean), fmt(h$q05), fmt(h$q95)), ""
  )
  if (!is.null(m$overtime)) {
    lines <- c(lines, sprintf(
      "Regulation-time coverage of season %s's training rows: %.1f %%. Overtime points per period: mean %.1f, sd %.1f, from %s (%d overtime games).",
      m$current_season, 100 * m$regulation_coverage, m$overtime$mean, m$overtime$sd, m$overtime$source, m$overtime$n
    ), "")
  }
  z <- m$zero_shift
  lines <- c(
    lines, "Level-score shift (logit):", "", "| Group | Shift | Level scores | Rows |", "|---|---|---|---|",
    sprintf("| %s | %s | %s | %s |", z$group, fmt(z$shift), z$ties, z$n), ""
  )
}

both <- do.call(rbind, lapply(cells, `[[`, "both"))
lines <- c(lines, "## Forecasts against production's stored forecasts of the same fit date", "")
if (is.null(both) || !nrow(both)) {
  lines <- c(lines, "No fixture in common with a production extract of this fit date.", "")
} else {
  utils::write.csv(both, file.path(out_dir, sprintf("%s-%s-fixtures.csv", sport, format(end_date))), row.names = FALSE)
  lines <- c(
    lines,
    sprintf("%d fixtures in common (production predicted from the morning's data; the replay reads today's results, so a fixture played on the fit date is training data here and drops out). Per sex: mean absolute difference in the home-win probability and in the expected margin, and the largest.", nrow(both)),
    "", "| Sex | Fixtures | Mean abs diff p(home) | Max | Mean abs diff expected margin | Max | Mean expected total v2 / Stan |", "|---|---|---|---|---|---|---|"
  )
  for (sex in unique(both$sex)) {
    b <- both[both$sex == sex, ]
    dp <- abs(b$p_home_win_v2 - b$p_home_win_stan)
    dm <- abs((b$mean_home_goals_v2 - b$mean_away_goals_v2) - (b$mean_home_goals_stan - b$mean_away_goals_stan))
    lines <- c(lines, sprintf(
      "| %s | %d | %s | %s | %s | %s | %s / %s |", sex, nrow(b), fmt(mean(dp)), fmt(max(dp)), fmt(mean(dm), 2), fmt(max(dm), 2),
      fmt(mean(b$mean_home_goals_v2 + b$mean_away_goals_v2), 1), fmt(mean(b$mean_home_goals_stan + b$mean_away_goals_stan), 1)
    ))
  }
  lines <- c(lines, "", sprintf(
    "Draw probability: v2 mean %s, Stan mean %s (handball's published draw is the level-score band; basketball's is 0).",
    fmt(mean(both$p_draw_v2)), fmt(mean(both$p_draw_stan))
  ), "")
}
writeLines(lines, file.path(out_dir, sprintf("%s-%s.md", sport, format(end_date))))
cat(lines, sep = "\n")
if (!data_unchanged) stop("data/ changed during the replay")
if (!isTRUE(validation$ok)) stop("published JSON failed validation")

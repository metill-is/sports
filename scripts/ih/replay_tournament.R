#!/usr/bin/env Rscript
# Replay a past championship through the live forecast path: for each day,
# fit on the matches up to and including it, simulate the rest of the
# championship from its own schedule, write the five files a live forecast
# writes, and check them against what is known on that day (R/ih-replay.R).
# A replay never publishes: it writes only under --out, which may not be
# inside data/publish.
#
# Needs the match table in IH_DATA_DIR. Run by .github/workflows/ih-tournament.yml
# (input `replay`) or by hand.
#
# Usage:
#   IH_DATA_DIR=... Rscript scripts/ih/replay_tournament.R --replay euro2024w --out DIR \
#     [--as-of-dates per-round|2024-11-27,2024-12-08] [--n-sims 5000] [--draws 2000] \
#     [--sims-matches 5000] [--seed 1]
# Writes DIR/<id>/<as_of>/ (five files per day), DIR/<id>/checks.csv and
# DIR/<id>/summary.md. Exit status 1 when a fit fails its checks or any check
# fails.
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else args[[i + 1L]]
}
id <- arg("replay")
out_root <- arg("out")
if (is.null(id) || !id %in% names(IH_REPLAYS)) cli::cli_abort("--replay must be one of {.val {names(IH_REPLAYS)}}.")
if (is.null(out_root)) cli::cli_abort("--out is required.")
publish_root <- normalizePath(here::here("data", "publish"), mustWork = FALSE)
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
if (startsWith(normalizePath(out_root), publish_root)) cli::cli_abort("A replay never writes under data/publish.")
n_sims <- as.integer(arg("n-sims", "5000"))
n_draws <- as.integer(arg("draws", "2000"))
sims_matches <- as.integer(arg("sims-matches", "5000"))
seed <- as.integer(arg("seed", "1"))

spec <- ih_tournament_spec(id)
facts <- ih_replay_facts(id)
matches <- ih_read_matches(spec$sex, forecast = spec)
all_results <- ih_tournament_results(matches, spec)
days <- ih_replay_as_of_dates(spec, all_results, arg("as-of-dates", "per-round"))
cli::cli_inform("{spec$title}: {nrow(all_results)} championship results; {length(days)} day{?s} to replay.")

rows <- list()
t_all <- Sys.time()
for (k in seq_along(days)) {
  d <- days[k]
  t0 <- Sys.time()
  dir <- file.path(out_root, id, format(d))
  ff <- ih_forecast_fit(matches, spec, d, n_draws = n_draws, seed = seed)
  problems <- ih_fit_check(ff$fit)
  if (length(problems)) {
    ck <- data.frame(check = "fit passes ih_fit_check()", ok = FALSE, detail = paste(problems, collapse = "; "))
  } else {
    sim <- ih_simulate_tournament(ff$fit, spec, ff$results, n_sims = n_sims, seed = seed, h2h_reapply = "none")
    ih_publish_tournament(ff$fit, sim, spec, ff$results, dir, n_sims_matches = sims_matches, seed = seed)
    ck <- rbind(
      data.frame(check = "fit passes ih_fit_check()", ok = TRUE, detail = ""),
      ih_replay_checks(dir, spec, ff$results, d, facts)
    )
  }
  secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  ck$as_of <- format(d)
  ck$results <- nrow(ff$results)
  ck$secs <- secs
  rows[[k]] <- ck
  cli::cli_inform("{format(d)}: {nrow(ff$results)} result{?s}, {sum(ck$ok)}/{nrow(ck)} checks passed, {secs} s.")
}
checks <- do.call(rbind, rows)
total <- round(as.numeric(difftime(Sys.time(), t_all, units = "mins")), 1)
utils::write.csv(checks, file.path(out_root, id, "checks.csv"), row.names = FALSE)

failed <- checks[!checks$ok, ]
lines <- c(
  sprintf("### Replay of %s", spec$title), "",
  sprintf(
    "%d days, %d checks each, %s minutes in all; %s.", length(days), length(unique(checks$check)), total,
    if (nrow(failed)) sprintf("**%d checks failed**", nrow(failed)) else "every check passed"
  ),
  "", "| Day | Results | Checks passed | Seconds |", "|---|---|---|---|",
  vapply(split(checks, checks$as_of), function(x) sprintf("| %s | %d | %d/%d | %d |", x$as_of[1], x$results[1], sum(x$ok), nrow(x), x$secs[1]), ""),
  if (nrow(failed)) c("", "Failed:", sprintf("- %s, %s: %s", failed$as_of, failed$check, failed$detail))
)
writeLines(lines, file.path(out_root, id, "summary.md"))
cat(lines, sep = "\n")
quit(status = if (nrow(failed)) 1L else 0L)

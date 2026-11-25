#!/usr/bin/env Rscript
# International handball: fit the nations model, simulate a championship and
# write its forecast for metill-platform.
#
# Needs the match table (handball_matches.parquet) in the folder named by
# IH_DATA_DIR. Local only: no workflow runs this.
#
# Usage:
#   IH_DATA_DIR=... Rscript scripts/ih/run_tournament.R --tournament euro2026w \
#     [--as-of 2026-12-03] [--n-sims 20000] [--draws 4000] [--seed 1] \
#     [--h2h none|once|repeat] [--out data/publish]
# --as-of: the fit trains on matches before this day and the simulation keeps
# the championship's results before it (default: today).
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else args[[i + 1L]]
}
id <- arg("tournament", "euro2026w")
as_of <- as.Date(arg("as-of", as.character(Sys.Date())))
n_sims <- as.integer(arg("n-sims", "20000"))
n_draws <- as.integer(arg("draws", "4000"))
seed <- as.integer(arg("seed", "1"))
h2h <- arg("h2h", "none")
out_root <- arg("out", here::here("data", "publish"))

spec <- ih_tournament_spec(id)
matches <- ih_read_matches(spec$sex)
results <- ih_tournament_results(matches[matches$match_date < as_of, , drop = FALSE], spec)
cli::cli_inform("{spec$title}: {nrow(matches)} matches read, last {max(matches$match_date)}; {nrow(results)} championship result{?s} before {as_of}.")

season <- ih_season(spec$start)
empty <- data.frame(match_date = as.Date(character()), home_team = character(), away_team = character(), venue = integer())
t0 <- Sys.time()
fit <- ih_fit(matches, empty, as_of, season = season, n_draws = n_draws, seed = seed)
cli::cli_inform("Fit: {fit$n_train} training matches ({fit$n_train_60} with a 60-minute score), mode status {paste(fit$mode_status, collapse = '/')}, {round(as.numeric(difftime(Sys.time(), t0, units = 'secs')))} s.")
problems <- ih_fit_check(fit)
if (length(problems)) cli::cli_abort(c("The fit failed its checks; nothing published.", stats::setNames(problems, rep("x", length(problems)))))
missing <- setdiff(unlist(spec$groups), fit$teams)
if (length(missing)) cli::cli_abort("No training match for {.val {missing}}.")

sim <- ih_simulate_tournament(fit, spec, results, n_sims = n_sims, seed = seed, h2h_reapply = h2h)
out_dir <- file.path(out_root, spec$publish[1], spec$publish[2])
paths <- ih_publish_tournament(fit, sim, spec, results, out_dir, h2h_reapply = h2h, seed = seed)
cli::cli_inform("Wrote {length(paths)} file{?s} to {.path {out_dir}}.")
top <- sim$teams[order(-sim$teams$p_Champion), c("team", "group", "p_advance", spec$rounds |> (\(r) paste0("p_", r))())]
print(utils::head(top, 12), digits = 3, row.names = FALSE)

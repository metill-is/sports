#!/usr/bin/env Rscript
# Leak-free refit of the 2026 production World Cup approach before a past
# tournament: the club bivariate-Poisson model, unchanged, on internationals in
# a window as long as the one production used (2022-01-01 to 2026-06-11), with
# production's training filter (a tournament participant in every match, both
# teams a participant or with >= 8 in-window matches).
#
# Usage: Rscript scripts/nt/prod_refit.R <key> <results.csv> <out_dir>
#   key: wc2022 | euro2024 | wc2026

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 120)

args <- commandArgs(trailingOnly = TRUE)
key <- args[[1]]
csv_path <- args[[2]]
out_dir <- args[[3]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

production_window_days <- as.integer(as.Date("2026-06-11") - as.Date("2022-01-01"))
min_team_matches <- 8L

results <- nt_read_results(csv_path)
spec <- nt_tournaments()
spec <- spec[spec$key == key, ]
tm <- nt_tournament_matches(results, key)
participants <- sort(unique(c(tm$home_team, tm$away_team)))
window_start <- spec$start - production_window_days

d <- results[
  results$match_date >= window_start & results$match_date < spec$start &
    !is.na(results$home_score) & !is.na(results$away_score), ,
  drop = FALSE
]
counts <- table(c(d$home_team, d$away_team))
eligible <- union(participants, names(counts)[counts >= min_team_matches])
d <- d[
  (d$home_team %in% participants | d$away_team %in% participants) &
    d$home_team %in% eligible & d$away_team %in% eligible, ,
  drop = FALSE
]
cat(sprintf(
  "%s: window %s to %s, %d matches, %d teams, %d participants\n",
  key, window_start, spec$start - 1, nrow(d),
  length(unique(c(d$home_team, d$away_team))), length(participants)
))

root <- file.path(out_dir, paste0("prod_root_", key))
if (dir.exists(root)) unlink(root, recursive = TRUE)
write_table(
  tibble::tibble(
    sport = "football", country = "world", sex = "male",
    season = as.integer(format(d$match_date, "%Y")),
    match_date = d$match_date,
    home_team = d$home_team, away_team = d$away_team,
    home_score = d$home_score, away_score = d$away_score,
    division = d$tournament, round = NA_integer_
  ),
  "results",
  root = root
)

prep <- prepare_data(
  list(sport = "football", country = "world"),
  sex = "male", end_date = spec$start - 1, root = root,
  schedule_horizon_days = 0L
)
sd <- prep$stan_data
cat(sprintf("stan_data: K=%d N=%d N_rounds=%d\n", sd$K, sd$N, sd$N_rounds))

t0 <- Sys.time()
fit <- fit_model(
  sd, here::here("Stan", "football_iceland", "bivariate_poisson_no_inflation.stan"),
  method = "sample", chains = 4L, parallel_chains = 4L,
  iter_warmup = 750L, iter_sampling = 750L,
  adapt_delta = 0.95, seed = 42L,
  check_diagnostics = FALSE, show_progress = FALSE
)
wall_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
metrics <- extract_stan_metrics(fit)
cat(sprintf("fit time: %.1f min\n", wall_min))
print(metrics[c("n_divergent", "max_rhat", "min_ess_bulk", "min_ess_tail", "min_ebfmi")])

saveRDS(
  list(
    key = key, sim_inputs = .extract_sim_inputs_pfi(fit, prep$teams),
    teams = prep$teams, n_train = nrow(d), window_start = window_start,
    wall_min = wall_min, metrics = metrics
  ),
  file.path(out_dir, paste0("prod_", key, ".rds"))
)
cat("saved", file.path(out_dir, paste0("prod_", key, ".rds")), "\n")

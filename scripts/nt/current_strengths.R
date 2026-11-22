#!/usr/bin/env Rscript
# Where the national-team model stands today: fit on everything in the results
# file and report the UEFA teams' strengths plus one team's match forecasts at
# home and away against each of them. Illustrates what a Euro 2028
# qualifying-odds product would start from.
#
# Usage:
#   Rscript scripts/nt/current_strengths.R <results.csv> <out.rds> [team=Iceland] [threads=4:1]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
team <- opt("team", "Iceland")
results <- nt_read_results(args[[1]])
played <- results[!is.na(results$home_score), ]
cutoff <- max(played$match_date) + 1
prep <- nt_prepare(results, cutoff)
fit <- nt_fit_inla(prep, threads = opt("threads", "4:1"))

uefa_rows <- grepl("^UEFA (Euro qualification|Nations League)$", prep$matches$tournament)
uefa <- sort(unique(c(prep$matches$home_team[uefa_rows], prep$matches$away_team[uefa_rows])))
strength <- tibble::tibble(
  team = uefa,
  mean = colMeans(fit$draws$theta[, uefa]),
  sd = apply(fit$draws$theta[, uefa], 2L, stats::sd)
)
strength <- strength[order(-strength$mean), ]
strength$rank <- seq_len(nrow(strength))

others <- setdiff(strength$team, team)
forecast <- function(home, away) {
  nt_outcome_probs(nt_scoreline_probs(nt_match_rates(fit, home, away, venue = 1)))
}
at_home <- forecast(rep(team, length(others)), others)
away <- forecast(others, rep(team, length(others)))
matchups <- tibble::tibble(
  opponent = others,
  opponent_rank = strength$rank[match(others, strength$team)],
  home_win = at_home$p_home, home_draw = at_home$p_draw, home_loss = at_home$p_away,
  away_win = away$p_away, away_draw = away$p_draw, away_loss = away$p_home
)

saveRDS(
  list(
    team = team, cutoff = cutoff, n_matches = nrow(prep$matches),
    strength = strength, matchups = matchups, hyper = fit$hyper, fixed = fit$fixed
  ),
  args[[2]]
)
cat(sprintf("%s: cutoff %s, %d UEFA teams, %s ranked %d\n", basename(args[[1]]), cutoff,
  nrow(strength), team, strength$rank[strength$team == team]))
print(as.data.frame(head(strength, 12)), digits = 3)

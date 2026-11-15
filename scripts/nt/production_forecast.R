#!/usr/bin/env Rscript
# Pre-tournament World Cup 2026 forecast of the production approach, from its
# leak-free refit (scripts/nt/prod_refit.R) and its own simulator, with every
# group match unplayed.
#
# Usage: Rscript scripts/nt/production_forecast.R <prod_wc2026.rds> <out.rds>

suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
prod <- readRDS(args[[1]])
s <- wc_structure()

fx <- do.call(rbind, lapply(names(s$groups), function(g) {
  pairs <- utils::combn(s$groups[[g]], 2L)
  home <- pairs[1, ]
  away <- pairs[2, ]
  flip <- away %in% s$hosts
  tibble::tibble(
    match_date = as.Date("2026-06-11"),
    home_team = ifelse(flip, away, home), away_team = ifelse(flip, home, away),
    home_score = NA_integer_, away_score = NA_integer_, played = FALSE, group = g
  )
}))
fx$venue <- ifelse(fx$home_team %in% s$hosts, "home", "neutral")

out <- simulate_world_cup(
  prod$sim_inputs$team, prod$sim_inputs$scalar, fx, s, pairing_seed = 2026L
)
champ <- out$placement_probs[out$placement_probs$round_name == "Champion", ]
champ <- champ[order(-champ$probability), ]
print(as.data.frame(head(champ, 12)), digits = 3)
saveRDS(
  list(placement = out$placement_probs, group = out$group_probs, n_draws = out$n_draws),
  args[[2]]
)

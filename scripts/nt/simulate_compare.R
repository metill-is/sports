#!/usr/bin/env Rscript
# How much do World Cup title and advancement probabilities move when knockout
# ties are played out (extra time at a reduced rate, then a 50/50 shootout)
# instead of read from a head-to-head matrix that splits 90-minute draws in
# proportion to the 90-minute win probabilities?
#
# All three treatments share one fit (the pre-tournament national-team fit):
#   matrix     production method: posterior-mean head-to-head matrix with
#              proportional draw split, propagated round by round assuming the
#              rounds are independent
#   replay     the same per-match rule, but simulated jointly per posterior draw
#   extra_time joint simulation with extra time and a coin-toss shootout
#
# Usage: Rscript scripts/nt/simulate_compare.R <fit.rds> <out.rds> [n_sims]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 170)

args <- commandArgs(trailingOnly = TRUE)
fit <- readRDS(args[[1]])
out_path <- args[[2]]
n_sims <- if (length(args) >= 3L) as.integer(args[[3]]) else 200000L

s <- wc_structure()
teams <- unlist(s$groups, use.names = FALSE)

sims <- list(
  extra_time = nt_simulate_world_cup(fit, s, n_sims, "extra_time", seed = 11L),
  replay = nt_simulate_world_cup(fit, s, n_sims, "replay", seed = 11L),
  et_quarter = nt_simulate_world_cup(fit, s, n_sims, "extra_time", et_factor = 0.25, seed = 11L),
  et_tilt = nt_simulate_world_cup(fit, s, n_sims, "extra_time", shootout_p = 0.5, seed = 12L)
)

w <- nt_head_to_head(fit, teams, method = "replay")
w[is.na(w)] <- 0
occupancy <- function(idx) {
  t(apply(idx, 2L, function(col) tabulate(col, nbins = length(teams)) / nrow(idx)))
}
fwd <- wc_forward_bracket(
  w, occupancy(sims$replay$r32$a), occupancy(sims$replay$r32$b), s$bracket, teams
)
placement <- fwd$placement
matrix_probs <- function(round) {
  p <- placement[placement$round_name == round, ]
  p$probability[match(teams, p$team)]
}

tab <- tibble::tibble(
  team = teams,
  group = unname(s$group_of[teams]),
  advance = sims$extra_time$teams$p_advance,
  champion_matrix = matrix_probs("Champion"),
  champion_replay = sims$replay$teams$p_champion,
  champion_extra_time = sims$extra_time$teams$p_champion,
  champion_et_quarter = sims$et_quarter$teams$p_champion,
  final_matrix = matrix_probs("Final"),
  final_replay = sims$replay$teams$p_final,
  final_extra_time = sims$extra_time$teams$p_final,
  sf_matrix = matrix_probs("SF"),
  sf_replay = sims$replay$teams$p_sf,
  sf_extra_time = sims$extra_time$teams$p_sf,
  qf_matrix = matrix_probs("QF"),
  qf_replay = sims$replay$teams$p_qf,
  qf_extra_time = sims$extra_time$teams$p_qf,
  r16_matrix = matrix_probs("R16"),
  r16_replay = sims$replay$teams$p_r16,
  r16_extra_time = sims$extra_time$teams$p_r16
)
tab <- tab[order(-tab$champion_extra_time), ]

gap <- seq(0, 2, by = 0.05)
curve <- tibble::tibble(
  log_rate_gap = gap,
  rate_a = 1.3 * exp(gap / 2), rate_b = 1.3 * exp(-gap / 2)
)
curve$p_win_90 <- .nt_poisson_compare(curve$rate_a, curve$rate_b)$win
curve$p_draw_90 <- .nt_poisson_compare(curve$rate_a, curve$rate_b)$draw
curve$replay <- nt_knockout_win_prob(curve$rate_a, curve$rate_b, "replay")
curve$extra_time <- nt_knockout_win_prob(curve$rate_a, curve$rate_b, "extra_time")
curve$et_quarter <- nt_knockout_win_prob(curve$rate_a, curve$rate_b, "extra_time", et_factor = 0.25)

mc_se <- sqrt(0.13 * 0.87 / n_sims)
saveRDS(
  list(
    table = tab, curve = curve, n_sims = n_sims, mc_se_top = mc_se,
    decided = lapply(sims, function(x) x$decided),
    noise_check = max(abs(sims$et_tilt$teams$p_champion - sims$extra_time$teams$p_champion))
  ),
  out_path
)

cat("== champion probability, top 16 (percent) ==\n")
show <- as.data.frame(tab[1:16, c(
  "team", "champion_matrix", "champion_replay", "champion_extra_time", "champion_et_quarter"
)])
show[-1] <- round(100 * show[-1], 2)
print(show)
cat(sprintf(
  "\nMonte Carlo se at 13%%: %.3f points; max |diff| between two seeds: %.3f points\n",
  100 * mc_se, 100 * max(abs(sims$et_tilt$teams$p_champion - sims$extra_time$teams$p_champion))
))
d <- function(a, b) 100 * (tab[[a]] - tab[[b]])
cat(sprintf(
  "extra_time - matrix, champion: max |change| %.2f points, top-5 mean %.2f, sum |change| %.2f\n",
  max(abs(d("champion_extra_time", "champion_matrix"))),
  mean(d("champion_extra_time", "champion_matrix")[1:5]),
  sum(abs(d("champion_extra_time", "champion_matrix")))
))
cat(sprintf(
  "replay - matrix (bracket-independence effect), champion: max |change| %.2f points\n",
  max(abs(d("champion_replay", "champion_matrix")))
))
cat(sprintf(
  "extra_time - replay (tie-rule effect), champion: max |change| %.2f points; R16: %.2f; QF: %.2f\n",
  max(abs(d("champion_extra_time", "champion_replay"))),
  max(abs(d("r16_extra_time", "r16_replay"))), max(abs(d("qf_extra_time", "qf_replay")))
))
print(round(unlist(sims$extra_time$decided), 3))

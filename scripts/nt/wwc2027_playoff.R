#!/usr/bin/env Rscript
# 2027 Women's World Cup, UEFA play-off round 2: two-legged tie forecasts from
# the women's v2 fit (scripts/nt/wwc2027_fit.R), with Iceland v Portugal in
# detail. Light work: reads the saved draws, no refit.
#
# Rules (UEFA regulations of the 2026 Women's European Qualifiers, read
# 2026-11-24): most goals over two legs; level, two 15-minute periods of extra
# time at the second leg, then penalties; no away goals (Arts 27.03, 28.01).
# The seven best-ranked winners on the overall league ranking qualify, the
# last goes to the inter-confederation play-off (Art. 27.08). Ties, hosts and
# ranks: uefa.com, 11 November 2026.
#
# Usage:
#   Rscript scripts/nt/wwc2027_playoff.R <fit.rds> <womens results.csv> <out.rds>

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
saved <- readRDS(args[[1]])
fit <- saved$fit
results <- nt_read_results(args[[2]])
out_path <- args[[3]]

# First-named team hosts the first leg (Tuesday 1 December); second legs on
# Saturday 5 December. WHY venue 0 for Ukraine: it has played its home matches
# abroad since 2022 (martj42 lists them as neutral), and no venue in Ukraine
# was announced.
ties <- tibble::tibble(
  team_a = c("Ukraine", "Finland", "Portugal", "Wales", "Switzerland", "Scotland", "Poland", "Slovenia"),
  team_b = c("England", "Italy", "Iceland", "Norway", "Austria", "Sweden", "Republic of Ireland", "Netherlands"),
  rank_a = c(16L, 21L, 18L, 20L, 17L, 19L, 14L, 13L),
  rank_b = c(5L, 8L, 11L, 6L, 12L, 10L, 9L, 7L),
  leg1_venue = c(0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L)
)
leg1 <- nt_match_rates(fit, ties$team_a, ties$team_b, venue = ties$leg1_venue)
leg2 <- nt_match_rates(fit, ties$team_b, ties$team_a, venue = 1L)
tie <- nt_two_leg_win_prob(leg1$home, leg1$away, leg2$away, leg2$home)
n_draws <- nrow(tie$a_wins)

summ <- function(x) {
  c(
    mean = mean(x), mc_se = stats::sd(x) / sqrt(length(x)),
    q05 = unname(stats::quantile(x, 0.05)), q95 = unname(stats::quantile(x, 0.95))
  )
}

all_ties <- tibble::tibble(
  first_leg_host = ties$team_a, second_leg_host = ties$team_b,
  p_first_host = colMeans(tie$a_wins), p_second_host = 1 - colMeans(tie$a_wins),
  p_extra_time = colMeans(tie$a_extra_time + tie$b_extra_time + tie$a_shootout + tie$b_shootout),
  p_penalties = colMeans(tie$a_shootout + tie$b_shootout)
)
places <- nt_playoff_places(tie$a_wins, ties, n_direct = 7L)

# Iceland v Portugal: Portugal is side A (hosts leg 1), Iceland side B.
j <- which(ties$team_a == "Portugal")
pick <- function(x) x[, j]
ice <- list(
  win = summ(1 - pick(tie$a_wins)),
  ice_regulation = summ(pick(tie$b_regulation)),
  ice_extra_time = summ(pick(tie$b_extra_time)),
  ice_penalties = summ(pick(tie$b_shootout)),
  por_regulation = summ(pick(tie$a_regulation)),
  por_extra_time = summ(pick(tie$a_extra_time)),
  por_penalties = summ(pick(tie$a_shootout)),
  goes_to_extra_time = summ(pick(tie$a_extra_time + tie$b_extra_time + tie$a_shootout + tie$b_shootout)),
  goes_to_penalties = summ(pick(tie$a_shootout + tie$b_shootout))
)
ice_tab <- tibble::tibble(
  quantity = names(ice),
  mean = vapply(ice, `[[`, 1, "mean"), mc_se = vapply(ice, `[[`, 1, "mc_se"),
  q05 = vapply(ice, `[[`, 1, "q05"), q95 = vapply(ice, `[[`, 1, "q95")
)

# Leg scorelines (home side first).
rate1 <- list(home = leg1$home[, j, drop = FALSE], away = leg1$away[, j, drop = FALSE])
rate2 <- list(home = leg2$home[, j, drop = FALSE], away = leg2$away[, j, drop = FALSE])
grid1 <- nt_scoreline_probs(rate1, max_goals = 12L)[1, , ]
grid2 <- nt_scoreline_probs(rate2, max_goals = 12L)[1, , ]
dimnames(grid1) <- dimnames(grid2) <- list(0:12, 0:12)
top_scores <- function(g, home, away) {
  idx <- arrayInd(order(g, decreasing = TRUE)[1:10], dim(g))
  tibble::tibble(
    score = sprintf("%s %d-%d %s", home, idx[, 1] - 1L, idx[, 2] - 1L, away),
    p = g[idx]
  )
}
legs <- list(
  leg1 = list(
    label = "1 Dec, Portugal (home) v Iceland",
    outcome = nt_outcome_probs(array(grid1, c(1, dim(grid1)))),
    top = top_scores(grid1, "Portugal", "Iceland"), grid = grid1
  ),
  leg2 = list(
    label = "5 Dec, Iceland (home) v Portugal",
    outcome = nt_outcome_probs(array(grid2, c(1, dim(grid2)))),
    top = top_scores(grid2, "Iceland", "Portugal"), grid = grid2
  )
)

# Aggregate margin over 180 minutes, Iceland's view: per draw Iceland's total
# is Poisson(b1 + b2) and Portugal's Poisson(a1 + a2).
lam_ice <- pick(leg1$away) + pick(leg2$home)
lam_por <- pick(leg1$home) + pick(leg2$away)
margins <- -4:4
agg <- vapply(margins, function(m) {
  mean(rowSums(outer(lam_por, 0:40, function(l, g) stats::dpois(g, l)) *
    outer(lam_ice, 0:40, function(l, g) stats::dpois(g + m, l))))
}, numeric(1))
aggregate <- tibble::tibble(iceland_margin = margins, p = agg)

# After the first leg: reweight the draws by the probability of the leg-1
# score (the posterior update from that result), then play the second leg
# with the margin carried in.
after_leg1 <- tidyr::expand_grid(portugal = 0:4, iceland = 0:4)
after_leg1$p_score <- grid1[cbind(after_leg1$portugal + 1L, after_leg1$iceland + 1L)]
after_leg1$p_iceland <- vapply(seq_len(nrow(after_leg1)), function(r) {
  x <- after_leg1$portugal[r]
  y <- after_leg1$iceland[r]
  w <- stats::dpois(x, pick(leg1$home)) * stats::dpois(y, pick(leg1$away))
  second <- nt_two_leg_win_prob(0, 0, pick(leg2$away), pick(leg2$home), lead = x - y)
  sum(w * (1 - second$a_wins)) / sum(w)
}, numeric(1))

# WHY: averaged over every first-leg score, the conditional chances must give
# back the unconditional one (exact up to the 0-12 truncation).
full <- tidyr::expand_grid(portugal = 0:12, iceland = 0:12)
total <- sum(vapply(seq_len(nrow(full)), function(r) {
  x <- full$portugal[r]
  y <- full$iceland[r]
  w <- stats::dpois(x, pick(leg1$home)) * stats::dpois(y, pick(leg1$away))
  second <- nt_two_leg_win_prob(0, 0, pick(leg2$away), pick(leg2$home), lead = x - y)
  sum(w * (1 - second$a_wins)) / n_draws
}, numeric(1)))
stopifnot(abs(total - mean(1 - pick(tie$a_wins))) < 1e-6)

# Sensitivity of P(Iceland) to the extra-time rate and a tilted shoot-out.
sens <- tibble::tibble(
  setting = c(
    "baseline (et 1/3, pens 50/50)", "extra time at 1/4", "extra time at 2/5",
    "Iceland wins pens 45%", "Iceland wins pens 55%"
  ),
  p_iceland = c(
    mean(1 - pick(tie$a_wins)),
    mean(1 - nt_two_leg_win_prob(pick(leg1$home), pick(leg1$away), pick(leg2$away), pick(leg2$home), et_factor = 0.25)$a_wins),
    mean(1 - nt_two_leg_win_prob(pick(leg1$home), pick(leg1$away), pick(leg2$away), pick(leg2$home), et_factor = 0.4)$a_wins),
    mean(1 - nt_two_leg_win_prob(pick(leg1$home), pick(leg1$away), pick(leg2$away), pick(leg2$home), shootout_p = 0.55)$a_wins),
    mean(1 - nt_two_leg_win_prob(pick(leg1$home), pick(leg1$away), pick(leg2$away), pick(leg2$home), shootout_p = 0.45)$a_wins)
  )
)

# Cross-check: the Elo baseline (ordered logit for 1X2, Poisson goal links for
# scorelines), legs independent. A level aggregate is split as v2 splits it.
elo <- nt_elo_baseline(results, saved$cutoff, c("Portugal", "Iceland"), c("Iceland", "Portugal"),
  venue = c(1L, 1L), max_goals = 12L
)
eg1 <- elo$grid[1, , ]
eg2 <- elo$grid[2, , ]
g <- 0:12
ice_tot <- stats::convolve(colSums(eg1), rev(rowSums(eg2)), type = "open")
por_tot <- stats::convolve(rowSums(eg1), rev(colSums(eg2)), type = "open")
ice_tot <- pmax(ice_tot, 0)
por_tot <- pmax(por_tot, 0)
gg <- seq_along(ice_tot) - 1L
elo_ahead <- sum(outer(ice_tot, por_tot) * outer(gg, gg, ">"))
elo_level <- sum(outer(ice_tot, por_tot) * outer(gg, gg, "=="))
v2_level <- mean(pick(tie$a_extra_time + tie$b_extra_time + tie$a_shootout + tie$b_shootout))
v2_ice_given_level <- mean(pick(tie$b_extra_time + tie$b_shootout)) / v2_level
elo_check <- list(
  legs = elo$probs, ratings = elo$ratings[c("Iceland", "Portugal")],
  ice_ahead_180 = elo_ahead, level_180 = elo_level,
  p_iceland = elo_ahead + elo_level * v2_ice_given_level
)

strength <- tibble::tibble(
  team = colnames(fit$draws$theta), mean = colMeans(fit$draws$theta),
  sd = apply(fit$draws$theta, 2L, stats::sd)
)
strength <- strength[order(-strength$mean), ]
strength$rank <- seq_len(nrow(strength))

res <- list(
  run = list(
    cutoff = saved$cutoff, last_match = saved$last_match, n_matches = saved$n_matches,
    n_corrected = saved$n_corrected, n_draws = n_draws, results_md5 = saved$md5,
    hyper = fit$hyper, fixed = fit$fixed, settings = fit$settings
  ),
  ties = all_ties, places = places, iceland = ice_tab, legs = legs,
  aggregate = aggregate, after_leg1 = after_leg1, sensitivity = sens,
  elo = elo_check, strength = head(strength, 40)
)
saveRDS(res, out_path)

cat(sprintf(
  "Fit to %s (last match %s), %d matches, %d draws\n\n",
  saved$cutoff, saved$last_match, saved$n_matches, n_draws
))
cat("Iceland v Portugal\n")
print(as.data.frame(ice_tab), digits = 3)
for (l in legs) {
  cat("\n", l$label, "\n")
  print(as.data.frame(l$outcome), digits = 3)
  print(as.data.frame(l$top), digits = 3)
}
cat("\nAggregate margin (Iceland)\n")
print(as.data.frame(aggregate), digits = 3)
cat("\nAfter leg 1 (Portugal-Iceland score -> P(Iceland))\n")
print(as.data.frame(after_leg1), digits = 3)
cat("\nSensitivity\n")
print(as.data.frame(sens), digits = 3)
cat("\nElo cross-check\n")
print(elo_check$legs, digits = 3)
cat(sprintf(
  "Elo: Iceland ahead after 180 %.3f, level %.3f, P(Iceland) %.3f\n",
  elo_ahead, elo_level, elo_check$p_iceland
))
cat("\nAll ties\n")
print(as.data.frame(all_ties), digits = 3)
cat("\nPlaces\n")
print(as.data.frame(places), digits = 3)
cat("\nStrengths (top 25)\n")
print(as.data.frame(head(strength, 25)), digits = 3)

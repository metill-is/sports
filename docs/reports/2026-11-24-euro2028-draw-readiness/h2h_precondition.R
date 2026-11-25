#!/usr/bin/env Rscript
# When can the two readings of Article 15 d) differ? Only when four or more
# teams finish level on points and the first re-application of a) to c) still
# leaves two of them level. This counts, per group and simulation, the
# largest set of teams level on points and whether the "repeat" and "once"
# rankings differ, with the qualifying simulator's own ranking code.
# Usage: ml run -- Rscript docs/reports/2026-11-24-euro2028-draw-readiness/h2h_precondition.R <fit.rds> [n=200000] [seed=15]
# Measured 2026-11-24: 137 s and 1.54 GB peak at the defaults; output in h2h_precondition.txt beside this file.
suppressMessages(devtools::load_all(quiet = TRUE))
args <- commandArgs(trailingOnly = TRUE)
fit <- readRDS(args[[1]])
n <- as.integer(if (length(args) >= 2) args[[2]] else 200000L)
set.seed(as.integer(if (length(args) >= 3) args[[3]] else 15L))
run <- function(label, uefa) {
  teams <- nt_uefa_to_martj42(uefa)
  k <- length(teams)
  pr <- expand.grid(h = teams, a = teams, stringsAsFactors = FALSE)
  pr <- pr[pr$h != pr$a, ]
  m <- nrow(pr)
  draw <- sample(length(fit$draws$mu), n, replace = TRUE)
  hi <- matrix(match(pr$h, fit$teams), n, m, byrow = TRUE)
  ai <- matrix(match(pr$a, fit$teams), n, m, byrow = TRUE)
  r <- .nt_sim_rates(fit, draw, hi, ai, venue = 1)
  gh <- matrix(stats::rpois(n * m, r$home), n, m)
  ga <- matrix(stats::rpois(n * m, r$away), n, m)
  x <- .nt_group_arrays(pr$h, pr$a, teams, gh, ga)
  tb <- matrix(stats::runif(n * k), n, k)
  differ <- rowSums(.nt_rank_group_arrays(x, tb, "repeat") != .nt_rank_group_arrays(x, tb, "once")) > 0
  pts <- apply(x$points, c(1, 2), sum)
  level <- apply(pts, 1, function(v) max(tabulate(match(v, unique(v)))))
  cat(sprintf(
    "%s: %s simulations; 3+ level on points %d (%.3f%%); 4+ level %d (%.3f%%); readings differ %d\n",
    label, format(n, big.mark = ","), sum(level >= 3), 100 * mean(level >= 3),
    sum(level >= 4), 100 * mean(level >= 4), sum(differ)
  ))
}
run("Rehearsal group H (Italy, Slovenia, Poland, Finland, Azerbaijan)", c("Italy", "Slovenia", "Poland", "Finland", "Azerbaijan"))
run("Rehearsal group B (Denmark, Austria, Iceland, Moldova)", c("Denmark", "Austria", "Iceland", "Moldova"))
run("Stress test, five evenly matched teams (Serbia, Slovenia, Hungary, Georgia, Romania)", c("Serbia", "Slovenia", "Hungary", "Georgia", "Romania"))

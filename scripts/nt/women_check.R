#!/usr/bin/env Rscript
# Does the national-team code run unchanged on the women's results file, and
# does it forecast sensibly? Fits before the 2023 Women's World Cup and the
# 2025 Women's Euro and scores their group stages (group matches have no extra
# time, so the file's scores are 90-minute scores) against the Elo baseline.
#
# Usage: Rscript scripts/nt/women_check.R <womens results.csv> <out.rds> [threads=4:1]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
results <- nt_read_results(args[[1]])
out_path <- args[[2]]
threads <- if (length(args) >= 3L) sub("^threads=", "", args[[3]]) else "4:1"

events <- tibble::tibble(
  label = c("Women's World Cup 2023", "Women's Euro 2025"),
  tournament = c("FIFA World Cup", "UEFA Euro"),
  start = as.Date(c("2023-07-20", "2025-07-02")),
  group_end = as.Date(c("2023-08-03", "2025-07-13"))
)

out <- list(n_rows = nrow(results), range = range(results$match_date))
for (i in seq_len(nrow(events))) {
  ev <- events[i, ]
  tm <- results[
    results$tournament == ev$tournament &
      results$match_date >= ev$start & results$match_date <= ev$group_end, ,
    drop = FALSE
  ]
  venue <- as.integer(!tm$neutral)
  prep <- nt_prepare(results, ev$start)
  fit <- nt_fit_inla(prep, threads = threads)
  v2 <- nt_evaluate_grid(
    nt_scoreline_probs(nt_match_rates(fit, tm$home_team, tm$away_team, venue), max_goals = 20L),
    tm$home_score, tm$away_score
  )
  elo <- nt_elo_baseline(results, ev$start, tm$home_team, tm$away_team, venue, max_goals = 20L)
  elo_sc <- nt_score_outcomes(
    elo$probs$p_home, elo$probs$p_draw, elo$probs$p_away, tm$home_score, tm$away_score
  )
  elo_sc$log_score <- nt_score_scorelines(elo$grid, tm$home_score, tm$away_score)
  top <- sort(colMeans(fit$draws$theta), decreasing = TRUE)
  out[[ev$label]] <- list(
    n_train = nrow(prep$matches), n_teams = length(prep$teams), n_test = nrow(tm),
    hyper = fit$hyper, fixed = fit$fixed, seconds = fit$seconds, top = head(top, 12),
    v2 = v2, elo = tibble::tibble(elo$probs, elo_sc),
    diff_log_1x2 = nt_paired_diff(v2$log_1x2, elo_sc$log_1x2),
    diff_rps = nt_paired_diff(v2$rps, elo_sc$rps),
    diff_scoreline = nt_paired_diff(v2$log_score, elo_sc$log_score),
    calibration = nt_edge_calibration(v2, tm$home_score, tm$away_score, breaks = c(0, 0.7, 1.75, Inf))
  )
  cat(sprintf(
    "%s: train %d matches, %d teams; %d group matches; log1x2 v2 %.4f vs Elo %.4f; rps %.4f vs %.4f\n",
    ev$label, nrow(prep$matches), length(prep$teams), nrow(tm),
    mean(v2$log_1x2), mean(elo_sc$log_1x2), mean(v2$rps), mean(elo_sc$rps)
  ))
  print(fit$hyper)
  print(round(head(top, 10), 2))
}
saveRDS(out, out_path)

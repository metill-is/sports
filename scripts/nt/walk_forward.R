#!/usr/bin/env Rscript
# Development panel for the national-team model: refit at half-yearly cutoffs
# and score every match between fitted teams in the following six months, so
# model variants can be compared on thousands of matches instead of the ~220 of
# the three target tournaments. Scores here are 1X2 only: outside the target
# tournaments the source file's knockout scores include extra time.
#
# Usage:
#   Rscript scripts/nt/walk_forward.R <results.csv> <out_dir> \
#     [variants=base,damp08,hl3,novenue] [threads=4:1] [first=2021-01-01] [last=2026-07-01]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
csv_path <- args[[1]]
out_dir <- args[[2]]
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
threads <- opt("threads", "4:1")
wanted <- strsplit(opt("variants", "base,damp08,hl3,novenue"), ",")[[1]]
cutoffs <- seq(as.Date(opt("first", "2021-01-01")), as.Date(opt("last", "2026-07-01")), by = "6 months")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

variants <- list(
  base = list(),
  hl3 = list(fit = list(rw_sd = nt_rw_sd_for_half_life(3))),
  ar1 = list(fit = list(dynamics = "ar1")),
  damp07 = list(fit = list(friendly = "damped", kappa = 0.7)),
  damp08 = list(fit = list(friendly = "damped", kappa = 0.8)),
  damp09 = list(fit = list(friendly = "damped", kappa = 0.9)),
  novenue = list(all_home = TRUE),
  w8 = list(prep = list(window_years = 8))
)[wanted]

results <- nt_read_results(csv_path)
results <- results[!is.na(results$home_score) & !is.na(results$away_score), ]
spec <- nt_tournaments()
in_target <- rep(FALSE, nrow(results))
for (i in seq_len(nrow(spec))) {
  in_target <- in_target | (results$tournament == spec$tournament[i] &
    results$match_date >= spec$start[i] & results$match_date <= spec$end[i])
}
results$in_target <- in_target

out_path <- file.path(out_dir, "walk_forward.rds")
done <- if (file.exists(out_path)) readRDS(out_path) else list()

for (ci in seq_along(cutoffs)) {
  cutoff <- cutoffs[ci]
  horizon_end <- seq(cutoff, by = "6 months", length.out = 2L)[2]
  ck <- format(cutoff)
  if (is.null(done[[ck]])) done[[ck]] <- list(scores = list(), hyper = list())
  base_prep <- nt_prepare(results, cutoff)
  test <- results[
    results$match_date >= cutoff & results$match_date < horizon_end &
      results$home_team %in% base_prep$teams & results$away_team %in% base_prep$teams, ,
    drop = FALSE
  ]
  if (nrow(test) == 0L) next
  venue <- as.integer(!test$neutral)
  friendly <- test$tournament == "Friendly"
  done[[ck]]$matches <- test

  if (is.null(done[[ck]]$scores$elo)) {
    elo <- nt_elo_baseline(results, cutoff, test$home_team, test$away_team, venue)
    done[[ck]]$scores$elo <- tibble::tibble(
      elo$probs,
      nt_score_outcomes(
        elo$probs$p_home, elo$probs$p_draw, elo$probs$p_away,
        test$home_score, test$away_score
      )
    )
  }
  for (v in names(variants)) {
    if (!is.null(done[[ck]]$scores[[v]])) next
    cfg <- variants[[v]]
    prep <- do.call(nt_prepare, c(list(results, cutoff), cfg$prep))
    venue_v <- venue
    if (isTRUE(cfg$all_home)) {
      prep$matches$venue <- 1L
      venue_v <- rep(1L, length(venue))
    }
    ok <- test$home_team %in% prep$teams & test$away_team %in% prep$teams
    fit <- do.call(nt_fit_inla, c(list(prep, threads = threads, n_draws = 500L), cfg$fit))
    rates <- nt_match_rates(
      fit, test$home_team[ok], test$away_team[ok], venue_v[ok], friendly[ok]
    )
    probs <- nt_outcome_probs(nt_scoreline_probs(rates))
    sc <- tibble::tibble(
      probs,
      nt_score_outcomes(
        probs$p_home, probs$p_draw, probs$p_away,
        test$home_score[ok], test$away_score[ok]
      )
    )
    full <- sc[rep(NA_integer_, nrow(test)), ]
    full[ok, ] <- sc
    done[[ck]]$scores[[v]] <- full
    done[[ck]]$hyper[[v]] <- fit$hyper
    saveRDS(done, out_path)
    cat(sprintf(
      "%s %-8s n=%d  competitive log1x2 %.4f  (elo %.4f)\n", ck, v, nrow(test),
      mean(full$log_1x2[!friendly], na.rm = TRUE),
      mean(done[[ck]]$scores$elo$log_1x2[!friendly])
    ))
  }
  saveRDS(done, out_path)
}
cat("walk-forward done\n")

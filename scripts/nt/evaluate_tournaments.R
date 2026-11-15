#!/usr/bin/env Rscript
# Leak-free tournament evaluation of the national-team model: fit every model
# on matches before the opening match, score all tournament matches on their
# 90-minute result (1X2 log score, RPS, exact-scoreline log score).
#
# Usage:
#   Rscript scripts/nt/evaluate_tournaments.R <results.csv> <out_dir> <key> \
#     [variants=comma,separated] [threads=2:1] [prod_dir=<dir with prod_<key>.rds>]
#   key: wc2022 | euro2024 | wc2026

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
csv_path <- args[[1]]
out_dir <- args[[2]]
key <- args[[3]]
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
threads <- opt("threads", "2:1")
prod_dir <- opt("prod_dir", file.path(out_dir, "prod"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

production_window_years <-
  as.numeric(as.Date("2026-06-11") - as.Date("2022-01-01")) / 365.25
variants <- list(
  base = list(),
  hl3 = list(fit = list(rw_sd = nt_rw_sd_for_half_life(3))),
  ar1 = list(fit = list(dynamics = "ar1")),
  damp07 = list(fit = list(friendly = "damped", kappa = 0.7)),
  damp08 = list(fit = list(friendly = "damped", kappa = 0.8)),
  damp09 = list(fit = list(friendly = "damped", kappa = 0.9)),
  disp = list(fit = list(friendly = "dispersion")),
  novenue = list(all_home = TRUE),
  w8 = list(prep = list(window_years = 8)),
  w4 = list(prep = list(window_years = production_window_years)),
  monthly = list(prep = list(step_months = 1L)),
  ccd = list(fit = list(int_strategy = "auto"))
)
wanted <- strsplit(opt("variants", paste(names(variants), collapse = ",")), ",")[[1]]
variants <- variants[wanted]

results <- nt_read_results(csv_path)
spec <- nt_tournaments()
spec <- spec[spec$key == key, ]
tm <- nt_tournament_matches(results, key)
venue <- as.integer(!tm$neutral)
cat(sprintf(
  "%s: %d matches (%d group), cutoff %s, %d at a home venue\n",
  key, nrow(tm), sum(tm$stage == "group"), spec$start, sum(venue)
))

out_path <- file.path(out_dir, paste0("eval_", key, ".rds"))
done <- if (file.exists(out_path)) readRDS(out_path) else list(scores = list(), fits = list())
save_progress <- function() {
  saveRDS(c(done, list(key = key, matches = tm)), out_path)
}

if (is.null(done$scores$elo)) {
  elo <- nt_elo_baseline(results, spec$start, tm$home_team, tm$away_team, venue)
  sc <- nt_score_outcomes(
    elo$probs$p_home, elo$probs$p_draw, elo$probs$p_away, tm$home_90, tm$away_90
  )
  done$scores$elo <- tibble::tibble(
    elo$probs, sc,
    log_score = nt_score_scorelines(elo$grid, tm$home_90, tm$away_90)
  )
  save_progress()
}

prod_path <- file.path(prod_dir, paste0("prod_", key, ".rds"))
if (is.null(done$scores$production) && file.exists(prod_path)) {
  prod <- readRDS(prod_path)
  rates <- nt_production_rates(prod$sim_inputs, tm$home_team, tm$away_team, venue)
  grid <- nt_bivariate_scoreline_probs(rates)
  done$scores$production <- nt_evaluate_grid(grid, tm$home_90, tm$away_90)
  done$fits$production <- prod[c("n_train", "window_start", "wall_min", "metrics")]
  save_progress()
}

for (v in names(variants)) {
  if (!is.null(done$scores[[v]])) next
  cfg <- variants[[v]]
  prep <- do.call(nt_prepare, c(list(results, spec$start), cfg$prep))
  # WHY: the ablation ignores the neutral flag everywhere. With every training
  # match coded as home vs away only mu + home terms are identified, so the
  # forecast must use the same coding.
  venue_v <- venue
  if (isTRUE(cfg$all_home)) {
    prep$matches$venue <- 1L
    venue_v <- rep(1L, length(venue))
  }
  t0 <- Sys.time()
  fit <- do.call(nt_fit_inla, c(list(prep, threads = threads), cfg$fit))
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  rates <- nt_match_rates(fit, tm$home_team, tm$away_team, venue_v)
  grid <- nt_scoreline_probs(rates)
  done$scores[[v]] <- nt_evaluate_grid(grid, tm$home_90, tm$away_90)
  done$fits[[v]] <- c(
    fit[c("hyper", "fixed", "mlik", "kappa", "seconds", "settings")],
    list(total_seconds = secs)
  )
  if (v %in% c("base", "damp08")) {
    saveRDS(fit, file.path(out_dir, paste0("fit_", key, "_", v, ".rds")))
  }
  save_progress()
  cat(sprintf(
    "%s %-8s %5.0fs  mlik %.1f  log1x2 %.4f  rps %.4f  scoreline %.4f\n",
    key, v, secs, fit$mlik, mean(done$scores[[v]]$log_1x2),
    mean(done$scores[[v]]$rps), mean(done$scores[[v]]$log_score)
  ))
}

cat("\n== mean scores,", key, "==\n")
print(do.call(rbind, lapply(names(done$scores), function(v) {
  s <- done$scores[[v]]
  data.frame(
    model = v, log_1x2 = mean(s$log_1x2), rps = mean(s$rps),
    scoreline = mean(s$log_score), group_log_1x2 = mean(s$log_1x2[tm$stage == "group"])
  )
})), digits = 4)

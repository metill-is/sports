#!/usr/bin/env Rscript
# Backtest of the international handball nations model (v2 on R-INLA) against
# the legacy model, on recent championships.
#
# Each model is fitted once, on the matches before a championship's first
# match, and predicts every match of the championship (pairings as played).
# Scored on the result after 60 minutes: 1X2 log score and ranked probability
# score, and the log score of the 60-minute margin and total.
#
# Needs IH_DATA_DIR (the match table) and IH_RESULTS_DIR (where the per-match
# predictions are written; they stay outside this repository).
#
# Usage: Rscript docs/reports/2026-11-24-intl-handball/backtest.R [--models v2,legacy,base]
#          [--targets w_euro2024,...] [--draws 4000]
options(width = 140)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("docs", "reports", "2026-11-24-intl-handball", "legacy.R"))

args <- commandArgs(trailingOnly = TRUE)
arg <- function(name, default = NULL) {
  i <- match(paste0("--", name), args)
  if (is.na(i)) default else args[[i + 1L]]
}
models <- strsplit(arg("models", "v2,base"), ",")[[1]]
n_draws <- as.integer(arg("draws", "4000"))
legacy_chains <- as.integer(arg("legacy-chains", "4"))
legacy_iter <- as.integer(arg("legacy-iter", "1000"))
out_dir <- Sys.getenv("IH_RESULTS_DIR")
if (!nzchar(out_dir)) stop("set IH_RESULTS_DIR")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

TARGETS <- data.frame(
  id = c("w_euro2022", "w_wc2023", "w_euro2024", "w_wc2025", "m_wc2023", "m_euro2024", "m_wc2025", "m_euro2026"),
  sex = rep(c("female", "male"), each = 4),
  competition = c("EURO", "WC", "EURO", "WC", "WC", "EURO", "WC", "EURO"),
  edition = c(2022L, 2023L, 2024L, 2025L, 2023L, 2024L, 2025L, 2026L),
  stringsAsFactors = FALSE
)
only <- arg("targets")
if (!is.null(only)) TARGETS <- TARGETS[TARGETS$id %in% strsplit(only, ",")[[1]], ]

hosts_all <- ih_hosts()
scores_of <- function(p_home, p_draw, p_away, outcome) {
  p_obs <- ifelse(outcome == "H", p_home, ifelse(outcome == "D", p_draw, p_away))
  o <- cbind(outcome == "H", outcome == "D", outcome == "A") * 1
  cp <- cbind(p_home, p_home + p_draw)
  co <- cbind(o[, 1], o[, 1] + o[, 2])
  list(log = -log(p_obs), rps = rowSums((cp - co)^2) / 2)
}

for (r in seq_len(nrow(TARGETS))) {
  tg <- TARGETS[r, ]
  matches <- ih_read_matches(tg$sex)
  champ <- matches[matches$competition == tg$competition & matches$edition == tg$edition &
    !grepl("qualif", matches$block, ignore.case = TRUE) & !is.na(matches$home_score), , drop = FALSE]
  as_of <- min(champ$match_date)
  champ <- champ[!is.na(champ$home_60), , drop = FALSE]
  hosts <- hosts_all$host[hosts_all$competition == tg$competition & hosts_all$sex == tg$sex & hosts_all$edition == tg$edition]
  champ$outcome <- ifelse(champ$home_60 > champ$away_60, "H", ifelse(champ$home_60 == champ$away_60, "D", "A"))
  champ$margin <- champ$home_60 - champ$away_60
  champ$total <- champ$home_60 + champ$away_60
  cat(sprintf("\n== %s: %d matches from %s, hosts %s\n", tg$id, nrow(champ), as_of, paste(hosts, collapse = "/")))
  key <- champ[, c("match_id", "match_date", "home_team", "away_team", "venue", "outcome", "margin", "total")]
  preds <- list()

  if ("v2" %in% models) {
    t0 <- Sys.time()
    fit <- ih_fit(matches, champ[, c("match_date", "home_team", "away_team", "venue")], as_of, n_draws = n_draws, seed = 1L)
    p <- v2_outcome_probs(fit$loc_m, fit$sigma_m)
    preds$v2 <- data.frame(
      key,
      model = "v2", p_home = p$p_home, p_draw = p$p_draw, p_away = p$p_away,
      p_margin = v2_integer_pmf(champ$margin, fit$loc_m, fit$sigma_m),
      p_total = v2_integer_pmf(champ$total, fit$loc_t, fit$sigma_t),
      mean_margin = colMeans(fit$loc_m), mean_total = colMeans(fit$loc_t)
    )
    cat(sprintf(
      "v2: %d training matches, mode %s, hfa %.2f, %.0f s\n", fit$n_train,
      paste(fit$mode_status, collapse = "/"), mean(fit$hfa), as.numeric(difftime(Sys.time(), t0, units = "secs"))
    ))
    saveRDS(
      list(hyper_sd = fit$hyper_sd, fixed = fit$fixed, mode_status = fit$mode_status, n_train = fit$n_train),
      file.path(out_dir, sprintf("%s_v2_fit.rds", tg$id))
    )
  }
  if ("base" %in% models) {
    tr <- matches[matches$match_date < as_of & matches$kind == "tournament" & !is.na(matches$home_60) &
      matches$venue == 0 & !matches$awarded & matches$match_date >= as_of - 4 * 365, ]
    pd <- mean(tr$home_60 == tr$away_60)
    preds$base <- data.frame(key,
      model = "base", p_home = (1 - pd) / 2, p_draw = pd, p_away = (1 - pd) / 2,
      p_margin = NA_real_, p_total = NA_real_, mean_margin = 0, mean_total = NA_real_
    )
  }
  if ("legacy" %in% models) {
    lg <- legacy_predict(matches, tg$sex, as_of, champ[, c("match_date", "home_team", "away_team")], hosts,
      exe_dir = file.path(out_dir, "legacy_stan"), chains = legacy_chains, iter = legacy_iter
    )
    d <- lg$home - lg$away
    tot <- lg$home + lg$away
    pmf <- function(x, k) {
      vapply(seq_along(k), function(j) {
        if (all(is.na(x[, j]))) {
          return(NA_real_)
        }
        (sum(abs(x[, j] - k[j]) < 0.5) + 0.5) / (nrow(x) + 1)
      }, numeric(1))
    }
    preds$legacy <- data.frame(
      key,
      model = "legacy",
      p_home = colMeans(d >= 0.5), p_draw = colMeans(abs(d) < 0.5), p_away = colMeans(d <= -0.5),
      p_margin = pmf(d, champ$margin), p_total = pmf(tot, champ$total),
      mean_margin = colMeans(d), mean_total = colMeans(tot)
    )
    cat(sprintf(
      "legacy: %d training matches, %d teams, %d rounds, %d divergences, max Rhat %.3f, %.0f s\n",
      lg$n_train, lg$n_teams, lg$n_rounds, lg$divergences, lg$rhat_max, lg$secs
    ))
    saveRDS(lg[setdiff(names(lg), c("home", "away"))], file.path(out_dir, sprintf("%s_legacy_fit.rds", tg$id)))
  }
  for (m in names(preds)) {
    p <- preds[[m]]
    s <- scores_of(p$p_home, p$p_draw, p$p_away, p$outcome)
    p$log_1x2 <- s$log
    p$rps <- s$rps
    p$log_margin <- -log(p$p_margin)
    p$log_total <- -log(p$p_total)
    p$target <- tg$id
    saveRDS(p, file.path(out_dir, sprintf("%s_%s_pred.rds", tg$id, m)))
    cat(sprintf(
      "%-7s log1x2 %.4f  rps %.4f  log margin %.4f  log total %.4f  (n=%d)\n", m,
      mean(p$log_1x2, na.rm = TRUE), mean(p$rps, na.rm = TRUE), mean(p$log_margin, na.rm = TRUE),
      mean(p$log_total, na.rm = TRUE), sum(!is.na(p$log_1x2))
    ))
  }
}

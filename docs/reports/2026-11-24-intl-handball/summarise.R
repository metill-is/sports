#!/usr/bin/env Rscript
# Summarise the backtest (backtest.R) into the tables the report quotes.
# Reads the per-match predictions from IH_RESULTS_DIR and writes aggregate
# tables only (no per-match rows) to this folder:
#   backtest_scores.csv     mean scores per championship and model, and pooled
#   backtest_paired.csv     v2 minus each other model per championship and pooled,
#                           mean paired difference with its standard error
#   backtest_calibration.csv  predicted against observed draws and home wins
options(width = 140)
dir <- Sys.getenv("IH_RESULTS_DIR")
if (!nzchar(dir)) stop("set IH_RESULTS_DIR")
here_dir <- here::here("docs", "reports", "2026-11-24-intl-handball")
files <- list.files(dir, pattern = "_pred\\.rds$", full.names = TRUE)
p <- do.call(rbind, lapply(files, function(f) {
  x <- readRDS(f)
  x[, c(
    "target", "model", "match_id", "outcome", "p_home", "p_draw", "p_away", "log_1x2", "rps",
    "log_margin", "log_total"
  )]
}))
order_t <- c("w_euro2022", "w_wc2023", "w_euro2024", "w_wc2025", "m_wc2023", "m_euro2024", "m_wc2025", "m_euro2026")
p$target <- factor(p$target, levels = intersect(order_t, unique(p$target)))
metrics <- c("log_1x2", "rps", "log_margin", "log_total")

# WHY: the legacy model cannot score a match with a nation it never trained on;
# n counts the matches a model scored, and the means are over those.
agg <- function(d) {
  ok <- !is.na(d$log_1x2)
  m <- vapply(metrics, function(k) if (all(is.na(d[[k]][ok]))) NA_real_ else mean(d[[k]][ok], na.rm = TRUE), numeric(1))
  data.frame(n = sum(ok), n_target = nrow(d), t(m))
}
by_t <- do.call(rbind, lapply(split(p, list(p$target, p$model), drop = TRUE), function(d) {
  cbind(target = as.character(d$target[1]), model = d$model[1], agg(d))
}))
pooled <- do.call(rbind, lapply(split(p, p$model), function(d) cbind(target = "pooled", model = d$model[1], agg(d))))
scores <- rbind(by_t, pooled)
scores <- scores[order(match(scores$target, c(order_t, "pooled")), scores$model), ]
rownames(scores) <- NULL
utils::write.csv(scores, file.path(here_dir, "backtest_scores.csv"), row.names = FALSE)
print(scores, digits = 4)

others <- setdiff(unique(p$model), "v2")
paired <- do.call(rbind, lapply(others, function(o) {
  a <- p[p$model == "v2", ]
  b <- p[p$model == o, ]
  j <- merge(a, b, by = c("target", "match_id"), suffixes = c("_v2", "_o"))
  groups <- c(split(j, j$target, drop = TRUE), list(pooled = j))
  do.call(rbind, lapply(names(groups), function(gname) {
    d <- groups[[gname]]
    do.call(rbind, lapply(metrics, function(m) {
      dd <- d[[paste0(m, "_v2")]] - d[[paste0(m, "_o")]]
      dd <- dd[is.finite(dd)]
      data.frame(
        target = gname,
        versus = o, metric = m, n = length(dd), v2_minus_other = mean(dd),
        se = stats::sd(dd) / sqrt(length(dd))
      )
    }))
  }))
}))
rownames(paired) <- NULL
utils::write.csv(paired, file.path(here_dir, "backtest_paired.csv"), row.names = FALSE)
print(paired[paired$target == "pooled", ], digits = 3)

cal <- do.call(rbind, lapply(split(p, p$model), function(d) {
  br <- cut(d$p_draw, c(0, 0.04, 0.06, 0.08, 0.10, 0.15, 1), include.lowest = TRUE)
  dr <- stats::aggregate(cbind(pred = d$p_draw, obs = d$outcome == "D") ~ br, data = data.frame(br, d), FUN = mean)
  dr$n <- as.vector(table(br)[as.character(dr$br)])
  dr$event <- "draw"
  fav <- pmax(d$p_home, d$p_away)
  fav_won <- ifelse(d$p_home >= d$p_away, d$outcome == "H", d$outcome == "A")
  bf <- cut(fav, c(0, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))
  fw <- stats::aggregate(cbind(pred = fav, obs = fav_won) ~ bf, data = data.frame(bf, fav, fav_won), FUN = mean)
  fw$n <- as.vector(table(bf)[as.character(fw$bf)])
  fw$event <- "favourite wins"
  names(dr)[1] <- names(fw)[1] <- "bin"
  cbind(model = d$model[1], rbind(dr, fw))
}))
utils::write.csv(cal, file.path(here_dir, "backtest_calibration.csv"), row.names = FALSE)
print(cal, digits = 3)

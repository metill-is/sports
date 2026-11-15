# Leak-free weekly walk-forward of the v2 model with R-INLA (plug-in hyperparameters).
# At each weekly cutoff: train on results dated <= cutoff with production's training
# filter (forfeits dropped), predict the played matches of the following 7 days, store
# the predictive parameters of the margin and of the total. Scoring happens afterwards
# (wf_score.R), so tie treatment and overtime can be changed without refitting.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/wf_inla.R")' <cell> <within> <first_season> [reg]
#   within: none | ar1 | rw1.  reg: train on regulation-time scores (basketball only).
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
args <- commandArgs(trailingOnly = TRUE)
cell <- args[[1]]
within <- args[[2]]
first_season <- as.integer(args[[3]])
use_reg <- length(args) >= 4 && args[[4]] == "reg"
ci <- CELLS[[cell]]
league <- load_leagues()[[ci$league]]
pr <- v2_priors(ci$sport)

res_all <- cell_results(cell)
played <- played_only(res_all, ci$sport)
if (use_reg) {
  per <- readRDS(file.path(OUT, "kki", "periods_matched.rds"))
  per <- per[per$sex == ci$sex & per$ok, c("match_date", "home_team", "away_team", "home_reg", "away_reg")]
  key <- function(x) paste(x$match_date, x$home_team, x$away_team)
  hit <- match(key(res_all), key(per))
  res_all$home_reg <- per$home_reg[hit]
  res_all$away_reg <- per$away_reg[hit]
  hit <- match(key(played), key(per))
  played$home_reg <- per$home_reg[hit]
  played$away_reg <- per$away_reg[hit]
}
starts <- season_starts(res_all)
n_weeks <- season_weeks(res_all)
seasons <- sort(unique(played$season))
seasons <- seasons[seasons >= first_season]
cutoffs <- do.call(c, lapply(seasons, function(s) {
  d <- played$match_date[played$season == s]
  seq(min(d) - 1, max(d), by = 7)
}))
max_cut <- as.integer(Sys.getenv("WF_MAX", "0"))
if (max_cut > 0L) cutoffs <- utils::tail(cutoffs, max_cut)

tag <- paste0(within, if (use_reg) "_reg" else "", if (max_cut > 0L) "_test" else "")
out_file <- file.path(OUT, "out", sprintf("wf_%s_%s.rds", cell, tag))
pred_rows <- list()
cut_rows <- list()
theta <- list(margin = NULL, total = NULL)
t_start <- Sys.time()
for (i in seq_along(cutoffs)) {
  cu <- cutoffs[i]
  train <- as.data.frame(suppressMessages(model_training_results(res_all, league, end_date = cu)))
  if (use_reg) {
    # WHY: a training row without period scores cannot say what the regulation score was.
    train <- train[!is.na(train$home_reg), , drop = FALSE]
    train$home_score <- train$home_reg
    train$away_score <- train$away_reg
  }
  teams <- sort(unique(c(train$home_team, train$away_team)))
  pred <- played[played$match_date > cu & played$match_date <= cu + 7 &
    played$home_team %in% teams & played$away_team %in% teams, , drop = FALSE]
  if (nrow(pred) == 0L || nrow(train) < 50L) next
  v <- build_v2_data(train, pred, teams,
    season_start = starts, n_weeks = n_weeks, division_map = DIVISION_MAP[[ci$sport]]
  )
  fits <- list()
  for (resp in c("margin", "total")) {
    th <- theta[[resp]]
    f <- tryCatch(
      fit_v2_inla(v, resp, pr, within = within, int_strategy = "eb", theta_start = th),
      error = function(e) NULL
    )
    if (is.null(f) && !is.null(th)) {
      f <- tryCatch(fit_v2_inla(v, resp, pr, within = within, int_strategy = "eb"), error = function(e) NULL)
    }
    if (is.null(f)) break
    theta[[resp]] <- f$theta
    fits[[resp]] <- f
  }
  if (length(fits) < 2L) {
    cat(sprintf("FAIL %s cutoff %s\n", cell, cu))
    next
  }
  keep <- c("match_date", "season", "division", "home_team", "away_team", "home_score", "away_score",
            if (use_reg) c("home_reg", "away_reg"))
  pred_rows[[length(pred_rows) + 1L]] <- cbind(
    pred[, keep], cutoff = cu, n_train = nrow(train),
    week = v$wk_pred,
    mean_m = fits$margin$pred$mean, sd_m = fits$margin$pred$sd, sig2_m = fits$margin$sigma2,
    mean_t = fits$total$pred$mean, sd_t = fits$total$pred$sd, sig2_t = fits$total$sigma2
  )
  hy <- function(f) stats::setNames(f$hyper_sd$q50, paste0(f$response, "|", f$hyper_sd$parameter))
  cut_rows[[length(cut_rows) + 1L]] <- data.frame(
    cutoff = cu, n_train = nrow(train), n_pred = nrow(pred),
    secs_m = fits$margin$secs, secs_t = fits$total$secs,
    hfa = fits$margin$fixed$mean[1], hfa_sd = fits$margin$fixed$sd[1],
    t(c(hy(fits$margin), hy(fits$total))), check.names = FALSE
  )
  if (i %% 10 == 0) {
    cat(sprintf("%s %s cutoff %s (%d/%d): n_train %d, pred %d, fit %.1f + %.1f s, elapsed %.0f min\n",
                cell, tag, cu, i, length(cutoffs), nrow(train), nrow(pred),
                fits$margin$secs, fits$total$secs, as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
    saveRDS(list(pred = do.call(rbind, pred_rows), cut = do.call(rbind, cut_rows)), out_file)
  }
}
saveRDS(list(pred = do.call(rbind, pred_rows), cut = do.call(rbind, cut_rows)), out_file)
cat(sprintf("WF DONE %s %s: %d cutoffs fitted, %d matches predicted, %.0f min\n", cell, tag,
            length(cut_rows), nrow(do.call(rbind, pred_rows)), as.numeric(difftime(Sys.time(), t_start, units = "mins"))))

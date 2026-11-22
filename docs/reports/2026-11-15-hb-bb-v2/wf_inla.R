# Leak-free weekly walk-forward of the v2 model with R-INLA (plug-in hyperparameters).
# At each weekly cutoff: train on results dated <= cutoff with production's training
# filter (forfeits dropped), predict the played matches of the following 7 days, store
# the predictive parameters of the margin and of the total. Scoring happens afterwards
# (wf_score.R), so tie treatment and overtime can be changed without refitting.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/wf_inla.R")' <cell> <within> <first_season> [reg]
#   within: none | ar1 | rw1.  reg: train on regulation-time scores (basketball only).
# A fit that throws, or whose mode search fails (status other than 0), after a warm start is
# repeated once from a cold start (v2_fit_with_retry()); `retried_*` records it. A fit that
# is still failed after that keeps its status, is not used as the next cutoff's starting
# value (v2_warm_start()), and its predictions are left out by wf_score.R.
# WF_REFIT_FAILED=1 applies that rule to a stored walk-forward instead of running one: the
# cutoffs whose stored status is not 0 are refitted cold and their rows replaced.
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
refit_failed <- identical(Sys.getenv("WF_REFIT_FAILED"), "1")
stored <- NULL
if (refit_failed) {
  stored <- readRDS(out_file)
  failed <- stored$cut$cutoff[!(stored$cut$status_m %in% 0L) | !(stored$cut$status_t %in% 0L)]
  cutoffs <- cutoffs[cutoffs %in% failed]
  cat(sprintf("%s %s: %d stored cutoffs with a failed mode search: %s\n", cell, tag, length(cutoffs), paste(format(cutoffs), collapse = ", ")))
}
pred_rows <- list()
cut_rows <- list()
# WHY: a cutoff whose fit throws on its warm and on its cold attempt writes no row, so
# without this list it would vanish from every count of fits and failures.
lost_rows <- list()
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
  retried <- c(margin = FALSE, total = FALSE)
  threw <- NA_character_
  for (resp in c("margin", "total")) {
    th <- if (refit_failed) NULL else theta[[resp]]
    r <- v2_fit_with_retry(
      function(start) fit_v2_inla(v, resp, pr, within = within, int_strategy = "eb", theta_start = start),
      theta_start = th
    )
    f <- r$fit
    if (is.null(f)) {
      threw <- resp
      break
    }
    retried[[resp]] <- r$retried
    theta[[resp]] <- v2_warm_start(f)
    fits[[resp]] <- f
  }
  if (length(fits) < 2L) {
    cat(sprintf("FAIL %s cutoff %s: the %s fit threw on every attempt\n", cell, cu, threw))
    lost_rows[[length(lost_rows) + 1L]] <- data.frame(cutoff = cu, n_pred = nrow(pred), threw = threw)
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
    status_m = fits$margin$mode_status, status_t = fits$total$mode_status,
    retried_m = retried[["margin"]], retried_t = retried[["total"]],
    hfa = fits$margin$fixed$mean[1], hfa_sd = fits$margin$fixed$sd[1],
    t(c(hy(fits$margin), hy(fits$total))), check.names = FALSE
  )
  if (i %% 10 == 0 && !refit_failed) {
    cat(sprintf("%s %s cutoff %s (%d/%d): n_train %d, pred %d, fit %.1f + %.1f s, elapsed %.0f min\n",
                cell, tag, cu, i, length(cutoffs), nrow(train), nrow(pred),
                fits$margin$secs, fits$total$secs, as.numeric(difftime(Sys.time(), t_start, units = "mins"))))
    saveRDS(list(pred = do.call(rbind, pred_rows), cut = do.call(rbind, cut_rows), lost = do.call(rbind, lost_rows)), out_file)
  }
}
if (refit_failed) {
  # Replace the rows of the refitted cutoffs; keep what the failed fits had said beside them.
  new_pred <- do.call(rbind, pred_rows)
  new_cut <- do.call(rbind, cut_rows)
  if (!is.null(new_cut)) {
    new_cut$retried_m <- new_cut$retried_t <- TRUE
    k <- function(x) paste(x$cutoff, x$match_date, x$home_team, x$away_team)
    hit <- match(k(new_pred), k(stored$pred))
    stopifnot(!anyNA(hit), nrow(new_pred) == sum(stored$pred$cutoff %in% new_cut$cutoff))
    # WHY per response: only the fit whose search failed is replaced. The other response's
    # warm-started fit succeeded and stays as the walk-forward produced it.
    ci <- match(format(new_cut$cutoff), format(stored$cut$cutoff))
    row_cut <- match(format(new_pred$cutoff), format(stored$cut$cutoff))
    if (is.null(stored$cut$retried_m)) stored$cut$retried_m <- stored$cut$retried_t <- FALSE
    if (is.null(stored$cut$status_m_first)) {
      stored$cut$status_m_first <- stored$cut$status_m
      stored$cut$status_t_first <- stored$cut$status_t
    }
    for (r in c("m", "t")) {
      failed_cut <- !(stored$cut[[paste0("status_", r, "_first")]][ci] %in% 0L)
      failed_row <- !(stored$cut[[paste0("status_", r, "_first")]][row_cut] %in% 0L)
      for (cn in paste0(c("mean_", "sd_", "sig2_"), r)) {
        if (is.null(stored$pred[[paste0(cn, "_failed_fit")]])) stored$pred[[paste0(cn, "_failed_fit")]] <- NA_real_
        stored$pred[[paste0(cn, "_failed_fit")]][hit[failed_row]] <- stored$pred[[cn]][hit[failed_row]]
        stored$pred[[cn]][hit[failed_row]] <- new_pred[[cn]][failed_row]
      }
      cols <- c(
        grep(if (r == "m") "^margin\\|" else "^total\\|", names(new_cut), value = TRUE), paste0(c("status_", "retried_", "secs_"), r),
        if (r == "m") c("hfa", "hfa_sd")
      )
      stored$cut[ci[failed_cut], cols] <- new_cut[failed_cut, cols]
      if (any(failed_row)) {
        cat(sprintf("%s model: %d cutoff(s) refitted cold, new status %s; largest change in a prediction's mean %.3f\n",
                    if (r == "m") "margin" else "total", sum(failed_cut), paste(new_cut[[paste0("status_", r)]][failed_cut], collapse = ","),
                    max(abs(new_pred[[paste0("mean_", r)]][failed_row] - stored$pred[[paste0("mean_", r, "_failed_fit")]][hit[failed_row]]))))
      }
    }
  }
  stored$lost <- rbind(stored$lost, do.call(rbind, lost_rows))
  saveRDS(stored, out_file)
  cut_rows <- split(stored$cut, seq_len(nrow(stored$cut)))
  pred_rows <- list(stored$pred)
} else {
  saveRDS(list(pred = do.call(rbind, pred_rows), cut = do.call(rbind, cut_rows), lost = do.call(rbind, lost_rows)), out_file)
}
cut_all <- do.call(rbind, cut_rows)
cat(sprintf("WF DONE %s %s: %d cutoffs fitted, %d matches predicted; fits whose mode search did not end with status 0: %d of %d; cutoffs lost to a fit that threw on every attempt: %d\n", cell, tag,
            length(cut_rows), nrow(do.call(rbind, pred_rows)),
            sum(!(cut_all$status_m %in% 0L)) + sum(!(cut_all$status_t %in% 0L)), 2L * nrow(cut_all), length(lost_rows)))

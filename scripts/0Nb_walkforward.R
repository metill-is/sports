#!/usr/bin/env Rscript
# scripts/0Nb_walkforward.R --
# Leak-free walk-forward OOS validator for football iceland. Scores OOS
# Brier/log-loss (primary, over all bettable candidates) + PnL (secondary, over
# placed bets). Read-only on the money path; NEVER on CI.
#
# Two modes:
#   --reuse   REUSE saved per-fit predictions (beliefs/extracts/predicted_matches).
#             No Stan -- a whole season runs in SECONDS, scoring the model's
#             actual published predictions. Cutoffs = the saved extract fit_dates
#             (each exactly leak-free). The recommended default.
#   (re-fit)  Without --reuse, re-fit the model as-of each cutoff -- a full Stan
#             fit per cutoff (~hours for a season). Run detached. Use when you
#             need cutoffs that differ from the saved fit_dates.
#
# Usage:
#   Rscript scripts/0Nb_walkforward.R --sex male --reuse                 # whole saved history
#   Rscript scripts/0Nb_walkforward.R --sex male --reuse --season 2026   # one season
#   Rscript scripts/0Nb_walkforward.R --sex male --as-of 2026-05-15      # re-fit, one cutoff
#   Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round  # re-fit sweep (detached):
#   #   nohup Rscript scripts/0Nb_walkforward.R --sex male --season 2026 --per-round > /tmp/wf.log 2>&1 & disown

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
options(width = 120)
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
get_flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i) == 0L) default else args[[i + 1L]]
}
has_flag <- function(name) paste0("--", name) %in% args

sex <- get_flag("sex")
season_str <- get_flag("season")
as_of_str <- get_flag("as-of")
per_round <- has_flag("per-round")
reuse <- has_flag("reuse")
model <- get_flag("model", "bvp")
if (!model %in% c("bvp", "sd")) stop("--model must be 'bvp' or 'sd'", call. = FALSE)
horizon_days <- as.integer(get_flag("horizon", "14"))

if (is.null(sex)) stop("--sex required (male or female)", call. = FALSE)

# Football iceland only (engine stays general; handball is a deliberate later
# scope flip per spec Component C / plan P2).
league_key <- "football_iceland"
league <- load_leagues()[[league_key]]
tie_threshold <- league$betting$scoring$tie_threshold %||% 0

results <- read_table("results",
  filter = list(sport = league$sport, country = league$country, sex = sex)
)
odds <- read_table("odds",
  filter = list(sport = league$sport, country = league$country)
)
ledger <- tryCatch(read_table("ledger"), error = function(e) NULL)

# --reuse: cutoffs are the saved extract fit_dates; the decide step reconstructs
# beliefs from beliefs/extracts/.../predicted_matches.parquet -- no Stan re-fit
# (seconds, not hours), scoring the model's ACTUAL published predictions. Each
# fit_date is exactly leak-free (end_date == fit_date). Otherwise, each cutoff is
# a round-completion date and the model is re-fit as-of it (G1).
decide_fn <- wf_select_decide_fn(model, reuse = reuse, source_root = here::here("data"))

if (isTRUE(reuse)) {
  ext_dir <- here::here(
    "data", "beliefs", "extracts", "sport=football",
    "country=iceland", paste0("sex=", sex)
  )
  if (!dir.exists(ext_dir)) stop("--reuse: no extracts at ", ext_dir, call. = FALSE)
  fds <- sort(as.Date(sub("fit_date=", "", list.files(ext_dir))))
  if (!is.null(season_str)) fds <- fds[format(fds, "%Y") == season_str]
  if (length(fds) == 0L) stop("--reuse: no saved extract fit_dates", call. = FALSE)
  cutoffs <- fds
} else {
  cutoffs <- if (isTRUE(per_round)) {
    if (is.null(season_str)) stop("--per-round requires --season YYYY", call. = FALSE)
    season <- as.integer(season_str)
    dates <- as.Date(character())
    for (n in seq_len(50L)) {
      cd <- suppressWarnings(suppressMessages(
        compute_round_cutoff_date(results, season = season, round_cutoff = n, quiet = TRUE)
      ))
      if (is.null(cd)) break
      dates <- c(dates, cd)
    }
    if (length(dates) == 0L) stop("No completed rounds for season ", season, call. = FALSE)
    dates
  } else {
    if (is.null(as_of_str)) stop("--as-of YYYY-MM-DD required (or --per-round --season YYYY, or --reuse)", call. = FALSE)
    d <- as.Date(as_of_str)
    if (is.na(d)) stop("--as-of: could not parse '", as_of_str, "'", call. = FALSE)
    d
  }
}

mode <- if (isTRUE(reuse)) "REUSE saved fits" else "RE-FIT per cutoff"
cli::cli_h1("Walk-forward {league_key}/{sex} [{model} | {mode}]: {length(cutoffs)} cutoff(s), horizon={horizon_days}d")
wf <- bt_walkforward(
  sex = sex, cutoffs = cutoffs, horizon_days = horizon_days,
  results = results, odds = odds, ledger = ledger,
  decide_fn = decide_fn, tie_threshold = tie_threshold, league = league
)

out_dir <- here::here("data", "backtest", "walkforward")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(wf$bets, file.path(out_dir, paste0("bets_", model, "_", sex, ".parquet")))
arrow::write_parquet(wf$scores, file.path(out_dir, paste0("scores_", model, "_", sex, ".parquet")))
print(wf$scores)
print(wf$pnl)
cli::cli_alert_success("Walk-forward complete: {nrow(wf$bets)} OOS bets scored")

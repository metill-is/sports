#!/usr/bin/env Rscript
# Walk-forward replay to recover the FULL predicted goal distributions
# (home / away / total) for played group games whose pre-match snapshot predates
# the marginal-distribution contract — i.e. games for which only the goal-diff
# distribution was ever captured/backfillable (those played 11-13 June 2026).
#
# WHY a re-fit (not reconstruction): the published distributions are Bayesian
# posterior-predictive over the bivariate-Poisson model (covariance λ3 + posterior
# overdispersion), which the stored point `eg_home`/`eg_away` cannot reproduce.
# The only faithful source is the model itself. So for each missing match-date M
# we re-fit AS OF the day before (end_date = M-1, no result leakage), simulate the
# tournament treating every fixture on/after M as still-upcoming, and snapshot
# ONLY the date-M games — exactly the pre-match forecast the daily pipeline would
# have produced that morning, now carrying all four marginal distributions.
#
# This does NOT call publish_world_cup() (that would overwrite the LIVE
# groups/predictions/meta/bracket with stale as-of-M-1 versions). It touches only
# the accountability snapshot log + results.json.
#
# Usage:
#   Rscript scripts/wc/backfill-distributions.R                 # auto-discover gaps
#   Rscript scripts/wc/backfill-distributions.R 2026-06-13      # explicit date(s)
#   WC_BACKFILL_WARMUP=100 WC_BACKFILL_SAMPLING=100 Rscript ... # fast plumbing smoke test
#
# Each date is one Stan fit (~minutes). Re-runnable + idempotent: a date already
# carrying full distributions is skipped unless named explicitly.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(dplyr))
options(width = 120)

root <- here::here("data")
league <- list(sport = "football", country = "world")
stan_path <- here::here(
  "Stan", "football_iceland", "bivariate_poisson_no_inflation.stan"
)
warmup <- as.integer(Sys.getenv("WC_BACKFILL_WARMUP", "750"))
sampling <- as.integer(Sys.getenv("WC_BACKFILL_SAMPLING", "750"))

s <- wc_structure()
is_name <- .wc_country_namer(
  here::here("data", "wc", "structure", "country_names_is.csv")
)
log_path <- file.path(root, "wc", "accountability", "prediction_log.json")

# ---- which played group games still lack home/away/total distributions? --------
discover_targets <- function() {
  fx <- wc_group_fixtures(s, root = root)
  played <- fx[fx$played %in% TRUE & !is.na(fx$home_score), , drop = FALSE]
  have_full <- character(0)
  if (file.exists(log_path)) {
    lg <- jsonlite::read_json(log_path, simplifyVector = FALSE)
    for (m in (if (is.null(lg$matches)) list() else lg$matches)) {
      if (!is.null(m$dist_home) && !is.null(m$dist_total)) {
        have_full <- c(have_full, paste(m$home, m$away, m$match_date, sep = "|"))
      }
    }
  }
  keys <- paste(played$home_team, played$away_team, as.character(played$match_date), sep = "|")
  miss <- played[!(keys %in% have_full), , drop = FALSE]
  sort(unique(as.character(miss$match_date)))
}

args <- commandArgs(trailingOnly = TRUE)
target_dates <- if (length(args)) args else discover_targets()

if (!length(target_dates)) {
  cat("No played games are missing home/away/total distributions. Nothing to do.\n")
  quit(status = 0L)
}
cat(sprintf(
  "Target match-dates to backfill (%d): %s\n",
  length(target_dates), paste(target_dates, collapse = ", ")
))
cat(sprintf("Fit budget: %d warmup + %d sampling x 4 chains per date.\n\n", warmup, sampling))

# ---- one walk-forward as-of fit ------------------------------------------------
fit_as_of <- function(asof) {
  prep <- prepare_data(league,
    sex = "male", end_date = asof, schedule_horizon_days = 0L
  )
  fit <- fit_model(
    prep$stan_data, stan_path,
    method = "sample",
    chains = 4L, parallel_chains = 4L,
    iter_warmup = warmup, iter_sampling = sampling,
    adapt_delta = 0.95, seed = 42L,
    check_diagnostics = FALSE, show_progress = FALSE
  )
  .extract_sim_inputs_pfi(fit, prep$teams)
}

for (md in target_dates) {
  M <- as.Date(md)
  asof <- M - 1L
  cat(sprintf("=== %s  (fit as of %s) ===\n", md, asof))

  si <- fit_as_of(asof)

  # Current fixtures, then force every game on/after M back to "upcoming" so the
  # simulator emits full predictive distributions for them (the strengths are
  # already frozen at end_date = M-1, so this is leakage-free).
  fx <- wc_group_fixtures(s, root = root)
  future <- as.Date(fx$match_date) >= M
  fx$played[future] <- FALSE
  fx$home_score[future] <- NA_integer_
  fx$away_score[future] <- NA_integer_

  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 2026L)

  # Snapshot ONLY the date-M games (don't overwrite later dates' fresher fits).
  preds <- out$predictions[as.character(out$predictions$match_date) == md, , drop = FALSE]
  if (!nrow(preds)) {
    cat(sprintf("  ! no upcoming predictions for %s — skipped\n\n", md))
    next
  }
  # fit_date = M matches the live "morning-of" convention (data is through M-1, so
  # no leakage); fit_date <= match_date keeps the snapshot's pre-match guard happy.
  wc_snapshot_predictions(preds, fit_date = md, root = root)
  cat(sprintf(
    "  snapshotted %d game(s) with full home/away/total/diff distributions\n\n",
    nrow(preds)
  ))
}

# ---- rebuild results.json with the REAL (current) fixtures for scoring ---------
res <- wc_build_results(wc_group_fixtures(s, root = root), root = root, is_name = is_name)
out_path <- here::here("data", "publish", "world_cup", "karla", "results.json")
jsonlite::write_json(
  c(list(generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")), res),
  out_path,
  auto_unbox = TRUE, pretty = TRUE
)

full <- 0L
for (m in res$matches) if (!is.null(m$pred_dist$home) && !is.null(m$pred_dist$total)) full <- full + 1L
cat(sprintf(
  "results.json rebuilt: %d played, %d now carry full home/away/total distributions.\n",
  length(res$matches), full
))

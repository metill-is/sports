#!/usr/bin/env Rscript
# Backfill final_positions_history.json for all football league divisions.
#
# For each (sex, completed BD round R): fit the model on games <= round-R
# cutoff, run the full-season simulator per league division, and assemble a
# corrected per-round placement history. Writes data/publish/.../<sex>-<div>/
# final_positions_history.json (full replace).
#
# Usage:
#   Rscript scripts/backfill_final_positions_history.R [--sex male|female|both] [--dry-run]
# Env knobs (for a fast smoke run): SPORTS_FIT_ITER_WARMUP=250 SPORTS_FIT_ITER_SAMPLING=250
suppressMessages(devtools::load_all(quiet = TRUE))
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)
sex_arg <- {
  i <- which(args == "--sex")
  if (length(i)) args[i + 1L] else "both"
}
dry_run <- "--dry-run" %in% args
# --from-round N: recompute only rounds >= N and MERGE into the existing
# history (preserve every round we don't recompute), instead of a full replace.
# Used to correct just the late rounds after a fixture-logic fix without paying
# the full-season re-fit cost. Default 1 == original full-replace behaviour.
from_round <- {
  i <- which(args == "--from-round")
  if (length(i)) as.integer(args[i + 1L]) else 1L
}
sexes <- if (sex_arg == "both") c("male", "female") else sex_arg

root <- here::here("data")
league <- load_leagues()[["football_iceland"]]
stan_path <- here::here("Stan", league$stan_model)
GENERATED_AT <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S+0000", tz = "UTC")

SEX_SLUG <- c(male = "karla", female = "kvenna")
DIV_SLUG <- c(BD = "bd", LD1 = "ld", LD2 = "2deild", LD3 = "3deild")

for (sex in sexes) {
  league_divs <- setdiff(.football_iceland_division_codes(sex), "CUP")
  results_all <- read_table("results",
    root = root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )
  schedule_all <- read_table("schedules",
    root = root,
    filter = list(sport = "football", country = "iceland", sex = sex)
  )
  season <- max(results_all$season, na.rm = TRUE)
  schedule_season <- schedule_all[
    !is.na(schedule_all$match_date) & schedule_all$season == season, ,
    drop = FALSE
  ]

  rounds <- completed_bd_rounds(results_all, season)
  cli::cli_alert_info(
    "{sex}: {nrow(rounds)} completed BD round(s); divisions {toString(league_divs)}"
  )

  all_recs <- list()
  for (i in seq_len(nrow(rounds))) {
    R <- rounds$round[i]
    if (R < from_round) next
    cutoff <- rounds$cutoff_date[i]
    cli::cli_alert_info("  fitting {sex} round {R} (cutoff {format(cutoff)}) ...")
    prep <- prepare_data(league, sex,
      end_date = cutoff, root = root,
      schedule_horizon_days = 200L
    )
    warmup <- .env_iter("SPORTS_FIT_ITER_WARMUP")
    sampling <- .env_iter("SPORTS_FIT_ITER_SAMPLING")
    # fit_model() enforces a hard R-hat <= 1.05 convergence gate (it stop()s on
    # an unconverged posterior). Sparse early rounds weakly identify the RW
    # variance params and may trip it, so escalate adapt_delta + warmup once
    # before giving up on a round (the whole-season backfill must not halt on a
    # single straggler). Model note: 0.95 -> 0.99 is the documented escalation.
    fit <- tryCatch(
      fit_model(
        stan_data = prep$stan_data, stan_model_path = stan_path,
        method = "sample", chains = 4L,
        iter_warmup = warmup, iter_sampling = sampling,
        adapt_delta = 0.95, seed = 1000L + R
      ),
      error = function(e) {
        cli::cli_alert_warning(
          "  round {R}: fit did not pass diagnostics at adapt_delta=0.95 ({conditionMessage(e)}); retrying at 0.99 with more warmup ..."
        )
        tryCatch(
          fit_model(
            stan_data = prep$stan_data, stan_model_path = stan_path,
            method = "sample", chains = 4L,
            iter_warmup = max(2000L, warmup), iter_sampling = max(1000L, sampling),
            adapt_delta = 0.99, max_treedepth = 12L, seed = 1000L + R
          ),
          error = function(e2) {
            cli::cli_alert_danger(
              "  round {R}: fit failed again ({conditionMessage(e2)}); SKIPPING this round."
            )
            NULL
          }
        )
      }
    )
    if (is.null(fit)) next
    recs <- build_round_final_positions(
      fit, prep, results_all, schedule_season, R, cutoff, season,
      league_divs, GENERATED_AT,
      split_configs = .football_iceland_division_split(sex)
    )
    all_recs[[length(all_recs) + 1L]] <- recs
    rm(fit)
    gc()
  }
  history <- dplyr::bind_rows(all_recs)
  if (nrow(history) == 0L) {
    cli::cli_alert_info(
      "{sex}: no completed round >= {from_round} to recompute; leaving history unchanged."
    )
    next
  }

  for (div in league_divs) {
    new_hist <- history[history$division == div, , drop = FALSE] |>
      dplyr::select(-"division")
    if (nrow(new_hist) == 0L) next
    out_dir <- file.path(
      root, "publish", "football", "iceland",
      paste0(SEX_SLUG[[sex]], "-", DIV_SLUG[[div]])
    )
    path <- file.path(out_dir, "final_positions_history.json")

    # Targeted backfill (--from-round N): keep the existing rows for every round
    # we did NOT recompute (rounds < N, plus any late round whose fit was
    # skipped), and splice in the freshly-computed rows. A skipped round keeps
    # its prior value rather than vanishing. Default (from_round == 1) is a full
    # replace -- `new_hist` already covers every round, so the merge is a no-op.
    div_hist <- new_hist
    if (from_round > 1L && file.exists(path)) {
      existing <- tryCatch(jsonlite::fromJSON(path)$records, error = function(e) NULL)
      if (!is.null(existing) && nrow(existing) > 0L) {
        recomputed <- unique(new_hist$round)
        kept <- existing[!(existing$round %in% recomputed), , drop = FALSE]
        div_hist <- dplyr::bind_rows(kept, new_hist)
      }
    }
    div_hist <- div_hist |> dplyr::arrange(.data$as_of, .data$team, .data$placement)

    n_rounds <- dplyr::n_distinct(div_hist$as_of)
    cli::cli_alert_success(
      "  {sex}/{div}: {nrow(div_hist)} rows over {n_rounds} round(s) -> {path}"
    )
    if (!dry_run) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      write_json_consistent(
        list(schema_version = 1L, records = div_hist), path,
        auto_unbox = TRUE, dataframe = "rows", digits = 5, na = "null"
      )
    }
  }
}
cli::cli_alert_success(
  "Backfill complete{if (dry_run) ' (dry run -- no files written)' else ''}."
)

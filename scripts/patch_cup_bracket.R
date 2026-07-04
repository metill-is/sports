# patch_cup_bracket.R
#
# Deterministic full rebuild of the cup `bracket.json` payload inside the
# latest fit's `cup_bracket.parquet`, for both sexes. No Stan re-fit: the
# payload is recomputed from persisted inputs only —
#
#   * bracket_state  <- results + schedules (pure bookkeeping, no RNG)
#   * matchup W      <- sim_inputs_{team,scalar}.parquet in the SAME
#                       fit_date partition (a mean over stored posterior
#                       draws — deterministic, byte-stable)
#   * completed[] / played[] <- results (bookkeeping)
#
# Supersedes scripts/patch_cup_completed.R (2026-06-27 incident), which only
# injected `completed[]`. This rebuild also carries the 2026-07-04 fix: a
# DECIDED frontier match (the Mjolkurbikar SF1, Breidablik 3-0 Vikingur R.,
# played 28 Jun while SF2 waited until 21 Jul) must pin its matchup cells to
# 1/0, appear in the WC-contract `played[]`, and appear in `completed[]` —
# the round-granular builder dropped it from all three.
#
# WHY THIS PATCHES THE PARQUET *AND* RE-DERIVES THE JSON
# ------------------------------------------------------
# cup_bracket.parquet is the publisher's source of truth: publish runs write
# its payload verbatim to bracket.json, so a json-only patch is silently
# reverted by the next decide-publish/republish run. Patching the parquet
# makes every subsequent publish keep the fix, until the next fresh fit
# (running post-fix code) carries it natively.
#
# Drift gate: posterior-derived keys (teams/teams_is/matches/r32, and every
# matchup cell EXCEPT a decided leaf pair's) must be unchanged by the rebuild.

devtools::load_all(".")

league <- list(sport = "football", country = "iceland")
root <- here::here("data")
end_date <- Sys.Date()

slug_for <- c(male = "karla-bikar", female = "kvenna-bikar")

for (sex in c("male", "female")) {
  cat(sprintf("==== %s ====\n", sex))

  # ---- Rebuild bracket_state exactly as the extract layer does --------------
  prep <- prepare_data(league, sex, end_date = end_date, root = root)
  pred_d <- prep$pred_d

  results <- read_table(
    "results",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  results <- results[
    !is.na(results$match_date) & results$match_date <= end_date, ,
    drop = FALSE
  ]
  current_season <- max(results$season, na.rm = TRUE)

  season_schedule <- read_table(
    "schedules",
    root   = root,
    filter = list(sport = league$sport, country = league$country, sex = sex)
  )
  season_schedule <- season_schedule[
    !is.na(season_schedule$match_date) &
      season_schedule$season == current_season, ,
    drop = FALSE
  ]

  bracket_state <- .build_bracket_state_pfi(
    pred_d,
    results        = results,
    current_season = current_season,
    schedule       = season_schedule
  )
  stopifnot(!is.null(bracket_state))

  # ---- Locate the partition the publisher reads (latest with a payload) -----
  extract_base <- here::here(
    "data", "beliefs", "extracts",
    "sport=football", "country=iceland", paste0("sex=", sex)
  )
  fit_dirs <- sort(
    list.files(extract_base, pattern = "^fit_date=", full.names = TRUE),
    decreasing = TRUE
  )
  parquet_path <- NULL
  for (d in fit_dirs) {
    cand <- file.path(d, "cup_bracket.parquet")
    if (file.exists(cand)) {
      parquet_path <- cand
      break
    }
  }
  if (is.null(parquet_path)) {
    cat("no cup_bracket.parquet in any fit_date partition; skipping\n")
    next
  }
  fit_dir <- dirname(parquet_path)

  old_payload <- jsonlite::fromJSON(
    arrow::read_parquet(parquet_path)$payload_json[[1]],
    simplifyVector = FALSE
  )

  # ---- Full rebuild from the SAME fit's persisted sim inputs ----------------
  sim_inputs <- list(
    team   = arrow::read_parquet(file.path(fit_dir, "sim_inputs_team.parquet")),
    scalar = arrow::read_parquet(file.path(fit_dir, "sim_inputs_scalar.parquet"))
  )
  new_payload <- .build_cup_bracket_payload_pfi(
    bracket_state = bracket_state,
    sim_inputs    = sim_inputs,
    generated_at  = old_payload$generated_at, # posterior content unchanged
    n_draws       = old_payload$n_draws,
    results       = results,
    season        = current_season
  )
  stopifnot(!is.null(new_payload))

  # ---- Drift gate ------------------------------------------------------------
  for (k in c("generated_at", "n_draws", "teams", "teams_is")) {
    stopifnot(identical(
      unlist(old_payload[[k]]), unlist(as.list(new_payload)[[k]])
    ))
  }
  stopifnot(identical(
    jsonlite::toJSON(old_payload$matches, auto_unbox = TRUE),
    jsonlite::toJSON(new_payload$matches, auto_unbox = TRUE)
  ))
  stopifnot(identical(
    jsonlite::toJSON(old_payload$r32, auto_unbox = TRUE),
    jsonlite::toJSON(new_payload$r32, auto_unbox = TRUE)
  ))
  # matchup: unchanged except cells of decided leaf pairs (pinned 1/0).
  teams <- unlist(new_payload$teams)
  w_old <- matrix(unlist(old_payload$matchup), length(teams), byrow = TRUE)
  w_new <- new_payload$matchup
  decided_mask <- matrix(FALSE, length(teams), length(teams))
  for (p in new_payload$played) {
    decided_mask[p$winner + 1L, p$loser + 1L] <- TRUE
    decided_mask[p$loser + 1L, p$winner + 1L] <- TRUE
  }
  drift <- max(abs(w_old[!decided_mask] - w_new[!decided_mask]))
  stopifnot(is.finite(drift), drift < 1e-9)

  # ---- 1. Patch the parquet (the source of truth the publisher reads) -------
  arrow::write_parquet(
    tibble::tibble(payload_json = as.character(jsonlite::toJSON(
      new_payload,
      auto_unbox = TRUE, matrix = "rowmajor"
    ))),
    parquet_path
  )

  # ---- 2. Re-derive bracket.json FROM the patched parquet -------------------
  bj_path <- here::here(
    "data", "publish", "football", "iceland", slug_for[[sex]], "bracket.json"
  )
  cup_bracket <- jsonlite::fromJSON(
    arrow::read_parquet(parquet_path)$payload_json[[1]],
    simplifyVector = FALSE
  )
  jsonlite::write_json(
    cup_bracket, bj_path,
    auto_unbox = TRUE, matrix = "rowmajor"
  )

  round_tab <- table(vapply(new_payload$completed, `[[`, "", "round"))
  cat(sprintf(
    "patched %s\n  completed: %s | played: %d | matchup drift (non-decided): %.2g\n",
    parquet_path,
    paste(sprintf("%s=%d", names(round_tab), as.integer(round_tab)), collapse = ", "),
    length(new_payload$played),
    drift
  ))
}

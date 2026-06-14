#' @include wc-publish.R
NULL

# Accountability layer for the World Cup forecast — the "did the model call it?"
# surface on the platform's /hm2026 page. Pairs each PLAYED group fixture with
# the model's PRE-match prediction so the page can show hit/miss + calibration.
#
# The actual scores already live in `group_fixtures` (built from the `results`
# facts table). The predictions, though, are recomputed every fit and pruned
# once a match is played — so the pre-match prediction would be lost. We persist
# a small per-match snapshot log keyed by (home, away, match_date): every fit
# UPSERTS the latest pre-match prediction for each upcoming fixture, which then
# freezes when the match drops out of `predictions`. One JSON file, git-diffable,
# committed so it survives across CI runs.

.wc_accountability_log_path <- function(root) {
  file.path(root, "wc", "accountability", "prediction_log.json")
}

.wc_empty_results <- function() {
  list(
    summary = list(n_played = 0L, n_hit = 0L, hit_rate = 0, mean_p_outcome = 0),
    matches = list()
  )
}

#' Snapshot the current upcoming-match predictions into the accountability log.
#'
#' One row per (home, away, match_date): the latest pre-match 1X2 + expected-goal
#' prediction and the `fit_date` that produced it. Idempotent within a fit; only
#' genuine pre-match predictions (fit made on or before match day) are retained.
#'
#' @param predictions `sim_out$predictions` (NULL / 0-row before the draw).
#' @param fit_date Date of the underlying fit.
#' @param root Data root.
#' @return Invisibly, the log path.
#' @export
wc_snapshot_predictions <- function(predictions, fit_date = Sys.Date(),
                                    root = here::here("data")) {
  path <- .wc_accountability_log_path(root)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  fit_date <- as.character(fit_date)

  cols <- c(
    "match_date", "group", "home", "away", "fit_date",
    "p_home", "p_draw", "p_away", "eg_home", "eg_away"
  )
  empty_log <- tibble::tibble(
    match_date = character(), group = character(), home = character(),
    away = character(), fit_date = character(),
    p_home = numeric(), p_draw = numeric(), p_away = numeric(),
    eg_home = numeric(), eg_away = numeric()
  )
  log_df <- empty_log
  if (file.exists(path)) {
    existing <- jsonlite::read_json(path, simplifyVector = TRUE)
    if (length(existing) && NROW(existing$matches) > 0L) {
      log_df <- tibble::as_tibble(existing$matches)[, cols, drop = FALSE]
    }
  }

  if (!is.null(predictions) && nrow(predictions) > 0L) {
    incoming <- tibble::tibble(
      match_date = as.character(predictions$match_date),
      group = as.character(predictions$group),
      home = predictions$home, away = predictions$away,
      fit_date = fit_date,
      p_home = predictions$p_home, p_draw = predictions$p_draw,
      p_away = predictions$p_away,
      eg_home = predictions$eg_home, eg_away = predictions$eg_away
    )
    # Keep only genuine pre-match predictions (fit on/before match day).
    incoming <- incoming[incoming$fit_date <= incoming$match_date, , drop = FALSE]
    if (nrow(incoming) > 0L) {
      key_old <- paste(log_df$home, log_df$away, log_df$match_date)
      key_new <- paste(incoming$home, incoming$away, incoming$match_date)
      log_df <- log_df[!(key_old %in% key_new), , drop = FALSE]
      log_df <- dplyr::bind_rows(log_df, incoming)
    }
  }

  log_df <- log_df[order(log_df$match_date, log_df$group, log_df$home), , drop = FALSE]
  jsonlite::write_json(
    list(
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      matches = log_df
    ),
    path,
    auto_unbox = TRUE, pretty = TRUE
  )
  invisible(path)
}

#' Build the accountability payload: played group fixtures vs their pre-match
#' prediction (the `results.json` data contract).
#'
#' For each played group fixture, joins to the latest pre-match snapshot
#' (`fit_date <= match_date`) and records the actual outcome, the probability the
#' model assigned to it, whether the model's most-likely call was right, and a
#' "surprise" score. Matches with no pre-match prediction on record (e.g. played
#' before this layer shipped) are skipped.
#'
#' @param group_fixtures Output of [wc_group_fixtures()] (carries actual scores).
#' @param root Data root (locates the snapshot log).
#' @param is_name English -> Icelandic team namer.
#' @return `list(summary = ..., matches = ...)`; the empty shape when nothing
#'   qualifies (so the platform section stays gated off).
#' @export
wc_build_results <- function(group_fixtures, root = here::here("data"),
                             is_name = identity) {
  path <- .wc_accountability_log_path(root)
  if (!file.exists(path)) {
    return(.wc_empty_results())
  }
  log <- jsonlite::read_json(path, simplifyVector = TRUE)
  if (!length(log) || NROW(log$matches) == 0L) {
    return(.wc_empty_results())
  }
  snap <- tibble::as_tibble(log$matches)

  played <- group_fixtures[group_fixtures$played %in% TRUE, , drop = FALSE]
  played <- played[
    !is.na(played$home_score) & !is.na(played$away_score), ,
    drop = FALSE
  ]
  if (nrow(played) == 0L) {
    return(.wc_empty_results())
  }

  rnd <- function(x, d = 4) round(x, d)
  rows <- list()
  for (i in seq_len(nrow(played))) {
    f <- played[i, ]
    cand <- snap[
      snap$home == f$home_team & snap$away == f$away_team &
        snap$match_date == as.character(f$match_date) &
        snap$fit_date <= as.character(f$match_date), ,
      drop = FALSE
    ]
    if (nrow(cand) == 0L) next # no pre-match prediction on record -> skip
    cand <- cand[order(cand$fit_date), , drop = FALSE]
    pr <- cand[nrow(cand), ] # the latest pre-match fit
    hs <- as.integer(f$home_score)
    as_ <- as.integer(f$away_score)
    outcome <- if (hs > as_) "H" else if (hs == as_) "D" else "A"
    probs <- c(H = pr$p_home, D = pr$p_draw, A = pr$p_away)
    p_outcome <- unname(probs[[outcome]])
    pred_call <- names(probs)[which.max(probs)]
    rows[[length(rows) + 1L]] <- list(
      match_date = as.character(f$match_date), group = as.character(f$group),
      home = f$home_team, home_is = is_name(f$home_team),
      away = f$away_team, away_is = is_name(f$away_team),
      home_score = hs, away_score = as_, outcome = outcome,
      pred = list(
        p_home = rnd(pr$p_home), p_draw = rnd(pr$p_draw), p_away = rnd(pr$p_away),
        eg_home = rnd(pr$eg_home, 2), eg_away = rnd(pr$eg_away, 2)
      ),
      pred_fit_date = as.character(pr$fit_date),
      p_outcome = rnd(p_outcome),
      hit = isTRUE(pred_call == outcome),
      surprise = rnd(1 - p_outcome)
    )
  }
  if (length(rows) == 0L) {
    return(.wc_empty_results())
  }

  hits <- vapply(rows, function(r) isTRUE(r$hit), logical(1))
  pouts <- vapply(rows, function(r) r$p_outcome, numeric(1))
  # Most recent first (the platform shows the top ~10).
  ord <- order(vapply(rows, function(r) r$match_date, character(1)), decreasing = TRUE)
  rows <- rows[ord]
  list(
    summary = list(
      n_played = length(rows), n_hit = sum(hits),
      hit_rate = rnd(mean(hits)), mean_p_outcome = rnd(mean(pouts))
    ),
    matches = rows
  )
}

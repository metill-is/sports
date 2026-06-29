#' @include wc-publish.R
NULL

# Accountability layer for the World Cup forecast — the "did the model call it?"
# surface on the platform's /hm2026 page. Pairs each PLAYED group fixture with
# the model's PRE-match prediction: the 1X2 outcome AND the full predicted goal
# distributions (home / away / total / difference), so the page can render both
# the hit/miss + calibration view and the score-prediction-vs-actual interval
# plot (cf. the basketball/handball historical_testing.R panels).
#
# Scores live in `group_fixtures` (the `results` facts table). Predictions are
# recomputed every fit and pruned once a match is played, so the pre-match
# prediction would be lost — we persist a small per-match snapshot log keyed by
# (home, away, match_date): every fit UPSERTS the latest pre-match prediction
# (fit_date <= match_date), which freezes when the fixture drops out of
# `predictions`. One git-diffable JSON, committed so it survives across CI runs.

.wc_accountability_log_path <- function(root) {
  file.path(root, "wc", "accountability", "prediction_log.json")
}

.wc_empty_results <- function() {
  list(
    summary = list(n_played = 0L, n_hit = 0L, hit_rate = 0, mean_p_outcome = 0),
    matches = list()
  )
}

# Pull the i-th match's distribution list-column cell (tibble / data.frame), or
# NULL when the column is absent (e.g. historical predictions.json that predate
# the marginal-distribution contract carry only goal_diff_distribution).
.wc_dist_cell <- function(preds, col, i) {
  if (!(col %in% names(preds))) {
    return(NULL)
  }
  preds[[col]][[i]]
}

# Coerce a possibly-NULL / length-0 scalar to a length-1 character so the
# order() vapplys below never abort. A logged knockout match carries no group
# (bind_rows leaves it NA -> serialized as JSON null -> read back as NULL); once
# that fixture is played it is not re-upserted, so the NULL flows into order().
.wc_chr1 <- function(x) {
  if (length(x) == 0L) NA_character_ else as.character(x)[[1L]]
}

# Normalise an incoming distribution (tibble / data.frame with a value field +
# `p`) to a compact list of `{value, p}` elements, or NULL when empty/malformed.
.wc_norm_dist <- function(df, vfield) {
  if (is.null(df) || NROW(df) == 0L || !all(c(vfield, "p") %in% names(df))) {
    return(NULL)
  }
  lapply(seq_len(NROW(df)), function(k) {
    el <- list(as.integer(df[[vfield]][k]), round(as.numeric(df$p[k]), 4))
    names(el) <- c(vfield, "p")
    el
  })
}

#' Snapshot the current upcoming-match predictions into the accountability log.
#'
#' One object per (home, away, match_date): the latest pre-match 1X2 + expected
#' goals + the four predicted goal distributions (diff / home / away / total),
#' and the `fit_date` that produced them. Idempotent within a fit; only genuine
#' pre-match predictions (fit on or before match day) are retained. Distribution
#' columns absent from `predictions` are simply omitted (so a feed carrying only
#' goal-diff still snapshots cleanly).
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

  # Existing log -> a store keyed by home|away|match_date (nested distributions
  # preserved verbatim; read with simplifyVector = FALSE).
  store <- list()
  if (file.exists(path)) {
    existing <- jsonlite::read_json(path, simplifyVector = FALSE)
    for (m in (if (is.null(existing$matches)) list() else existing$matches)) {
      store[[paste(m$home, m$away, m$match_date, sep = "|")]] <- m
    }
  }

  if (!is.null(predictions) && nrow(predictions) > 0L) {
    for (i in seq_len(nrow(predictions))) {
      md <- as.character(predictions$match_date[i])
      if (fit_date > md) next # not a pre-match prediction
      obj <- list(
        match_date = md,
        group = as.character(predictions$group[i]),
        home = predictions$home[i], away = predictions$away[i],
        fit_date = fit_date,
        p_home = round(predictions$p_home[i], 4),
        p_draw = round(predictions$p_draw[i], 4),
        p_away = round(predictions$p_away[i], 4),
        eg_home = round(predictions$eg_home[i], 2),
        eg_away = round(predictions$eg_away[i], 2)
      )
      dd <- .wc_norm_dist(.wc_dist_cell(predictions, "goal_diff_distribution", i), "diff")
      dh <- .wc_norm_dist(.wc_dist_cell(predictions, "home_goal_distribution", i), "goals")
      da <- .wc_norm_dist(.wc_dist_cell(predictions, "away_goal_distribution", i), "goals")
      dt <- .wc_norm_dist(.wc_dist_cell(predictions, "total_goal_distribution", i), "total")
      if (!is.null(dd)) obj$dist_diff <- dd
      if (!is.null(dh)) obj$dist_home <- dh
      if (!is.null(da)) obj$dist_away <- da
      if (!is.null(dt)) obj$dist_total <- dt
      store[[paste(obj$home, obj$away, obj$match_date, sep = "|")]] <- obj # upsert
    }
  }

  matches <- unname(store)
  ord <- order(
    vapply(matches, function(m) .wc_chr1(m$match_date), character(1)),
    vapply(matches, function(m) .wc_chr1(m$group), character(1)),
    vapply(matches, function(m) .wc_chr1(m$home), character(1))
  )
  matches <- matches[ord]
  jsonlite::write_json(
    list(
      updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      matches = matches
    ),
    path,
    auto_unbox = TRUE, pretty = TRUE
  )
  invisible(path)
}

#' Build the accountability payload from played group fixtures + their pre-match
#' snapshot (the `results.json` data contract).
#'
#' Each record carries the 1X2 result (outcome, p_outcome, hit, surprise) AND
#' the score-prediction view: `pred_dist` (the predicted home/away/total/diff
#' goal distributions, whichever the snapshot holds) against `actual` (the
#' realised home/away/total/diff goals). Matches with no pre-match prediction on
#' record are skipped.
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
  log <- jsonlite::read_json(path, simplifyVector = FALSE)
  if (is.null(log$matches) || length(log$matches) == 0L) {
    return(.wc_empty_results())
  }
  store <- list()
  for (m in log$matches) store[[paste(m$home, m$away, m$match_date, sep = "|")]] <- m

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
    md <- as.character(f$match_date)
    pr <- store[[paste(f$home_team, f$away_team, md, sep = "|")]]
    if (is.null(pr)) next # no pre-match prediction on record
    if (!is.null(pr$fit_date) && as.character(pr$fit_date) > md) next
    hs <- as.integer(f$home_score)
    as_ <- as.integer(f$away_score)
    outcome <- if (hs > as_) "H" else if (hs == as_) "D" else "A"
    probs <- c(H = pr$p_home, D = pr$p_draw, A = pr$p_away)
    p_outcome <- unname(probs[[outcome]])
    pred_call <- names(probs)[which.max(probs)]
    pred_dist <- list()
    if (!is.null(pr$dist_home)) pred_dist$home <- pr$dist_home
    if (!is.null(pr$dist_away)) pred_dist$away <- pr$dist_away
    if (!is.null(pr$dist_total)) pred_dist$total <- pr$dist_total
    if (!is.null(pr$dist_diff)) pred_dist$diff <- pr$dist_diff
    rows[[length(rows) + 1L]] <- list(
      match_date = md, group = as.character(f$group),
      home = f$home_team, home_is = is_name(f$home_team),
      away = f$away_team, away_is = is_name(f$away_team),
      home_score = hs, away_score = as_, outcome = outcome,
      pred = list(
        p_home = rnd(pr$p_home), p_draw = rnd(pr$p_draw), p_away = rnd(pr$p_away),
        eg_home = rnd(pr$eg_home, 2), eg_away = rnd(pr$eg_away, 2)
      ),
      pred_dist = pred_dist,
      actual = list(home = hs, away = as_, total = hs + as_, diff = hs - as_),
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

#' Backfill the accountability snapshot log from the git history of
#' `predictions.json`.
#'
#' Every daily fit committed a `predictions.json` covering the then-upcoming
#' fixtures. Walking those commits oldest -> newest and upserting each into the
#' snapshot log reconstructs exactly what [wc_snapshot_predictions()] would have
#' accumulated day by day — recovering the pre-match prediction for every
#' fixture ever published, including matches played before this layer shipped.
#' Historical files carry only `goal_diff_distribution`, so the home/away/total
#' distributions backfill as absent and accrue from deployment forward.
#' Idempotent.
#'
#' @param root Data root (where the snapshot log is written).
#' @param repo Repository root for git operations.
#' @param ref Git ref to walk (default `"HEAD"`).
#' @param predictions_path Repo-relative path to the published predictions.json.
#' @return Invisibly, the number of historical commits folded in.
#' @export
wc_backfill_snapshots <- function(root = here::here("data"),
                                  repo = here::here(),
                                  ref = "HEAD",
                                  predictions_path =
                                    "data/publish/world_cup/karla/predictions.json") {
  commits <- tryCatch(
    system2(
      "git",
      c("-C", repo, "log", "--reverse", "--format=%H", ref, "--", predictions_path),
      stdout = TRUE, stderr = FALSE
    ),
    error = function(e) character()
  )
  commits <- commits[nzchar(commits)]
  if (length(commits) == 0L) {
    cli::cli_warn(
      "wc_backfill_snapshots: no predictions.json history at {.path {predictions_path}}."
    )
    return(invisible(0L))
  }

  scalars <- c(
    "match_date", "group", "home", "away",
    "p_home", "p_draw", "p_away", "eg_home", "eg_away"
  )
  n_ok <- 0L
  for (sha in commits) {
    raw <- tryCatch(
      system2("git", c("-C", repo, "show", paste0(sha, ":", predictions_path)),
        stdout = TRUE, stderr = FALSE
      ),
      error = function(e) character()
    )
    if (!length(raw)) next
    pj <- tryCatch(
      jsonlite::fromJSON(paste(raw, collapse = "\n"), simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (is.null(pj) || NROW(pj$matches) == 0L) next
    m <- pj$matches
    if (!all(scalars %in% names(m))) next
    # Keep the scalars + whatever distribution list-columns this vintage carried
    # (historical files: goal_diff only; current: all four).
    keep <- intersect(
      c(
        scalars, "goal_diff_distribution", "home_goal_distribution",
        "away_goal_distribution", "total_goal_distribution"
      ),
      names(m)
    )
    preds <- tibble::as_tibble(m[, keep])
    gen <- pj$generated_at
    fit_date <- suppressWarnings(as.Date(substr(if (is.null(gen)) "" else gen, 1, 10)))
    if (is.na(fit_date)) fit_date <- as.Date(min(preds$match_date)) # fallback
    wc_snapshot_predictions(preds, fit_date = fit_date, root = root)
    n_ok <- n_ok + 1L
  }
  cli::cli_alert_success("wc_backfill_snapshots: folded in {n_ok} commit(s).")
  invisible(n_ok)
}

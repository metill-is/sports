#' @include storage.R config.R pipeline-freshness.R
NULL

# Health thresholds live here as documented named constants rather than an
# externalised SLO config file: for a solo maintainer a 10-line block where the
# function reads them is a clearer single source of truth than a YAML + a
# lockstep-sync test (reality-check 2026-05-30). Revisit if they start churning.
#' @noRd
health_thresholds <- function() {
  list(
    fit_age_warn_days = 2,
    fit_age_fail_days = 4,
    odds_age_warn_hours = 8,
    odds_age_fail_hours = 24,
    orphan_age_days = 10,
    div_frac_warn = 0.005, # half the 0.01 abort gate — the early-warning band
    rhat_warn = 1.02,
    drawdown_warn_frac = 0.6, # current_pool < 60% of initial_pool
    capture_window_days = 21, # look-back for placed-vs-recommended
    capture_min_n = 20, # below this many recs in window, don't escalate
    capture_warn_rate = 0.7, # placed/recommended below this -> WARN
    capture_fail_rate = 0.3 # below this -> FAIL (near-total placement collapse)
  )
}

#' @noRd
health_row <- function(check, scope, status, value, threshold) {
  tibble::tibble(
    check = check, scope = scope, status = status,
    value = as.character(value), threshold = as.character(threshold)
  )
}

#' @noRd
health_empty <- function() {
  tibble::tibble(
    check = character(), scope = character(), status = character(),
    value = character(), threshold = character()
  )
}

#' @noRd
.cell_sexes <- function(lg) {
  s <- lg$sexes %||% lg$sex
  if (is.null(s)) character(0) else as.character(unlist(s))
}

#' @noRd
latest_fit_date <- function(static, sex, root) {
  bl <- tryCatch(
    read_table("beliefs_latest",
      root = root,
      filter = list(sport = static$sport, country = static$country, sex = sex)
    ),
    error = function(e) tibble::tibble()
  )
  if (nrow(bl) == 0L || !("fit_date" %in% names(bl))) {
    return(as.Date(NA))
  }
  max(as.Date(bl$fit_date), na.rm = TRUE)
}

#' @noRd
check_fit_freshness <- function(leagues, root, now, th) {
  today <- as.Date(now, tz = "UTC")
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    static <- list(sport = lg$sport, country = lg$country)
    for (sx in .cell_sexes(lg)) {
      scope <- paste(key, sx)
      upcoming <- tryCatch(has_upcoming_games(static, sx, root = root),
        error = function(e) FALSE
      )
      if (!isTRUE(upcoming)) {
        rows[[scope]] <- health_row(
          "fit_freshness", scope, "PAUSED", "no upcoming games", "n/a"
        )
        next
      }
      fd <- latest_fit_date(static, sx, root)
      if (is.na(fd)) {
        rows[[scope]] <- health_row(
          "fit_freshness", scope, "FAIL", "no fit", paste0("<= ", th$fit_age_warn_days, "d")
        )
        next
      }
      age <- as.numeric(today - fd)
      status <- if (age > th$fit_age_fail_days) {
        "FAIL"
      } else if (age > th$fit_age_warn_days) {
        "WARN"
      } else {
        "OK"
      }
      rows[[scope]] <- health_row(
        "fit_freshness", scope, status,
        paste0(age, "d old"), paste0("<= ", th$fit_age_warn_days, "d")
      )
    }
  }
  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' @noRd
check_odds_freshness <- function(leagues, root, now, th) {
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    static <- list(sport = lg$sport, country = lg$country)
    upcoming <- any(vapply(.cell_sexes(lg), function(sx) {
      tryCatch(has_upcoming_games(static, sx, root = root), error = function(e) FALSE)
    }, logical(1)))
    if (!isTRUE(upcoming)) next # off-season: odds intentionally not scraped
    od <- tryCatch(
      read_table("odds",
        root = root,
        filter = list(sport = lg$sport, country = lg$country)
      ),
      error = function(e) tibble::tibble()
    )
    if (nrow(od) == 0L || !("scraped_at" %in% names(od))) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "FAIL", "no odds",
        paste0("<= ", th$odds_age_warn_hours, "h")
      )
      next
    }
    age_h <- as.numeric(difftime(now, max(od$scraped_at, na.rm = TRUE), units = "hours"))
    status <- if (age_h > th$odds_age_fail_hours) {
      "FAIL"
    } else if (age_h > th$odds_age_warn_hours) {
      "WARN"
    } else {
      "OK"
    }
    rows[[key]] <- health_row(
      "odds_freshness", key, status,
      sprintf("%.1fh old", age_h), paste0("<= ", th$odds_age_warn_hours, "h")
    )
  }
  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' @noRd
check_diagnostics_drift <- function(root, th) {
  d <- tryCatch(read_table("fit_diagnostics", root = root), error = function(e) tibble::tibble())
  if (nrow(d) == 0L) {
    return(health_empty())
  }
  d$fit_date <- as.Date(d$fit_date)
  d$cell <- paste(d$sport, d$country, d$sex, sep = "/")
  rows <- list()
  for (cell in unique(d$cell)) {
    sub <- d[d$cell == cell, , drop = FALSE]
    r <- sub[which.max(sub$fit_date), , drop = FALSE]
    div <- r$div_frac
    rows[[paste0(cell, ":div")]] <- health_row(
      "divergence_drift", cell,
      if (!is.na(div) && div > th$div_frac_warn) "WARN" else "OK",
      if (is.na(div)) "NA" else sprintf("%.3f%%", 100 * div),
      sprintf("< %.1f%%", 100 * th$div_frac_warn)
    )
    rh <- r$max_rhat
    rows[[paste0(cell, ":rhat")]] <- health_row(
      "rhat_drift", cell,
      if (!is.na(rh) && rh > th$rhat_warn) "WARN" else "OK",
      if (is.na(rh)) "NA" else sprintf("%.3f", rh),
      sprintf("< %.3f", th$rhat_warn)
    )
  }
  dplyr::bind_rows(rows)
}

#' @noRd
check_orphaned_bets <- function(root, now, th) {
  led <- tryCatch(read_table("ledger", root = root), error = function(e) tibble::tibble())
  thr_lbl <- paste0("0 unsettled > ", th$orphan_age_days, "d")
  if (nrow(led) == 0L || !all(c("settled", "match_date") %in% names(led))) {
    return(health_row("orphaned_bets", "ledger", "OK", 0, thr_lbl))
  }
  today <- as.Date(now, tz = "UTC")
  unsettled <- led[!is.na(led$settled) & !led$settled, , drop = FALSE]
  orphans <- unsettled[
    !is.na(unsettled$match_date) &
      unsettled$match_date < (today - th$orphan_age_days), ,
    drop = FALSE
  ]
  n <- nrow(orphans)
  health_row("orphaned_bets", "ledger", if (n > 0L) "WARN" else "OK", n, thr_lbl)
}

#' Capture rate: of bets recommended for matches that have now been played,
#' how many were actually placed (appear in the ledger). A low rate is the
#' operational defect the forensic review (2026-05-30) found dominates the
#' modelling question — the pipeline placed ~45% of recommendations. Read-only
#' on committed recommendations + ledger, so CI-safe. Recommendations exclude
#' already-placed bets per run, but earlier run_date partitions retain a bet
#' from before it was placed, so the distinct union across run_dates is the set
#' of bets ever recommended; the inner-join to the ledger is what was placed.
#' @noRd
check_capture_rate <- function(root, now, th) {
  thr_lbl <- paste0(">= ", round(100 * th$capture_warn_rate), "% placed")
  key <- c(
    "sport", "country", "sex", "match_date",
    "home_team", "away_team", "market", "outcome", "line"
  )
  recs <- tryCatch(read_table("recommendations", root = root),
    error = function(e) tibble::tibble()
  )
  if (nrow(recs) == 0L || !all(key %in% names(recs))) {
    return(health_row("capture_rate", "recommendations", "OK", "no recommendations", thr_lbl))
  }
  today <- as.Date(now, tz = "UTC")
  recs$match_date <- as.Date(recs$match_date)
  recent <- recs[
    !is.na(recs$match_date) &
      recs$match_date >= (today - th$capture_window_days) &
      recs$match_date < today, ,
    drop = FALSE
  ]
  rec_d <- dplyr::distinct(recent[, key, drop = FALSE])
  n_rec <- nrow(rec_d)
  if (n_rec == 0L) {
    return(health_row("capture_rate", "recommendations", "OK", "no settled-window recs", thr_lbl))
  }
  led <- tryCatch(read_table("ledger", root = root), error = function(e) tibble::tibble())
  led_d <- if (nrow(led) > 0L && all(key %in% names(led))) {
    led$match_date <- as.Date(led$match_date)
    dplyr::distinct(led[, key, drop = FALSE])
  } else {
    rec_d[0, , drop = FALSE]
  }
  n_placed <- nrow(dplyr::inner_join(rec_d, led_d, by = key))
  rate <- n_placed / n_rec
  # Only escalate past OK once there are enough recs to be meaningful.
  status <- if (n_rec < th$capture_min_n) {
    "OK"
  } else if (rate < th$capture_fail_rate) {
    "FAIL"
  } else if (rate < th$capture_warn_rate) {
    "WARN"
  } else {
    "OK"
  }
  health_row(
    "capture_rate", "recommendations", status,
    sprintf("%.0f%% (%d/%d placed)", 100 * rate, n_placed, n_rec), thr_lbl
  )
}

#' @noRd
check_bankroll <- function(root, th) {
  bk <- tryCatch(load_bankroll(ledger_root = root), error = function(e) NULL)
  if (is.null(bk) || is.null(bk$current_pool) || is.null(bk$initial_pool)) {
    return(health_empty())
  }
  frac <- bk$current_pool / bk$initial_pool
  status <- if (bk$current_pool <= 0) {
    "FAIL"
  } else if (frac < th$drawdown_warn_frac) {
    "WARN"
  } else {
    "OK"
  }
  health_row(
    "bankroll", "all", status,
    sprintf("%.0f (%.0f%% of initial)", bk$current_pool, 100 * frac),
    paste0(">= ", round(100 * th$drawdown_warn_frac), "% of initial")
  )
}

#' Read-only pipeline health snapshot.
#'
#' Composes freshness, persisted Stan-diagnostics drift, orphaned-bet,
#' placement-capture-rate, and bankroll checks into one tibble of
#' `{check, scope, status, value, threshold}`
#' rows. `status` is one of `OK` < `WARN` < `FAIL`, plus `PAUSED` for a cell
#' that is intentionally off-season (no upcoming games — reuses
#' [has_upcoming_games()] so the seasonal basketball/handball pause yields
#' `PAUSED` rather than a false `FAIL`). Every input is read via [read_table()]
#' over committed Parquet; nothing is written, so this is safe to run on CI
#' against the local-only ledger (a reader cannot race the placer's
#' non-atomic write). Each sub-check is wrapped so one failure degrades to a
#' single `check_error` row rather than aborting the whole snapshot.
#'
#' @param root Data root. Default `here::here("data")`.
#' @param now Reference time (POSIXct). Default `Sys.time()`.
#' @param leagues Pre-loaded leagues list. Default `NULL` = `load_leagues()`.
#' @return A tibble with columns `check`, `scope`, `status`, `value`, `threshold`.
#' @export
pipeline_health <- function(root = here::here("data"),
                            now = Sys.time(),
                            leagues = NULL) {
  th <- health_thresholds()
  if (is.null(leagues)) {
    leagues <- tryCatch(load_leagues(), error = function(e) list())
  }
  safe <- function(expr) {
    tryCatch(expr, error = function(e) {
      health_row("check_error", "pipeline_health", "WARN", conditionMessage(e), "n/a")
    })
  }
  dplyr::bind_rows(
    safe(check_fit_freshness(leagues, root, now, th)),
    safe(check_odds_freshness(leagues, root, now, th)),
    safe(check_diagnostics_drift(root, th)),
    safe(check_orphaned_bets(root, now, th)),
    safe(check_capture_rate(root, now, th)),
    safe(check_bankroll(root, th))
  )
}

#' Collapse a pipeline-health tibble to a single worst-case status.
#'
#' `FAIL` if any row failed, else `WARN` if any warned, else `OK`. `PAUSED`
#' rows do not escalate.
#'
#' @param health A tibble from [pipeline_health()].
#' @return One of `"OK"`, `"WARN"`, `"FAIL"`.
#' @export
overall_health_status <- function(health) {
  s <- health$status
  if ("FAIL" %in% s) {
    "FAIL"
  } else if ("WARN" %in% s) {
    "WARN"
  } else {
    "OK"
  }
}

#' Write a pipeline-health snapshot to JSON.
#'
#' Serialises a [pipeline_health()] tibble plus the overall status and
#' fail/warn counts to `path` (typically `data/health/status.json`) — a
#' committable, diffable, machine-readable health record a human, the
#' `/pipeline-doctor` skill, or an external consumer can poll.
#'
#' @param health A tibble from [pipeline_health()].
#' @param path Destination JSON path.
#' @param now Reference time stamped into the payload. Default `Sys.time()`.
#' @return invisible(path).
#' @export
write_health_status <- function(health, path, now = Sys.time()) {
  payload <- list(
    generated_at = format(now, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    overall = overall_health_status(health),
    n_fail = sum(health$status == "FAIL"),
    n_warn = sum(health$status == "WARN"),
    checks = health
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json_consistent(payload, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}

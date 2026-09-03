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
    odds_lead_days = 3, # Lengjan posts football odds with a ~2-day median lead;
    # only expect odds once a fixture falls inside this window (see
    # check_odds_freshness). Wider than the lead so a normal between-match lull
    # never alarms.
    orphan_age_days = 10,
    div_frac_warn = 0.005, # half the 0.01 abort gate — the early-warning band
    rhat_warn = 1.02,
    drawdown_warn_frac = 0.6, # current_pool < 60% of initial_pool
    capture_window_days = 21, # look-back for placed-vs-recommended
    capture_min_n = 20, # below this many recs in window, don't escalate
    capture_warn_rate = 0.7, # placed/recommended below this -> WARN
    capture_fail_rate = 0.3, # below this -> FAIL (near-total placement collapse)
    placement_stale_warn_hours = 6, # pending bets + last healthy run older -> WARN
    placement_stale_fail_hours = 14, # ...older still -> FAIL
    # A published cell older than this is stale. Judgement, not measurement:
    # decide-publish commits roughly 4x/day (git log -3 -- data/publish/ on
    # 2026-09-02 shows 12:37Z, 18:00Z, 22:27Z), so 36h is a full day of missed
    # runs plus slack. Too tight and every quiet weekend goes red; too loose
    # and a two-day publish outage looks healthy. Revisit after the first
    # month of bb/hb publishing, citing observed inter-commit gaps.
    publish_max_age_hours = 36,
    unmapped_window_days = 14 # look-back for unmapped Lengjan team names;
    # matches check_odds_freshness's fixture horizon, so a name Lengjan is
    # actively pricing stays flagged while a retired rendering ages out.
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

#' Data-currency-aware fit freshness.
#'
#' A fit goes stale only when completed results exist that it has not yet
#' conditioned on -- not merely because wall-clock days pass. The previous
#' absolute-age design false-FAILed on every routine inter-match lull: women's
#' football in particular runs ~5-day gaps between fixtures, during which
#' `needs_refit()` correctly skips the refit (no new data to fit on) yet the fit
#' aged past the FAIL threshold. So staleness is gated on `needs_refit()` -- the
#' pipeline's own "completed results moved past the fit" predicate, reused here
#' so the health signal stays self-consistent with what `03_fit.R` acts on --
#' and fit age only sets the *severity* once the fit is genuinely behind the
#' data. Off-season cells (no upcoming games) are PAUSED, as before. This
#' mirrors the match-proximity fix applied to `check_odds_freshness`.
#'
#' Neither predicate is allowed to fail into a healthy answer: an error from
#' either yields a per-cell `check_error` FAIL row naming the message. Both
#' already return `FALSE` for a missing or empty partition, so an error is
#' never the benign case -- it is data this snapshot could not read, and
#' reading that as "off-season" or "current with results" is exactly the silent
#' green the check exists to prevent. The catch is per cell rather than left to
#' `pipeline_health()`'s `safe()` wrapper, which would collapse every other
#' cell's row into one `check_error` row and lose their identity.
#' @noRd
check_fit_freshness <- function(leagues, root, now, th) {
  today <- as.Date(now, tz = "UTC")
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    # `training_filter` too: needs_refit() applies it, as the daily fit does
    # (run_fit_targets() hands the fit the same field). Without it health
    # counted results the fit drops and FAILed a fit 03_fit.R would not redo.
    static <- list(
      sport = lg$sport, country = lg$country,
      training_filter = lg$training_filter
    )
    for (sx in .cell_sexes(lg)) {
      scope <- paste(key, sx)
      # Captured, not coerced: `error = function(e) FALSE` made an unreadable
      # schedule partition indistinguishable from a genuine off-season, and
      # PAUSED is the one status overall_health_status() never escalates -- so
      # a corrupt partition reported as "intentionally paused" and the whole
      # snapshot stayed green.
      upcoming <- tryCatch(has_upcoming_games(static, sx, root = root),
        error = function(e) e
      )
      if (inherits(upcoming, "error")) {
        rows[[scope]] <- health_row(
          "check_error", scope, "FAIL",
          paste0("has_upcoming_games() errored: ", conditionMessage(upcoming)),
          "n/a"
        )
        next
      }
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
      # Same reason as above, and worse: `error = function(e) FALSE` landed on
      # the `!isTRUE(behind)` branch, so an unreadable results or beliefs
      # partition was reported OK with the value "current with results" -- the
      # snapshot asserted the fit was up to date on data it had just failed to
      # read. needs_refit() returns FALSE on its own for empty results, so an
      # error here is always a real read failure.
      behind <- tryCatch(needs_refit(static, sx, root = root),
        error = function(e) e
      )
      if (inherits(behind, "error")) {
        rows[[scope]] <- health_row(
          "check_error", scope, "FAIL",
          paste0("needs_refit() errored: ", conditionMessage(behind)),
          "n/a"
        )
        next
      }
      status <- if (!isTRUE(behind)) {
        "OK"
      } else if (age > th$fit_age_fail_days) {
        "FAIL"
      } else if (age > th$fit_age_warn_days) {
        "WARN"
      } else {
        "OK"
      }
      value <- if (isTRUE(behind)) {
        paste0(age, "d old, results pending fit")
      } else {
        paste0(age, "d old, current with results")
      }
      rows[[scope]] <- health_row(
        "fit_freshness", scope, status,
        value, paste0("<= ", th$fit_age_warn_days, "d")
      )
    }
  }
  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' All schedule rows for a league's cells, with `sex` guaranteed present.
#' Derives nothing from `Sys.Date()` so the health snapshot stays
#' deterministic.
#' @noRd
.schedule_frame <- function(static, sexes, root) {
  out <- list()
  for (sx in sexes) {
    sch <- tryCatch(
      read_table("schedules",
        root = root,
        filter = list(sport = static$sport, country = static$country, sex = sx)
      ),
      error = function(e) tibble::tibble()
    )
    if (nrow(sch) == 0L || !("match_date" %in% names(sch))) next
    if (!("sex" %in% names(sch))) sch$sex <- sx
    out[[sx]] <- sch
  }
  if (length(out) == 0L) tibble::tibble() else dplyr::bind_rows(out)
}

#' (sex, division) pairs in which the decide layer has ever produced a
#' candidate -- the empirically bettable universe. KSI schedules span
#' divisions Lengjan never prices (LD4 false-FAILed the 2026-06-09 evening
#' healthcheck), so the odds expectation is scoped to divisions with candidate
#' history. Candidates -- not raw odds -- because odds rows carry Lengjan
#' display names while candidates are post-join: canonical names plus `sex`.
#' Self-maintaining: a new division enters coverage with its first decide run.
#' Returns `NULL` (callers fall back to expecting odds everywhere) when no
#' candidate history joins the schedule -- the conservative cold-start.
#' @noRd
.covered_divisions <- function(static, sch, root) {
  if (nrow(sch) == 0L || !("division" %in% names(sch))) {
    return(NULL)
  }
  cand <- tryCatch(
    read_table("candidates",
      root = root,
      filter = list(sport = static$sport, country = static$country)
    ),
    error = function(e) tibble::tibble()
  )
  need <- c("sex", "match_date", "home_team", "away_team")
  if (nrow(cand) == 0L || !all(need %in% names(cand))) {
    return(NULL)
  }
  cm <- unique(cand[, need, drop = FALSE])
  cm$match_date <- as.Date(cm$match_date)
  sm <- sch[, c(need, "division"), drop = FALSE]
  sm$match_date <- as.Date(sm$match_date)
  hit <- merge(cm, sm, by = need)
  hit <- hit[!is.na(hit$division), c("sex", "division"), drop = FALSE]
  if (nrow(hit) == 0L) {
    return(NULL)
  }
  unique(hit)
}

#' (sex, division) cells a league's configured Lengjan competitions cover, or
#' `NULL` when any competition lacks a `division` (candidate history then
#' decides, as before). Spec 2026-09-23 WS7.
#' @noRd
.configured_divisions <- function(lg) {
  comps <- lg$lengjan$competitions %||% list()
  if (length(comps) == 0L) {
    return(NULL)
  }
  div <- vapply(comps, function(cp) {
    as.character(cp$division %||% NA_character_)
  }, character(1))
  if (anyNA(div)) {
    return(NULL)
  }
  unique(tibble::tibble(
    sex = vapply(comps, function(cp) as.character(cp$sex), character(1)),
    division = div
  ))
}

#' Match-proximity-aware odds freshness.
#'
#' A between-match lull — when Lengjan posts no odds because no fixture is near —
#' is benign and must not alarm; absolute-age staleness (the previous design)
#' false-FAILed on every such gap, since Lengjan posts football odds with a
#' ~2-day median lead and routine inter-match gaps run 2–3 days. So staleness is
#' escalated only once the next fixture falls inside `odds_lead_days`, and the
#' signal that "we have what the decider needs" is odds covering some fixture
#' on/after today. A fixture arriving *today* with no odds scraped is a genuine
#' pipeline stall and FAILs (the alert email). Off-season cells (no upcoming
#' fixture) produce no row, as before. Fixtures in (sex, division) cells that
#' have never produced a candidate (see [.covered_divisions()]) carry no odds
#' expectation: Lengjan does not price them, so their absence is not a stall.
#' @noRd
check_odds_freshness <- function(leagues, root, now, th) {
  today <- as.Date(now, tz = "UTC")
  horizon_days <- 14L
  thr_lbl <- paste0("odds for fixtures within ", th$odds_lead_days, "d")
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    # Betting ladder (spec 2026-09-23 WS1/WS7). "off" is never scraped, so
    # absent odds are correct: PAUSED, which overall_health_status() does not
    # escalate. Below "manual" a league with no competitions wired (basketball
    # before its comps appear) cannot have odds either.
    mode <- betting_mode(lg)
    if (!betting_mode_at_least(lg, "scrape")) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "PAUSED",
        "betting disabled (betting.mode: off)", thr_lbl
      )
      next
    }
    if (!betting_mode_at_least(lg, "manual") &&
      length(lg$lengjan$competitions %||% list()) == 0L) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "PAUSED",
        sprintf("no Lengjan competitions wired (betting.mode: %s)", mode), thr_lbl
      )
      next
    }
    static <- list(sport = lg$sport, country = lg$country)
    sch <- .schedule_frame(static, .cell_sexes(lg), root)
    if (nrow(sch) == 0L) next
    md <- as.Date(sch$match_date)
    upcoming <- sch[!is.na(md) & md >= today & md <= today + horizon_days, , drop = FALSE]
    if (nrow(upcoming) == 0L) next # off-season: no fixture, so no odds expected

    configured <- .configured_divisions(lg)
    covered <- if (is.null(configured)) .covered_divisions(static, sch, root) else configured
    expected <- if (is.null(covered) || !("division" %in% names(upcoming))) {
      upcoming
    } else {
      keep <- is.na(upcoming$division) |
        paste(upcoming$sex, upcoming$division) %in% paste(covered$sex, covered$division)
      upcoming[keep, , drop = FALSE]
    }
    if (nrow(expected) == 0L) {
      rows[[key]] <- health_row(
        "odds_freshness", key, "OK",
        if (is.null(configured)) {
          "upcoming fixtures only in divisions Lengjan has never priced"
        } else {
          "upcoming fixtures only in cells with no configured Lengjan competition"
        },
        thr_lbl
      )
      next
    }

    next_match <- min(as.Date(expected$match_date))
    days_to_next <- as.numeric(next_match - today)
    od <- tryCatch(
      read_table("odds",
        root = root,
        filter = list(sport = lg$sport, country = lg$country)
      ),
      error = function(e) tibble::tibble()
    )
    have_upcoming_odds <- nrow(od) > 0L && "match_date" %in% names(od) &&
      any(as.Date(od$match_date) >= today, na.rm = TRUE)

    if (have_upcoming_odds) {
      status <- "OK"
      value <- sprintf("odds cover upcoming fixtures (next in %dd)", as.integer(days_to_next))
    } else if (days_to_next > th$odds_lead_days) {
      status <- "OK"
      value <- sprintf("next fixture in %dd; odds not posted yet", as.integer(days_to_next))
    } else if (days_to_next >= 1) {
      status <- "WARN"
      value <- sprintf("next fixture in %dd, no upcoming odds scraped", as.integer(days_to_next))
    } else {
      status <- "FAIL"
      value <- "fixture today, no odds scraped"
    }
    # Below "manual" no money rides on these odds: a stall is worth a WARN,
    # never the FAIL that fires the alert email (spec 2026-09-23 WS7).
    if (status == "FAIL" && !betting_mode_at_least(lg, "manual")) {
      status <- "WARN"
      value <- sprintf("%s (betting.mode: %s, capped at WARN)", value, mode)
    }
    rows[[key]] <- health_row("odds_freshness", key, status, value, thr_lbl)
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
#' @param leagues Named league config; when given, only recommendations from leagues at betting.mode "manual" or above count (paper recommendations are never placed, spec 2026-09-23 WS7). NULL or an empty list counts all.
#' @noRd
check_capture_rate <- function(root, now, th, leagues = NULL) {
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
  if (!is.null(leagues) && length(leagues) > 0L) {
    # pipeline_health() passes list() when load_leagues() failed: judge every
    # rec then rather than none (the config-error row already FAILs).
    placeable <- names(leagues)[
      vapply(leagues, betting_mode_at_least, logical(1), stage = "manual")
    ]
    recs <- recs[paste0(recs$sport, "_", recs$country) %in% placeable, , drop = FALSE]
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

#' Discovery: Lengjan now lists a modelled competition we do not yet scrape.
#'
#' Reads `data/discovery/proposals.json` and WARNs on any proposed competition
#' that is `modelled` and whose `comp_id` is not yet in
#' `config/leagues.yml::*.lengjan.competitions`. Keying "un-actioned" off the
#' LIVE config (not the proposal's `status`) makes the WARN self-clear the
#' instant the comp is wired, even before the next discovery run refreshes the
#' file. Never escalates past WARN -- a newly-listed league is not an outage.
#' @noRd
check_discovery <- function(root, th) {
  thr_lbl <- "0 un-actioned modelled competitions"
  path <- file.path(root, "discovery", "proposals.json")
  if (!file.exists(path)) {
    return(health_row("discovery", "lengjan", "OK", "no proposals file", thr_lbl))
  }
  prop <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  comps <- prop$competitions %||% list()
  if (length(comps) == 0L) {
    return(health_row("discovery", "lengjan", "OK", 0, thr_lbl))
  }
  leagues <- tryCatch(load_leagues(), error = function(e) list())
  unactioned <- Filter(function(comp) {
    isTRUE(comp$modelled) &&
      !(as.character(comp$comp_id) %in%
        .configured_comp_ids(leagues, comp$sport, comp$country))
  }, comps)
  n <- length(unactioned)
  if (n == 0L) {
    return(health_row("discovery", "lengjan", "OK", 0, thr_lbl))
  }
  labels <- vapply(unactioned, function(comp) {
    sprintf(
      "%s/%s %s (id=%s)", comp$sport, comp$inferred_sex %||% "?",
      comp$inferred_division %||% "?", comp$comp_id
    )
  }, character(1))
  health_row(
    "discovery", "lengjan", "WARN",
    paste0(n, ": ", paste(labels, collapse = "; ")), thr_lbl
  )
}

#' @noRd
check_placement_health <- function(root, now, th) {
  healthy <- c("placed", "nothing_pending", "ev_rejected", "daily_cap_reached")
  thr_lbl <- paste0("healthy run < ", th$placement_stale_fail_hours, "h when pending")

  pending <- tryCatch(
    {
      recs <- load_recommendations(root = root)
      if (nrow(recs) == 0L) {
        recs
      } else {
        recs <- recs[as.Date(recs$match_date) >= as.Date(now, tz = "UTC"), , drop = FALSE]
        dedup_against_ledger(recs, root = root)
      }
    },
    error = function(e) tibble::tibble()
  )
  n_pending <- nrow(pending)

  last <- read_placement_status(root)

  if (n_pending == 0L) {
    if (!is.null(last) && !(last$status %in% healthy)) {
      age_h <- as.numeric(difftime(now,
        as.POSIXct(last$run_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
        units = "hours"
      ))
      return(health_row(
        "placement_health", "auto_place", "WARN",
        sprintf("agent unhealthy: last=%s (%.0fh), 0 pending", last$status, age_h),
        thr_lbl
      ))
    }
    return(health_row(
      "placement_health", "auto_place", "OK",
      "no pending bets", thr_lbl
    ))
  }
  if (is.null(last)) {
    return(health_row(
      "placement_health", "auto_place", "WARN",
      sprintf("%d pending, auto-place never run", n_pending), thr_lbl
    ))
  }

  failed <- !(last$status %in% healthy)
  age_h <- as.numeric(difftime(now,
    as.POSIXct(last$run_at, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    units = "hours"
  ))

  status <- if (failed || age_h > th$placement_stale_fail_hours) {
    "FAIL"
  } else if (age_h > th$placement_stale_warn_hours) {
    "WARN"
  } else {
    "OK"
  }
  health_row(
    "placement_health", "auto_place", status,
    sprintf("%d pending, last=%s (%.0fh)", n_pending, last$status, age_h), thr_lbl
  )
}

#' Per-sex inverse maps: Lengjan rendering -> canonical (federation) name.
#'
#' The same inversion [normalise_lengjan_team_names()] builds at decide time,
#' hoisted so the health check measures exactly what the decide layer will see.
#' Injectivity is already a `load_leagues()` hard error, so no re-guard here.
#' @noRd
.lengjan_inverse_maps <- function(lg, sexes) {
  tn_all <- lg$lengjan$team_names
  out <- list()
  for (sx in sexes) {
    renders <- tn_renderings(if (is.null(tn_all)) NULL else tn_all[[sx]])
    out[[sx]] <- if (length(renders) == 0L) {
      stats::setNames(character(0), character(0))
    } else {
      stats::setNames(
        tag_utf8(rep(names(renders), lengths(renders))),
        tag_utf8(unlist(renders, use.names = FALSE))
      )
    }
  }
  out
}

#' Resolve an unmapped Lengjan name to the canonical name we model it under.
#'
#' `data/beliefs/latest` is keyed on `(match_date, home_team, away_team)` in
#' canonical names for *upcoming* fixtures. So for a fixture carrying the
#' unmapped name, we invert its already-mapped opponent to canonical and look
#' for the same date + same opponent on the same side; the counterpart's name
#' is the canonical the missing rendering belongs under. A hit means we hold a
#' model for this team and are provably unable to bet it -- the rename case.
#' No hit means no such fixture is modelled at all -- a genuinely new or
#' unmodelled side, which `config/leagues.yml` explicitly expects.
#'
#' Reads beliefs only when an unmapped name exists, so the common (clean) path
#' never touches the ~200k-row posterior-draw store.
#' @return Character vector of `"canonical" (team_names.sex)` labels; empty
#'   when nothing resolves.
#' @noRd
.identify_unmapped_canonical <- function(nm, fx, lg, inv, root, today) {
  up <- fx[!is.na(fx$match_date) & as.Date(fx$match_date) >= today, , drop = FALSE]
  if (nrow(up) == 0L) {
    return(character(0))
  }
  hits <- character(0)
  for (sx in names(inv)) {
    bl <- tryCatch(
      read_table("beliefs_latest",
        root = root,
        filter = list(sport = lg$sport, country = lg$country, sex = sx)
      ),
      error = function(e) tibble::tibble()
    )
    need <- c("match_date", "home_team", "away_team")
    if (nrow(bl) == 0L || !all(need %in% names(bl))) next
    bl <- unique(bl[, need, drop = FALSE])
    bl$match_date <- as.Date(bl$match_date)
    bl$home_team <- tag_utf8(bl$home_team)
    bl$away_team <- tag_utf8(bl$away_team)
    imap <- inv[[sx]]
    for (i in seq_len(nrow(up))) {
      at_home <- identical(up$home_team[i], nm)
      opp <- if (at_home) up$away_team[i] else up$home_team[i]
      opp_canon <- if (opp %in% names(imap)) unname(imap[[opp]]) else opp
      cand <- if (at_home) {
        bl[bl$match_date == as.Date(up$match_date[i]) & bl$away_team == opp_canon, , drop = FALSE]
      } else {
        bl[bl$match_date == as.Date(up$match_date[i]) & bl$home_team == opp_canon, , drop = FALSE]
      }
      if (nrow(cand) == 0L) next
      side <- unique(if (at_home) cand$home_team else cand$away_team)
      hits <- c(hits, sprintf("\"%s\" (team_names.%s)", side, sx))
    }
  }
  unique(hits)
}

#' @noRd
.fmt_unmapped <- function(f) {
  span <- if (identical(f$first, f$last)) {
    as.character(f$first)
  } else {
    sprintf("%s..%s", f$first, f$last)
  }
  fixtures <- sprintf("%d fixture%s", f$n_fixtures, if (f$n_fixtures == 1L) "" else "s")
  if (length(f$canonical) > 0L) {
    sprintf(
      "%s (%s, %s) -> modelled as %s", f$name, span, fixtures,
      paste(f$canonical, collapse = " / ")
    )
  } else {
    sprintf("%s (%s, %s) -> no beliefs match; likely a new team", f$name, span, fixtures)
  }
}

#' Lengjan odds names with no `team_names` rendering in config/leagues.yml.
#'
#' On 2026-07-17 Lengjan renamed "Vikingur Rvk" to "Vikingur Reykjavik"
#' mid-season. `config/leagues.yml` mapped only the old rendering, so
#' [normalise_lengjan_team_names()] left the name unmapped, `decide_league()`
#' warn-and-skipped, and 8 Besta deildin fixtures could never be bet. The
#' warning fired on every decide run for 6.5 weeks -- to the run log only.
#' This check promotes that signal to the health snapshot.
#'
#' It cannot be a build-time assertion: `config/leagues.yml` explicitly expects
#' unmapped names ("further LD3 sides will surface unmapped as their odds first
#' appear -- fill them opportunistically"), so a hard failure would be flaky.
#' Severity instead follows whether we *model* the team
#' (see [.identify_unmapped_canonical()]):
#'
#' * resolves to a canonical -> `FAIL`. We hold a fitted model and are losing
#'   every bet on it; same class as `check_odds_freshness`'s "fixture today,
#'   no odds scraped".
#' * resolves to nothing -> `WARN`. A new or unmodelled side, the expected case.
#'
#' Odds carry no `sex` or `division` column (partitions are
#' `sport/country/scraped_date`), so the rendering universe is the union across
#' the league's per-sex `team_names`; the beliefs join recovers the sex, and the
#' reported label names the sub-map the rendering belongs in. Self-clearing off
#' the live config, like [check_discovery()] -- the row goes `OK` the moment the
#' rendering lands, with no state file to reconcile. Leagues with no odds inside
#' the window emit no row, so the seasonal basketball/handball pause stays
#' silent.
#' @noRd
check_unmapped_team_names <- function(leagues, root, now, th) {
  today <- as.Date(now, tz = "UTC")
  thr_lbl <- "0 unmapped Lengjan renderings"
  cap <- 8L
  rows <- list()
  for (key in names(leagues)) {
    lg <- leagues[[key]]
    od <- tryCatch(
      read_table("odds",
        root = root,
        filter = list(sport = lg$sport, country = lg$country)
      ),
      error = function(e) tibble::tibble()
    )
    if (nrow(od) == 0L || !all(c("home_team", "away_team") %in% names(od))) next
    # scraped_date is a hive partition, so it reads back as character -- an
    # uncoerced `>=` against a Date silently compares the wrong things.
    sd <- if ("scraped_date" %in% names(od)) {
      as.Date(od$scraped_date)
    } else {
      as.Date(od$scraped_at, tz = "UTC")
    }
    keep <- !is.na(sd) & sd >= (today - th$unmapped_window_days)
    win <- od[keep, , drop = FALSE]
    if (nrow(win) == 0L) next # nothing scraped in the window: off-season
    win$scraped_date <- sd[keep]
    win$home_team <- tag_utf8(win$home_team)
    win$away_team <- tag_utf8(win$away_team)

    inv <- .lengjan_inverse_maps(lg, .cell_sexes(lg))
    renderings <- unique(unlist(lapply(inv, names), use.names = FALSE))
    unmapped <- setdiff(unique(c(win$home_team, win$away_team)), renderings)
    if (length(unmapped) == 0L) {
      rows[[key]] <- health_row("unmapped_team_names", key, "OK", 0, thr_lbl)
      next
    }

    findings <- lapply(unmapped, function(nm) {
      hit <- win[win$home_team == nm | win$away_team == nm, , drop = FALSE]
      fx <- unique(hit[, c("match_date", "home_team", "away_team"), drop = FALSE])
      list(
        name = nm,
        first = min(hit$scraped_date), last = max(hit$scraped_date),
        n_fixtures = nrow(fx),
        canonical = .identify_unmapped_canonical(nm, fx, lg, inv, root, today)
      )
    })
    modelled <- vapply(findings, function(f) length(f$canonical) > 0L, logical(1))
    # Modelled names first: they are the actionable, bet-losing ones, and the
    # label list is capped.
    labels <- vapply(c(findings[modelled], findings[!modelled]), .fmt_unmapped, character(1))
    shown <- utils::head(labels, cap)
    rows[[key]] <- health_row(
      "unmapped_team_names", key,
      if (any(modelled)) "FAIL" else "WARN",
      sprintf(
        "%d unmapped: %s%s", length(labels), paste(shown, collapse = "; "),
        if (length(labels) > cap) sprintf("; +%d more", length(labels) - cap) else ""
      ),
      thr_lbl
    )
  }
  if (length(rows) == 0L) health_empty() else dplyr::bind_rows(rows)
}

#' Read-only pipeline health snapshot.
#'
#' Composes freshness, persisted Stan-diagnostics drift, orphaned-bet,
#' placement-capture-rate, bankroll, unmapped-Lengjan-team-name,
#' publish-freshness, season-resolution and publish-format checks into one
#' tibble of
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
#' `check_error` is the one check name that means "this did not run", as
#' opposed to every other name, which means "this ran and judged". Such rows
#' are always `FAIL`, never `WARN`: the sole alert channel fires on
#' `overall == "FAIL"` (see HONEST LIMIT below), so a crashed check reported as
#' `WARN` was invisible to the only thing watching. They are emitted for a
#' `load_leagues()` failure (scope `load_leagues`, which used to be swallowed
#' into an empty, green snapshot), for a per-cell read failure inside
#' [check_fit_freshness()] (scope = the cell), and by the `safe()` wrapper for
#' anything else that aborts (scope `pipeline_health`).
#'
#' The three publish-side checks were added 2026-09-04. Until then NOTHING here
#' read `data/publish/`, which is how basketball and handball published nothing
#' at all from the Plan-7 cutover to 2026-09 while every composed check stayed
#' green. `check_publish_freshness()` is the row that would have said so.
#'
#' HONEST LIMIT. The alert channel is a GitHub workflow-failure email: signal,
#' not a pager. `healthcheck.yml` runs twice daily and fails the run on
#' `overall == "FAIL"`; there is no push notification, no escalation and no
#' on-call, so a FAIL is noticed within roughly twelve hours if the maintainer
#' reads mail and not at all if they do not. Because the channel is that
#' low-bandwidth, a permanently-WARN check is worse than no check -- which is
#' why `check_season_resolution()` scopes FAIL to the league divisions and
#' leaves federation-deferred cups at WARN.
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
  # A config that will not parse is the loudest failure in this file, not an
  # empty clean report. Coerced to `list()`, every per-league check returned
  # zero rows and overall_health_status() collapsed to "OK": the snapshot went
  # green *because* its central input was unreadable. Aborting would be worse
  # than a row -- 07_healthcheck.R would die before writing status.json, and
  # healthcheck.yml's `if: always()` alert step would then read the last
  # COMMITTED snapshot and decide on stale data -- so the message is carried
  # into a FAIL row and the root-scoped checks below still run.
  config_error <- NULL
  if (is.null(leagues)) {
    leagues <- tryCatch(load_leagues(), error = function(e) {
      config_error <<- conditionMessage(e)
      list()
    })
  }
  # A sub-check that CRASHED is not a soft signal. This used to downgrade any
  # error to WARN, but the only alert channel -- healthcheck.yml failing the run
  # on `overall == "FAIL"` so GitHub emails the maintainer -- fires on FAIL
  # alone, so a crashed check was unreachable by it: the snapshot silently lost
  # a whole check's worth of coverage and nobody was told. Crashes therefore
  # FAIL. The severity vocabulary is deliberately unchanged (OK < WARN < FAIL,
  # plus PAUSED) -- a fourth status would drop straight out of
  # 07_healthcheck.R's `status %in% c("WARN", "FAIL")` breach printer and out of
  # write_health_status()'s n_fail/n_warn counts. What separates a crash from a
  # breached threshold is the `check` column: "check_error" means the check
  # itself died, any other name means it ran and judged.
  safe <- function(expr) {
    tryCatch(expr, error = function(e) {
      health_row("check_error", "pipeline_health", "FAIL", conditionMessage(e), "n/a")
    })
  }
  dplyr::bind_rows(
    if (is.null(config_error)) {
      NULL
    } else {
      health_row(
        "check_error", "load_leagues", "FAIL",
        paste0("load_leagues() errored: ", config_error), "config parses"
      )
    },
    safe(check_fit_freshness(leagues, root, now, th)),
    safe(check_odds_freshness(leagues, root, now, th)),
    safe(check_diagnostics_drift(root, th)),
    safe(check_orphaned_bets(root, now, th)),
    safe(check_capture_rate(root, now, th, leagues)),
    safe(check_placement_health(root, now, th)),
    safe(check_bankroll(root, th)),
    safe(check_discovery(root, th)),
    safe(check_unmapped_team_names(leagues, root, now, th)),
    # The publish-side checks sort last so the new rows read as a block in the
    # printed table.
    safe(check_publish_freshness(leagues, root, now, th)),
    safe(check_season_resolution(leagues, root, now)),
    safe(check_publish_format_agreement(leagues, root))
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

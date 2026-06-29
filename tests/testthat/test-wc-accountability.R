# ---- Accountability (results.json: "did the model call it?") ----------------

# A minimal predictions tibble (the sim_out$predictions shape).
.acc_pred <- function(match_date, group, home, away, ph, pd, pa) {
  tibble::tibble(
    match_date = match_date, group = group, home = home, away = away,
    p_home = ph, p_draw = pd, p_away = pa, eg_home = 1.5, eg_away = 1.0
  )
}

.acc_log <- function(root) {
  jsonlite::read_json(
    file.path(root, "wc", "accountability", "prediction_log.json"),
    simplifyVector = TRUE
  )
}

# Predictions tibble carrying the four goal distributions (the full contract).
.acc_pred_dist <- function(match_date, group, home, away, ph, pd, pa) {
  one <- function(vfield, vals, ps) {
    df <- tibble::tibble(p = ps)
    df[[vfield]] <- vals
    list(df[, c(vfield, "p")])
  }
  tibble::tibble(
    match_date = match_date, group = group, home = home, away = away,
    p_home = ph, p_draw = pd, p_away = pa, eg_home = 1.6, eg_away = 0.7,
    goal_diff_distribution = one("diff", c(-1L, 0L, 1L, 2L), c(0.15, 0.25, 0.35, 0.25)),
    home_goal_distribution = one("goals", c(0L, 1L, 2L, 3L), c(0.2, 0.4, 0.3, 0.1)),
    away_goal_distribution = one("goals", c(0L, 1L, 2L), c(0.5, 0.35, 0.15)),
    total_goal_distribution = one("total", c(1L, 2L, 3L, 4L), c(0.25, 0.35, 0.25, 0.15))
  )
}

test_that("wc_snapshot_predictions upserts the latest pre-match prediction per fixture", {
  root <- withr::local_tempdir()
  wc_snapshot_predictions(
    .acc_pred("2026-06-20", "A", "Argentina", "Tunisia", 0.50, 0.25, 0.25),
    fit_date = "2026-06-18", root = root
  )
  # A later fit revises the same fixture -> the log keeps ONE row, the latest.
  wc_snapshot_predictions(
    .acc_pred("2026-06-20", "A", "Argentina", "Tunisia", 0.64, 0.22, 0.14),
    fit_date = "2026-06-19", root = root
  )
  log <- .acc_log(root)
  expect_equal(nrow(log$matches), 1L)
  expect_equal(log$matches$fit_date, "2026-06-19")
  expect_equal(log$matches$p_home, 0.64)
})

test_that("wc_snapshot_predictions rejects predictions made after match day", {
  root <- withr::local_tempdir()
  # fit_date AFTER match_date is not a pre-match prediction -> dropped.
  wc_snapshot_predictions(
    .acc_pred("2026-06-10", "A", "X", "Y", 0.4, 0.3, 0.3),
    fit_date = "2026-06-12", root = root
  )
  expect_equal(NROW(.acc_log(root)$matches), 0L)
})

test_that("wc_snapshot_predictions tolerates a logged null-group (knockout) match", {
  root <- withr::local_tempdir()
  # Knockout cards carry no `group`, so bind_rows leaves group = NA and the
  # snapshot serializes it to JSON null. Snapshot it as a future fixture.
  wc_snapshot_predictions(
    .acc_pred("2026-06-28", NA_character_, "South Africa", "Canada", 0.45, 0.27, 0.28),
    fit_date = "2026-06-25", root = root
  )
  # On the next run that fixture has passed (fit_date > match_date), so it is NOT
  # re-upserted -- the null-group entry is read back from the log (as NULL) and
  # must not break the order() over the store.
  expect_no_error(
    wc_snapshot_predictions(
      .acc_pred("2026-06-30", "A", "France", "Sweden", 0.50, 0.25, 0.25),
      fit_date = "2026-06-29", root = root
    )
  )
  log <- .acc_log(root)
  expect_equal(nrow(log$matches), 2L)
})

test_that("wc_build_results pairs a played fixture with its pre-match prediction", {
  root <- withr::local_tempdir()
  wc_snapshot_predictions(
    .acc_pred("2026-06-20", "A", "Argentina", "Tunisia", 0.64, 0.22, 0.14),
    fit_date = "2026-06-19", root = root
  )
  fx <- tibble::tibble(
    match_date = as.Date("2026-06-20"), group = "A",
    home_team = "Argentina", away_team = "Tunisia",
    home_score = 2L, away_score = 0L, played = TRUE
  )
  res <- wc_build_results(fx, root = root)
  expect_equal(res$summary$n_played, 1L)
  expect_equal(res$summary$n_hit, 1L) # home win predicted + happened
  m <- res$matches[[1]]
  expect_equal(m$outcome, "H")
  expect_equal(m$p_outcome, 0.64) # prob given to the actual result
  expect_true(m$hit)
  expect_equal(m$home_score, 2L)
  expect_equal(m$pred_fit_date, "2026-06-19")
  expect_equal(m$home_is, "Argentina") # identity namer in the default
})

test_that("wc_build_results emits pred_dist (4 metrics) + actual for the score view", {
  root <- withr::local_tempdir()
  wc_snapshot_predictions(
    .acc_pred_dist("2026-06-20", "A", "Argentina", "Tunisia", 0.64, 0.22, 0.14),
    fit_date = "2026-06-19", root = root
  )
  fx <- tibble::tibble(
    match_date = as.Date("2026-06-20"), group = "A",
    home_team = "Argentina", away_team = "Tunisia",
    home_score = 2L, away_score = 1L, played = TRUE
  )
  res <- wc_build_results(fx, root = root)
  m <- res$matches[[1]]
  # Predicted distributions round-tripped through the snapshot for all 4 metrics.
  expect_setequal(names(m$pred_dist), c("home", "away", "total", "diff"))
  expect_true(all(c("diff", "p") %in% names(m$pred_dist$diff[[1]])))
  expect_true(all(c("goals", "p") %in% names(m$pred_dist$home[[1]])))
  expect_true(all(c("total", "p") %in% names(m$pred_dist$total[[1]])))
  # Actuals derived from the scoreline (2-1).
  expect_equal(m$actual$home, 2L)
  expect_equal(m$actual$away, 1L)
  expect_equal(m$actual$total, 3L)
  expect_equal(m$actual$diff, 1L)
})

test_that("wc_build_results carries only goal-diff when that's all the snapshot holds", {
  # Mirrors a backfilled-from-history match: goal_diff_distribution only.
  root <- withr::local_tempdir()
  pred <- .acc_pred("2026-06-20", "A", "Argentina", "Tunisia", 0.64, 0.22, 0.14)
  df <- tibble::tibble(diff = c(0L, 1L, 2L), p = c(0.3, 0.45, 0.25))
  pred$goal_diff_distribution <- list(df)
  wc_snapshot_predictions(pred, fit_date = "2026-06-19", root = root)
  fx <- tibble::tibble(
    match_date = as.Date("2026-06-20"), group = "A",
    home_team = "Argentina", away_team = "Tunisia",
    home_score = 2L, away_score = 0L, played = TRUE
  )
  m <- wc_build_results(fx, root = root)$matches[[1]]
  expect_setequal(names(m$pred_dist), "diff") # home/away/total absent
  expect_equal(m$actual$diff, 2L)
})

test_that("wc_build_results records a miss (upset) and computes surprise", {
  root <- withr::local_tempdir()
  wc_snapshot_predictions(
    .acc_pred("2026-06-20", "B", "Germany", "Japan", 0.55, 0.26, 0.19),
    fit_date = "2026-06-19", root = root
  )
  fx <- tibble::tibble(
    match_date = as.Date("2026-06-20"), group = "B",
    home_team = "Germany", away_team = "Japan",
    home_score = 1L, away_score = 2L, played = TRUE
  )
  res <- wc_build_results(fx, root = root)
  m <- res$matches[[1]]
  expect_equal(m$outcome, "A")
  expect_false(m$hit) # model called home, away won
  expect_equal(m$p_outcome, 0.19)
  expect_equal(m$surprise, 0.81)
})

test_that("wc_build_results is empty when no played fixture has a snapshot", {
  root <- withr::local_tempdir()
  fx <- tibble::tibble(
    match_date = as.Date("2026-06-20"), group = "A",
    home_team = "Argentina", away_team = "Tunisia",
    home_score = 2L, away_score = 0L, played = TRUE
  )
  # No log written at all -> graceful empty shape (section stays gated off).
  res <- wc_build_results(fx, root = root)
  expect_equal(res$summary$n_played, 0L)
  expect_length(res$matches, 0L)
})

test_that("wc_build_results sorts most-recent-first and summarises hit-rate", {
  root <- withr::local_tempdir()
  wc_snapshot_predictions(
    .acc_pred("2026-06-18", "A", "Spain", "Qatar", 0.78, 0.15, 0.07),
    fit_date = "2026-06-17", root = root
  )
  wc_snapshot_predictions(
    .acc_pred("2026-06-20", "A", "Brazil", "Serbia", 0.58, 0.25, 0.17),
    fit_date = "2026-06-19", root = root
  )
  fx <- tibble::tibble(
    match_date = as.Date(c("2026-06-18", "2026-06-20")), group = c("A", "A"),
    home_team = c("Spain", "Brazil"), away_team = c("Qatar", "Serbia"),
    home_score = c(3L, 2L), away_score = c(0L, 1L), played = c(TRUE, TRUE)
  )
  res <- wc_build_results(fx, root = root)
  expect_equal(res$summary$n_played, 2L)
  expect_equal(res$summary$n_hit, 2L)
  expect_equal(res$summary$hit_rate, 1)
  # Most recent first.
  expect_equal(res$matches[[1]]$match_date, "2026-06-20")
  expect_equal(res$matches[[2]]$match_date, "2026-06-18")
})

test_that("publish_world_cup writes results.json with the contract shape", {
  s <- wc_structure()
  fx <- make_wc_fixtures(s)
  teams <- unlist(s$groups, use.names = FALSE)
  si <- make_sim_inputs(teams, n_draws = 50L)
  out <- simulate_world_cup(si$team, si$scalar, fx, s, pairing_seed = 1L)
  root <- withr::local_tempdir()
  publish_world_cup(out, si$team, s, fx, root = root)

  res <- jsonlite::read_json(
    file.path(root, "publish", "world_cup", "karla", "results.json")
  )
  expect_true(all(c("generated_at", "summary", "matches") %in% names(res)))
  expect_setequal(
    names(res$summary),
    c("n_played", "n_hit", "hit_rate", "mean_p_outcome")
  )
  # The synthetic schedule is all-upcoming -> no played fixtures -> empty
  # matches (the gated section stays off), but the snapshot log was written.
  expect_length(res$matches, 0L)
  expect_true(file.exists(
    file.path(root, "wc", "accountability", "prediction_log.json")
  ))
})

test_that("wc_backfill_snapshots no-ops gracefully when there is no history", {
  root <- withr::local_tempdir()
  expect_warning(
    n <- wc_backfill_snapshots(
      root = root, repo = here::here(),
      predictions_path = "data/publish/world_cup/karla/does-not-exist.json"
    ),
    "no predictions.json history"
  )
  expect_equal(n, 0L)
  expect_false(file.exists(
    file.path(root, "wc", "accountability", "prediction_log.json")
  ))
})

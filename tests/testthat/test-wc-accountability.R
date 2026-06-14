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

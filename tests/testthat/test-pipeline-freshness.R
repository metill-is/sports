test_that("needs_refit() is TRUE when no fit exists", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  expect_true(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is FALSE when fit covers all completed games", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  # needs_refit() also checks beliefs/latest/ — production state has both.
  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() is TRUE when a new game was played after last fit", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "C"), away_team = c("B", "D"),
      match_date = as.Date(c("2026-04-29", "2026-05-02")),
      home_score = c(1L, 2L), away_score = c(0L, 1L),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_true(needs_refit(static, "male", root = root))
})

test_that("needs_refit() ignores unplayed games", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "C"), away_team = c("B", "D"),
      match_date = as.Date(c("2026-04-29", "2026-05-15")),
      home_score = c(1L, NA_integer_),
      away_score = c(0L, NA_integer_),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() consults beliefs/extracts/ as a fit-date source", {
  # Post Phase 3b (2026-05-04), fit_league() skips the legacy beliefs_archive
  # write for football iceland — extracts/ is the canonical per-fit
  # accretive store for that league. needs_refit() must read both stores
  # and take the max, otherwise football iceland's archive freezes at the
  # last pre-Phase-3b partition and refits run unconditionally.
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-05-20"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  # Only extracts/ partition exists for the post-match fit. No archive
  # partition — mirrors football iceland's on-disk state post Phase 3b.
  extracts_dir <- fs::path(
    root, "beliefs", "extracts",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-05-21"
  )
  fs::dir_create(extracts_dir)
  fs::file_create(fs::path(extracts_dir, "final_positions.parquet"))

  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() takes max(extracts/, archive/) when both exist", {
  # During the transition window, archive/ may carry yesterday's fit while
  # extracts/ carries today's. needs_refit() should see the freshest of
  # the two and not refit unnecessarily.
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-05-04"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  # Stale archive — would force a refit if it were the only source consulted.
  archive_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-05-03"
  )
  fs::dir_create(archive_dir)
  fs::file_create(fs::path(archive_dir, "part-0.parquet"))

  # Fresh extracts covers the new match.
  extracts_dir <- fs::path(
    root, "beliefs", "extracts",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-05-04"
  )
  fs::dir_create(extracts_dir)
  fs::file_create(fs::path(extracts_dir, "final_positions.parquet"))

  latest_dir <- fs::path(
    root, "beliefs", "latest",
    "sport=football", "country=iceland", "sex=male"
  )
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  expect_false(needs_refit(static, "male", root = root))
})

test_that("needs_refit() returns TRUE when beliefs/latest/ is wiped despite archive history", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  results_dir <- fs::path(
    root, "facts", "results",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B",
      match_date = as.Date("2026-04-29"),
      home_score = 1L, away_score = 0L,
      division = NA_character_, round = NA_integer_
    ),
    fs::path(results_dir, "part-0.parquet")
  )

  # archive has a fit_date but latest/ is empty (post-2026-05-15 defence-in-
  # depth against latest/-wipe scenarios) — refit forced.
  fit_dir <- fs::path(
    root, "beliefs", "archive",
    "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-30"
  )
  fs::dir_create(fit_dir)
  fs::file_create(fs::path(fit_dir, "beliefs.parquet"))

  expect_true(needs_refit(static, "male", root = root))
})

test_that("has_upcoming_games() filters by horizon", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")

  sched_dir <- fs::path(
    root, "facts", "schedules",
    "sport=football", "country=iceland", "sex=male", "season=2026"
  )
  fs::dir_create(sched_dir)
  today <- Sys.Date()
  arrow::write_parquet(
    tibble::tibble(
      home_team = c("A", "B", "C"), away_team = c("X", "Y", "Z"),
      match_date = c(today + 3L, today + 30L, today - 1L),
      division = NA_character_, round = NA_integer_
    ),
    fs::path(sched_dir, "part-0.parquet")
  )

  expect_true(has_upcoming_games(static, "male", root = root, days = 14L))
  expect_false(has_upcoming_games(static, "male", root = root, days = 1L))
})

test_that("has_upcoming_games() returns FALSE when schedule dir missing", {
  root <- withr::local_tempdir()
  static <- list(sport = "football", country = "iceland")
  expect_false(has_upcoming_games(static, "male", root = root))
})

# --- fit_skip_reason(): the scripts/03_fit.R skip decision -------------------
# The predicates are mocked so these pin the --force / explicit-league
# semantics, not the predicates themselves (those are covered above).

test_that("fit_skip_reason() skips when not forced and there are no new games", {
  testthat::local_mocked_bindings(
    needs_refit = function(...) FALSE,
    has_upcoming_games = function(...) TRUE
  )
  static <- list(sport = "football", country = "iceland")
  reason <- fit_skip_reason(static, "male", force = FALSE, league_named = FALSE)
  expect_match(reason, "no new games")
})

test_that("fit_skip_reason() fits an in-season league that needs a refit", {
  testthat::local_mocked_bindings(
    needs_refit = function(...) TRUE,
    has_upcoming_games = function(...) TRUE
  )
  static <- list(sport = "football", country = "iceland")
  expect_null(fit_skip_reason(static, "male", force = FALSE, league_named = FALSE))
})

test_that("fit_skip_reason() skips a paused league under a bulk --force", {
  # CI-email regression: a manual force-all fit must not attempt an off-season
  # (no-upcoming-games) league and trip the Stan diagnostic gate.
  testthat::local_mocked_bindings(
    needs_refit = function(...) TRUE,
    has_upcoming_games = function(...) FALSE
  )
  static <- list(sport = "basketball", country = "iceland")
  reason <- fit_skip_reason(static, "male", force = TRUE, league_named = FALSE)
  expect_match(reason, "no upcoming games")
})

test_that("fit_skip_reason() force-refits an in-season league with no new games", {
  testthat::local_mocked_bindings(
    needs_refit = function(...) FALSE,
    has_upcoming_games = function(...) TRUE
  )
  static <- list(sport = "football", country = "iceland")
  expect_null(fit_skip_reason(static, "male", force = TRUE, league_named = FALSE))
})

test_that("fit_skip_reason() honours an explicit --league even when paused", {
  # Explicit single-league intent overrides the paused skip, so
  # `--force --league basketball_iceland` still refits the off-season league.
  testthat::local_mocked_bindings(
    needs_refit = function(...) TRUE,
    has_upcoming_games = function(...) FALSE
  )
  static <- list(sport = "basketball", country = "iceland")
  expect_null(fit_skip_reason(static, "male", force = TRUE, league_named = TRUE))
})

test_that("fit_skip_reason() still applies the no-new-games guard to a named, unforced league", {
  testthat::local_mocked_bindings(
    needs_refit = function(...) FALSE,
    has_upcoming_games = function(...) TRUE
  )
  static <- list(sport = "basketball", country = "iceland")
  reason <- fit_skip_reason(static, "male", force = FALSE, league_named = TRUE)
  expect_match(reason, "no new games")
})

# --- needs_refit(): fixtures the newest fit never predicted ------------------
# Pre-season nothing is played after the last fit, so the played-games rule
# never fires, and basketball was never refit before its season (spec
# 2026-09-16 §8, F15). Dates are far-future and `today` is injected.

.seed_refit_cell <- function(root, fit_date, predicted = NULL,
                             schedule = NULL, store = "extracts") {
  cell <- c("sport=basketball", "country=iceland", "sex=male")
  at <- function(...) do.call(fs::path, as.list(c(root, ...)))

  results_dir <- at("facts", "results", cell, "season=2100")
  fs::dir_create(results_dir)
  arrow::write_parquet(
    tibble::tibble(
      home_team = "A", away_team = "B", match_date = as.Date("2100-04-01"),
      home_score = 80L, away_score = 70L, division = "BD", round = 22L
    ),
    fs::path(results_dir, "part-0.parquet")
  )
  latest_dir <- at("beliefs", "latest", cell)
  fs::dir_create(latest_dir)
  fs::file_create(fs::path(latest_dir, "part-0.parquet"))

  fit_dir <- at("beliefs", store, cell, paste0("fit_date=", fit_date))
  fs::dir_create(fit_dir)
  if (!is.null(predicted)) {
    file <- if (identical(store, "extracts")) {
      "predicted_matches.parquet"
    } else {
      "part-0.parquet"
    }
    arrow::write_parquet(predicted, fs::path(fit_dir, file))
  }
  if (!is.null(schedule)) {
    sched_dir <- at("facts", "schedules", cell, "season=2101")
    fs::dir_create(sched_dir)
    arrow::write_parquet(schedule, fs::path(sched_dir, "part-0.parquet"))
  }
  invisible(fit_dir)
}

.fixture_rows <- function(dates, home = "A", away = "B") {
  tibble::tibble(
    home_team = home, away_team = away, match_date = as.Date(dates),
    division = "BD", round = NA_integer_
  )
}

.bb_static <- list(sport = "basketball", country = "iceland")

test_that("needs_refit() is TRUE when the horizon holds only fixtures the newest fit never predicted", {
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-04-02",
    predicted = .fixture_rows("2100-04-05"),
    schedule = .fixture_rows(c("2100-09-29", "2100-09-30"))
  )
  expect_true(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})

test_that("needs_refit() stays FALSE while the newest fit predicted part of the horizon", {
  # In season the window gains a fixture most days; refitting for that alone
  # would refit daily without a game having been played.
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-09-21",
    predicted = .fixture_rows("2100-09-29"),
    schedule = .fixture_rows(
      c("2100-09-29", "2100-10-05"),
      home = c("A", "C"), away = c("B", "D")
    )
  )
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-24")))
})

test_that("needs_refit() ignores fixtures beyond the horizon", {
  root <- withr::local_tempdir()
  .seed_refit_cell(root, "2100-04-02",
    predicted = .fixture_rows("2100-04-05"),
    schedule = .fixture_rows("2100-10-20")
  )
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})

test_that("needs_refit() reads an archive partition when it is the newest fit", {
  long_form <- function(date) {
    tibble::tibble(
      match_date = as.Date(date), home_team = "A", away_team = "B",
      draw_id = 1:3, home_goals = 80, away_goals = 75
    )
  }
  covered <- withr::local_tempdir()
  .seed_refit_cell(covered, "2100-09-21",
    predicted = long_form("2100-09-29"),
    schedule = .fixture_rows("2100-09-29"), store = "archive"
  )
  expect_false(needs_refit(.bb_static, "male", root = covered, today = as.Date("2100-09-24")))

  stale <- withr::local_tempdir()
  .seed_refit_cell(stale, "2100-04-02",
    predicted = long_form("2100-04-05"),
    schedule = .fixture_rows("2100-09-29"), store = "archive"
  )
  expect_true(needs_refit(.bb_static, "male", root = stale, today = as.Date("2100-09-24")))
})

test_that("needs_refit() does not trigger on a prediction file it cannot read", {
  # Unknown is not uncovered: an unreadable file must not start a fit.
  root <- withr::local_tempdir()
  fit_dir <- .seed_refit_cell(root, "2100-04-02",
    schedule = .fixture_rows("2100-09-29")
  )
  fs::file_create(fs::path(fit_dir, "predicted_matches.parquet"))
  expect_false(needs_refit(.bb_static, "male", root = root, today = as.Date("2100-09-20")))
})

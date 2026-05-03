# Tests for the round-predictions pipeline added to publish_football_iceland:
# pre-round (frozen) xG/xPts/xPoints per (matchweek, team), sourced from the
# beliefs archive (the latest fit_date strictly less than the matchweek's
# first kickoff). The point of the freeze is to prevent retroactive
# improvement -- once a round starts, no later fit may overwrite its row.

# ---- .assign_matchweeks_pfi() ------------------------------------------------

test_that(".assign_matchweeks_pfi: empty input returns empty tibble with matchweek col", {
  empty <- tibble::tibble(
    home_team = character(),
    away_team = character(),
    match_date = as.Date(character())
  )
  out <- sports:::.assign_matchweeks_pfi(empty)
  expect_s3_class(out, "tbl_df")
  expect_true("matchweek" %in% names(out))
  expect_equal(nrow(out), 0L)
  expect_type(out$matchweek, "integer")
})

test_that(".assign_matchweeks_pfi: 4-team synchronised round 1 -> all matchweek=1", {
  matches <- tibble::tibble(
    home_team  = c("A", "C"),
    away_team  = c("B", "D"),
    match_date = as.Date(c("2026-04-01", "2026-04-01"))
  )
  out <- sports:::.assign_matchweeks_pfi(matches)
  expect_equal(out$matchweek, c(1L, 1L))
})

test_that(".assign_matchweeks_pfi: round 2 increments per team", {
  matches <- tibble::tibble(
    home_team  = c("A", "C", "A", "B"),
    away_team  = c("B", "D", "C", "D"),
    match_date = as.Date(c("2026-04-01", "2026-04-01", "2026-04-08", "2026-04-08"))
  )
  out <- sports:::.assign_matchweeks_pfi(matches)
  expect_equal(out$matchweek, c(1L, 1L, 2L, 2L))
})

test_that(".assign_matchweeks_pfi: postponed match keyed to team-chronological round", {
  # 4-team league. R1 + R2 played normally. R3 had two scheduled matches
  # (A-D and C-B); A-D played on 04-15, C-B postponed to 04-22.
  # Per-team chronological counts:
  #   A: 04-01(1), 04-08(2), 04-15(3)
  #   B: 04-01(1), 04-08(2), 04-22(3)
  #   C: 04-01(1), 04-08(2), 04-22(3)
  #   D: 04-01(1), 04-08(2), 04-15(3)
  # The postponed match resolves to round 3 because both teams' chronological
  # count is 3 at that point.
  matches <- tibble::tibble(
    home_team = c("A", "C", "A", "B", "A", "C"),
    away_team = c("B", "D", "C", "D", "D", "B"),
    match_date = as.Date(c(
      "2026-04-01", "2026-04-01",
      "2026-04-08", "2026-04-08",
      "2026-04-15",
      "2026-04-22"
    ))
  )
  out <- sports:::.assign_matchweeks_pfi(matches)
  expect_equal(out$matchweek, c(1L, 1L, 2L, 2L, 3L, 3L))
})

test_that(".assign_matchweeks_pfi: preserves input columns", {
  matches <- tibble::tibble(
    home_team  = c("A"),
    away_team  = c("B"),
    match_date = as.Date(c("2026-04-01")),
    home_score = 2L,
    away_score = 1L
  )
  out <- sports:::.assign_matchweeks_pfi(matches)
  expect_equal(out$home_score, 2L)
  expect_equal(out$away_score, 1L)
})

# ---- .find_pre_round_fit_path_pfi() -----------------------------------------

test_that(".find_pre_round_fit_path_pfi: returns NULL when archive empty", {
  tmp <- withr::local_tempdir()
  out <- sports:::.find_pre_round_fit_path_pfi(
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    target_date = as.Date("2026-05-01")
  )
  expect_null(out)
})

test_that(".find_pre_round_fit_path_pfi: returns latest fit_date strictly less than target", {
  tmp <- withr::local_tempdir()
  base <- file.path(
    tmp, "sport=football", "country=iceland", "sex=male"
  )
  for (d in c("2026-04-24", "2026-04-27", "2026-04-29")) {
    pdir <- file.path(base, paste0("fit_date=", d))
    dir.create(pdir, recursive = TRUE)
    file.create(file.path(pdir, "part-0.parquet"))
  }
  out <- sports:::.find_pre_round_fit_path_pfi(
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    target_date = as.Date("2026-05-01")
  )
  expect_type(out, "character")
  expect_match(out, "fit_date=2026-04-29")
})

test_that(".find_pre_round_fit_path_pfi: target equal to a fit_date is excluded (strictly less)", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  for (d in c("2026-04-24", "2026-04-29")) {
    pdir <- file.path(base, paste0("fit_date=", d))
    dir.create(pdir, recursive = TRUE)
    file.create(file.path(pdir, "part-0.parquet"))
  }
  out <- sports:::.find_pre_round_fit_path_pfi(
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    target_date = as.Date("2026-04-29")
  )
  expect_match(out, "fit_date=2026-04-24")
})

test_that(".find_pre_round_fit_path_pfi: returns NULL when target precedes earliest fit_date", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  pdir <- file.path(base, "fit_date=2026-04-24")
  dir.create(pdir, recursive = TRUE)
  file.create(file.path(pdir, "part-0.parquet"))
  out <- sports:::.find_pre_round_fit_path_pfi(
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male",
    target_date = as.Date("2026-04-01")
  )
  expect_null(out)
})

# ---- .aggregate_round_predictions_pfi() -------------------------------------

test_that(".aggregate_round_predictions_pfi: returns one row per (round, team) with required cols", {
  tmp <- withr::local_tempdir()
  pdir <- file.path(
    tmp, "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-10"
  )
  dir.create(pdir, recursive = TRUE)
  draws <- 100L
  beliefs <- dplyr::bind_rows(
    tibble::tibble(
      fit_date = as.Date("2026-04-10"),
      match_date = as.Date("2026-04-15"),
      home_team = "A", away_team = "B",
      draw_id = seq_len(draws),
      home_goals = stats::rpois(draws, lambda = 1.5),
      away_goals = stats::rpois(draws, lambda = 0.8),
      country = "iceland", sex = "male", sport = "football"
    ),
    tibble::tibble(
      fit_date = as.Date("2026-04-10"),
      match_date = as.Date("2026-04-15"),
      home_team = "C", away_team = "D",
      draw_id = seq_len(draws),
      home_goals = stats::rpois(draws, lambda = 1.0),
      away_goals = stats::rpois(draws, lambda = 1.2),
      country = "iceland", sex = "male", sport = "football"
    )
  )
  arrow::write_parquet(beliefs, file.path(pdir, "part-0.parquet"))

  played <- tibble::tibble(
    home_team  = c("A", "C"),
    away_team  = c("B", "D"),
    match_date = as.Date(c("2026-04-15", "2026-04-15")),
    home_score = c(2L, 1L),
    away_score = c(1L, 2L)
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male"
  )

  expect_s3_class(out, "tbl_df")
  expect_setequal(
    names(out),
    c(
      "round", "team", "fit_date", "n_matches",
      "xg_for", "xg_against", "xpts",
      "p_win", "p_draw", "p_loss"
    )
  )
  expect_equal(nrow(out), 4L)
  expect_setequal(out$team, c("A", "B", "C", "D"))
  expect_true(all(out$round == 1L))
  expect_true(all(out$n_matches == 1L))
  expect_true(all(out$fit_date == "2026-04-10"))
})

test_that(".aggregate_round_predictions_pfi: skips matchweeks with no pre-round fit", {
  tmp <- withr::local_tempdir()
  pdir <- file.path(
    tmp, "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-20"
  )
  dir.create(pdir, recursive = TRUE)
  beliefs <- tibble::tibble(
    fit_date = as.Date("2026-04-20"),
    match_date = as.Date("2026-04-22"),
    home_team = "A", away_team = "B",
    draw_id = seq_len(50L),
    home_goals = stats::rpois(50L, 1.5),
    away_goals = stats::rpois(50L, 0.8),
    country = "iceland", sex = "male", sport = "football"
  )
  arrow::write_parquet(beliefs, file.path(pdir, "part-0.parquet"))

  played <- tibble::tibble(
    home_team  = c("A", "A"),
    away_team  = c("B", "B"),
    match_date = as.Date(c("2026-04-15", "2026-04-22")),
    home_score = c(0L, 1L),
    away_score = c(0L, 1L)
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male"
  )

  expect_true(all(out$round == 2L))
  expect_setequal(out$team, c("A", "B"))
})

test_that(".aggregate_round_predictions_pfi: xpts equals 3*p_win + p_draw", {
  tmp <- withr::local_tempdir()
  pdir <- file.path(
    tmp, "sport=football", "country=iceland", "sex=male", "fit_date=2026-04-10"
  )
  dir.create(pdir, recursive = TRUE)
  set.seed(7L)
  draws <- 200L
  beliefs <- tibble::tibble(
    fit_date = as.Date("2026-04-10"),
    match_date = as.Date("2026-04-15"),
    home_team = "A", away_team = "B",
    draw_id = seq_len(draws),
    home_goals = stats::rpois(draws, lambda = 1.6),
    away_goals = stats::rpois(draws, lambda = 1.0),
    country = "iceland", sex = "male", sport = "football"
  )
  arrow::write_parquet(beliefs, file.path(pdir, "part-0.parquet"))

  played <- tibble::tibble(
    home_team  = "A", away_team = "B",
    match_date = as.Date("2026-04-15"),
    home_score = 2L, away_score = 1L
  )

  out <- sports:::.aggregate_round_predictions_pfi(
    played_matches = played,
    archive_root = tmp,
    sport = "football", country = "iceland", sex = "male"
  )

  for (team in c("A", "B")) {
    row <- out[out$team == team, ]
    expect_equal(row$xpts, 3 * row$p_win + row$p_draw, tolerance = 1e-9)
  }
})

# ---- Integration: publish_football_iceland writes round_predictions_history --

backup_fit_path_rp <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

test_that("publish_football_iceland: empty archive -> empty history JSON, NA standings xG", {
  fit_path <- backup_fit_path_rp("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  empty_archive <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-04-25")
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out,
      archive_root = empty_archive
    )
  )

  out_dir <- file.path(out, "football", "iceland", "karla")

  history_path <- file.path(out_dir, "round_predictions_history.json")
  expect_true(file.exists(history_path))
  parsed <- jsonlite::read_json(history_path)
  expect_true(all(c("schema_version", "records") %in% names(parsed)))
  expect_length(parsed$records, 0L)

  standings <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  if (length(standings$rows) > 0L) {
    for (row in standings$rows) {
      expect_null(row$xg_for, info = paste("team:", row$team))
      expect_null(row$xg_against, info = paste("team:", row$team))
      expect_null(row$xpts, info = paste("team:", row$team))
      expect_equal(length(row$xg_trend), 0L, info = paste("team:", row$team))
      expect_equal(
        row$n_predicted_matches, 0L,
        info = paste("team:", row$team)
      )
      expect_equal(
        row$n_played_matches, row$played,
        info = paste("team:", row$team)
      )
    }
  }
})

test_that("publish_football_iceland: partial archive -> partial cumulative xG with coverage indicators", {
  # Lookahead-free cumulative xG. With archive coverage for some rounds
  # but not all, we expect:
  #   * xg_for / xg_against / xpts are non-null (partial cumulative sums)
  #   * n_predicted_matches > 0 but < n_played_matches for at least one team
  #   * the xg vs goals comparison can be disclosed via the coverage fields
  fit_path <- backup_fit_path_rp("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }
  archive_root <- here::here("data", "beliefs", "archive")
  if (!dir.exists(archive_root)) {
    testthat::skip("beliefs archive absent -- cannot test partial coverage")
  }
  archive_male <- file.path(
    archive_root, "sport=football", "country=iceland", "sex=male"
  )
  fit_dates <- list.files(archive_male, pattern = "^fit_date=")
  if (length(fit_dates) == 0L) {
    testthat::skip("no fit_date partitions in male football archive")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = Sys.Date()
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = Sys.Date(),
      output_root = out,
      archive_root = archive_root
    )
  )

  out_dir <- file.path(out, "football", "iceland", "karla")
  standings <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  if (length(standings$rows) == 0L) {
    testthat::skip("no played matches in current season")
  }

  any_partial <- FALSE
  any_with_xg <- FALSE
  for (row in standings$rows) {
    expect_true(
      "n_predicted_matches" %in% names(row),
      info = paste("team:", row$team)
    )
    expect_true(
      "n_played_matches" %in% names(row),
      info = paste("team:", row$team)
    )
    expect_equal(
      row$n_played_matches, row$played,
      info = paste("team:", row$team)
    )
    if (row$n_predicted_matches > 0L) {
      any_with_xg <- TRUE
      expect_false(is.null(row$xg_for), info = paste("team:", row$team))
      expect_false(is.null(row$xg_against), info = paste("team:", row$team))
      expect_false(is.null(row$xpts), info = paste("team:", row$team))
      if (row$n_predicted_matches < row$n_played_matches) {
        any_partial <- TRUE
      }
    }
  }
  expect_true(
    any_with_xg,
    info = "expected at least one team to have non-null xg_for"
  )
  # If the archive's earliest fit_date is after some played matches' kickoffs,
  # we expect at least one team to be in the partial-coverage state. The
  # current data layout (round 4 played, archive starts mid-season) makes
  # this near-certain for the male side.
  expect_true(
    any_partial,
    info = "expected at least one team with n_predicted_matches < n_played_matches"
  )
})

test_that("team_strengths_history covers every played matchweek from a single fit", {
  # The strength-trajectory chart wants one trajectory point per matchweek
  # (umferð eftir umferð). The model holds posterior estimates for every
  # past round in `offense[1..N_rounds, K]`, so a single fit can populate
  # the whole trajectory -- we don't need to accrete per-fit snapshots.
  local_fit_path <- here::here(
    "data", "beliefs", "fits",
    "sport=football", "country=iceland", "sex=male", "fit.rds"
  )
  if (!file.exists(local_fit_path)) {
    testthat::skip("local football fit unavailable -- requires fresh local fit")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(local_fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-05-01")
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-05-01"),
      output_root = out
    )
  )

  hist <- jsonlite::fromJSON(
    file.path(out, "football", "iceland", "karla", "team_strengths_history.json"),
    simplifyDataFrame = TRUE
  )
  rounds <- sort(unique(hist$records$round))
  # Each BD karla team has played 4 matches; trajectory should span rounds 1..4.
  expect_true(all(c(1L, 2L, 3L, 4L) %in% rounds))
})

test_that("publish_football_iceland: standings xg_trend serialises as array (auto_unbox guard)", {
  # Regression: jsonlite::write_json(auto_unbox = TRUE) unboxes length-1
  # vectors, which broke the website's standings-table.js when only one
  # matchweek had a pre-round fit. The publisher must keep xg_trend as a
  # JSON array regardless of length (0, 1, or many). Run with the legacy
  # backup fit + the real archive (which currently covers only round 4),
  # producing per-team length-1 xg_trend arrays.
  fit_path <- backup_fit_path_rp("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }
  if (!dir.exists(here::here("data", "beliefs", "archive"))) {
    testthat::skip("archive absent -- nothing to predict from")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  extracted <- .build_extracted_football_for_test(
    fit, league,
    sex = "male", end_date = as.Date("2026-05-01")
  )

  suppressWarnings(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-05-01"),
      output_root = out
    )
  )

  standings <- jsonlite::read_json(
    file.path(out, "football", "iceland", "karla", "standings.json"),
    simplifyVector = FALSE
  )
  testthat::skip_if(length(standings$rows) == 0L, "standings empty -- cannot exercise xg_trend")

  for (row in standings$rows) {
    expect_type(row$xg_trend, "list")
  }
})

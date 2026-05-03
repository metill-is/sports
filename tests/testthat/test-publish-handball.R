# Locate the legacy backup fit. Defaults to the developer's local path; can
# be overridden with SPORTS_BACKUP_ROOT env var (or unset for CI to skip).
backup_handball_fit <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "handball", "iceland", "results", sex, "fit.rds")
}

test_that("publish_handball_iceland produces 2 JSONs for male", {
  fit_path <- backup_handball_fit("male")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy handball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["handball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_handball_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "handball", "iceland", "karla")
  for (f in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_equal(meta$sport, "handball")
  expect_equal(meta$sex, "male")
  expect_type(meta$n_draws, "integer")
  expect_true("season" %in% names(meta))

  ng <- jsonlite::read_json(file.path(out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_true("generated_at" %in% names(ng))
})

test_that("publish_handball_iceland produces 2 JSONs for female", {
  fit_path <- backup_handball_fit("female")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy handball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["handball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_handball_iceland(
      fit, league,
      sex = "female",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "handball", "iceland", "kvenna")
  for (f in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_equal(meta$sex, "female")
})

test_that("publish_handball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, bad_league, sex = "male"),
    "handball"
  )
})

test_that("publish_handball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "handball", country = "iceland")
  expect_error(
    publish_handball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})

test_that("publish_handball_iceland emits the 7 publish surface files (male)", {
  fit_path <- backup_handball_fit("male")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy handball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["handball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_handball_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "handball", "iceland", "karla")
  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (f in expected) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  ts <- jsonlite::read_json(file.path(out_dir, "team_strengths.json"))
  expect_true(all(c("generated_at", "records") %in% names(ts)))
  if (length(ts$records) > 0L) {
    components <- unique(vapply(ts$records, \(r) r$component, character(1)))
    locations <- unique(vapply(ts$records, \(r) r$location, character(1)))
    expect_setequal(components, c("offence", "defence", "total"))
    expect_setequal(locations, c("home", "away", "avg"))
  }

  fp <- jsonlite::read_json(file.path(out_dir, "final_positions.json"))
  expect_true(all(c("generated_at", "season", "n_teams", "records", "summary") %in% names(fp)))

  pd <- jsonlite::read_json(file.path(out_dir, "points_distribution.json"))
  expect_true(all(c("generated_at", "season", "records", "summary") %in% names(pd)))

  ha <- jsonlite::read_json(file.path(out_dir, "home_advantage.json"))
  expect_true(all(c("generated_at", "records") %in% names(ha)))
})

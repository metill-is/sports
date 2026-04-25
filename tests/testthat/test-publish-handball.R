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
      fit, league, sex = "male",
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
      fit, league, sex = "female",
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

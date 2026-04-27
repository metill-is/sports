# Locate the legacy backup fit. Defaults to the developer's local path; can
# be overridden with SPORTS_BACKUP_ROOT env var (or unset for CI to skip).
backup_basketball_fit <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "basketball", "iceland", "results", sex, "fit.rds")
}

test_that("publish_basketball_iceland produces 2 JSONs for male", {
  fit_path <- backup_basketball_fit("male")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy basketball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_basketball_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "basketball", "iceland", "karla")
  for (f in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_equal(meta$sport, "basketball")
  expect_equal(meta$sex, "male")
  expect_type(meta$n_draws, "integer")
  expect_true("season" %in% names(meta))

  ng <- jsonlite::read_json(file.path(out_dir, "next_games.json"))
  expect_true("matches" %in% names(ng))
  expect_true("generated_at" %in% names(ng))
})

test_that("publish_basketball_iceland produces 2 JSONs for female", {
  fit_path <- backup_basketball_fit("female")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy basketball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_basketball_iceland(
      fit, league,
      sex = "female",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "basketball", "iceland", "kvenna")
  for (f in c("meta.json", "next_games.json")) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  meta <- jsonlite::read_json(file.path(out_dir, "meta.json"))
  expect_equal(meta$sex, "female")
})

test_that("publish_basketball_iceland rejects wrong sport", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  bad_league <- list(sport = "football", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, bad_league, sex = "male"),
    "basketball"
  )
})

test_that("publish_basketball_iceland rejects invalid sex", {
  fake_fit <- structure(list(), class = "CmdStanMCMC")
  league <- list(sport = "basketball", country = "iceland")
  expect_error(
    publish_basketball_iceland(fake_fit, league, sex = "other"),
    "male.*female|female.*male"
  )
})

test_that("publish_basketball_iceland emits the 7 publish surface files (male)", {
  fit_path <- backup_basketball_fit("male")
  if (!file.exists(fit_path)) {
    testthat::skip(paste("legacy basketball fit unavailable:", fit_path))
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_basketball_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  out_dir <- file.path(out, "basketball", "iceland", "karla")
  expected <- c(
    "meta.json", "next_games.json", "standings.json",
    "team_strengths.json", "final_positions.json",
    "points_distribution.json", "home_advantage.json"
  )
  for (f in expected) {
    expect_true(file.exists(file.path(out_dir, f)), info = paste("missing:", f))
  }

  # standings.json structure
  st <- jsonlite::read_json(file.path(out_dir, "standings.json"))
  expect_true(all(c("generated_at", "season", "as_of", "rows") %in% names(st)))

  # team_strengths.json: offence/defence/total, home/away, three coverages
  ts <- jsonlite::read_json(file.path(out_dir, "team_strengths.json"))
  expect_true(all(c("generated_at", "records") %in% names(ts)))
  if (length(ts$records) > 0L) {
    components <- unique(vapply(ts$records, \(r) r$component, character(1)))
    locations <- unique(vapply(ts$records, \(r) r$location, character(1)))
    expect_setequal(components, c("offence", "defence", "total"))
    expect_setequal(locations, c("home", "away"))
  }

  # final_positions.json: placement distribution + summary
  fp <- jsonlite::read_json(file.path(out_dir, "final_positions.json"))
  expect_true(all(c("generated_at", "season", "n_teams", "records", "summary") %in% names(fp)))

  # points_distribution.json
  pd <- jsonlite::read_json(file.path(out_dir, "points_distribution.json"))
  expect_true(all(c("generated_at", "season", "records", "summary") %in% names(pd)))

  # home_advantage.json
  ha <- jsonlite::read_json(file.path(out_dir, "home_advantage.json"))
  expect_true(all(c("generated_at", "records") %in% names(ha)))
})

test_that("publish_basketball_iceland: standings has draws=0 (no ties in basketball)", {
  fit_path <- backup_basketball_fit("male")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy male basketball fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_basketball_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  st <- jsonlite::read_json(
    file.path(out, "basketball", "iceland", "karla", "standings.json")
  )
  if (length(st$rows) > 0L) {
    draws_vec <- vapply(st$rows, \(r) r$draws %||% 0L, integer(1))
    expect_true(all(draws_vec == 0L))
  }
})

test_that("publish_basketball_iceland: female next_games schema uses {p_home, p_away}", {
  fit_path <- backup_basketball_fit("female")
  if (!file.exists(fit_path)) {
    testthat::skip("legacy female basketball fit unavailable")
  }
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  out <- withr::local_tempdir()
  suppressWarnings(
    publish_basketball_iceland(
      fit, league,
      sex = "female",
      end_date = as.Date("2026-04-25"),
      output_root = out
    )
  )

  ng_path <- file.path(out, "basketball", "iceland", "kvenna", "next_games.json")
  expect_true(file.exists(ng_path))
  ng <- jsonlite::read_json(ng_path)
  expect_true("matches" %in% names(ng))
  if (length(ng$matches) > 0L) {
    m1 <- ng$matches[[1]]
    expect_true(all(c("p_home", "p_away") %in% names(m1)))
  }
})

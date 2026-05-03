# Phase 1 of the extraction-layer migration. extract_football_iceland()
# writes six Parquet files into data/beliefs/archive/.../fit_date=D/.
# Tests verify (a) shape contracts and (b) that the extracted summaries
# agree with what the in-memory publisher would compute.

backup_fit_path_extract <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

test_that("extract_football_iceland writes all 6 Parquets with expected schemas", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent -- cannot reconstruct prepare_data")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-05-03"),
      end_date = as.Date("2026-05-03"),
      archive_root = out
    )
  ))

  archive_dir <- file.path(
    out,
    "sport=football", "country=iceland", "sex=male",
    "fit_date=2026-05-03"
  )
  expected <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "round_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  for (f in expected) {
    expect_true(
      file.exists(file.path(archive_dir, f)),
      info = paste("missing:", f)
    )
  }

  pm <- arrow::read_parquet(file.path(archive_dir, "predicted_matches.parquet"))
  expect_setequal(
    names(pm),
    c(
      "home_team", "away_team", "match_date",
      "home_goals", "away_goals", "count"
    )
  )
  expect_true(all(pm$count > 0L))
  expect_true(is.integer(pm$home_goals))
  expect_true(is.integer(pm$away_goals))
  expect_true(is.integer(pm$count))

  ts <- arrow::read_parquet(file.path(archive_dir, "team_strengths_quantiles.parquet"))
  expect_setequal(
    names(ts),
    c("team", "component", "location", "quantile", "value")
  )
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), 1:99)

  rs <- arrow::read_parquet(file.path(archive_dir, "round_strengths_quantiles.parquet"))
  expect_setequal(
    names(rs),
    c("round", "team", "component", "location", "quantile", "value")
  )
  if (nrow(rs) > 0L) {
    expect_setequal(unique(rs$component), c("offence", "defence", "total"))
    expect_setequal(unique(rs$location), c("home", "away", "avg"))
    expect_setequal(unique(rs$quantile), 1:99)
  }

  ha <- arrow::read_parquet(file.path(archive_dir, "home_advantage_quantiles.parquet"))
  expect_setequal(
    names(ha),
    c("team", "component", "quantile", "value")
  )
  expect_setequal(unique(ha$component), c("offence", "defence", "total"))
  expect_setequal(unique(ha$quantile), 1:99)
  expect_true(all(ha$value > 0))

  fp <- arrow::read_parquet(file.path(archive_dir, "final_positions.parquet"))
  expect_setequal(names(fp), c("team", "placement", "probability"))
  if (nrow(fp) > 0L) {
    sums <- fp |>
      dplyr::summarise(s = sum(.data$probability), .by = "team")
    expect_true(all(abs(sums$s - 1) < 1e-9))
  }

  pd <- arrow::read_parquet(file.path(archive_dir, "points_distribution.parquet"))
  expect_setequal(names(pd), c("team", "points", "probability"))
  if (nrow(pd) > 0L) {
    sums <- pd |>
      dplyr::summarise(s = sum(.data$probability), .by = "team")
    expect_true(all(abs(sums$s - 1) < 1e-9))
  }
})

test_that("predicted_matches.parquet preserves total draw count per match", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-05-03"),
      end_date = as.Date("2026-05-03"),
      archive_root = out
    )
  ))

  pm <- arrow::read_parquet(file.path(
    out,
    "sport=football", "country=iceland", "sex=male",
    "fit_date=2026-05-03",
    "predicted_matches.parquet"
  ))
  if (nrow(pm) == 0L) testthat::skip("predicted_matches empty (N_pred mismatch)")

  per_match_total <- pm |>
    dplyr::summarise(
      total = sum(.data$count),
      .by = c("home_team", "away_team", "match_date")
    )
  expect_equal(length(unique(per_match_total$total)), 1L)
})

test_that("team_strengths_quantiles q=50 matches publisher's median per cell", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  pub <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-05-03"),
      end_date = as.Date("2026-05-03"),
      archive_root = out
    )
  ))
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-05-03"),
      output_root = pub
    )
  ))

  ts_q <- arrow::read_parquet(file.path(
    out,
    "sport=football", "country=iceland", "sex=male",
    "fit_date=2026-05-03",
    "team_strengths_quantiles.parquet"
  ))
  ts_pub <- jsonlite::read_json(file.path(
    pub, "football", "iceland", "karla", "team_strengths.json"
  ))$records

  ts_pub_df <- tibble::tibble(
    team = vapply(ts_pub, \(r) r$team, character(1)),
    component = vapply(ts_pub, \(r) r$component, character(1)),
    location = vapply(ts_pub, \(r) r$location, character(1)),
    coverage = vapply(ts_pub, \(r) r$coverage, numeric(1)),
    median = vapply(ts_pub, \(r) r$median, numeric(1))
  ) |>
    dplyr::filter(.data$coverage == 0.5) |>
    dplyr::distinct(.data$team, .data$component, .data$location, .data$median)

  q50 <- ts_q |>
    dplyr::filter(.data$quantile == 50L) |>
    dplyr::select("team", "component", "location", q50_value = "value")

  joined <- dplyr::inner_join(
    ts_pub_df, q50,
    by = c("team", "component", "location")
  )
  expect_gt(nrow(joined), 0L)
  expect_true(all(abs(joined$median - joined$q50_value) < 1e-3))
})

test_that("final_positions.parquet matches publisher's final_positions.json", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")
  if (!dir.exists(here::here("data", "facts", "results"))) {
    testthat::skip("facts/results absent")
  }

  fit <- readRDS(fit_path)
  league <- load_leagues()[["football_iceland"]]
  out <- withr::local_tempdir()
  pub <- withr::local_tempdir()

  suppressWarnings(suppressMessages(
    extract_football_iceland(
      fit, league,
      sex = "male",
      fit_date = as.Date("2026-05-03"),
      end_date = as.Date("2026-05-03"),
      archive_root = out
    )
  ))
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      fit, league,
      sex = "male",
      end_date = as.Date("2026-05-03"),
      output_root = pub
    )
  ))

  fp_extract <- arrow::read_parquet(file.path(
    out,
    "sport=football", "country=iceland", "sex=male",
    "fit_date=2026-05-03",
    "final_positions.parquet"
  ))
  fp_pub <- jsonlite::read_json(file.path(
    pub, "football", "iceland", "karla", "final_positions.json"
  ))$records

  if (length(fp_pub) == 0L) testthat::skip("final_positions empty (N_pred mismatch)")

  fp_pub_df <- tibble::tibble(
    team = vapply(fp_pub, \(r) r$team, character(1)),
    placement = vapply(fp_pub, \(r) r$placement, integer(1)),
    pub_probability = vapply(fp_pub, \(r) r$probability, numeric(1))
  )

  joined <- dplyr::inner_join(
    fp_extract, fp_pub_df,
    by = c("team", "placement")
  )
  expect_equal(nrow(joined), nrow(fp_extract))
  expect_true(all(abs(joined$probability - joined$pub_probability) < 1e-9))
})

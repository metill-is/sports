test_that("extract_basketball_iceland writes the 5 expected parquets", {
  skip_if_not(
    file.exists(here::here(
      "data", "beliefs", "fits", "sport=basketball",
      "country=iceland", "sex=male", "fit.rds"
    )),
    "Basketball male fit RDS not present"
  )
  skip_if_not_installed("arrow")

  fit <- readRDS(here::here(
    "data", "beliefs", "fits", "sport=basketball",
    "country=iceland", "sex=male", "fit.rds"
  ))

  leagues <- load_leagues()
  league <- leagues[["basketball_iceland"]]

  tmp <- withr::local_tempdir()
  extracts_root <- file.path(tmp, "extracts")

  extract_basketball_iceland(
    fit = fit,
    league = league,
    sex = "male",
    fit_date = as.Date("2026-04-29"),
    end_date = as.Date("2026-04-29"),
    root = here::here("data"),
    extracts_root = extracts_root
  )

  partition <- file.path(
    extracts_root,
    "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2026-04-29"
  )

  expected_files <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  for (f in expected_files) {
    expect_true(
      file.exists(file.path(partition, f)),
      info = paste("missing", f)
    )
  }
})

test_that("extracted team_strengths_quantiles covers the 9-cell grid", {
  skip_if_not(
    file.exists(here::here(
      "data", "beliefs", "fits", "sport=basketball",
      "country=iceland", "sex=male", "fit.rds"
    )),
    "Basketball male fit RDS not present"
  )
  skip_if_not_installed("arrow")

  fit <- readRDS(here::here(
    "data", "beliefs", "fits", "sport=basketball",
    "country=iceland", "sex=male", "fit.rds"
  ))
  league <- load_leagues()[["basketball_iceland"]]

  tmp <- withr::local_tempdir()
  extract_basketball_iceland(
    fit = fit,
    league = league,
    sex = "male",
    fit_date = as.Date("2026-04-29"),
    end_date = as.Date("2026-04-29"),
    root = here::here("data"),
    extracts_root = file.path(tmp, "extracts")
  )

  ts <- arrow::read_parquet(file.path(
    tmp, "extracts",
    "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2026-04-29",
    "team_strengths_quantiles.parquet"
  ))

  expect_true(all(c("team", "component", "location", "quantile", "value") %in% names(ts)))
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), seq_len(99L))
})

test_that("extract_basketball_iceland is idempotent (rerun overwrites cleanly)", {
  skip_if_not(
    file.exists(here::here(
      "data", "beliefs", "fits", "sport=basketball",
      "country=iceland", "sex=male", "fit.rds"
    )),
    "Basketball male fit RDS not present"
  )

  fit <- readRDS(here::here(
    "data", "beliefs", "fits", "sport=basketball",
    "country=iceland", "sex=male", "fit.rds"
  ))
  league <- load_leagues()[["basketball_iceland"]]

  tmp <- withr::local_tempdir()
  args <- list(
    fit = fit, league = league, sex = "male",
    fit_date = as.Date("2026-04-29"),
    end_date = as.Date("2026-04-29"),
    root = here::here("data"),
    extracts_root = file.path(tmp, "extracts")
  )
  do.call(extract_basketball_iceland, args)
  size_before <- file.info(file.path(
    tmp, "extracts",
    "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2026-04-29",
    "team_strengths_quantiles.parquet"
  ))$size
  do.call(extract_basketball_iceland, args)
  size_after <- file.info(file.path(
    tmp, "extracts",
    "sport=basketball", "country=iceland",
    "sex=male", "fit_date=2026-04-29",
    "team_strengths_quantiles.parquet"
  ))$size

  expect_equal(size_before, size_after)
})

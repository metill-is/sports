# Phase 1 of the extraction-layer migration. extract_football_iceland()
# writes six Parquet files per division into
# data/beliefs/archive/.../fit_date=D/division={BD,LD1}/. Tests verify
# (a) shape contracts and (b) that the extracted summaries agree with
# what the in-memory publisher would compute.

backup_fit_path_extract <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

# Helper: archive partition path for a given (sex, fit_date, division).
.archive_div_dir <- function(out, sex, fit_date_chr, target_div) {
  file.path(
    out,
    "sport=football", "country=iceland", paste0("sex=", sex),
    paste0("fit_date=", fit_date_chr),
    paste0("division=", target_div)
  )
}

test_that("extract_football_iceland writes all 6 Parquets per division with expected schemas", {
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

  expected <- c(
    "predicted_matches.parquet",
    "team_strengths_quantiles.parquet",
    "round_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet",
    "final_positions.parquet",
    "points_distribution.parquet"
  )
  for (target_div in c("BD", "LD1")) {
    archive_dir <- .archive_div_dir(out, "male", "2026-05-03", target_div)
    for (f in expected) {
      expect_true(
        file.exists(file.path(archive_dir, f)),
        info = paste("missing:", target_div, "/", f)
      )
    }
  }

  # BD schema checks (LD1 may be empty pre-refactor publisher; schemas
  # are identical so spot-check BD). pm = predicted_matches, ts = team_strengths.
  bd <- .archive_div_dir(out, "male", "2026-05-03", "BD")

  pm <- arrow::read_parquet(file.path(bd, "predicted_matches.parquet"))
  expect_setequal(
    names(pm),
    c(
      "home_team", "away_team", "match_date",
      "home_goals", "away_goals", "count"
    )
  )
  if (nrow(pm) > 0L) {
    expect_true(all(pm$count > 0L))
    expect_true(is.integer(pm$home_goals))
    expect_true(is.integer(pm$away_goals))
    expect_true(is.integer(pm$count))
  }

  ts <- arrow::read_parquet(file.path(bd, "team_strengths_quantiles.parquet"))
  expect_setequal(
    names(ts),
    c("team", "component", "location", "quantile", "value")
  )
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), 1:99)

  rs <- arrow::read_parquet(file.path(bd, "round_strengths_quantiles.parquet"))
  expect_setequal(
    names(rs),
    c("round", "team", "component", "location", "quantile", "value")
  )
  if (nrow(rs) > 0L) {
    expect_setequal(unique(rs$component), c("offence", "defence", "total"))
    expect_setequal(unique(rs$location), c("home", "away", "avg"))
    expect_setequal(unique(rs$quantile), 1:99)
  }

  ha <- arrow::read_parquet(file.path(bd, "home_advantage_quantiles.parquet"))
  expect_setequal(
    names(ha),
    c("team", "component", "quantile", "value")
  )
  expect_setequal(unique(ha$component), c("offence", "defence", "total"))
  expect_setequal(unique(ha$quantile), 1:99)
  expect_true(all(ha$value > 0))

  fp <- arrow::read_parquet(file.path(bd, "final_positions.parquet"))
  expect_setequal(names(fp), c("team", "placement", "probability"))
  if (nrow(fp) > 0L) {
    sums <- fp |>
      dplyr::summarise(s = sum(.data$probability), .by = "team")
    expect_true(all(abs(sums$s - 1) < 1e-9))
  }

  pd <- arrow::read_parquet(file.path(bd, "points_distribution.parquet"))
  expect_setequal(names(pd), c("team", "points", "probability"))
  if (nrow(pd) > 0L) {
    sums <- pd |>
      dplyr::summarise(s = sum(.data$probability), .by = "team")
    expect_true(all(abs(sums$s - 1) < 1e-9))
  }
})

test_that("extract_football_iceland: BD and LD1 partitions contain disjoint team sets", {
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

  bd_ts <- arrow::read_parquet(file.path(
    .archive_div_dir(out, "male", "2026-05-03", "BD"),
    "team_strengths_quantiles.parquet"
  ))
  ld_ts <- arrow::read_parquet(file.path(
    .archive_div_dir(out, "male", "2026-05-03", "LD1"),
    "team_strengths_quantiles.parquet"
  ))

  if (nrow(ld_ts) == 0L) {
    testthat::skip("LD1 strengths empty for this fit (no Lengjudeild matches)")
  }
  bd_teams <- unique(bd_ts$team)
  ld_teams <- unique(ld_ts$team)
  expect_length(intersect(bd_teams, ld_teams), 0L)
})

test_that("extract_football_iceland: target_divs subset writes only requested partitions", {
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
      archive_root = out,
      target_divs = "BD"
    )
  ))

  expect_true(dir.exists(.archive_div_dir(out, "male", "2026-05-03", "BD")))
  expect_false(dir.exists(.archive_div_dir(out, "male", "2026-05-03", "LD1")))
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
    .archive_div_dir(out, "male", "2026-05-03", "BD"),
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
  extracted <- read_extracted_football(
    league,
    sex = "male",
    fit_date = as.Date("2026-05-03"),
    archive_root = out
  )
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-05-03"),
      output_root = pub
    )
  ))

  ts_q <- arrow::read_parquet(file.path(
    .archive_div_dir(out, "male", "2026-05-03", "BD"),
    "team_strengths_quantiles.parquet"
  ))
  ts_pub <- jsonlite::read_json(file.path(
    pub, "football", "iceland", "karla-bd", "team_strengths.json"
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

# ---- read_extracted_football() ----------------------------------------------

# Build a "modern" partition (per-division parquets) for one or both divisions.
# `divisions = c("BD", "LD1")` writes both; payload identifies which sub-dir.
.write_modern_partition_extract <- function(base, fit_date_chr,
                                            divisions = c("BD", "LD1"),
                                            payload = tibble::tibble(x = 1L)) {
  fit_dir <- file.path(base, paste0("fit_date=", fit_date_chr))
  dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
  for (target_div in divisions) {
    pdir <- file.path(fit_dir, paste0("division=", target_div))
    dir.create(pdir, recursive = TRUE)
    for (f in c(
      "predicted_matches.parquet", "team_strengths_quantiles.parquet",
      "round_strengths_quantiles.parquet", "home_advantage_quantiles.parquet",
      "final_positions.parquet", "points_distribution.parquet"
    )) {
      arrow::write_parquet(payload, file.path(pdir, f))
    }
  }
  invisible(fit_dir)
}

# Build a legacy partition (6 parquets directly under fit_date=D/, BD-only).
.write_legacy_partition_extract <- function(base, fit_date_chr,
                                            payload = tibble::tibble(x = 1L)) {
  pdir <- file.path(base, paste0("fit_date=", fit_date_chr))
  dir.create(pdir, recursive = TRUE)
  for (f in c(
    "predicted_matches.parquet", "team_strengths_quantiles.parquet",
    "round_strengths_quantiles.parquet", "home_advantage_quantiles.parquet",
    "final_positions.parquet", "points_distribution.parquet"
  )) {
    arrow::write_parquet(payload, file.path(pdir, f))
  }
  invisible(pdir)
}

test_that("read_extracted_football: latest auto-discovery skips legacy-pre-extract partitions", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  legacy <- file.path(base, "fit_date=2026-04-24")
  dir.create(legacy, recursive = TRUE)
  file.create(file.path(legacy, "part-0.parquet"))
  .write_modern_partition_extract(base, "2026-05-03")

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_football(league, sex = "male", archive_root = tmp)
  expect_equal(out$fit_date, as.Date("2026-05-03"))
  expect_named(out, c("BD", "LD1", "fit_date"))
  expect_named(
    out$BD,
    c(
      "predicted_matches", "team_strengths_quantiles",
      "round_strengths_quantiles", "home_advantage_quantiles",
      "final_positions", "points_distribution"
    )
  )
})

test_that("read_extracted_football: latest auto-discovery picks newest modern partition", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  .write_modern_partition_extract(base, "2026-04-29",
    payload = tibble::tibble(d = "2026-04-29")
  )
  .write_modern_partition_extract(base, "2026-05-03",
    payload = tibble::tibble(d = "2026-05-03")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_football(league, sex = "male", archive_root = tmp)
  expect_equal(out$fit_date, as.Date("2026-05-03"))
  expect_equal(out$BD$predicted_matches$d, "2026-05-03")
})

test_that("read_extracted_football: lifts legacy single-dir partition into BD slot", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  .write_legacy_partition_extract(base, "2026-05-01",
    payload = tibble::tibble(d = "legacy")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_football(league, sex = "male", archive_root = tmp)
  expect_equal(out$fit_date, as.Date("2026-05-01"))
  expect_equal(out$BD$predicted_matches$d, "legacy")
  # LD1 falls back to empty tibbles for legacy partitions.
  expect_equal(nrow(out$LD1$predicted_matches), 0L)
  expect_equal(nrow(out$LD1$final_positions), 0L)
})

test_that("read_extracted_football: explicit fit_date loads exactly that partition", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=female")
  .write_modern_partition_extract(base, "2026-04-25",
    payload = tibble::tibble(d = "2026-04-25")
  )
  .write_modern_partition_extract(base, "2026-05-01",
    payload = tibble::tibble(d = "2026-05-01")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_football(
    league,
    sex = "female",
    fit_date = as.Date("2026-04-25"),
    archive_root = tmp
  )
  expect_equal(out$fit_date, as.Date("2026-04-25"))
  expect_equal(out$BD$predicted_matches$d, "2026-04-25")
})

test_that("read_extracted_football: errors when no archive directory exists", {
  tmp <- withr::local_tempdir()
  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_football(league, sex = "male", archive_root = tmp),
    "No archive directory"
  )
})

test_that("read_extracted_football: errors when no partition has a complete BD set", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  legacy <- file.path(base, "fit_date=2026-04-24")
  dir.create(legacy, recursive = TRUE)
  file.create(file.path(legacy, "part-0.parquet"))

  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_football(league, sex = "male", archive_root = tmp),
    "complete extracted set"
  )
})

test_that("read_extracted_football: explicit fit_date errors when partition is incomplete", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  pdir <- file.path(base, "fit_date=2026-05-01", "division=BD")
  dir.create(pdir, recursive = TRUE)
  arrow::write_parquet(
    tibble::tibble(x = 1L),
    file.path(pdir, "predicted_matches.parquet")
  )
  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_football(
      league,
      sex = "male",
      fit_date = as.Date("2026-05-01"),
      archive_root = tmp
    ),
    "incomplete"
  )
})

test_that("read_extracted_football: round-trips with extract_football_iceland", {
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

  loaded <- read_extracted_football(
    league,
    sex = "male",
    archive_root = out
  )
  expect_equal(loaded$fit_date, as.Date("2026-05-03"))

  bd_dir <- .archive_div_dir(out, "male", "2026-05-03", "BD")
  expect_equal(
    loaded$BD$predicted_matches,
    arrow::read_parquet(file.path(bd_dir, "predicted_matches.parquet"))
  )
  expect_equal(
    loaded$BD$team_strengths_quantiles,
    arrow::read_parquet(file.path(bd_dir, "team_strengths_quantiles.parquet"))
  )
  expect_equal(
    loaded$BD$final_positions,
    arrow::read_parquet(file.path(bd_dir, "final_positions.parquet"))
  )
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
  extracted <- read_extracted_football(
    league,
    sex = "male",
    fit_date = as.Date("2026-05-03"),
    archive_root = out
  )
  suppressWarnings(suppressMessages(
    publish_football_iceland(
      extracted = extracted,
      league = league,
      sex = "male",
      end_date = as.Date("2026-05-03"),
      output_root = pub
    )
  ))

  fp_extract <- arrow::read_parquet(file.path(
    .archive_div_dir(out, "male", "2026-05-03", "BD"),
    "final_positions.parquet"
  ))
  fp_pub <- jsonlite::read_json(file.path(
    pub, "football", "iceland", "karla-bd", "final_positions.json"
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

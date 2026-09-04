# extract_football_iceland() writes 6 Parquet files per fit into
# data/beliefs/extracts/.../fit_date=D/, with `division` ("BD" or "LD1") as
# a payload column in each. Tests verify (a) shape contracts and (b) that
# the extracted summaries agree with what the in-memory publisher would
# compute.

backup_fit_path_extract <- function(sex) {
  root <- Sys.getenv(
    "SPORTS_BACKUP_ROOT",
    "/Users/brynjolfurjonsson/sports-backup-20260424-163153"
  )
  file.path(root, "Sports", "football", "iceland", "results", sex, "fit.rds")
}

# Helper: extracts partition path for a given (sex, fit_date).
.extracts_fit_dir <- function(out, sex, fit_date_chr) {
  file.path(
    out,
    "sport=football", "country=iceland", paste0("sex=", sex),
    paste0("fit_date=", fit_date_chr)
  )
}

test_that("extract_football_iceland writes all 6 Parquets with expected schemas + division column", {
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
      extracts_root = out
    )
  ))

  fit_dir <- .extracts_fit_dir(out, "male", "2026-05-03")
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
      file.exists(file.path(fit_dir, f)),
      info = paste("missing:", f)
    )
  }
  # No nested division=*/ subdirs -- partition depth is 4, not 5.
  expect_false(dir.exists(file.path(fit_dir, "division=BD")))
  expect_false(dir.exists(file.path(fit_dir, "division=LD1")))

  pm <- arrow::read_parquet(file.path(fit_dir, "predicted_matches.parquet"))
  expect_setequal(
    names(pm),
    c(
      "home_team", "away_team", "match_date",
      "home_goals", "away_goals", "count", "division"
    )
  )
  # Extract output should be restricted to the per-sex publish set, sourced from
  # config/leagues.yml::publish_divisions[[sex]]. Helper kept in sync with config.
  publishable_divs <- .iceland_division_codes("football_iceland", "male")
  if (nrow(pm) > 0L) {
    expect_true(all(pm$count > 0L))
    expect_true(is.integer(pm$home_goals))
    expect_true(is.integer(pm$away_goals))
    expect_true(is.integer(pm$count))
    expect_true(all(pm$division %in% publishable_divs))
  }

  ts <- arrow::read_parquet(file.path(fit_dir, "team_strengths_quantiles.parquet"))
  expect_setequal(
    names(ts),
    c("team", "component", "location", "quantile", "value", "division")
  )
  expect_setequal(unique(ts$component), c("offence", "defence", "total"))
  expect_setequal(unique(ts$location), c("home", "away", "avg"))
  expect_setequal(unique(ts$quantile), 1:99)
  expect_true(all(ts$division %in% publishable_divs))

  rs <- arrow::read_parquet(file.path(fit_dir, "round_strengths_quantiles.parquet"))
  expect_setequal(
    names(rs),
    c("round", "team", "component", "location", "quantile", "value", "division")
  )
  if (nrow(rs) > 0L) {
    expect_setequal(unique(rs$component), c("offence", "defence", "total"))
    expect_setequal(unique(rs$location), c("home", "away", "avg"))
    expect_setequal(unique(rs$quantile), 1:99)
  }

  ha <- arrow::read_parquet(file.path(fit_dir, "home_advantage_quantiles.parquet"))
  expect_setequal(
    names(ha),
    c("team", "component", "quantile", "value", "division")
  )
  expect_setequal(unique(ha$component), c("offence", "defence", "total"))
  expect_setequal(unique(ha$quantile), 1:99)
  expect_true(all(ha$value > 0))

  fp <- arrow::read_parquet(file.path(fit_dir, "final_positions.parquet"))
  expect_setequal(names(fp), c("team", "placement", "probability", "division"))
  if (nrow(fp) > 0L) {
    sums <- fp |>
      dplyr::summarise(s = sum(.data$probability), .by = c("division", "team"))
    expect_true(all(abs(sums$s - 1) < 1e-9))
  }

  pd <- arrow::read_parquet(file.path(fit_dir, "points_distribution.parquet"))
  expect_setequal(names(pd), c("team", "points", "probability", "division"))
  if (nrow(pd) > 0L) {
    sums <- pd |>
      dplyr::summarise(s = sum(.data$probability), .by = c("division", "team"))
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
      extracts_root = out
    )
  ))

  ts <- arrow::read_parquet(file.path(
    .extracts_fit_dir(out, "male", "2026-05-03"),
    "team_strengths_quantiles.parquet"
  ))
  bd_teams <- unique(ts$team[ts$division == "BD"])
  ld_teams <- unique(ts$team[ts$division == "LD1"])

  if (length(ld_teams) == 0L) {
    testthat::skip("LD1 strengths empty for this fit (no Lengjudeild matches)")
  }
  expect_length(intersect(bd_teams, ld_teams), 0L)
})

test_that("extract_football_iceland: target_divs subset only includes requested divisions", {
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
      extracts_root = out,
      target_divs = "BD"
    )
  ))

  ts <- arrow::read_parquet(file.path(
    .extracts_fit_dir(out, "male", "2026-05-03"),
    "team_strengths_quantiles.parquet"
  ))
  expect_setequal(unique(ts$division), "BD")
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
      extracts_root = out
    )
  ))

  pm <- arrow::read_parquet(file.path(
    .extracts_fit_dir(out, "male", "2026-05-03"),
    "predicted_matches.parquet"
  ))
  if (nrow(pm) == 0L) testthat::skip("predicted_matches empty (N_pred mismatch)")

  per_match_total <- pm |>
    dplyr::summarise(
      total = sum(.data$count),
      .by = c("division", "home_team", "away_team", "match_date")
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
      extracts_root = out
    )
  ))
  extracted <- read_extracted_iceland(
    league,
    sex = "male",
    fit_date = as.Date("2026-05-03"),
    extracts_root = out
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
    .extracts_fit_dir(out, "male", "2026-05-03"),
    "team_strengths_quantiles.parquet"
  )) |>
    dplyr::filter(.data$division == "BD")

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

# ---- read_extracted_iceland() ----------------------------------------------

# Build a "modern" partition (6 parquets at fit_date=*/, with `division` payload).
# `divisions = c("BD", "LD1")` writes both; payload identifies which sub-dir.
.write_extracts_partition <- function(base, fit_date_chr,
                                      divisions = c("BD", "LD1"),
                                      payload = tibble::tibble(x = 1L)) {
  fit_dir <- file.path(base, paste0("fit_date=", fit_date_chr))
  dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)
  for (f in c(
    "predicted_matches.parquet", "team_strengths_quantiles.parquet",
    "round_strengths_quantiles.parquet", "home_advantage_quantiles.parquet",
    "final_positions.parquet", "points_distribution.parquet"
  )) {
    df_per_div <- lapply(divisions, function(d) {
      out <- payload
      out$division <- d
      out
    })
    arrow::write_parquet(dplyr::bind_rows(df_per_div), file.path(fit_dir, f))
  }
  invisible(fit_dir)
}

test_that("read_extracted_iceland: latest auto-discovery picks newest complete partition", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  .write_extracts_partition(base, "2026-04-29",
    payload = tibble::tibble(d = "2026-04-29")
  )
  .write_extracts_partition(base, "2026-05-03",
    payload = tibble::tibble(d = "2026-05-03")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_iceland(league, sex = "male", extracts_root = tmp)
  expect_equal(out$fit_date, as.Date("2026-05-03"))
  expect_equal(out$BD$predicted_matches$d, "2026-05-03")
})

test_that("read_extracted_iceland: returns BD-only when LD1 rows absent", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  .write_extracts_partition(base, "2026-05-01",
    divisions = "BD",
    payload = tibble::tibble(d = "bd-only")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_iceland(league, sex = "male", extracts_root = tmp)
  expect_equal(out$fit_date, as.Date("2026-05-01"))
  expect_equal(out$BD$predicted_matches$d, "bd-only")
  expect_equal(nrow(out$LD1$predicted_matches), 0L)
  expect_equal(nrow(out$LD1$final_positions), 0L)
})

test_that("read_extracted_iceland: explicit fit_date loads exactly that partition", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=female")
  .write_extracts_partition(base, "2026-04-25",
    payload = tibble::tibble(d = "2026-04-25")
  )
  .write_extracts_partition(base, "2026-05-01",
    payload = tibble::tibble(d = "2026-05-01")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_iceland(
    league,
    sex = "female",
    fit_date = as.Date("2026-04-25"),
    extracts_root = tmp
  )
  expect_equal(out$fit_date, as.Date("2026-04-25"))
  expect_equal(out$BD$predicted_matches$d, "2026-04-25")
})

test_that("read_extracted_iceland: errors when no extracts directory exists", {
  tmp <- withr::local_tempdir()
  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_iceland(league, sex = "male", extracts_root = tmp),
    "No extracts directory"
  )
})

test_that("read_extracted_iceland: errors when no partition is complete", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  pdir <- file.path(base, "fit_date=2026-04-24")
  dir.create(pdir, recursive = TRUE)
  arrow::write_parquet(
    tibble::tibble(x = 1L, division = "BD"),
    file.path(pdir, "predicted_matches.parquet")
  )

  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_iceland(league, sex = "male", extracts_root = tmp),
    "complete extracted set"
  )
})

test_that("read_extracted_iceland: explicit fit_date errors when partition is incomplete", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  pdir <- file.path(base, "fit_date=2026-05-01")
  dir.create(pdir, recursive = TRUE)
  arrow::write_parquet(
    tibble::tibble(x = 1L, division = "BD"),
    file.path(pdir, "predicted_matches.parquet")
  )
  league <- list(sport = "football", country = "iceland")
  expect_error(
    read_extracted_iceland(
      league,
      sex = "male",
      fit_date = as.Date("2026-05-01"),
      extracts_root = tmp
    ),
    "incomplete"
  )
})

test_that("read_extracted_iceland: round-trips with extract_football_iceland", {
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
      extracts_root = out
    )
  ))

  loaded <- read_extracted_iceland(
    league,
    sex = "male",
    extracts_root = out
  )
  expect_equal(loaded$fit_date, as.Date("2026-05-03"))

  fit_dir <- .extracts_fit_dir(out, "male", "2026-05-03")
  full <- arrow::read_parquet(file.path(fit_dir, "predicted_matches.parquet"))
  bd_only <- full[full$division == "BD", ]
  bd_only$division <- NULL
  expect_equal(loaded$BD$predicted_matches, tibble::as_tibble(bd_only))
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
      extracts_root = out
    )
  ))
  extracted <- read_extracted_iceland(
    league,
    sex = "male",
    fit_date = as.Date("2026-05-03"),
    extracts_root = out
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

  fp_full <- arrow::read_parquet(file.path(
    .extracts_fit_dir(out, "male", "2026-05-03"),
    "final_positions.parquet"
  ))
  fp_extract <- fp_full[fp_full$division == "BD", c("team", "placement", "probability")]
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

test_that("extract_football_iceland: CUP target_div writes empty final_positions + points_distribution", {
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
      extracts_root = out,
      target_divs = "CUP"
    )
  ))

  fit_dir <- .extracts_fit_dir(out, "male", "2026-05-03")

  # CUP is a knockout: final_positions + points_distribution are skipped
  # in the extract (no league-table simulation makes sense for a bracket).
  fp <- arrow::read_parquet(file.path(fit_dir, "final_positions.parquet"))
  pd <- arrow::read_parquet(file.path(fit_dir, "points_distribution.parquet"))
  expect_equal(nrow(fp), 0L)
  expect_equal(nrow(pd), 0L)

  # The other four parquets still carry CUP rows when prediction matches
  # or training data exist for cup teams. We assert only that whatever
  # rows are present carry the CUP division marker — no BD/LD1 leakage.
  pm <- arrow::read_parquet(file.path(fit_dir, "predicted_matches.parquet"))
  if (nrow(pm) > 0L) {
    expect_true(all(pm$division == "CUP"))
  }
  ts <- arrow::read_parquet(file.path(fit_dir, "team_strengths_quantiles.parquet"))
  if (nrow(ts) > 0L) {
    expect_true(all(ts$division == "CUP"))
  }
})

test_that("read_extracted_iceland: CUP slot present in default target_divs", {
  tmp <- withr::local_tempdir()
  base <- file.path(tmp, "sport=football", "country=iceland", "sex=male")
  .write_extracts_partition(base, "2026-05-12",
    divisions = c("BD", "LD1", "CUP"),
    payload = tibble::tibble(marker = "v1")
  )

  league <- list(sport = "football", country = "iceland")
  out <- read_extracted_iceland(league, sex = "male", extracts_root = tmp)
  expect_true("CUP" %in% names(out))
  expect_equal(out$CUP$predicted_matches$marker, "v1")
})

# ---- .build_bracket_state_pfi ------------------------------------------------

# Helper: 8 R16 fixtures with 16 unique teams within a 1-day window.
.cup_fixtures_r16 <- function(date_offset = 0L, n = 8L) {
  tibble::tibble(
    home_team  = paste0("H", seq_len(n)),
    away_team  = paste0("A", seq_len(n)),
    match_date = as.Date("2026-05-13") + date_offset,
    division   = "CUP"
  )
}

test_that(".build_bracket_state_pfi: returns NULL when fewer than 8 upcoming cup matches", {
  pred_d <- tibble::tibble(
    home_team  = c("A", "B"),
    away_team  = c("C", "D"),
    match_date = as.Date(c("2026-05-13", "2026-05-14")),
    division   = c("CUP", "CUP")
  )
  expect_null(.build_bracket_state_pfi(pred_d))
})

test_that(".build_bracket_state_pfi: returns NULL when fewer than 8 CUP-tagged matches", {
  pred_d <- tibble::tibble(
    home_team  = paste0("H", 1:10),
    away_team  = paste0("A", 1:10),
    match_date = as.Date("2026-05-13") + 0:9,
    division   = c(rep("BD", 4), rep("CUP", 6))
  )
  expect_null(.build_bracket_state_pfi(pred_d))
})

test_that(".build_bracket_state_pfi: builds cup_teams + rounds shape when R16 fully upcoming", {
  pred_d <- .cup_fixtures_r16()
  bs <- .build_bracket_state_pfi(pred_d)
  expect_false(is.null(bs))
  expect_named(bs, c("cup_teams", "rounds"))
  expect_equal(length(bs$cup_teams), 16L)
  expect_named(bs$rounds, c("R16", "R8", "SF", "Final"))
  expect_true(bs$rounds$R16$pairings_known)
  expect_equal(nrow(bs$rounds$R16$matches), 8L)
  expect_named(
    bs$rounds$R16$matches,
    c("home_team", "away_team", "venue", "known_winner")
  )
  expect_true(all(is.na(bs$rounds$R16$matches$known_winner)))
  # No R8/SF/Final fixtures in the synthetic input → pairings unknown.
  expect_false(bs$rounds$R8$pairings_known)
  expect_false(bs$rounds$SF$pairings_known)
  expect_false(bs$rounds$Final$pairings_known)
})

test_that(".build_bracket_state_pfi: handles NULL / empty inputs", {
  expect_null(.build_bracket_state_pfi(NULL))
  expect_null(.build_bracket_state_pfi(tibble::tibble()))
  expect_null(.build_bracket_state_pfi(NULL, results = NULL))
})

test_that(".build_bracket_state_pfi: post-R16 — R16 results populate cup_teams + known_winners", {
  # Scenario: R16 was played 2 days ago (results), no upcoming cup fixtures.
  # The builder should still identify the 16 cup teams from results.
  r16_results <- tibble::tibble(
    home_team  = paste0("H", 1:8),
    away_team  = paste0("A", 1:8),
    match_date = as.Date("2026-05-13"),
    home_score = c(2L, 1L, 3L, 0L, 4L, 1L, 2L, 3L),
    away_score = c(1L, 0L, 1L, 2L, 0L, 0L, 1L, 2L),
    division   = "CUP",
    season     = 2026L
  )
  bs <- .build_bracket_state_pfi(
    pred_d = NULL, results = r16_results, current_season = 2026L
  )
  expect_false(is.null(bs))
  expect_equal(length(bs$cup_teams), 16L)
  expect_true(bs$rounds$R16$pairings_known)
  expect_equal(nrow(bs$rounds$R16$matches), 8L)
  # All 8 R16 matches have known_winners populated since results have scores.
  expect_true(all(!is.na(bs$rounds$R16$matches$known_winner)))
  # Match 1: H1 (2) beat A1 (1) -> known_winner is H1
  expect_equal(bs$rounds$R16$matches$known_winner[1], "H1")
  expect_equal(bs$rounds$R16$matches$known_winner[4], "A4") # H4 lost 0-2
  # R8/SF/Final still unknown (KSÍ hasn't drawn yet).
  expect_false(bs$rounds$R8$pairings_known)
})

test_that(".build_bracket_state_pfi: post-R16 with R8 drawn — R8 pairings_known = TRUE", {
  r16_results <- tibble::tibble(
    home_team  = paste0("H", 1:8),
    away_team  = paste0("A", 1:8),
    match_date = as.Date("2026-05-13"),
    home_score = rep(2L, 8L),
    away_score = rep(1L, 8L), # all home teams won
    division   = "CUP",
    season     = 2026L
  )
  # KSÍ now schedules R8: 4 matches between the 8 R16-winners (H1..H8).
  r8_schedule <- tibble::tibble(
    home_team  = c("H1", "H3", "H5", "H7"),
    away_team  = c("H2", "H4", "H6", "H8"),
    match_date = as.Date("2026-06-07"),
    division   = "CUP"
  )
  bs <- .build_bracket_state_pfi(
    pred_d = r8_schedule, results = r16_results, current_season = 2026L
  )
  expect_false(is.null(bs))
  expect_true(bs$rounds$R16$pairings_known)
  expect_true(bs$rounds$R8$pairings_known)
  expect_equal(nrow(bs$rounds$R8$matches), 4L)
  expect_equal(bs$rounds$R8$matches$home_team, c("H1", "H3", "H5", "H7"))
  expect_true(all(is.na(bs$rounds$R8$matches$known_winner))) # R8 not played yet
  expect_false(bs$rounds$SF$pairings_known)
})

# ---- .extract_sim_inputs_pfi NA-safety (issue #14) --------------------------

test_that(".extract_sim_inputs_pfi: drops orphan teams + warns when fit's team count exceeds nrow(teams)", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")

  fit <- readRDS(fit_path)
  n_teams_fit <- length(posterior::variables(fit$draws("cur_offense_away")))

  # Build a deliberately too-short teams tibble. In real life this happens
  # when a saved fit (e.g. a 2026-04-24 backup) is paired with a freshly-built
  # prep$teams whose 365-day training filter has aged out some of the fit's
  # original teams. Pre-fix this triggered a 16 GB NA-cartesian OOM in the
  # full_join chain. Post-fix the NA filter drops the orphans cleanly and
  # the function warns about the mismatch.
  truncated_teams <- tibble::tibble(team = paste0("t", seq_len(n_teams_fit - 5L)))

  expect_warning(
    out <- .extract_sim_inputs_pfi(fit, truncated_teams),
    regexp = "team-indexed parameters but `teams` has"
  )
  # Output contains only the named teams — orphan rows are filtered out
  # before the join, so no NA team names survive.
  expect_false(any(is.na(out$team$team)))
  expect_setequal(unique(out$team$team), truncated_teams$team)
  # And no NA-cartesian explosion: row count is exactly N_named × N_draws.
  n_draws <- nrow(out$scalar)
  expect_equal(nrow(out$team), nrow(truncated_teams) * n_draws)
})

test_that(".extract_sim_inputs_pfi: succeeds without warning when fit's team count matches nrow(teams)", {
  fit_path <- backup_fit_path_extract("male")
  if (!file.exists(fit_path)) testthat::skip("legacy football fit unavailable")

  fit <- readRDS(fit_path)
  n_teams_fit <- length(posterior::variables(fit$draws("cur_offense_away")))
  matched_teams <- tibble::tibble(team = paste0("t", seq_len(n_teams_fit)))

  # The matched-count path must NOT emit the mismatch warning — that signal
  # is reserved for stale-fit callers.
  expect_no_warning(
    out <- .extract_sim_inputs_pfi(fit, matched_teams)
  )
  expect_named(out, c("team", "scalar"))
  expect_setequal(
    names(out$team),
    c(
      "team", ".draw", "cur_offense", "cur_defense",
      "home_advantage_off", "home_advantage_def"
    )
  )
  expect_false(any(is.na(out$team$team)))
  expect_setequal(unique(out$team$team), matched_teams$team)
})

# ---- .build_bracket_state_pfi: real-season shapes (2026 regressions) ---------
# The 2026 Mjólkurbikar exposed three holes in the original heuristic, all
# reproduced here from the real season's shape:
#   1. Early qualifying rounds (March, 16 minnows over one weekend) satisfied
#      "first 8-match window with 16 distinct teams in <=4 days", so the
#      bracket simulated the wrong 16 teams (no Besta-deild entrants at all).
#   2. A rescheduled fixture leaves a ghost row at the old date (the schedules
#      store keys on match_date), corrupting the rank-positional round slicing.
#   3. A 90' tie (ÍA 2-2 Grindavík) left known_winner = NA even though the
#      QF draw proves who advanced, so the simulator re-tossed a decided tie.

# Helper: a played early qualifying round — 8 matches, 16 distinct minnows,
# inside one weekend. Home teams advance.
.cup_results_early_round <- function() {
  tibble::tibble(
    home_team  = paste0("E", 1:8),
    away_team  = paste0("E", 9:16),
    match_date = as.Date("2026-03-18") + c(0L, 0L, 1L, 1L, 1L, 2L, 2L, 2L),
    home_score = 2L,
    away_score = 0L,
    division   = "CUP",
    season     = 2026L
  )
}

# Helper: the real R16 — early-round survivors E1..E8 meet top-flight
# entrants T1..T8 (who skip the qualifying rounds). Home teams advance.
.cup_results_real_r16 <- function() {
  tibble::tibble(
    home_team  = c(paste0("E", 1:4), paste0("T", 1:4)),
    away_team  = c(paste0("T", 5:8), paste0("E", 5:8)),
    match_date = as.Date("2026-05-13") + rep(0:1, each = 4L),
    home_score = 1L,
    away_score = 0L,
    division   = "CUP",
    season     = 2026L
  )
}

test_that(".build_bracket_state_pfi: early qualifying round does not hijack R16 detection", {
  results <- dplyr::bind_rows(.cup_results_early_round(), .cup_results_real_r16())
  bs <- .build_bracket_state_pfi(
    pred_d = NULL, results = results, current_season = 2026L
  )
  expect_false(is.null(bs))
  # The March window has 16 distinct teams in <=4 days, but later cup matches
  # involve the T* entrants — only the May window is consistent.
  expect_setequal(bs$cup_teams, c(paste0("E", 1:8), paste0("T", 1:8)))
  expect_true(bs$rounds$R16$pairings_known)
  expect_setequal(
    bs$rounds$R16$matches$known_winner,
    c(paste0("E", 1:4), paste0("T", 1:4))
  )
})

test_that(".build_bracket_state_pfi: reschedule ghost of a bracket pairing is deduped", {
  r16 <- .cup_results_real_r16()
  # QF draw: 4 matches between the 8 R16 winners... plus a ghost — the
  # E3-E4 tie was first scheduled Jun 10, then moved to Jun 13; both rows
  # survive in the schedules store.
  qf <- tibble::tibble(
    home_team = c("E1", "E3", "T1", "T3", "E3"),
    away_team = c("E2", "E4", "T2", "T4", "E4"),
    match_date = as.Date(c(
      "2026-06-10", "2026-06-13", "2026-06-12", "2026-06-12", "2026-06-10"
    )),
    division = "CUP"
  )
  bs <- .build_bracket_state_pfi(
    pred_d = qf, results = r16, current_season = 2026L
  )
  expect_false(is.null(bs))
  expect_true(bs$rounds$R8$pairings_known)
  expect_equal(nrow(bs$rounds$R8$matches), 4L)
  pairs <- paste(bs$rounds$R8$matches$home_team, bs$rounds$R8$matches$away_team)
  expect_equal(sum(pairs == "E3 E4"), 1L)
  # The ghost must not leak into SF as a phantom 13th bracket match.
  expect_false(bs$rounds$SF$pairings_known)
})

test_that(".build_bracket_state_pfi: played row wins over its stale future duplicate", {
  r16 <- .cup_results_real_r16() # includes E1 1-0 T5 played 2026-05-13
  ghost <- tibble::tibble(
    home_team  = "E1",
    away_team  = "T5",
    match_date = as.Date("2026-06-10"), # stale future copy of the played tie
    division   = "CUP"
  )
  bs <- .build_bracket_state_pfi(
    pred_d = ghost, results = r16, current_season = 2026L
  )
  expect_false(is.null(bs))
  m <- bs$rounds$R16$matches
  expect_equal(sum(m$home_team == "E1" & m$away_team == "T5"), 1L)
  expect_equal(m$known_winner[m$home_team == "E1" & m$away_team == "T5"], "E1")
  # The ghost must not register as an R8 fixture.
  expect_false(bs$rounds$R8$pairings_known)
})

test_that(".build_bracket_state_pfi: 90' tie resolved via next-round participation", {
  r16 <- .cup_results_real_r16()
  # E1 vs T5 ended level after 90' — the store has no ET/shootout outcome.
  r16$away_score[1] <- 1L
  # But the QF draw includes E1, so E1 demonstrably advanced.
  qf <- tibble::tibble(
    home_team  = c("E1", "T1", "E3", "T3"),
    away_team  = c("E2", "T2", "E4", "T4"),
    match_date = as.Date("2026-06-12"),
    division   = "CUP"
  )
  bs <- .build_bracket_state_pfi(
    pred_d = qf, results = r16, current_season = 2026L
  )
  m <- bs$rounds$R16$matches
  expect_equal(m$known_winner[m$home_team == "E1" & m$away_team == "T5"], "E1")
})

test_that(".build_bracket_state_pfi: 90' tie stays NA when the next round is undrawn", {
  r16 <- .cup_results_real_r16()
  r16$away_score[1] <- 1L # E1 vs T5 level, no later matches to infer from
  bs <- .build_bracket_state_pfi(
    pred_d = NULL, results = r16, current_season = 2026L
  )
  m <- bs$rounds$R16$matches
  expect_true(is.na(m$known_winner[m$home_team == "E1" & m$away_team == "T5"]))
})

test_that(".build_bracket_state_pfi: KSÍ TBD placeholder rows are ignored", {
  r16 <- .cup_results_real_r16()
  # Placeholder stubs (round-name vs ".") are parse-filtered since 2026-05-13
  # but legacy rows persist in the schedules store. If they reached the
  # consistency check they would reject every window ("." is never a window
  # team) and the bracket would vanish.
  placeholders <- tibble::tibble(
    home_team  = c("Undanúrslit", "Úrslitaleikur"),
    away_team  = c(".", "."),
    match_date = as.Date(c("2026-06-24", "2026-09-11")),
    division   = "CUP"
  )
  bs <- .build_bracket_state_pfi(
    pred_d = placeholders, results = r16, current_season = 2026L
  )
  expect_false(is.null(bs))
  expect_setequal(bs$cup_teams, c(paste0("E", 1:8), paste0("T", 1:8)))
  expect_false(bs$rounds$R8$pairings_known)
})

test_that(".build_bracket_state_pfi: SF leg beyond the prediction horizon is recovered from the schedule", {
  # Production bug (2026 Mjólkurbikar): prepare_data()'s pred_d truncates cup
  # fixtures at the prediction horizon, so a late SF leg (Fylkir-Afturelding,
  # 21 Jul, fit on 25 Jun) is absent from pred_d even though KSÍ has drawn it.
  # The raw schedule store carries BOTH legs; feeding it in completes the SF.
  # Without it the SF reads pairings_known = FALSE and no bracket.json / the
  # wrong (random-pairing) champion probabilities are published.
  r16 <- .cup_results_real_r16() # winners E1-E4, T1-T4
  r8 <- tibble::tibble(
    home_team  = c("E1", "E3", "T1", "T3"),
    away_team  = c("E2", "E4", "T2", "T4"),
    match_date = as.Date("2026-06-12"),
    home_score = 2L,
    away_score = 1L, # home teams advance: E1, E3, T1, T3
    division   = "CUP",
    season     = 2026L
  )
  results <- dplyr::bind_rows(r16, r8)
  sf1 <- tibble::tibble(
    home_team = "E1", away_team = "E3",
    match_date = as.Date("2026-06-28"), division = "CUP"
  )
  sf2 <- tibble::tibble(
    home_team = "T1", away_team = "T3",
    match_date = as.Date("2026-07-21"), division = "CUP"
  )
  # pred_d sees only SF1 (inside the horizon); the schedule carries both legs.
  bs <- .build_bracket_state_pfi(
    pred_d         = sf1,
    results        = results,
    current_season = 2026L,
    schedule       = dplyr::bind_rows(sf1, sf2)
  )
  expect_false(is.null(bs))
  expect_true(bs$rounds$SF$pairings_known)
  expect_equal(nrow(bs$rounds$SF$matches), 2L)
  expect_setequal(
    paste(bs$rounds$SF$matches$home_team, bs$rounds$SF$matches$away_team),
    c("E1 E3", "T1 T3")
  )
  expect_true(all(is.na(bs$rounds$SF$matches$known_winner))) # SF not played
})

test_that(".extract_division_parquets_pfi: split cell keeps split-phase predicted matches", {
  teams8 <- LETTERS[1:8]
  teams_df <- tibble::tibble(team = teams8, team_nr = seq_along(teams8))
  results <- .mini_reg_results(teams8)

  pgl <- tidyr::expand_grid(
    .draw = 1:5,
    tibble::tibble(
      game_nr = c(1L, 2L),
      home_team = c("A", "G"), away_team = c("B", "H"),
      match_date = as.Date("2026-09-12"),
      division = c("BD_UPPER_PO", "BD_LOWER_PO")
    )
  ) |>
    dplyr::mutate(home_goals = 1, away_goals = 0)

  empty_draws <- tibble::tibble(
    team = character(), component = character(),
    .draw = integer(), value = numeric()
  )
  empty_trajectory <- tibble::tibble(
    round = integer(), team = character(), .draw = integer(),
    component = character(), location = character(), value = numeric()
  )
  testthat::local_mocked_bindings(
    .compute_team_strength_trajectory = function(...) empty_trajectory
  )

  extract_parts <- function(split_config) {
    .extract_division_parquets_pfi(
      target_div = "BD", fit = NULL, teams = teams_df, results = results,
      current_season = 2026L,
      posterior_goals_long = pgl,
      team_strengths_draws = empty_draws,
      home_advantage_draws = empty_draws,
      n_pred_fit = 2L, n_pred_data = 2L,
      sim_inputs = NULL, season_schedule = NULL,
      split_config = split_config
    )
  }

  parts <- extract_parts(list(upper = 4L, lower = 4L))
  expect_setequal(
    paste(parts$predicted_matches$home_team, parts$predicted_matches$away_team),
    c("A B", "G H")
  )
  expect_equal(unique(parts$predicted_matches$count), 5L)

  parts_flat <- extract_parts(NULL)
  expect_equal(nrow(parts_flat$predicted_matches), 0L)
})

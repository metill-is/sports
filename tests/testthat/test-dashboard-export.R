# tests/testthat/test-dashboard-export.R

de_wf_fixture <- function() {
  one <- function(md, h, a, winner) {
    tibble::tibble(
      sport = "football", country = "iceland", sex = "male",
      match_date = as.Date(md), home_team = h, away_team = a,
      market = "moneyline", outcome = c("home", "draw", "away"), line = NA_real_,
      p = c(0.5, 0.3, 0.2), odds = c(2.0, 3.5, 5.0),
      win = c(winner == "home", winner == "draw", winner == "away"),
      division = "BD", round = 1L
    )
  }
  dplyr::bind_rows(one("2026-05-20", "A", "B", "draw"), one("2026-05-21", "C", "D", "away"))
}

de_predicted_fixture <- function() {
  tibble::tibble(
    home_team = "A", away_team = "B", match_date = as.Date("2026-05-20"),
    home_goals = c(0L, 0L, 1L, 1L), away_goals = c(0L, 1L, 0L, 1L),
    count = 1000L, division = "BD", sex = "male", fit_date = as.Date("2026-05-17")
  )
}

de_results_fixture <- function() {
  tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    match_date = as.Date(c("2026-05-20", "2026-05-21")),
    home_team = c("A", "C"), away_team = c("B", "D"),
    home_score = c(1L, 0L), away_score = c(1L, 1L),
    division = "BD", round = 1L, season = 2026L
  )
}

de_odds_fixture <- function() {
  tibble::tibble(
    match_date = as.Date("2026-05-20"), home_team = "A", away_team = "B",
    market = "moneyline", outcome = "home", line = NA_real_,
    odds = c(2.0, 2.1),
    scraped_at = as.POSIXct(c("2026-05-18 09:00", "2026-05-19 09:00"), tz = "UTC")
  )
}

test_that("dashboard_assemble returns the full contract with sex-stratified tables", {
  contract <- dashboard_assemble(
    de_predicted_fixture(), de_results_fixture(), de_odds_fixture(),
    wf_list = list(male = de_wf_fixture()),
    now = as.POSIXct("2026-06-16 12:00:00", tz = "UTC")
  )
  expect_setequal(names(contract), c(
    "meta", "pit_total", "pit_diff", "pit_uniformity", "draw_rate", "scoreline",
    "model_calibration", "skill", "brier_decomp", "market_calibration",
    "market_bias", "disagreement", "line_stability"
  ))
  expect_equal(nrow(contract$meta), 1L)
  expect_equal(contract$meta$sexes, "male")
  expect_true("sex" %in% names(contract$skill))
  expect_equal(unique(contract$skill$sex), "male")
  expect_equal(nrow(contract$line_stability), 1L)
  expect_true(contract$meta$n_devig >= 1)
})

test_that("dashboard_assemble tolerates a sex with no walk-forward data", {
  contract <- dashboard_assemble(
    de_predicted_fixture(), de_results_fixture(), de_odds_fixture(),
    wf_list = list(male = de_wf_fixture(), female = de_wf_fixture()[0, ]),
    now = as.POSIXct("2026-06-16 12:00:00", tz = "UTC")
  )
  expect_equal(unique(contract$skill$sex), "male")
})

test_that("dashboard_write_json writes one valid JSON file per contract table", {
  contract <- dashboard_assemble(
    de_predicted_fixture(), de_results_fixture(), de_odds_fixture(),
    wf_list = list(male = de_wf_fixture()),
    now = as.POSIXct("2026-06-16 12:00:00", tz = "UTC")
  )
  out <- withr::local_tempdir()
  dashboard_write_json(contract, out)
  expect_true(file.exists(file.path(out, "skill.json")))
  expect_true(file.exists(file.path(out, "meta.json")))
  sk <- jsonlite::read_json(file.path(out, "skill.json"))
  expect_gte(length(sk), 1L)
})

test_that("the dashboard export is read-only on the money path (CI-safe)", {
  src <- c(
    readLines(here::here("R", "dashboard-export.R"), warn = FALSE),
    readLines(here::here("scripts", "0Nd_dashboard.R"), warn = FALSE)
  )
  forbidden <- c(
    "append_to_ledger", "store_bets", "settle_ledger", "commit_ledger",
    "place_bets", "preview_bets", "R/placer-", "data/decisions/ledger"
  )
  hits <- forbidden[vapply(forbidden, function(t) any(grepl(t, src, fixed = TRUE)), logical(1))]
  expect_equal(hits, character(0))
})

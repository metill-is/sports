# tests/testthat/test-model-golden.R
#
# Sanity gate: new pipeline's posterior predictive means vs the pre-migration
# fit.rds backups. Local-only — skips on CI (backup isn't committed).
#
# This is NOT bit-exact reproducibility. Data sources differ (new federation
# scrapers vs legacy Excel/Chromote), Stan MCMC is stochastic, and 2 months
# of matches have been played since the backup was taken. The gate asks:
#   does the new pipeline produce predictions in the same ballpark as the
#   old, for the matches the two share?

BACKUP_ROOT <- "/Users/brynjolfurjonsson/sports-backup-20260424-163153"

skip_if_no_backup <- function() {
  if (!dir.exists(BACKUP_ROOT)) {
    testthat::skip(paste("backup absent:", BACKUP_ROOT))
  }
  if (!dir.exists(here::here("data", "beliefs", "latest"))) {
    testthat::skip("new beliefs absent; fit_all.R hasn't run")
  }
}

# Load a legacy fit's posterior-means-per-match (sport, sex) from backup.
# Returns a tibble with (home_team, away_team, home_mean_legacy, away_mean_legacy).
legacy_means <- function(sport, sex) {
  fit_path <- file.path(
    BACKUP_ROOT, "Sports", sport, "iceland",
    "results", sex, "fit.rds"
  )
  pred_path <- file.path(
    BACKUP_ROOT, "Sports", sport, "iceland",
    "results", sex, "pred_d.csv"
  )
  if (!file.exists(fit_path) || !file.exists(pred_path)) {
    return(NULL)
  }

  fit <- readRDS(fit_path)
  pred_d <- readr::read_csv(pred_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "UTF-8")
  )

  draws <- posterior::as_draws_df(fit$draws(c("goals1_pred", "goals2_pred"))) |>
    tibble::as_tibble()
  long <- draws |>
    tidyr::pivot_longer(-c(".chain", ".iteration", ".draw"),
      names_to = "parameter", values_to = "value"
    ) |>
    dplyr::mutate(
      type = dplyr::if_else(stringr::str_detect(.data$parameter, "^goals1_pred"),
        "home_goals", "away_goals"
      ),
      game_nr = as.integer(stringr::str_match(.data$parameter, "\\[(\\d+)\\]$")[, 2])
    ) |>
    dplyr::group_by(.data$game_nr, .data$type) |>
    dplyr::summarise(mean_val = mean(.data$value), .groups = "drop") |>
    tidyr::pivot_wider(names_from = .data$type, values_from = .data$mean_val)

  long |>
    dplyr::inner_join(pred_d[, c("game_nr", "home", "away")], by = "game_nr") |>
    dplyr::transmute(
      home_team = .data$home, away_team = .data$away,
      home_mean_legacy = .data$home_goals,
      away_mean_legacy = .data$away_goals
    )
}

new_means <- function(sport, sex) {
  bl <- read_table("beliefs_latest",
    filter = list(sport = sport, country = "iceland", sex = sex)
  )
  bl |>
    dplyr::group_by(home_team, away_team) |>
    dplyr::summarise(
      home_mean_new = mean(home_goals),
      away_mean_new = mean(away_goals),
      .groups = "drop"
    )
}

TOLERANCES <- list(
  basketball = c(home = 8, away = 8),
  handball   = c(home = 3, away = 3),
  football   = c(home = 0.5, away = 0.5)
)

check_one <- function(sport, sex) {
  legacy <- legacy_means(sport, sex)
  skip_if(is.null(legacy), paste("no legacy fit for", sport, "/", sex))

  new <- new_means(sport, sex)
  merged <- dplyr::inner_join(legacy, new,
    by = c("home_team", "away_team")
  )
  skip_if(
    nrow(merged) == 0L,
    paste("no overlapping matches for", sport, "/", sex)
  )

  tol <- TOLERANCES[[sport]]
  home_diff <- abs(merged$home_mean_new - merged$home_mean_legacy)
  away_diff <- abs(merged$away_mean_new - merged$away_mean_legacy)

  # Allow up to 20% of the overlap to exceed tolerance — this covers matches
  # where the post-backup 2 months of data meaningfully shifted beliefs.
  home_bad <- mean(home_diff > tol[["home"]], na.rm = TRUE)
  away_bad <- mean(away_diff > tol[["away"]], na.rm = TRUE)
  expect_lt(home_bad, 0.20,
    label = paste(sport, sex, "home-mean within", tol[["home"]])
  )
  expect_lt(away_bad, 0.20,
    label = paste(sport, sex, "away-mean within", tol[["away"]])
  )
}

test_that("basketball male beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("basketball", "male")
})
test_that("basketball female beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("basketball", "female")
})
test_that("handball male beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("handball", "male")
})
test_that("handball female beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("handball", "female")
})
test_that("football male beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("football", "male")
})
test_that("football female beliefs are sane vs backup", {
  skip_if_no_backup()
  check_one("football", "female")
})

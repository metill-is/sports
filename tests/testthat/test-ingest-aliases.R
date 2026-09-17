# renormalise_team_aliases(): bring stored results + schedules into line with
# data_source.team_aliases. ingest_league() aliases only newly fetched rows, and
# upsert_table() keys on team names, so stored rows need this pass.

alias_rows <- function(table, sex, season, home, away, date = "2100-01-01") {
  out <- tibble::tibble(
    sport = "basketball", country = "iceland", sex = sex,
    season = as.integer(season),
    match_date = as.Date(date) + seq_along(home) - 1L,
    home_team = home, away_team = away,
    division = "BD", round = seq_along(home)
  )
  if (identical(table, "results")) {
    out$home_score <- 80L
    out$away_score <- 70L
  } else {
    out$kickoff_time <- NA_character_
  }
  out
}

alias_leagues <- function(male = list(`Long A` = "A"), female = NULL) {
  list(basketball_iceland = list(
    sport = "basketball", country = "iceland",
    data_source = list(
      results = "kki_basketball", schedule = "kki_basketball",
      team_aliases = Filter(Negate(is.null), list(male = male, female = female))
    )
  ))
}

alias_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  write_table(dplyr::bind_rows(
    alias_rows("results", "male", 2100, c("Long A", "B"), c("B", "Long A")),
    alias_rows("results", "male", 2101, c("C", "B"), c("B", "C")),
    # The same source spelling in the women's store is not the men's club.
    alias_rows("results", "female", 2100, "Long A", "B")
  ), "results", root = root)
  write_table(
    alias_rows("schedules", "male", 2101, "Long A", "C", date = "2101-02-01"),
    "schedules",
    root = root
  )
  root
}

stored <- function(root, table, sex) {
  d <- read_table(table, root = root, filter = list(sex = sex))
  sort(unique(c(d$home_team, d$away_team)))
}

test_that("a dry run reports the rows it would move and writes nothing", {
  root <- alias_root()
  plan <- renormalise_team_aliases(alias_leagues(), root = root)
  expect_equal(nrow(plan), 3L)
  expect_setequal(plan$table, c("results", "schedules"))
  expect_true(all(plan$sex == "male"))
  expect_true("Long A" %in% stored(root, "results", "male"))
  expect_true("Long A" %in% stored(root, "schedules", "male"))
})

test_that("apply rewrites results and schedules for the configured sex only", {
  root <- alias_root()
  n_before <- nrow(read_table("results", root = root))
  renormalise_team_aliases(alias_leagues(), root = root, apply = TRUE)

  expect_equal(stored(root, "results", "male"), c("A", "B", "C"))
  expect_equal(stored(root, "schedules", "male"), c("A", "C"))
  expect_equal(stored(root, "results", "female"), c("B", "Long A"))
  expect_equal(nrow(read_table("results", root = root)), n_before)

  # Idempotent: nothing left to move.
  again <- renormalise_team_aliases(alias_leagues(), root = root, apply = TRUE)
  expect_equal(nrow(again), 0L)
})

test_that("a colliding partition aborts before any partition is written", {
  root <- alias_root()
  # 2101 gains a row stored under BOTH spellings of the same fixture, so
  # aliasing 2101 collides -- while 2100 would rewrite cleanly on its own.
  write_table(
    dplyr::bind_rows(
      alias_rows("results", "male", 2101, c("C", "B"), c("B", "C")),
      alias_rows("results", "male", 2101, c("Long A", "A"), c("C", "C"), date = "2101-03-01")
    ) |> dplyr::mutate(match_date = as.Date(c("2101-01-01", "2101-01-02", "2101-03-01", "2101-03-01"))),
    "results",
    root = root
  )

  expect_error(
    renormalise_team_aliases(alias_leagues(), root = root, apply = TRUE),
    "duplicate natural keys"
  )
  # The clean 2100 partition was not rewritten either.
  r2100 <- read_table("results", root = root, filter = list(sex = "male", season = 2100L))
  expect_true("Long A" %in% c(r2100$home_team, r2100$away_team))
})

test_that("a stored row with a missing team name does not break the count", {
  root <- alias_root()
  write_table(
    alias_rows("results", "male", 2102, c("Long A", NA), c("B", "B")),
    "results",
    root = root
  )
  plan <- renormalise_team_aliases(alias_leagues(), root = root)
  expect_equal(sum(plan$season == 2102L), 1L)
})

test_that(".apply_team_aliases maps factor columns by label", {
  df <- data.frame(
    home_team = factor(c("B", "Long A")), away_team = factor(c("Long A", "B"))
  )
  out <- .apply_team_aliases(df, list(`Long A` = "A"))
  expect_equal(as.character(out$home_team), c("B", "A"))
  expect_equal(as.character(out$away_team), c("A", "B"))
})

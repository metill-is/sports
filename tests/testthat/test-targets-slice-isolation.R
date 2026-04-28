# Locks in the per-league config-slice contract from _targets.R.
#
# The DAG uses three slice targets per league (league_static_, league_lengjan_,
# league_betting_) so that downstream cache invalidation is precise:
# editing `lengjan.competitions` should rebuild only the lengjan slice (and
# its downstream odds + decide), leaving the static slice byte-identical so
# fits stay cached. These tests verify the slice extractions are stable
# against unrelated edits.

make_synthetic_league <- function() {
  list(
    sport = "basketball",
    country = "iceland",
    sexes = c("male", "female"),
    active = TRUE,
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan",
    data_source = list(
      results = "kki_basketball", schedule = "kki_basketball",
      odds = "lengjan_odds"
    ),
    lengjan = list(
      competitions = list(
        list(id = "1519", name = "BD karla", sex = "male"),
        list(id = "1528", name = "BD kvenna", sex = "female")
      ),
      team_names = list(male = list(), female = list(Haukar = "Haukar kv"))
    ),
    betting = list(
      kelly_frac = 0.10, ev_threshold = 0.0,
      markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
      scoring = list(has_ties = FALSE, tie_threshold = 0L),
      min_bet = 200L, max_age_hours = 48L
    )
  )
}

extract_static <- function(league) {
  league[c("sport", "country", "sexes", "active", "stan_model", "data_source")]
}

test_that("static slice is invariant to lengjan changes", {
  before <- extract_static(make_synthetic_league())

  modified <- make_synthetic_league()
  modified$lengjan$competitions[[1]]$id <- "9999"
  modified$lengjan$team_names$female$Stjarnan <- "Stjarnan kv"
  after <- extract_static(modified)

  expect_identical(before, after)
  expect_identical(rlang::hash(before), rlang::hash(after))
})

test_that("static slice is invariant to betting changes", {
  before <- extract_static(make_synthetic_league())

  modified <- make_synthetic_league()
  modified$betting$kelly_frac <- 0.05
  modified$betting$ev_threshold <- 0.02
  after <- extract_static(modified)

  expect_identical(before, after)
  expect_identical(rlang::hash(before), rlang::hash(after))
})

test_that("lengjan slice is invariant to betting changes", {
  before <- make_synthetic_league()$lengjan

  modified <- make_synthetic_league()
  modified$betting$kelly_frac <- 0.05
  after <- modified$lengjan

  expect_identical(before, after)
  expect_identical(rlang::hash(before), rlang::hash(after))
})

test_that("betting slice is invariant to lengjan changes", {
  before <- make_synthetic_league()$betting

  modified <- make_synthetic_league()
  modified$lengjan$competitions[[1]]$id <- "9999"
  after <- modified$betting

  expect_identical(before, after)
  expect_identical(rlang::hash(before), rlang::hash(after))
})

test_that("lengjan slice DOES change when lengjan changes", {
  before <- make_synthetic_league()$lengjan

  modified <- make_synthetic_league()
  modified$lengjan$competitions[[1]]$id <- "9999"
  after <- modified$lengjan

  expect_false(identical(before, after))
  expect_false(identical(rlang::hash(before), rlang::hash(after)))
})

test_that("static slice carries every field fit_one needs", {
  static <- extract_static(make_synthetic_league())
  expect_true(all(c("sport", "country", "stan_model") %in% names(static)))
  expect_false("lengjan" %in% names(static))
  expect_false("betting" %in% names(static))
})

test_that("targets manifest wires slices, not leagues_config, into fit/decide/publish", {
  withr::with_dir(here::here(), {
    m <- targets::tar_manifest(callr_function = NULL)
  })
  fit_rows <- m[grepl("^fit_", m$name), ]
  expect_gt(nrow(fit_rows), 0)
  expect_true(all(grepl("league_static_", fit_rows$command)))
  expect_false(any(grepl("leagues_config", fit_rows$command)))

  decide_rows <- m[grepl("^decide_", m$name), ]
  expect_gt(nrow(decide_rows), 0)
  expect_true(all(grepl("league_static_", decide_rows$command)))
  expect_true(all(grepl("league_lengjan_", decide_rows$command)))
  expect_true(all(grepl("league_betting_", decide_rows$command)))
  expect_false(any(grepl("leagues_config", decide_rows$command)))

  publish_rows <- m[grepl("^publish_", m$name), ]
  expect_gt(nrow(publish_rows), 0)
  expect_true(all(grepl("league_static_", publish_rows$command)))
  expect_true(all(grepl("league_betting_", publish_rows$command)))
  expect_false(any(grepl("leagues_config", publish_rows$command)))
})

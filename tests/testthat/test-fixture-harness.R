# Contract tests for the committed fixture harness (WS2, spec section 4).
#
# The generator under tools/ must be source()-able from a test *without*
# loading a package: inside a git worktree `here::here()` resolves to the MAIN
# checkout, so a top-level `devtools::load_all(here::here())` would regenerate
# fixtures against a different package than the one being edited.

test_that("the fixture generator is source-able without loading a package", {
  gen <- testthat::test_path("..", "..", "tools", "make-extract-fixtures.R")
  expect_true(file.exists(gen))

  src <- readLines(gen, warn = FALSE)
  # No unguarded load_all / here::here() at column 0 (top level).
  top_level <- grep("^[^ \t#]", src, value = TRUE)
  expect_false(any(grepl("load_all", top_level, fixed = TRUE)))
  expect_false(any(grepl("here::here", top_level, fixed = TRUE)))

  env <- new.env(parent = globalenv())
  expect_silent(sys.source(gen, envir = env))
  expect_true(is.function(env$make_extract_fixtures))
})

test_that("the facts fixture drives prepare_data for all three sports", {
  root <- fixture_facts_root()
  leagues <- load_leagues()

  cells <- list(
    list(key = "basketball_iceland", sex = "male"),
    list(key = "basketball_iceland", sex = "female"),
    list(key = "handball_iceland", sex = "male"),
    list(key = "handball_iceland", sex = "female"),
    list(key = "football_iceland", sex = "male"),
    list(key = "football_iceland", sex = "female")
  )

  for (cell in cells) {
    prep <- suppressMessages(prepare_data(
      leagues[[cell$key]], cell$sex,
      end_date = FIXTURE_END_DATE, root = root
    ))
    info <- paste(cell$key, cell$sex)
    # Training data present, upcoming fixtures inside the DEFAULT 14-day horizon.
    expect_gt(nrow(prep$teams), 5L)
    expect_gt(prep$stan_data$N, 10L)
    expect_gt(nrow(prep$pred_d), 0L)
    expect_equal(prep$stan_data$N_pred, nrow(prep$pred_d), info = info)
    expect_true(all(prep$pred_d$match_date > FIXTURE_END_DATE), info = info)
    expect_true(all(prep$pred_d$match_date <= FIXTURE_END_DATE + 14L), info = info)
  }
})

test_that("the committed 2DT extracts fixture has the 5-parquet contract", {
  base <- testthat::test_path("fixtures", "extracts")
  stamp <- paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))

  five <- c(
    "predicted_matches.parquet", "team_strengths_quantiles.parquet",
    "home_advantage_quantiles.parquet", "final_positions.parquet",
    "points_distribution.parquet"
  )
  cols <- list(
    team_strengths_quantiles = c("team", "component", "location", "quantile", "value"),
    home_advantage_quantiles = c("team", "component", "quantile", "value"),
    final_positions = c("team", "placement", "probability"),
    points_distribution = c("team", "points", "probability")
  )

  for (sport in c("basketball", "handball")) {
    for (sex in c("male", "female")) {
      part <- file.path(
        base, paste0("sport=", sport), "country=iceland",
        paste0("sex=", sex), stamp
      )
      expect_true(dir.exists(part), info = part)
      # `%in%`, not setequal: a later workstream adds fit_meta +
      # round_strengths_quantiles.
      expect_true(all(five %in% list.files(part)), info = part)

      for (ft in names(cols)) {
        df <- arrow::read_parquet(file.path(part, paste0(ft, ".parquet")))
        expect_true(all(cols[[ft]] %in% names(df)), info = paste(part, ft))
        expect_gt(nrow(df), 0L)
      }
    }
  }
})

test_that("the committed extracts fixture stays inside the 250 KB budget", {
  files <- list.files(
    testthat::test_path("fixtures", "extracts"),
    recursive = TRUE, full.names = TRUE
  )
  expect_gt(length(files), 0L)
  expect_lt(sum(file.info(files)$size), 250L * 1024L)
})

test_that("fixture_extracts_root materialises a readable tree", {
  root <- fixture_extracts_root()
  part <- file.path(
    root, "sport=handball", "country=iceland", "sex=female",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  expect_true(dir.exists(part))
  fp <- arrow::read_parquet(file.path(part, "final_positions.parquet"))
  expect_equal(
    sum(fp$probability),
    length(unique(fp$team)),
    tolerance = 1e-8
  )
})

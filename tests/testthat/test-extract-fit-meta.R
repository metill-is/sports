# fit_meta.parquet -- the one PARTITION-LEVEL file in an extracts partition.
#
# Everything else in a partition is division-keyed and the reader splits it on
# the payload `division` column. fit_meta describes the FIT, not a cell, so it
# carries no division column at all and the reader must not split it -- a split
# would filter it to zero rows for every division and silently publish an empty
# provenance block.
#
# It exists so the publisher stops having to hold a fit just to recompute
# `posterior::ndraws(fit$draws("lp__"))`, and so the published meta can say
# which model and which units produced the numbers.

fit_meta_of <- function(partition) {
  arrow::read_parquet(file.path(partition, "fit_meta.parquet"))
}

fm_2dt_cell <- function(sport, sex, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  st <- suppressMessages(local_stub_2dt(league, sex, root = root))
  extracts_root <- file.path(
    withr::local_tempdir(.local_envir = env), "extracts"
  )
  fn <- switch(sport,
    basketball = extract_basketball_iceland,
    handball = extract_handball_iceland
  )
  suppressMessages(fn(
    fit = st$fit, league = league, sex = sex,
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  list(
    league = league, st = st,
    partition = file.path(
      extracts_root, paste0("sport=", sport), "country=iceland",
      paste0("sex=", sex),
      paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
    )
  )
}

expect_fit_meta_shape <- function(fm, stan_model, model_units, n_draws) {
  expect_equal(nrow(fm), 1L)
  expect_setequal(
    names(fm), c("n_draws", "fit_date", "stan_model", "model_units")
  )
  expect_type(fm$n_draws, "integer")
  expect_s3_class(fm$fit_date, "Date")
  expect_type(fm$stan_model, "character")
  expect_type(fm$model_units, "character")
  expect_equal(fm$fit_date, FIXTURE_FIT_DATE)
  expect_equal(fm$stan_model, stan_model)
  expect_equal(fm$model_units, model_units)
  expect_equal(fm$n_draws, n_draws)
  # The seam WS9's reader depends on: partition-level, never division-keyed.
  expect_false("division" %in% names(fm))
}

test_that("basketball writes a partition-level fit_meta", {
  cell <- fm_2dt_cell("basketball", "male")
  expect_fit_meta_shape(
    fit_meta_of(cell$partition),
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan",
    model_units = "points",
    n_draws = posterior::ndraws(cell$st$fit$draws("lp__"))
  )
  expect_equal(
    fit_meta_of(cell$partition)$stan_model, cell$league$stan_model
  )
})

test_that("handball writes a partition-level fit_meta", {
  cell <- fm_2dt_cell("handball", "female")
  expect_fit_meta_shape(
    fit_meta_of(cell$partition),
    stan_model = "handball_iceland/2d_student_t.stan",
    model_units = "goals",
    n_draws = posterior::ndraws(cell$st$fit$draws("lp__"))
  )
  expect_equal(
    fit_meta_of(cell$partition)$stan_model, cell$league$stan_model
  )
})

test_that("the 2DT partition holds exactly the seven-parquet contract", {
  cell <- fm_2dt_cell("handball", "male")
  expect_setequal(
    list.files(cell$partition),
    c(
      "predicted_matches.parquet",
      "team_strengths_quantiles.parquet",
      "round_strengths_quantiles.parquet",
      "home_advantage_quantiles.parquet",
      "final_positions.parquet",
      "points_distribution.parquet",
      "fit_meta.parquet"
    )
  )
})

test_that("football writes the same fit_meta, on log_rate units", {
  root <- fixture_facts_root()
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  build_football_extracts_fixture(root, extracts_root, "male")
  partition <- file.path(
    extracts_root, "sport=football", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )

  expect_fit_meta_shape(
    fit_meta_of(partition),
    stan_model = "football_iceland/bivariate_poisson_no_inflation.stan",
    model_units = "log_rate",
    n_draws = FIXTURE_N_DRAWS
  )
  expect_equal(
    fit_meta_of(partition)$stan_model,
    load_leagues()[["football_iceland"]]$stan_model
  )
})

test_that("fit_meta does not enter football's division-keyed file loop", {
  # read_extracted_iceland() splits every file it reads on the payload
  # `division`. fit_meta has none, so it must be reached through the profile's
  # optional slot and degrade to the 0-row tibble rather than being filtered to
  # nothing without anybody noticing.
  profile <- sport_publish_profile("football")
  expect_false("fit_meta" %in% profile$required_extracts)
  expect_true("fit_meta" %in% profile$optional_extracts)
  expect_setequal(
    names(profile$empty_extracts$fit_meta),
    c("n_draws", "fit_date", "stan_model", "model_units")
  )
})

test_that(".fit_meta_tibble derives model_units from the sport, not config", {
  # The value derivation all three write sites share. `extract_football_iceland()`
  # itself needs a real CmdStanMCMC fit and is skip-gated on one, so this is
  # where football's branch is actually exercised.
  fit <- stub_fit(list(
    lp__ = matrix(-1, nrow = 7L, ncol = 1L, dimnames = list(NULL, "lp__"))
  ))
  leagues <- load_leagues()

  for (cell in list(
    list(key = "basketball_iceland", sport = "basketball", units = "points"),
    list(key = "handball_iceland", sport = "handball", units = "goals"),
    list(key = "football_iceland", sport = "football", units = "log_rate")
  )) {
    fm <- .fit_meta_tibble(
      fit, as.Date("2100-01-01"), leagues[[cell$key]]$stan_model, cell$sport
    )
    expect_equal(nrow(fm), 1L)
    expect_equal(fm$n_draws, 7L)
    expect_equal(fm$model_units, cell$units, info = cell$sport)
    expect_equal(fm$stan_model, leagues[[cell$key]]$stan_model)
    expect_false("division" %in% names(fm))
  }

  expect_error(
    .fit_meta_tibble(fit, as.Date("2100-01-01"), "x.stan", "curling"),
    "curling"
  )
})

# --- engine provenance (football v2 on INLA) ---------------------------------

test_that("an INLA fit's fit_meta names the engine and the model, not a Stan file", {
  stan_file <- "football_iceland/bivariate_poisson_no_inflation.stan"
  draws <- list(lp__ = matrix(0, nrow = 50L, ncol = 1L))
  inla_fit <- stub_fit(draws)
  inla_fit$engine <- "inla"
  inla_fit$version <- "2.0"
  fm <- .fit_meta_tibble(inla_fit, as.Date("2027-04-10"), stan_file, "football")
  expect_identical(fm$engine, "inla")
  expect_identical(fm$model_version, "2.0")
  # The league's Stan program did not produce these numbers.
  expect_identical(fm$stan_model, NA_character_)
  expect_identical(fm$n_draws, 50L)
  expect_identical(fm$model_units, "log_rate")

  # A Stan fit writes exactly what it wrote before the column existed.
  stan <- .fit_meta_tibble(stub_fit(draws), as.Date("2027-04-10"), stan_file, "football")
  expect_named(stan, c("n_draws", "fit_date", "stan_model", "model_units"))
  expect_identical(stan$stan_model, stan_file)
})

test_that("a fit_meta written before the engine column reads as a Stan fit", {
  dir <- withr::local_tempdir()
  old <- tibble::tibble(
    n_draws = 4000L, fit_date = as.Date("2026-10-20"),
    stan_model = "football_iceland/bivariate_poisson_no_inflation.stan", model_units = "log_rate"
  )
  arrow::write_parquet(old, file.path(dir, "fit_meta.parquet"))
  fm <- .read_fit_meta(dir)
  expect_identical(fm$engine, "stan")
  expect_identical(fm$model_version, NA_character_)
  expect_identical(fm$stan_model, old$stan_model)
  expect_identical(fm$n_draws, 4000L)

  new <- tibble::tibble(
    n_draws = 4000L, fit_date = as.Date("2027-04-10"), stan_model = NA_character_,
    model_units = "log_rate", engine = "inla", model_version = "2.0"
  )
  dir2 <- withr::local_tempdir()
  arrow::write_parquet(new, file.path(dir2, "fit_meta.parquet"))
  expect_identical(.read_fit_meta(dir2)$engine, "inla")
  expect_identical(.read_fit_meta(dir2)$model_version, "2.0")

  # A partition with no fit_meta at all: no rows, the same columns.
  none <- .read_fit_meta(withr::local_tempdir())
  expect_identical(nrow(none), 0L)
  expect_named(none, names(fm))
})

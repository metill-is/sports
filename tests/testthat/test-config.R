test_that("load_leagues() reads a valid leagues.yml", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    basketball_iceland = list(
      sport = "basketball",
      country = "iceland",
      sexes = list("male", "female"),
      active = TRUE,
      data_source = list(results = "kki_basketball", schedule = "kki_basketball", odds = "lengjan_odds"),
      stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
    )
  ), tmp)

  leagues <- load_leagues(tmp, validate = FALSE) # skip validation: the in-memory schema path resolves via here()

  expect_equal(names(leagues), "basketball_iceland")
  expect_equal(leagues$basketball_iceland$sport, "basketball")
  expect_equal(leagues$basketball_iceland$sexes, c("male", "female"))
})

test_that("load_leagues() rejects a schema-invalid yml when validation on", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    basketball_iceland = list(
      sport = "curling", # invalid enum
      country = "iceland",
      sexes = list("male"),
      active = TRUE,
      data_source = list(results = "x", schedule = "x"),
      stan_model = "x.stan"
    )
  ), tmp)

  schema <- file.path(rprojroot::find_package_root_file(), "config", "leagues.schema.json")
  expect_error(load_leagues(tmp, schema_path = schema), regexp = "curling|sport")
})

test_that("filter_leagues() narrows by selector", {
  leagues <- list(
    football_iceland   = list(sport = "football", country = "iceland", active = TRUE),
    basketball_iceland = list(sport = "basketball", country = "iceland", active = TRUE),
    football_england   = list(sport = "football", country = "england", active = FALSE)
  )

  expect_setequal(
    names(filter_leagues(leagues, sport = "football")),
    c("football_iceland", "football_england")
  )
  expect_setequal(
    names(filter_leagues(leagues, active_only = TRUE)),
    c("football_iceland", "basketball_iceland")
  )
  expect_equal(
    names(filter_leagues(leagues, league = "basketball_iceland")),
    "basketball_iceland"
  )
})

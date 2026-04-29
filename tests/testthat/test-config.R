minimal_league <- function(overrides = list()) {
  base <- list(
    sport = "basketball",
    country = "iceland",
    sexes = list("male", "female"),
    active = TRUE,
    data_source = list(results = "kki_basketball", schedule = "kki_basketball", odds = "lengjan_odds"),
    stan_model = "basketball_iceland/2d_student_t_scalarsigma.stan"
  )
  utils::modifyList(base, overrides)
}

schema_path <- function() {
  file.path(rprojroot::find_package_root_file(), "config", "leagues.schema.json")
}

test_that("load_leagues() reads a valid leagues.yml", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(basketball_iceland = minimal_league()), tmp)

  leagues <- load_leagues(tmp, validate = FALSE) # skip validation: the in-memory schema path resolves via here()

  expect_equal(names(leagues), "basketball_iceland")
  expect_equal(leagues$basketball_iceland$sport, "basketball")
  expect_equal(leagues$basketball_iceland$sexes, c("male", "female"))
})

test_that("load_leagues() rejects a schema-invalid yml when validation on", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    basketball_iceland = minimal_league(list(sport = "curling")) # invalid enum
  ), tmp)

  expect_error(load_leagues(tmp, schema_path = schema_path()), regexp = "curling|sport")
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

# ---------------------------------------------------------------------------
# Regression tests for code-review fixes
# ---------------------------------------------------------------------------

test_that("load_leagues() accepts single-sex leagues (C1 regression)", {
  # Single-element sexes must stay as arrays after YAML -> JSON conversion;
  # auto_unbox = TRUE would otherwise flatten them to scalars and trip schema validation.
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    football_england = minimal_league(list(
      sport = "football",
      country = "england",
      sexes = list("male"),
      stan_model = "football_england/model.stan",
      betting = list(
        kelly_frac = 0.25,
        ev_threshold = 0.0,
        markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
        scoring = list(has_ties = TRUE, tie_threshold = 0),
        min_bet = 200
      )
    ))
  ), tmp)

  expect_no_error(load_leagues(tmp, schema_path = schema_path()))
})

test_that("load_leagues() accepts multi-word country league keys (I1 regression)", {
  # handball_czech_republic is a real legacy league; the old regex ^[a-z]+_[a-z]+$ rejected it.
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(
    handball_czech_republic = minimal_league(list(
      sport = "handball",
      country = "czech-republic",
      sexes = list("male"),
      active = FALSE,
      stan_model = "handball/czech-republic/model.stan"
    ))
  ), tmp)

  expect_no_error(load_leagues(tmp, schema_path = schema_path()))
})

test_that("load_leagues() fails loud when schema file is missing (I2)", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(basketball_iceland = minimal_league()), tmp)

  expect_error(
    load_leagues(tmp, schema_path = "/nonexistent/leagues.schema.json"),
    regexp = "not found"
  )
})

test_that("load_leagues() fails loud when YAML is empty (I3)", {
  empty_tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines("", empty_tmp)

  expect_error(
    load_leagues(empty_tmp, validate = FALSE),
    regexp = "empty|NULL"
  )
})

test_that("load_leagues() rejects a config missing a required field (I4)", {
  tmp <- withr::local_tempfile(fileext = ".yml")
  bad <- minimal_league()
  bad$stan_model <- NULL # drop a required field
  yaml::write_yaml(list(basketball_iceland = bad), tmp)

  expect_error(
    load_leagues(tmp, schema_path = schema_path()),
    regexp = "stan_model|required"
  )
})

test_that("load_leagues() rejects unknown top-level keys (I4)", {
  # The league-key pattern is [a-z_]+ — uppercase letters must trip additionalProperties: false.
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(list(Basketball_Iceland = minimal_league()), tmp)

  expect_error(
    load_leagues(tmp, schema_path = schema_path()),
    regexp = "additional|properties|Basketball"
  )
})

test_that("filter_leagues() with no args returns input unchanged (I4)", {
  leagues <- list(
    football_iceland   = list(sport = "football", country = "iceland", active = TRUE),
    basketball_iceland = list(sport = "basketball", country = "iceland", active = FALSE)
  )
  expect_identical(filter_leagues(leagues), leagues)
})

test_that("filter_leagues() applies sport and active_only together (I4)", {
  leagues <- list(
    football_iceland   = list(sport = "football", country = "iceland", active = TRUE),
    football_england   = list(sport = "football", country = "england", active = FALSE),
    basketball_iceland = list(sport = "basketball", country = "iceland", active = TRUE)
  )
  result <- filter_leagues(leagues, sport = "football", active_only = TRUE)
  expect_equal(names(result), "football_iceland")
})

test_that("filter_leagues(has_lengjan = TRUE) keeps Icelandic leagues", {
  leagues <- load_leagues()
  active <- filter_leagues(leagues, active_only = TRUE, has_lengjan = TRUE)
  expect_gt(length(active), 0L)
  for (l in active) {
    expect_true(length(l$lengjan$competitions) > 0L)
  }
})

test_that("filter_leagues(has_lengjan = TRUE) drops leagues without competitions", {
  leagues <- list(
    with_lengjan = list(
      sport = "football", country = "iceland", active = TRUE,
      lengjan = list(competitions = list(list(id = "1", name = "x", sex = "male")))
    ),
    no_lengjan = list(
      sport = "basketball", country = "iceland", active = TRUE
    ),
    empty_competitions = list(
      sport = "handball", country = "iceland", active = TRUE,
      lengjan = list(competitions = list())
    )
  )
  result <- filter_leagues(leagues, has_lengjan = TRUE)
  expect_equal(names(result), "with_lengjan")
})

test_that("load_bankroll exposes kelly_ceiling and max_match_stake_default defaults", {
  tmp_yaml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "initial_pool: 10000",
    "daily_budget_frac: 0.05",
    "daily_budget_min_isk: 1000"
  ), tmp_yaml)
  empty_root <- withr::local_tempdir()
  b <- load_bankroll(path = tmp_yaml, ledger_root = empty_root)
  expect_equal(b$kelly_ceiling, 0.25)
  expect_equal(b$max_match_stake_default, 1.0)
})

test_that("load_bankroll honours explicit kelly_ceiling override", {
  tmp_yaml <- withr::local_tempfile(fileext = ".yml")
  writeLines(c(
    "initial_pool: 10000",
    "daily_budget_frac: 0.05",
    "daily_budget_min_isk: 1000",
    "kelly_ceiling: 0.15",
    "max_match_stake_default: 0.40"
  ), tmp_yaml)
  empty_root <- withr::local_tempdir()
  b <- load_bankroll(path = tmp_yaml, ledger_root = empty_root)
  expect_equal(b$kelly_ceiling, 0.15)
  expect_equal(b$max_match_stake_default, 0.40)
})

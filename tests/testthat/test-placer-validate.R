test_that("validate_team_names_config passes when all recs have a team_names entry", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list("KR" = "KR Reykjavik"))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "KR"
  )
  expect_invisible(validate_team_names_config(leagues, recs))
})

test_that("validate_team_names_config errors when a league lacks team_names", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list() # no team_names key
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "team_names"
  )
})

test_that("validate_team_names_config errors when a recommended team is unmapped", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list("KR" = "KR Reykjavik"))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "Mystery FC"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "Mystery FC"
  )
})

test_that("validate_recommendations_schema accepts the canonical column set", {
  recs <- tibble::tibble(
    run_id = Sys.time(), sport = "football", country = "iceland", sex = "male",
    match_date = as.Date("2026-04-25"),
    home_team = "KR", away_team = "FH",
    market = "moneyline", outcome = "home", line = NA_real_,
    p = 0.55, odds = 1.85, ev = 0.02, kelly = 0.05, bet_amount = 200
  )
  expect_invisible(validate_recommendations_schema(recs))
})

test_that("validate_recommendations_schema errors on a missing column", {
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male"
    # missing home_team, away_team, etc.
  )
  expect_error(
    validate_recommendations_schema(recs),
    "missing|column"
  )
})

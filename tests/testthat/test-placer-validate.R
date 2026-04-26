# Existing 6 cases migrated to nested team_names shape, plus 4 new cases
# for per-sex behaviour.

test_that("validate_team_names_config passes when all recs have a team_names entry", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR Reykjavik"),
        female = list()
      ))
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
      lengjan = list(team_names = list(
        male = list("KR" = "KR Reykjavik"),
        female = list()
      ))
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
  )
  expect_error(
    validate_recommendations_schema(recs),
    "missing|column"
  )
})

test_that("validate_team_names_config errors when league key is absent from leagues.yml", {
  leagues <- list()
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "football_iceland"
  )
})

test_that("validate_team_names_config errors loudly on non-list leagues input", {
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config("not a list", recs),
    "named list"
  )
})

test_that("validate_team_names_config errors loudly on recs missing core columns", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"), female = list()
      ))
    )
  )
  bad_recs <- tibble::tibble(sport = "football", country = "iceland")
  expect_error(
    validate_team_names_config(leagues, bad_recs),
    "missing column"
  )
})

# ---------- New per-sex behaviour ----------

test_that("validate_team_names_config errors when the rec's sex sub-map is empty", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "female",
    home_team = "Fram", away_team = "Stjarnan"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "empty.*sub-map|female|data/facts/odds"
  )
})

test_that("validate_team_names_config does not satisfy a male rec from the female sub-map", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list(),
        female = list("Fram" = "Fram kv")
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "male",
    home_team = "Fram", away_team = "KR"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "male|empty"
  )
})

test_that("validate_team_names_config errors when team_names lacks the rec's sex key entirely", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR")
        # no female key at all
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "female",
    home_team = "Fram", away_team = "Stjarnan"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "female|sub-map"
  )
})

test_that("validate_team_names_config errors with a clear message when rec$sex is unknown", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland",
      lengjan = list(team_names = list(
        male = list("KR" = "KR"),
        female = list()
      ))
    )
  )
  recs <- tibble::tibble(
    sport = "football", country = "iceland", sex = "all",
    home_team = "KR", away_team = "FH"
  )
  expect_error(
    validate_team_names_config(leagues, recs),
    "invalid sex|sex value"
  )
})

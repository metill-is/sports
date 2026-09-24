test_that("classify_competition maps Icelandic names to (sex, division)", {
  expect_equal(
    classify_competition("Besta deild karla", "football", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "male", division = "BD")
  )
  expect_equal(
    classify_competition("Besta deild kvenna", "football", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "female", division = "BD")
  )
  expect_equal(classify_competition("Lengjudeildin", "football", "iceland")$division, "LD1")
  expect_equal(classify_competition("Lengjudeild kv.", "football", "iceland")$sex, "female")
  expect_equal(classify_competition("2. deild karla", "football", "iceland")$division, "LD2")
  expect_equal(classify_competition("3. deild karla", "football", "iceland")$division, "LD3")
  expect_equal(
    classify_competition("Mjólkurbikar kvenna", "football", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "female", division = "CUP")
  )
  expect_equal(classify_competition("Mjólkurbikar karla", "football", "iceland")$division, "CUP")
})

test_that("classify_competition flags unknown names low-confidence with NA division", {
  r <- classify_competition("Some New Playoff", "football", "iceland")
  expect_true(is.na(r$division))
  expect_equal(r$confidence, "low")
})


test_that("match_team_names matches Lengjan renderings to canonical names", {
  known <- c("FH", "Víkingur R.", "Valur", "Þróttur R.")
  out <- match_team_names(
    c("FH kv", "Víkingur Rvk kv", "Þróttur Rvk kv", "Qwerty United"),
    known
  )
  expect_named(out, c("lengjan", "canonical_guess", "confidence"))
  expect_equal(out$canonical_guess[out$lengjan == "FH kv"], "FH")
  expect_equal(out$canonical_guess[out$lengjan == "Víkingur Rvk kv"], "Víkingur R.")
  expect_equal(out$confidence[out$lengjan == "FH kv"], "high")
  expect_true(is.na(out$canonical_guess[out$lengjan == "Qwerty United"]))
  expect_equal(out$confidence[out$lengjan == "Qwerty United"], "low")
})

test_that("match_team_names handles an empty rendering set", {
  out <- match_team_names(character(0), c("FH"))
  expect_equal(nrow(out), 0L)
  expect_named(out, c("lengjan", "canonical_guess", "confidence"))
})

test_that("match_team_names returns all low/NA when known_teams is empty", {
  out <- match_team_names(c("FH kv", "KR"), character(0))
  expect_equal(nrow(out), 2L)
  expect_true(all(is.na(out$canonical_guess)))
  expect_true(all(out$confidence == "low"))
})

test_that("match_team_names degrades an ambiguous distance tie to low", {
  out <- match_team_names("KX", c("KR", "KA")) # adist 1 to both
  expect_true(is.na(out$canonical_guess))
  expect_equal(out$confidence, "low")
})

test_that("discover_new_competitions flags a new modelled comp and skips configured + unmodelled ones", {
  leagues <- list(
    football_iceland = list(
      sport = "football", country = "iceland", active = TRUE,
      lengjan = list(competitions = list(list(id = "746", name = "Besta deild karla", sex = "male"))),
      publish_divisions = list(
        male = list(
          list(code = "BD"), list(code = "LD1"), list(code = "LD2"),
          list(code = "LD3"), list(code = "CUP")
        )
      )
    )
  )
  fake_list <- function(sport, country) {
    tibble::tibble(
      sport = sport, country = country,
      comp_id = c("746", "757", "9999"),
      lengjan_name = c("Besta deild karla", "Lengjudeildin", "4. deild karla")
    )
  }
  fake_tn <- function(comp_id, sport, country, sex, division) {
    tibble::tibble(
      lengjan = "Some Team", canonical_guess = "Some Team", confidence = "high"
    )
  }

  res <- discover_new_competitions(leagues, list_fn = fake_list, team_names_fn = fake_tn)

  expect_length(res$competitions, 1L) # 757 only
  f <- res$competitions[[1L]]
  expect_equal(f$comp_id, "757")
  expect_equal(f$inferred_division, "LD1")
  expect_true(f$modelled)
  expect_equal(res$unmodelled_offered_count, 1L) # 4. deild (LD4) not modelled
  expect_s3_class(f$proposed_team_names, "tbl_df")
})

test_that(".known_teams_for reads results and filters by division correctly", {
  tmp <- withr::local_tempdir()

  results <- tibble::tibble(
    sport = "football",
    country = "iceland",
    sex = "male",
    season = 2026L,
    match_date = as.Date("2026-05-01"),
    home_team = c("Valur", "Fram"),
    away_team = c("KR", "HK"),
    home_score = c(2L, 1L),
    away_score = c(1L, 0L),
    division = c("BD", "LD1"),
    round = c(1L, 1L)
  )
  write_table(results, "results", root = tmp)

  bd_teams <- sports:::.known_teams_for("football", "iceland", "male", "BD", root = tmp)
  expect_setequal(bd_teams, c("Valur", "KR")) # Bug 2: division filter must work
  expect_false("Fram" %in% bd_teams) # LD1 teams must not bleed in
  expect_false("HK" %in% bd_teams)

  all_teams <- sports:::.known_teams_for("football", "iceland", "male", NA, root = tmp)
  expect_setequal(all_teams, c("Valur", "KR", "Fram", "HK")) # Bug 1: read_table("results") must work
})


test_that("write_discovery_proposal round-trips JSON with UTF-8 intact", {
  tmp <- withr::local_tempdir()
  findings <- list(
    competitions = list(list(
      sport = "football", country = "iceland", comp_id = "757",
      lengjan_name = "Lengjudeildin", inferred_sex = "male",
      inferred_division = "LD1", classify_confidence = "high",
      modelled = TRUE, status = "new",
      proposed_team_names = tibble::tibble(
        lengjan = c("V\u00edkingur \u00d3l.", "Zzz"),
        canonical_guess = c("V\u00edkingur \u00d3.", NA_character_),
        confidence = c("high", "low")
      )
    )),
    unmodelled_offered_count = 2L
  )
  path <- write_discovery_proposal(findings, root = tmp)
  expect_true(file.exists(path))
  expect_true(file.exists(file.path(tmp, "discovery", "SUMMARY.md")))

  back <- jsonlite::read_json(path)
  expect_equal(length(back$competitions), 1L)
  expect_equal(back$competitions[[1]]$comp_id, "757")
  expect_equal(back$competitions[[1]]$proposed_team_names[[1]]$lengjan, "V\u00edkingur \u00d3l.")
  expect_equal(back$unmodelled_offered_count, 2L)
  # JSON null round-trips as a zero-length list via jsonlite::read_json (simplifyVector=FALSE)
  expect_true(is.list(back$competitions[[1]]$proposed_team_names[[2]]$canonical_guess) &&
    length(back$competitions[[1]]$proposed_team_names[[2]]$canonical_guess) == 0L)
})

test_that("write_discovery_proposal writes an empty competitions array cleanly", {
  tmp <- withr::local_tempdir()
  path <- write_discovery_proposal(
    list(competitions = list(), unmodelled_offered_count = 0L),
    root = tmp
  )
  back <- jsonlite::read_json(path)
  expect_equal(length(back$competitions), 0L)
})

test_that("classify_competition knows handball and basketball divisions", {
  expect_equal(
    classify_competition("Olísdeild karla", "handball", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "male", division = "OD")
  )
  expect_equal(classify_competition("Olísdeild kvenna", "handball", "iceland")$sex, "female")
  expect_equal(classify_competition("Grill 66 deild karla", "handball", "iceland")$division, "G66")
  expect_equal(classify_competition("Bónusdeild karla", "basketball", "iceland")$division, "BD")
  expect_equal(
    classify_competition("1. deild kvenna", "basketball", "iceland")[c("sex", "division")],
    tibble::tibble(sex = "female", division = "1D")
  )
  expect_equal(classify_competition("Powerade bikarinn", "handball", "iceland")$division, "CUP")
  # Football keeps its own vocabulary: "1. deild" is not a football code.
  expect_true(is.na(classify_competition("1. deild karla", "football", "iceland")$division))
})

test_that("lengjan_list_competitions lists Icelandic competitions per sport", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  hb <- lengjan_list_competitions("handball", "iceland", ev)
  expect_equal(hb$comp_id, "1269")
  expect_equal(hb$lengjan_name, "Olísdeild karla")
  fb <- lengjan_list_competitions("football", "iceland", ev)
  expect_setequal(fb$comp_id, c("27524", "20442"))
  # The fixture's only basketball event is the WNBA (country US).
  expect_equal(nrow(lengjan_list_competitions("basketball", "iceland", ev)), 0L)
})

test_that("propose_team_names drafts from the program's participants", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  tn <- propose_team_names("1269", ev, known_teams = c("FH", "Haukar", "Valur"))
  expect_setequal(tn$lengjan, c("FH", "Haukar"))
  expect_true(all(tn$confidence == "high"))
})

test_that("discover_new_competitions visits a scrape-mode league with no competitions", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  leagues <- list(handball_iceland = list(
    sport = "handball", country = "iceland", active = TRUE,
    lengjan = list(competitions = list()),
    betting = list(mode = "scrape"),
    publish_divisions = list(male = list(list(code = "OD"), list(code = "G66")))
  ))
  res <- discover_new_competitions(leagues, events = ev, root = withr::local_tempdir())
  expect_length(res$competitions, 1L)
  f <- res$competitions[[1L]]
  expect_equal(f$comp_id, "1269")
  expect_equal(f$inferred_division, "OD")
  expect_equal(f$inferred_sex, "male")
})

test_that("discover_new_competitions skips a league at betting.mode off", {
  ev <- parse_lengjan_program(.fx("current-program.json"))
  leagues <- list(handball_iceland = list(
    sport = "handball", country = "iceland", active = TRUE,
    lengjan = list(competitions = list()), betting = list(mode = "off")
  ))
  res <- discover_new_competitions(leagues, events = ev, root = withr::local_tempdir())
  expect_length(res$competitions, 0L)
})

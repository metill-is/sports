test_that("parse_competition_dropdown extracts comp_id + name from the league select", {
  html <- rvest::read_html(testthat::test_path("fixtures", "lengjan-parent-page.html"))
  comps <- parse_competition_dropdown(html)

  expect_s3_class(comps, "tbl_df")
  expect_named(comps, c("comp_id", "lengjan_name"))
  expect_true(nrow(comps) >= 1L)
  expect_true(all(nzchar(comps$comp_id))) # placeholder ("" value) dropped
  expect_false(any(comps$lengjan_name == "Veldu deild"))
  expect_true("746" %in% comps$comp_id) # Besta deild karla, present at capture
})

test_that("parse_competition_dropdown returns empty tibble when no league select present", {
  html <- rvest::read_html("<html><body><p>no selects</p></body></html>")
  comps <- parse_competition_dropdown(html)
  expect_named(comps, c("comp_id", "lengjan_name"))
  expect_equal(nrow(comps), 0L)
})

test_that("parse_competition_dropdown finds the league select when placeholder is not first", {
  # Placeholder option "Veldu deild" is the SECOND option (not first).
  # The function must still identify this as the league select and return
  # only the real options (value != "").
  html_str <- paste0(
    "<html><body>",
    "<select>",
    '<option value="100">Foo</option>',
    '<option value="">Veldu deild</option>',
    '<option value="200">Bar</option>',
    "</select>",
    "</body></html>"
  )
  html <- rvest::read_html(html_str)
  comps <- parse_competition_dropdown(html)
  expect_s3_class(comps, "tbl_df")
  expect_named(comps, c("comp_id", "lengjan_name"))
  expect_equal(sort(comps$comp_id), c("100", "200"))
  expect_false(any(comps$lengjan_name == "Veldu deild"))
})

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
})

test_that("write_discovery_proposal writes an empty competitions array cleanly", {
  tmp <- withr::local_tempdir()
  path <- write_discovery_proposal(
    list(competitions = list(), unmodelled_offered_count = 0L), root = tmp
  )
  back <- jsonlite::read_json(path)
  expect_equal(length(back$competitions), 0L)
})

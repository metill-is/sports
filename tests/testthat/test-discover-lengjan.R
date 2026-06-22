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
  expect_equal(classify_competition("Besta deild karla", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "male", division = "BD"))
  expect_equal(classify_competition("Besta deild kvenna", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "female", division = "BD"))
  expect_equal(classify_competition("Lengjudeildin", "football", "iceland")$division, "LD1")
  expect_equal(classify_competition("Lengjudeild kv.", "football", "iceland")$sex, "female")
  expect_equal(classify_competition("2. deild karla", "football", "iceland")$division, "LD2")
  expect_equal(classify_competition("3. deild karla", "football", "iceland")$division, "LD3")
  expect_equal(classify_competition("Mjólkurbikar kvenna", "football", "iceland")[c("sex","division")],
               tibble::tibble(sex = "female", division = "CUP"))
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

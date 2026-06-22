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

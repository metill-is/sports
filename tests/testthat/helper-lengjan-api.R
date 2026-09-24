# Reads the committed Lengjan JSON API fixtures (tests/testthat/fixtures/lengjan-api/).
.fx <- function(name) {
  jsonlite::read_json(
    testthat::test_path("fixtures", "lengjan-api", name),
    simplifyVector = FALSE
  )
}

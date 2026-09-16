# Spec 2026-09-16 §5.1 / D7: basketball women's 1. deild is held on its 2026
# season until its 2027 reserve-side aliases and format are confirmed.

test_that("only basketball women's 1. deild is held, on season 2026", {
  expect_identical(
    .iceland_division_preseason_hold("basketball_iceland", "female"),
    c(BD = NA_integer_, `1D` = 2026L)
  )
  expect_true(all(is.na(.iceland_division_preseason_hold("basketball_iceland", "male"))))
  for (sex in c("male", "female")) {
    expect_true(all(is.na(.iceland_division_preseason_hold("handball_iceland", sex))))
    expect_true(all(is.na(.iceland_division_preseason_hold("football_iceland", sex))))
  }
})

test_that("the leagues schema types preseason_hold as a season number", {
  leagues <- load_leagues(validate = FALSE)
  leagues$basketball_iceland$publish_divisions$female[[2]]$preseason_hold <- "yes"
  expect_error(
    validate_leagues(leagues, here::here("config", "leagues.schema.json")),
    "preseason_hold"
  )
})

# Plan B WS7 T1 (spec section 9). publishDivisionList.items is
# additionalProperties:false, so basketball/handball cannot describe
# themselves until the schema declares the keys they need.
#
# Basketball's "1D" is the awkward one: it is a legal division CODE but an
# illegal division_code BADGE, because both football publish-schemas pattern
# that field as ^[A-Z][A-Z0-9_]*$ and 1D starts with a digit. Hence code_badge.

.div_league <- function(divisions, sport = "football") {
  list(
    sport = sport, country = "iceland", sexes = list("male"), active = TRUE,
    data_source = list(results = "ksi_football", schedule = "ksi_football",
                       odds = "lengjan_odds"),
    stan_model = "football_iceland/bivariate_poisson.stan",
    publish_divisions = list(male = divisions),
    betting = list(
      kelly_frac = 0.10, ev_threshold = 0,
      markets = list(moneyline = TRUE),
      scoring = list(has_ties = TRUE, tie_threshold = 0),
      min_bet = 200
    )
  )
}

.div_write <- function(divisions, sport = "football") {
  tmp <- withr::local_tempfile(fileext = ".yml", .local_envir = parent.frame())
  key <- paste0(sport, "_iceland")
  writeLines(yaml::as.yaml(stats::setNames(
    list(.div_league(divisions, sport)), key
  )), tmp)
  tmp
}

.base_div <- list(code = "BD", slug = "bd", label_is = "Besta deild",
                  is_cup = FALSE)

test_that("the four new optional keys are accepted", {
  d <- utils::modifyList(.base_div, list(
    code_badge = "BD", expected_meetings = 2L, relegation_slots = 2L,
    qualify = list(slots = 6L, label_is = "Efri hluti")
  ))
  expect_no_error(load_leagues(path = .div_write(list(d))))
})

test_that("additionalProperties:false is still armed after the edit", {
  d <- utils::modifyList(.base_div, list(nonsense = 1L))
  expect_error(load_leagues(path = .div_write(list(d))),
               "leagues.yml failed schema validation")
})

test_that("code_badge rejects a leading digit", {
  # This is the whole reason the key exists: "1D" is a valid CODE but cannot
  # be a division_code badge under the publish-schema pattern.
  d <- utils::modifyList(.base_div, list(code_badge = "1D"))
  expect_error(load_leagues(path = .div_write(list(d))),
               "leagues.yml failed schema validation")
})

test_that("qualify requires both slots and label_is", {
  d <- utils::modifyList(.base_div, list(qualify = list(slots = 6L)))
  expect_error(load_leagues(path = .div_write(list(d))),
               "leagues.yml failed schema validation")
})

test_that("meta.schema.json's division pattern accepts basketball's 1D", {
  # SC-10: WS10 flagged this for WS11 and WS11 dropped it. Arming basketball
  # with the football pattern would abort every 1D cell.
  pat <- jsonlite::fromJSON(
    here::here("config", "publish-schemas", "football", "meta.schema.json")
  )$properties$division$pattern
  expect_true(grepl(pat, "1D"))
  expect_true(grepl(pat, "BD"))
})

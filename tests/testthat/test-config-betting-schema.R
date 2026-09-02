test_that("expanded betting block validates against schema", {
  expect_no_error(load_leagues()) # validates internally
  leagues <- load_leagues()
  expect_true(all(vapply(
    leagues, function(l) !is.null(l$betting$kelly_frac),
    logical(1)
  )))
  # Football + basketball use per-sex form (different ROI / sample size by sex)
  expect_true(is.list(leagues$football_iceland$betting$kelly_frac))
  expect_named(leagues$football_iceland$betting$kelly_frac,
    c("male", "female"),
    ignore.order = TRUE
  )
  expect_true(is.list(leagues$basketball_iceland$betting$kelly_frac))
  # Handball uses scalar form (women's handball not yet on Lengjan)
  expect_type(leagues$handball_iceland$betting$kelly_frac, "double")
  # All leagues now define max_match_stake
  expect_true(all(vapply(
    leagues, function(l) !is.null(l$betting$max_match_stake),
    logical(1)
  )))
})

test_that("malformed betting block fails validation", {
  bad <- yaml::as.yaml(list(
    bad_league = list(
      sport = "football", country = "iceland", sexes = list("male"),
      active = TRUE,
      data_source = list(results = "x", schedule = "x", odds = "y"),
      stan_model = "x.stan",
      betting = list(kelly_frac = 2.5) # > 1, invalid
    )
  ))
  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(bad, tmp)
  expect_error(load_leagues(path = tmp), "kelly_frac|maximum|<=")
})

# --- WS1: betting.enabled interlock (spec section 3, decision D2) -------------
# `betting` is additionalProperties:false, so the key must be declared in the
# schema before config/leagues.yml can carry it (spec finding N2). Without the
# schema edit, load_leagues() rejects the whole config and every pipeline
# script dies.

.bt_league <- function(..., sport = "handball") {
  betting <- utils::modifyList(
    list(
      kelly_frac = 0.05, ev_threshold = 0,
      markets = list(moneyline = TRUE, spread = TRUE, total = TRUE),
      scoring = list(has_ties = TRUE, tie_threshold = 0.5),
      min_bet = 200
    ),
    list(...)
  )
  list(
    sport = sport, country = "iceland", sexes = list("male"), active = TRUE,
    data_source = list(
      results = "hsi_handball", schedule = "hsi_handball", odds = "lengjan_odds"
    ),
    stan_model = "handball_iceland/2d_student_t.stan",
    betting = betting
  )
}

.bt_write <- function(league) {
  tmp <- withr::local_tempfile(fileext = ".yml", .local_envir = parent.frame())
  writeLines(yaml::as.yaml(list(handball_iceland = league)), tmp)
  tmp
}

test_that("schema accepts an optional betting.enabled flag", {
  expect_no_error(load_leagues(path = .bt_write(.bt_league(enabled = FALSE))))
})

test_that("betting.enabled is optional, not required", {
  # football_iceland ships no `enabled` key and must keep validating.
  expect_no_error(load_leagues(path = .bt_write(.bt_league())))
})

test_that("betting.enabled rejects a non-boolean", {
  expect_error(
    load_leagues(path = .bt_write(.bt_league(enabled = "false"))),
    "enabled|boolean"
  )
})

card_text <- function(name) {
  path <- testthat::test_path("fixtures", "kki", name)
  readChar(path, file.size(path), useBytes = TRUE)
}

# Played KKI results as fetch_kki() returns them: the regulation card's game is 109-96
# after four quarters, the overtime card's 109-109 after four and 121-118 final.
kki_rows <- function(...) {
  defaults <- list(
    sport = "basketball", country = "iceland", sex = "male", season = 2026L,
    match_date = as.Date("2026-03-01"), home_team = "A", away_team = "B",
    home_score = 109L, away_score = 96L, division = "BD", round = NA_integer_,
    game_id = "1"
  )
  tibble::as_tibble(utils::modifyList(defaults, list(...)))
}

# A stand-in for the card request: records each game asked for and answers from `cards`
# (a game id -> card text map), or throws when a card is a condition.
card_server <- function(cards) {
  asked <- character()
  list(
    fetch = function(game_id, season_id, sex) {
      asked <<- c(asked, game_id)
      card <- cards[[game_id]]
      if (inherits(card, "condition")) stop(card)
      card
    },
    asked = function() asked
  )
}

test_that("a played game without a regulation score gets one from its card", {
  root <- withr::local_tempdir()
  results <- dplyr::bind_rows(
    kki_rows(game_id = "1"),
    kki_rows(game_id = "2", home_team = "C", away_team = "D", home_score = 121L, away_score = 118L)
  )
  srv <- card_server(list(`1` = card_text("game_card_regulation.txt"), `2` = card_text("game_card_overtime.txt")))

  out <- kki_add_regulation_scores(results, "male", root = root, fetch_card = srv$fetch, pause = function() NULL)

  expect_identical(out$home_score_reg, c(109L, 109L))
  expect_identical(out$away_score_reg, c(96L, 109L))
  expect_setequal(srv$asked(), c("1", "2"))
})

test_that("a game whose regulation score is already stored is not fetched again", {
  root <- withr::local_tempdir()
  stored <- kki_rows(game_id = "1")
  stored$home_score_reg <- 109L
  stored$away_score_reg <- 96L
  upsert_table(stored, "results", root = root)
  srv <- card_server(list())

  out <- kki_add_regulation_scores(kki_rows(game_id = "1"), "male", root = root, fetch_card = srv$fetch, pause = function() NULL)

  expect_length(srv$asked(), 0L)
  expect_identical(c(out$home_score_reg, out$away_score_reg), c(109L, 96L))
})

test_that("unplayed games, games without an id and seasons without an id are not fetched", {
  root <- withr::local_tempdir()
  results <- dplyr::bind_rows(
    kki_rows(game_id = "1", home_score = NA_integer_, away_score = NA_integer_),
    kki_rows(game_id = NA_character_, home_team = "C", away_team = "D"),
    kki_rows(game_id = "3", home_team = "E", away_team = "F", season = 1999L)
  )
  srv <- card_server(list())

  out <- kki_add_regulation_scores(results, "male", root = root, fetch_card = srv$fetch, pause = function() NULL)

  expect_length(srv$asked(), 0L)
  expect_true(all(is.na(out$home_score_reg)))
})

test_that("a card that cannot be fetched or read leaves the scores empty, warns, and does not stop", {
  root <- withr::local_tempdir()
  results <- dplyr::bind_rows(
    kki_rows(game_id = "1"),
    kki_rows(game_id = "2", home_team = "C", away_team = "D")
  )
  srv <- card_server(list(`1` = simpleError("HTTP 503"), `2` = "<html>no card here</html>"))

  expect_warning(
    out <- kki_add_regulation_scores(results, "male", root = root, fetch_card = srv$fetch, pause = function() NULL),
    "1 card could not be fetched"
  )
  expect_true(all(is.na(c(out$home_score_reg, out$away_score_reg))))
  expect_setequal(srv$asked(), c("1", "2"))
})

test_that("a card whose final score disagrees with the result is not used", {
  root <- withr::local_tempdir()
  srv <- card_server(list(`1` = card_text("game_card_regulation.txt")))

  out <- kki_add_regulation_scores(kki_rows(game_id = "1", home_score = 110L), "male", root = root, fetch_card = srv$fetch, pause = function() NULL)

  expect_identical(c(out$home_score_reg, out$away_score_reg), c(NA_integer_, NA_integer_))
})

test_that("card requests are capped per run, newest first, and paced", {
  root <- withr::local_tempdir()
  results <- dplyr::bind_rows(lapply(1:5, function(i) {
    kki_rows(game_id = as.character(i), home_team = paste0("H", i), match_date = as.Date("2026-03-01") + i)
  }))
  cards <- stats::setNames(rep(list(card_text("game_card_regulation.txt")), 5), as.character(1:5))
  srv <- card_server(cards)
  pauses <- 0L

  out <- kki_add_regulation_scores(results, "male", root = root, fetch_card = srv$fetch, max_cards = 3L, pause = function() pauses <<- pauses + 1L)

  expect_identical(srv$asked(), c("5", "4", "3"))
  expect_identical(pauses, 2L)
  expect_identical(sum(!is.na(out$home_score_reg)), 3L)
})

test_that("the KKI source registers the regulation-time step", {
  expect_true(is.function(get_ingest_source("kki_basketball")$enrich_results))
})

test_that("an enrichment step that fails does not stop the ingest", {
  fake <- list(
    fetch_results = function(league, sex, seasons = NULL) {
      kki_rows(sport = league$sport, sex = sex)
    },
    fetch_schedule = function(league, sex) kki_rows()[0, setdiff(names(kki_rows()), c("home_score", "away_score", "game_id"))],
    enrich_results = function(results, league, sex, root) stop("network down")
  )
  register_ingest_source("enrich_stub", fake)
  on.exit(unregister_ingest_source("enrich_stub"), add = TRUE)
  league <- list(sport = "basketball", country = "iceland", sexes = "male", data_source = list(results = "enrich_stub", schedule = "enrich_stub"))
  root <- withr::local_tempdir()

  expect_warning(ingest_league(league, "male", root = root), "network down")

  back <- read_table("results", root = root)
  expect_equal(nrow(back), 1L)
  expect_identical(back$home_score, 109L)
})

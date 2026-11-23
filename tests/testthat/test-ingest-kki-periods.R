read_card <- function(name) {
  path <- testthat::test_path("fixtures", "kki", name)
  readChar(path, file.size(path), useBytes = TRUE)
}

test_that("a game decided in regulation yields four periods that add up to the final score", {
  p <- parse_baskethotel_game_periods(read_card("game_card_regulation.txt"))

  expect_true(p$ok)
  expect_false(p$overtime)
  expect_equal(c(p$home_final, p$away_final), c(109L, 96L))
  expect_equal(p$n_periods, 4L)
  expect_equal(c(p$home_reg, p$away_reg), c(109L, 96L))
  expect_equal(c(p$home_ot, p$away_ot), c(0L, 0L))
  expect_equal(p$periods, "27-29;27-22;31-16;24-29")
})

test_that("an overtime game is four level quarters with a final score above them", {
  # The fixture is the regulation card edited by hand to the layout the stored cards show
  # for overtime (four quarters only; measured on 3,286 cards, 138 of them overtime): the
  # fourth quarter and the final changed, 109-109 after regulation, 121-118 final. No
  # captured overtime card was kept.
  p <- parse_baskethotel_game_periods(read_card("game_card_overtime.txt"))

  expect_true(p$ok)
  expect_true(p$overtime)
  expect_equal(p$n_periods, 4L)
  expect_equal(c(p$home_final, p$away_final), c(121L, 118L))
  expect_equal(c(p$home_reg, p$away_reg), c(109L, 109L))
  expect_equal(c(p$home_ot, p$away_ot), c(12L, 9L))
})

test_that("a card that lists the overtime period keeps it out of regulation time", {
  listed <- sub(
    "<span>24 - 42<\\/span>", "<span>24 - 42<\\/span><span>12 - 9<\\/span>",
    read_card("game_card_overtime.txt"),
    fixed = TRUE
  )
  p <- parse_baskethotel_game_periods(listed)

  expect_true(p$ok)
  expect_true(p$overtime)
  expect_equal(p$n_periods, 5L)
  expect_equal(c(p$home_reg, p$away_reg), c(109L, 109L))
  expect_equal(c(p$home_ot, p$away_ot), c(12L, 9L))
})

test_that("a final score that is neither the quarter sum nor a broken tie is rejected", {
  # Quarters say 109-96; a recorded 110-96 is a data-entry error, and a recorded 0-20 is
  # a walkover awarded over a played game. Neither has a trustworthy regulation score.
  typo <- sub(">109<", ">110<", read_card("game_card_regulation.txt"), fixed = TRUE)
  expect_false(parse_baskethotel_game_periods(typo)$ok)

  walkover <- sub(">96<", ">20<", sub(">109<", ">0<", read_card("game_card_regulation.txt"), fixed = TRUE), fixed = TRUE)
  p <- parse_baskethotel_game_periods(walkover)
  expect_false(p$ok)
  expect_false(p$overtime)
  expect_equal(c(p$home_reg, p$away_reg), c(109L, 96L))
})

test_that("a card with fewer than four periods or no period block is rejected", {
  three <- sub("<span>24 - 29<\\/span>", "", read_card("game_card_regulation.txt"), fixed = TRUE)
  p3 <- parse_baskethotel_game_periods(three)
  expect_false(p3$ok)
  expect_equal(p3$n_periods, 3L)
  expect_true(is.na(p3$home_reg))

  none <- parse_baskethotel_game_periods("MBT.API.triggerError('Invalid params')")
  expect_false(none$ok)
  expect_equal(none$n_periods, 0L)
  expect_true(is.na(none$home_reg))
})

test_that("the game URL names the widget, the game and the league for that sex", {
  male <- baskethotel_game_url(6040504, 130403, "male")
  female <- baskethotel_game_url(6040504, 130403, "female")

  expect_match(male, "request[0][widget]=400", fixed = TRUE)
  expect_match(male, "[param][game_id]=6040504", fixed = TRUE)
  expect_match(male, "[param][league_id]=190", fixed = TRUE)
  expect_match(female, "[param][league_id]=189", fixed = TRUE)
})

test_that("team names in ISO-8859-1 bytes do not break the parse", {
  # WHY: the widget answers in ISO-8859-1. Bytes that are not valid UTF-8 must not stop the
  # pattern matching, which runs byte-wise.
  name <- rawToChar(as.raw(c(0xde, 0xf3, 0x72, 0x20, 0xde, 0x2e)))
  card <- sub(
    "<div class=\\\"mbt-gamecard-result", paste0("<span class=\\\"mbt-team-name\\\">", name, "</span><div class=\\\"mbt-gamecard-result"),
    read_card("game_card_regulation.txt"),
    fixed = TRUE, useBytes = TRUE
  )
  expect_true(grepl(name, card, fixed = TRUE, useBytes = TRUE))

  p <- parse_baskethotel_game_periods(card)

  expect_true(p$ok)
  expect_equal(c(p$home_reg, p$away_reg), c(109L, 96L))
})

test_that("a card level after four quarters and level at the end is rejected", {
  # WHY: the store rejects such a row (overtime cannot end level), so the parser must not
  # pass it on as a game decided in regulation.
  # The fixture is level after four quarters (109-109); make the final level too.
  level <- sub(">121<", ">109<", sub(">118<", ">109<", read_card("game_card_overtime.txt"), fixed = TRUE), fixed = TRUE)
  p <- parse_baskethotel_game_periods(level)

  expect_equal(c(p$home_reg, p$away_reg), c(109L, 109L))
  expect_equal(c(p$home_final, p$away_final), c(109L, 109L))
  expect_false(p$ok)
})

test_that("a captured overtime card parses as overtime", {
  # A real card, captured once on 22 November 2026: the bytes from the start of the
  # widget's answer to the end of its period block, unedited.
  card <- read_card("game_card_overtime_captured.txt")
  p <- parse_baskethotel_game_periods(card)

  expect_true(p$ok)
  expect_true(p$overtime)
  expect_identical(p$home_reg, p$away_reg)
  expect_true(p$home_final != p$away_final)
  expect_true(p$home_ot >= 0L && p$away_ot >= 0L)
})

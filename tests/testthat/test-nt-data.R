test_that("nt_read_results reads the martj42 schema and sorts by date", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2024-03-22,Iceland,Israel,4,1,UEFA Euro qualification,Budapest,Hungary,TRUE",
    "2023-06-17,Iceland,Slovakia,1,2,UEFA Euro qualification,Reykjavik,Iceland,FALSE",
    "2026-12-01,Iceland,Wales,NA,NA,Friendly,Reykjavik,Iceland,FALSE"
  ), path)
  r <- nt_read_results(path)
  expect_equal(r$match_date, as.Date(c("2023-06-17", "2024-03-22", "2026-12-01")))
  expect_type(r$home_score, "integer")
  expect_equal(r$neutral, c(FALSE, TRUE, FALSE))
  expect_true(is.na(r$home_score[3]))
})

test_that("tournament matches carry the 90-minute score of extra-time ties", {
  r <- nt_test_results(
    "2022-12-18", "Argentina", "France", 3, 3, "FIFA World Cup", "Qatar", TRUE,
    "2022-12-17", "Croatia", "Morocco", 2, 1, "FIFA World Cup", "Qatar", TRUE,
    "2022-11-22", "Argentina", "Saudi Arabia", 1, 2, "FIFA World Cup", "Qatar", TRUE,
    "2022-12-18", "Wales", "Malta", 1, 0, "Friendly", "Wales", FALSE
  )
  m <- nt_tournament_matches(r, "wc2022")
  expect_equal(nrow(m), 3L)
  final <- m[m$home_team == "Argentina" & m$away_team == "France", ]
  expect_equal(c(final$home_90, final$away_90), c(2L, 2L))
  expect_true(final$extra_time)
  expect_equal(final$stage, "knockout")
  group <- m[m$away_team == "Saudi Arabia", ]
  expect_equal(c(group$home_90, group$away_90, group$extra_time), c(1L, 2L, 0L))
  expect_equal(group$stage, "group")
})

test_that("the extra-time table matches either listing order of the pair", {
  listed <- nt_test_results(
    "2024-07-05", "Spain", "Germany", 2, 1, "UEFA Euro", "Germany", TRUE
  )
  swapped <- nt_test_results(
    "2024-07-05", "Germany", "Spain", 1, 2, "UEFA Euro", "Germany", FALSE
  )
  a <- nt_tournament_matches(listed, "euro2024")
  b <- nt_tournament_matches(swapped, "euro2024")
  expect_equal(c(a$home_90, a$away_90), c(1L, 1L))
  expect_equal(c(b$home_90, b$away_90), c(1L, 1L))
  expect_true(a$extra_time && b$extra_time)
})

test_that("extra-time ties are level after 90 minutes", {
  et <- nt_extra_time_matches()
  expect_true(all(et$a_90 == et$b_90))
  expect_equal(nrow(et), 19L)
  expect_equal(sum(et$shootout), 12L)
})

test_that("every extra-time tie falls in the knockout stage of its tournament", {
  et <- nt_extra_time_matches()
  spec <- nt_tournaments()
  per_key <- vapply(seq_len(nrow(spec)), function(i) {
    sum(et$match_date >= spec$knockout_start[i] & et$match_date <= spec$end[i])
  }, integer(1))
  expect_equal(stats::setNames(per_key, spec$key), c(wc2022 = 5L, euro2024 = 5L, wc2026 = 9L))
  expect_equal(sum(per_key), nrow(et))
})

test_that("an extra-time winner is scored as level after 90 minutes", {
  r <- nt_test_results(
    "2026-07-19", "Spain", "Argentina", 1, 0, "FIFA World Cup", "United States", TRUE,
    "2026-07-01", "Senegal", "Belgium", 2, 3, "FIFA World Cup", "United States", TRUE
  )
  m <- nt_tournament_matches(r, "wc2026")
  final <- m[m$home_team == "Spain", ]
  r32 <- m[m$home_team == "Senegal", ]
  expect_equal(c(final$home_90, final$away_90), c(0L, 0L))
  expect_equal(c(final$home_score, final$away_score), c(1L, 0L))
  expect_equal(c(r32$home_90, r32$away_90), c(2L, 2L))
  expect_equal(c(r32$home_score, r32$away_score), c(2L, 3L))
  expect_true(all(m$extra_time))
})

test_that("core teams need a sanctioned qualifier or finals match", {
  r <- nt_test_results(
    "2024-01-01", "A", "B", 1, 0, "FIFA World Cup qualification", "A", FALSE,
    "2024-01-02", "C", "D", 1, 0, "CONIFA World Football Cup qualification", "C", FALSE,
    "2024-01-03", "E", "A", 1, 0, "Friendly", "E", FALSE,
    "2024-01-04", "F", "B", 0, 0, "AFC Olympic Qualifying Tournament", "F", FALSE,
    "2024-01-05", "G", "H", 0, 0, "Island Games", "G", FALSE
  )
  expect_equal(nt_core_teams(r), c("A", "B", "F"))
})

test_that("teams that qualify through a finals tournament are core", {
  r <- nt_test_results(
    "2022-07-10", "Brazil", "Colombia", 1, 0, "Copa América", "Colombia", TRUE,
    "2022-07-20", "Vanuatu", "Fiji", 1, 0, "Oceania Nations Cup", "Qatar", TRUE,
    "2024-08-01", "Great Britain", "Chile", 2, 0, "Olympic Games", "France", TRUE,
    "2024-02-01", "Ghana", "Mali", 2, 0, "African Cup of Nations", "Ivory Coast", TRUE,
    "2024-02-02", "Zambia", "Kenya", 2, 0, "Africa Cup of Nations", "Morocco", TRUE,
    "2024-03-01", "X", "Y", 2, 0, "Friendly", "X", FALSE,
    "2024-03-02", "P", "Q", 2, 0, "Algarve Cup", "Portugal", TRUE,
    "2024-03-03", "R", "S", 2, 0, "UEFA Nations League", "R", FALSE
  )
  expect_equal(
    nt_core_teams(r),
    sort(c(
      "Brazil", "Colombia", "Vanuatu", "Fiji", "Great Britain", "Chile",
      "Ghana", "Mali", "Zambia", "Kenya"
    ))
  )
  expect_equal(nt_core_teams(r, pattern = "qualif"), character())
})

test_that("nt_prepare keeps only matches strictly before the cutoff, in the window", {
  r <- nt_test_results(
    "2010-01-01", "A", "B", 1, 0, "FIFA World Cup qualification", "A", FALSE,
    "2024-01-10", "A", "B", 2, 0, "FIFA World Cup qualification", "A", FALSE,
    "2024-04-10", "B", "A", 0, 0, "Friendly", "C", TRUE,
    "2024-06-01", "A", "B", 5, 5, "FIFA World Cup qualification", "A", FALSE,
    "2024-07-01", "A", "B", 9, 9, "FIFA World Cup qualification", "A", FALSE
  )
  p <- nt_prepare(r, as.Date("2024-06-01"), window_years = 12, min_matches = 1L)
  expect_equal(p$matches$match_date, as.Date(c("2024-01-10", "2024-04-10")))
  expect_equal(p$matches$venue, c(1L, 0L))
  expect_equal(p$matches$friendly, c(FALSE, TRUE))
  expect_equal(p$teams, c("A", "B"))
  expect_equal(p$matches$home_nr, c(1L, 2L))
})

test_that("the time index counts steps of step_months from the first month", {
  r <- nt_test_results(
    "2024-01-10", "A", "B", 2, 0, "FIFA World Cup qualification", "A", FALSE,
    "2024-03-31", "A", "B", 2, 0, "FIFA World Cup qualification", "A", FALSE,
    "2024-04-01", "A", "B", 2, 0, "FIFA World Cup qualification", "A", FALSE,
    "2025-01-01", "A", "B", 2, 0, "FIFA World Cup qualification", "A", FALSE
  )
  quarterly <- nt_prepare(r, as.Date("2025-06-15"), min_matches = 1L)
  expect_equal(quarterly$matches$period, c(1L, 1L, 2L, 5L))
  expect_equal(quarterly$n_periods, 6L)
  monthly <- nt_prepare(r, as.Date("2025-06-15"), min_matches = 1L, step_months = 1L)
  expect_equal(monthly$matches$period, c(1L, 3L, 4L, 13L))
  expect_equal(monthly$n_periods, 18L)
})

test_that("nt_prepare drops teams below the match floor until stable", {
  rows <- list(
    c("2024-01-01", "A", "B"), c("2024-01-02", "B", "A"), c("2024-01-03", "A", "B"),
    c("2024-01-04", "A", "C"), c("2024-01-05", "C", "D")
  )
  r <- tibble::tibble(
    match_date = as.Date(vapply(rows, `[`, "", 1)),
    home_team = vapply(rows, `[`, "", 2), away_team = vapply(rows, `[`, "", 3),
    home_score = 1L, away_score = 0L,
    tournament = "FIFA World Cup qualification", country = "X", neutral = TRUE
  )
  p <- nt_prepare(r, as.Date("2024-06-01"), min_matches = 2L)
  expect_equal(p$teams, c("A", "B"))
  expect_equal(nrow(p$matches), 3L)
})

test_that("the women's extra-time table holds the October 2026 play-off shoot-out", {
  et <- nt_extra_time_matches("women")
  pb <- et[et$team_a == "Poland" & et$team_b == "Belgium", ]
  expect_equal(pb$match_date, as.Date("2026-10-13"))
  expect_equal(c(pb$a_90, pb$b_90), c(2L, 1L))
  expect_true(pb$shootout)
  expect_identical(nt_extra_time_matches(), nt_extra_time_matches("men"))
})

test_that("nt_prepare trains on the 90-minute score when given corrections", {
  r <- nt_test_results(
    "2026-10-09", "Belgium", "Poland", 1, 0, "FIFA World Cup qualification", "Belgium", FALSE,
    "2026-10-13", "Poland", "Belgium", 3, 2, "FIFA World Cup qualification", "Poland", FALSE
  )
  ninety <- tibble::tibble(
    match_date = as.Date("2026-10-13"), team_a = "Belgium", team_b = "Poland",
    a_90 = 1L, b_90 = 2L, shootout = TRUE
  )
  raw <- nt_prepare(r, as.Date("2026-12-01"), min_matches = 1L)
  fixed <- nt_prepare(r, as.Date("2026-12-01"), min_matches = 1L, ninety = ninety)
  expect_equal(raw$matches$home_score, c(1L, 3L))
  expect_equal(fixed$matches$home_score, c(1L, 2L))
  expect_equal(fixed$matches$away_score, c(0L, 1L))
  expect_equal(fixed$n_corrected, 1L)
  expect_equal(raw$n_corrected, 0L)
})

test_that("a correction inside the window that matches no match warns", {
  r <- nt_test_results(
    "2026-10-13", "Poland", "Belgium", 3, 2, "FIFA World Cup qualification", "Poland", FALSE
  )
  typo <- tibble::tibble(
    match_date = as.Date("2026-10-13"), team_a = "Polland", team_b = "Belgium",
    a_90 = 2L, b_90 = 1L, shootout = TRUE
  )
  expect_warning(
    nt_prepare(r, as.Date("2026-12-01"), min_matches = 1L, ninety = typo),
    "match no"
  )
  outside <- typo
  outside$match_date <- as.Date("2027-01-10")
  expect_no_warning(nt_prepare(r, as.Date("2026-12-01"), min_matches = 1L, ninety = outside))
})

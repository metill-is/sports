make_rows <- function(dates) {
  tibble::tibble(match_date = as.Date(dates))
}

test_that(".assert_season_stamp passes an Icelandic winter season's two years", {
  rows <- make_rows(c("2024-09-14", "2024-12-02", "2025-01-18", "2025-05-03"))
  expect_identical(.assert_season_stamp(rows, 2025L, source = "test"), rows)
})

test_that(".assert_season_stamp aborts with its own class when the years are wrong", {
  rows <- make_rows(c("2024-09-14", "2024-12-02", "2025-01-18", "2025-05-03"))
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "hsi male/div2"),
    class = "sports_season_stamp_error"
  )
})

test_that(".assert_season_stamp tolerates a small out-of-span minority", {
  # 1 stray in 40 = 2.5%, under the 5% default.
  rows <- make_rows(c(rep("2024-10-01", 39), "2023-06-01"))
  expect_no_error(.assert_season_stamp(rows, 2025L, source = "test"))
  # 4 strays in 40 = 10%, over it.
  rows_bad <- make_rows(c(rep("2024-10-01", 36), rep("2023-06-01", 4)))
  expect_error(
    .assert_season_stamp(rows_bad, 2025L, source = "test"),
    class = "sports_season_stamp_error"
  )
})

test_that(".assert_season_stamp is a no-op on zero rows and on all-NA dates", {
  expect_no_error(.assert_season_stamp(make_rows(character()), 2027L))
  expect_no_error(.assert_season_stamp(make_rows(c(NA, NA)), 2027L))
  expect_no_error(.assert_season_stamp(NULL, 2027L))
})

test_that(".assert_season_stamp names the observed years in its message", {
  rows <- make_rows(c("2024-10-01", "2024-11-01", "2025-02-01"))
  expect_error(
    .assert_season_stamp(rows, 2027L, source = "hsi male/div1"),
    regexp = "hsi male/div1"
  )
})

test_that("competition_url builds the expected pattern", {
  url <- competition_url(sport_id = 1, competition_id = 14)
  expect_match(url, "lotto.is")
  expect_match(url, "competition=14", fixed = TRUE)
})

test_that("match_url builds the expected pattern", {
  url <- match_url(match_id = "12345", sport_id = 1)
  expect_match(url, "leikur")
  expect_match(url, "12345", fixed = TRUE)
})

test_that("find_match_in_extracted returns the matching row", {
  matches <- jsonlite::read_json(
    testthat::test_path("fixtures", "placer", "lengjan_matches_sample.json"),
    simplifyVector = TRUE
  ) |> tibble::as_tibble()

  hit <- find_match_in_extracted(matches, "KR", "FH")
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$match_id, "12345")
})

test_that("find_match_in_extracted returns 0 rows when no match", {
  matches <- jsonlite::read_json(
    testthat::test_path("fixtures", "placer", "lengjan_matches_sample.json"),
    simplifyVector = TRUE
  ) |> tibble::as_tibble()

  hit <- find_match_in_extracted(matches, "Mystery", "FC")
  expect_equal(nrow(hit), 0L)
})

test_that("find_match_in_extracted disambiguates by home/away ordering", {
  # KR plays home in 12345 vs FH; KR plays away in 12348 vs Valur.
  # Searching for (Valur, KR) must hit 12348, not 12345.
  matches <- jsonlite::read_json(
    testthat::test_path("fixtures", "placer", "lengjan_matches_sample.json"),
    simplifyVector = TRUE
  ) |> tibble::as_tibble()

  hit <- find_match_in_extracted(matches, "Valur", "KR")
  expect_equal(nrow(hit), 1L)
  expect_equal(hit$match_id, "12348")
})

test_that("sample_delay sleeps within the requested range", {
  t0 <- Sys.time()
  Sys.sleep(sample_delay(c(0.05, 0.1)))
  dt <- as.numeric(Sys.time() - t0)
  expect_gte(dt, 0.04)
  expect_lte(dt, 0.20)
})

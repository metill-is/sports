test_that("wc_ingest_internationals merges the overlay into results", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-06-20,Aland,Bland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-20,Cland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Aland,Cland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Bland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Aland,Dland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Bland,Cland,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  overlay <- file.path(tmp, "manual_results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score",
    "2026-06-20,Aland,Bland,2,1"
  ), overlay)

  root <- file.path(tmp, "data")
  wc_ingest_internationals(
    csv_path = csv, manual_overlay_path = overlay, root = root
  )

  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L) # only the overlay-filled match is "played"
  expect_equal(res$home_team, "Aland")
  expect_equal(res$home_score, 2L)
  expect_equal(res$away_score, 1L)
})

test_that("a missing overlay file is a clean no-op", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-06-20,Aland,Bland,3,0,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Aland,Cland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Bland,Cland,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  root <- file.path(tmp, "data")
  wc_ingest_internationals(
    csv_path = csv,
    manual_overlay_path = file.path(tmp, "does-not-exist.csv"),
    root = root
  )
  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$home_team, "Aland")
})

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

test_that("a header-only overlay file (comments + header, 0 data rows) is a clean no-op", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-06-20,Aland,Bland,3,0,FIFA World Cup,X,US,TRUE",
    "2026-06-23,Aland,Cland,NA,NA,FIFA World Cup,X,US,TRUE",
    "2026-06-25,Bland,Cland,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  overlay <- file.path(tmp, "manual_results.csv")
  writeLines(c(
    "# Manual WC results overlay — header only, no data rows.",
    "date,home_team,away_team,home_score,away_score"
  ), overlay)

  root <- file.path(tmp, "data")
  wc_ingest_internationals(
    csv_path = csv, manual_overlay_path = overlay, root = root
  )
  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L) # only the already-scored Aland-Bland match
  expect_equal(res$home_team, "Aland")
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

test_that("wc_ingest_internationals tolerates a pen_winner column in the overlay", {
  tmp <- withr::local_tempdir()
  csv <- file.path(tmp, "results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,tournament,city,country,neutral",
    "2026-07-04,Brazil,France,NA,NA,FIFA World Cup,X,US,TRUE"
  ), csv)
  overlay <- file.path(tmp, "manual_results.csv")
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,pen_winner",
    "2026-07-04,Brazil,France,1,1,France"
  ), overlay)

  root <- file.path(tmp, "data")
  expect_no_error(
    wc_ingest_internationals(csv_path = csv, manual_overlay_path = overlay, root = root)
  )
  res <- read_table("results", root = root, filter = list(country = "world"))
  expect_equal(nrow(res), 1L)
  expect_equal(res$home_score, 1L) # level score merged; pen_winner is ignored here
})

# ---- wc_ingest_shootouts ----------------------------------------------------

test_that("wc_ingest_shootouts writes WC knockout shootout winners (martj42 + manual)", {
  s <- wc_structure()
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "wc", "raw"), recursive = TRUE)
  writeLines(c(
    "date,home_team,away_team,winner,first_shooter",
    "2026-07-03,Canada,South Africa,Canada,Canada", # cross-group WC -> kept
    "1990-07-04,England,Germany,Germany," # historical -> dropped (wrong year)
  ), file.path(tmp, "wc", "raw", "shootouts.csv"))
  writeLines(c(
    "date,home_team,away_team,home_score,away_score,pen_winner",
    "2026-07-04,Brazil,France,1,1,France", # operator-supplied -> kept
    "2026-07-03,South Africa,Canada,1,1,South Africa" # conflicts martj42 -> martj42 wins
  ), file.path(tmp, "wc", "manual_results.csv"))

  n <- wc_ingest_shootouts(
    s,
    shootouts_csv = file.path(tmp, "wc", "raw", "shootouts.csv"),
    manual_overlay_path = file.path(tmp, "wc", "manual_results.csv"),
    root = tmp
  )

  m <- wc_shootout_winners(root = tmp)
  expect_equal(unname(m[[.wc_pair_key("Canada", "South Africa")]]), "Canada")
  expect_equal(unname(m[[.wc_pair_key("Brazil", "France")]]), "France")
  expect_false(.wc_pair_key("England", "Germany") %in% names(m))
  expect_equal(length(m), 2L)
})

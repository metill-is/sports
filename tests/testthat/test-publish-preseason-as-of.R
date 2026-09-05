# A division with no played match yet (handball kvenna OD on 2026-09-05, one
# round into the season) stamped `as_of = format(max(bd_results$match_date))`
# with bd_results EMPTY, so every history record and standings.json carried
# as_of = "-Inf". The handball schema rejected the cell, which is how it was
# caught; a schema that accepted it would have published a plausible-looking
# JSON whose date key was the string "-Inf". The honest as_of for a snapshot
# with nothing played is the snapshot date.

.facts_without_division_results <- function(env, sport, sex, division) {
  root <- fixture_facts_root(env)
  results <- read_table("results", root = root)
  drop <- results$sport == sport & results$sex == sex & results$division == division
  write_table(results[!drop, ], "results", root = root)
  root
}

test_that("a division with no played match stamps as_of with the snapshot date", {
  root <- .facts_without_division_results(environment(), "handball", "female", "OD")
  extracts <- file.path(root, "beliefs", "extracts")
  dir.create(extracts, recursive = TRUE, showWarnings = FALSE)
  file.copy(testthat::test_path("fixtures", "extracts", "sport=handball"), extracts, recursive = TRUE)
  league <- load_leagues()[["handball_iceland"]]
  st <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source", "publish_divisions")]
  suppressMessages(suppressWarnings(publish_one(
    st, league$betting, "handball_iceland", "female",
    root = root, validate = FALSE, end_date = FIXTURE_END_DATE
  )))
  cell <- file.path(root, "publish", "handball", "iceland", "kvenna-od")
  expected <- format(FIXTURE_END_DATE, "%Y-%m-%d")
  standings <- jsonlite::fromJSON(file.path(cell, "standings.json"))
  expect_identical(standings$as_of, expected)
  hist <- jsonlite::fromJSON(file.path(cell, "final_positions_history.json"))
  as_of <- unique(hist$records$as_of)
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", as_of)), info = paste(as_of, collapse = ","))
  expect_identical(as_of, expected)
})

test_that("a division with played matches keeps as_of = last match date", {
  root <- fixture_facts_root(environment())
  extracts <- file.path(root, "beliefs", "extracts")
  dir.create(extracts, recursive = TRUE, showWarnings = FALSE)
  file.copy(testthat::test_path("fixtures", "extracts", "sport=handball"), extracts, recursive = TRUE)
  league <- load_leagues()[["handball_iceland"]]
  st <- league[c("sport", "country", "sexes", "active", "stan_model", "data_source", "publish_divisions")]
  suppressMessages(suppressWarnings(publish_one(
    st, league$betting, "handball_iceland", "male",
    root = root, validate = FALSE, end_date = FIXTURE_END_DATE
  )))
  cell <- file.path(root, "publish", "handball", "iceland", "karla-od")
  results <- read_table("results", root = root)
  played <- results[results$sport == "handball" & results$sex == "male" & results$division == "OD", ]
  standings <- jsonlite::fromJSON(file.path(cell, "standings.json"))
  expect_identical(standings$as_of, format(max(played$match_date), "%Y-%m-%d"))
})

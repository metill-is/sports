# scrape-odds.yml commits what the scrape writes: both odds tables. The
# odds_markets folder exists only after the first JSON-API write, and a bare
# `git add` of a missing path exits 128, which would leave the run's odds
# uncommitted, so its add is guarded.

.scrape_odds_commit_step <- function() {
  yml <- readLines(testthat::test_path("..", "..", ".github", "workflows", "scrape-odds.yml"), warn = FALSE)
  start <- grep("^\\s*- name: Commit if data changed\\s*$", yml)
  expect_length(start, 1L)
  yml[start:length(yml)]
}

test_that("scrape-odds commits data/facts/odds/ unconditionally", {
  step <- .scrape_odds_commit_step()
  expect_true(any(grepl("^\\s*git add data/facts/odds/\\s*$", step)))
})

test_that("scrape-odds commits data/facts/odds_markets/ only once the folder exists", {
  step <- .scrape_odds_commit_step()
  add <- grep("git add data/facts/odds_markets/", step, fixed = TRUE)
  expect_length(add, 1L)
  expect_match(step[add - 1L], "if [ -d data/facts/odds_markets ]; then", fixed = TRUE)
  expect_match(step[add + 1L], "^\\s*fi\\s*$")
  commit <- grep("git commit", step, fixed = TRUE)
  expect_true(add < commit[[1]])
})

test_that("the scrape script hands every league one shared program fetcher", {
  script <- readLines(testthat::test_path("..", "..", "scripts", "02_scrape_odds.R"), warn = FALSE)
  made <- grep("program_once <- lengjan_program_once()", script, fixed = TRUE)
  passed <- grep("fetch_program = program_once", script, fixed = TRUE)
  expect_length(made, 1L)
  expect_length(passed, 1L)
  # Made once, outside the per-league function, before it is used.
  per_league <- grep("^scrape_one <- function", script)
  expect_true(made < per_league && per_league < passed)
})

test_that("scrape-odds.yml installs Chrome exactly when a league scrapes the DOM", {
  # The DOM scraper (lengjan.source absent or dom) needs Chrome on the runner;
  # once every league is on the JSON API the workflow carries none. A league
  # rolled back to dom without Chrome would fail every scrape.
  yml <- readLines(testthat::test_path("..", "..", ".github", "workflows", "scrape-odds.yml"), warn = FALSE)
  has_chrome <- any(grepl("setup-chrome", yml, fixed = TRUE))
  lg <- filter_leagues(load_leagues(), active_only = TRUE, has_lengjan = TRUE)
  on_dom <- names(lg)[!vapply(lg, function(l) identical(l$lengjan$source, "api"), logical(1))]
  expect_identical(has_chrome, length(on_dom) > 0L, info = paste(on_dom, collapse = ", "))
})

# The unified Icelandic-league publisher. Renamed from publish_iceland_league
# and de-literalised; football's 92 golden hashes (test-publish-football-golden.R)
# prove the rename is behaviour-preserving on the live path, so the blocks here
# cover the guards and one end-to-end cell pair.

test_that("publish_iceland_league rejects a sport with no publish profile", {
  expect_error(
    publish_iceland_league(
      extracted = list(),
      league = list(sport = "cricket", country = "iceland"),
      sex = "male"
    ),
    regexp = "sport"
  )
})

test_that("publish_iceland_league rejects a non-Icelandic league", {
  expect_error(
    publish_iceland_league(
      extracted = list(),
      league = list(sport = "football", country = "england"),
      sex = "male"
    ),
    regexp = "iceland"
  )
})

test_that("publish_iceland_league rejects an unknown sex", {
  expect_error(
    publish_iceland_league(
      extracted = list(),
      league = list(sport = "football", country = "iceland"),
      sex = "other"
    ),
    regexp = "sex"
  )
})

test_that("publish_iceland_league reproduces the golden football BD cells", {
  golden <- utils::read.csv(
    testthat::test_path("fixtures", "golden", "football-publish-hashes.csv"),
    stringsAsFactors = FALSE
  )
  bd_cells <- c(
    "football/iceland/karla-bd/", "football/iceland/kvenna-bd/"
  )
  wanted <- golden[
    Reduce(`|`, lapply(bd_cells, function(p) startsWith(golden$file, p))),
    ,
    drop = FALSE
  ]
  expect_gt(nrow(wanted), 0L)

  facts_root <- fixture_facts_root()
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  out <- withr::local_tempdir()
  league <- load_leagues()[["football_iceland"]]

  for (sex in c("male", "female")) {
    build_football_extracts_fixture(facts_root, extracts_root, sex)
    extracted <- read_extracted_iceland(
      league,
      sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    suppressMessages(suppressWarnings(publish_iceland_league(
      extracted = extracted, league = league, sex = sex,
      end_date = FIXTURE_END_DATE,
      root = facts_root,
      output_root = out,
      extracts_root = extracts_root,
      archive_root = file.path(withr::local_tempdir(), "archive"),
      round_predictions_history_root = file.path(
        withr::local_tempdir(), "rph"
      )
    )))
  }

  actual <- vapply(
    file.path(out, wanted$file), publish_json_digest, character(1),
    USE.NAMES = FALSE
  )
  changed <- wanted$file[actual != wanted$sha256]
  expect_equal(
    length(changed), 0L,
    info = paste("changed payloads:", paste(changed, collapse = ", "))
  )
})

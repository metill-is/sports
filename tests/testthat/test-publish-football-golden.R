# Safety net for the publisher refactor in the follow-on plan: publishing
# football from a PINNED fixture must produce byte-identical JSON (modulo
# generated_at) before and after the two 2DT publishers are deleted and the
# football publisher is rewritten.

test_that("football publish output is byte-identical to the golden manifest", {
  golden_path <- testthat::test_path("fixtures", "golden", "football-publish-hashes.csv")
  expect_true(
    file.exists(golden_path),
    info = "regenerate with: Rscript tools/make-extract-fixtures.R --golden"
  )
  golden <- utils::read.csv(golden_path, stringsAsFactors = FALSE)

  facts_root <- fixture_facts_root()
  extracts_root <- file.path(withr::local_tempdir(), "extracts")
  out <- withr::local_tempdir()
  league <- load_leagues()[["football_iceland"]]

  for (sex in c("male", "female")) {
    build_football_extracts_fixture(facts_root, extracts_root, sex)
    extracted <- read_extracted_football(
      league, sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    suppressMessages(suppressWarnings(publish_football_iceland(
      extracted = extracted, league = league, sex = sex,
      end_date = FIXTURE_END_DATE,
      root = facts_root,
      output_root = out,
      extracts_root = extracts_root,
      archive_root = file.path(withr::local_tempdir(), "archive")
    )))
  }

  produced <- list.files(
    file.path(out, "football"), pattern = "\\.json$",
    recursive = TRUE, full.names = TRUE
  )
  rel <- sub(paste0("^", out, "/"), "", produced)
  expect_setequal(rel, golden$file)

  actual <- vapply(produced, publish_json_digest, character(1), USE.NAMES = FALSE)
  names(actual) <- rel
  changed <- names(actual)[actual[golden$file] != golden$sha256]
  expect_equal(
    length(changed), 0L,
    info = paste("changed payloads:", paste(changed, collapse = ", "))
  )
})

test_that("the golden manifest covers every configured football cell", {
  golden <- utils::read.csv(
    testthat::test_path("fixtures", "golden", "football-publish-hashes.csv"),
    stringsAsFactors = FALSE
  )
  for (sex in c("male", "female")) {
    sex_folder <- if (sex == "male") "karla" else "kvenna"
    for (slug in .football_iceland_division_slugs(sex)) {
      prefix <- file.path("football", "iceland", paste0(sex_folder, "-", slug))
      expect_true(
        any(startsWith(golden$file, prefix)),
        info = prefix
      )
    }
  }
})

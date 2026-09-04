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
    extracted <- read_extracted_iceland(
      league, sex = sex, fit_date = FIXTURE_FIT_DATE, extracts_root = extracts_root
    )
    suppressMessages(suppressWarnings(publish_iceland_league(
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
    for (slug in .iceland_division_slugs("football_iceland", sex)) {
      prefix <- file.path("football", "iceland", paste0(sex_folder, "-", slug))
      expect_true(
        any(startsWith(golden$file, prefix)),
        info = prefix
      )
    }
  }
})

# The same manifest, reproduced through publish_one()'s dispatch rather than
# through publish_iceland_league() directly. Without this block the dispatch
# rewrite that makes extracts the sole publish input (B4) would be unnetted:
# the block above calls the publisher directly and never exercises the branch
# publish_one() chooses.
#
# MEASURED DIVERGENCE, deliberate. `static` below is the slice
# scripts/05_publish.R:27-31 passes, and it drops `training_filter`. The
# publisher rebuilds `pred_d` via prepare_data(league, ...), so with the filter
# absent the CUP fixtures the filter would have removed stay in pred_d and the
# two bikar cells publish them. That is what PRODUCTION does -- the live
# data/publish/football/iceland/{karla,kvenna}-bikar/next_games.json each carry
# a CUP match today -- and the direct-call block above, which passes the FULL
# league, is the one that sees an empty cup fixture list. So 90 of the 92
# payloads are byte-identical across the two entry points and the two bikar
# next_games.json are pinned on content instead.
#
# The temp root is self-contained: publish_one derives
# output_root = file.path(root, "publish"), and the publisher derives
# round_predictions_history_root = file.path(dirname(output_root), "beliefs",
# "round_predictions_history"). With `root` a tempdir BOTH land inside it, so
# nothing is written into the repo's real data/beliefs/ tree (risk R5).
.GOLDEN_PUBLISH_ONE_DIVERGENT <- c(
  "football/iceland/karla-bikar/next_games.json",
  "football/iceland/kvenna-bikar/next_games.json"
)

test_that("publish_one reproduces the golden football manifest", {
  golden <- utils::read.csv(
    testthat::test_path("fixtures", "golden", "football-publish-hashes.csv"),
    stringsAsFactors = FALSE
  )

  root <- fixture_facts_root()
  extracts_root <- file.path(root, "beliefs", "extracts")
  league <- load_leagues()[["football_iceland"]]
  static <- league[c(
    "sport", "country", "sexes", "active", "stan_model", "data_source"
  )]

  for (sex in c("male", "female")) {
    build_football_extracts_fixture(root, extracts_root, sex)
    suppressMessages(suppressWarnings(publish_one(
      static, league$betting, "football_iceland", sex,
      root = root, validate = FALSE, end_date = FIXTURE_END_DATE
    )))
  }

  out <- file.path(root, "publish")
  produced <- list.files(
    file.path(out, "football"), pattern = "\\.json$",
    recursive = TRUE, full.names = TRUE
  )
  rel <- sub(paste0("^", out, "/"), "", produced)
  expect_setequal(rel, golden$file)

  actual <- vapply(produced, publish_json_digest, character(1), USE.NAMES = FALSE)
  names(actual) <- rel
  netted <- setdiff(golden$file, .GOLDEN_PUBLISH_ONE_DIVERGENT)
  changed <- netted[
    actual[netted] != golden$sha256[match(netted, golden$file)]
  ]
  expect_equal(
    length(changed), 0L,
    info = paste("changed payloads:", paste(changed, collapse = ", "))
  )

  # Content pin for the two payloads the production slice legitimately moves.
  for (cell in c("karla-bikar", "kvenna-bikar")) {
    ng <- jsonlite::read_json(
      file.path(out, "football", "iceland", cell, "next_games.json")
    )
    expect_gt(length(ng$matches), 0L)
    expect_setequal(
      unique(vapply(ng$matches, function(m) m$division, character(1))), "CUP"
    )
    expect_named(ng$matches[[1]], .NEXT_GAMES_COLUMNS, ignore.order = FALSE)
  }
})

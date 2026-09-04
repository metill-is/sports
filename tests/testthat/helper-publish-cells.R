# Publish every configured cell of every sport from the committed fixtures.
#
# Shared by the meta v2 contract and the next_games contract: both are
# CROSS-CELL assertions, and a contract that only holds on the cells one test
# happens to publish is not a contract.

# Publish all 17 configured cells -- 9 football, 4 basketball, 4 handball --
# from the committed fixtures into one tempdir. Everything (facts, extracts,
# publish output, the round_predictions_history sidecar) lands inside temp
# roots, so no test writes into the repo's data tree.
.publish_all_cells <- function(env = parent.frame()) {
  facts <- fixture_facts_root(env)
  extracts <- file.path(facts, "beliefs", "extracts")
  dir.create(extracts, recursive = TRUE, showWarnings = FALSE)
  for (sport in c("basketball", "handball")) {
    file.copy(
      testthat::test_path("fixtures", "extracts", paste0("sport=", sport)),
      extracts,
      recursive = TRUE
    )
  }
  out <- file.path(withr::local_tempdir(.local_envir = env), "publish")
  leagues <- load_leagues()
  for (key in c("basketball_iceland", "handball_iceland", "football_iceland")) {
    league <- leagues[[key]]
    for (sex in c("male", "female")) {
      if (identical(league$sport, "football")) {
        build_football_extracts_fixture(facts, extracts, sex)
      }
      extracted <- read_extracted_iceland(
        league, sex = sex, fit_date = FIXTURE_FIT_DATE,
        extracts_root = extracts
      )
      suppressMessages(suppressWarnings(publish_iceland_league(
        extracted = extracted, league = league, sex = sex,
        end_date = FIXTURE_END_DATE,
        root = facts, output_root = out, extracts_root = extracts,
        archive_root = file.path(facts, "beliefs", "archive")
      )))
    }
  }
  out
}

# (sport, sex, division, dir) for every configured cell.
.published_cells <- function(out) {
  rows <- list()
  for (key in c("basketball_iceland", "handball_iceland", "football_iceland")) {
    sport <- load_leagues()[[key]]$sport
    for (sex in c("male", "female")) {
      slugs <- .iceland_division_slugs(key, sex)
      sex_folder <- if (sex == "male") "karla" else "kvenna"
      for (code in names(slugs)) {
        rows[[length(rows) + 1L]] <- list(
          sport = sport, sex = sex, division = code, key = key,
          dir = file.path(
            out, sport, "iceland", paste0(sex_folder, "-", slugs[[code]])
          )
        )
      }
    }
  }
  rows
}

.read_cell_json <- function(cell, name) {
  jsonlite::read_json(file.path(cell$dir, name), simplifyVector = FALSE)
}


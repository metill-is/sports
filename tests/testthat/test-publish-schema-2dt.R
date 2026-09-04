# The basketball and handball publish schemas, ARMED.
#
# They were authored and proved under config/publish-schemas/_draft/, which
# resolves in NEITHER validator, and moved into place by a single `git mv` only
# once the stale June cells were deleted. The staging was not ceremony: the
# schema tree rsyncs to metill-platform as data/ithrottir-schemas/ from ONE
# clone at ONE SHA, 7x/day, so the moment config/publish-schemas/<sport>/
# exists the platform's validate_publish.py fails closed for that sport on the
# very next pull -- and any non-conforming JSON still sitting in data/publish/
# would then freeze fly.metill.is on the last-known-good payload.
#
# Rollback is `git rm -r config/publish-schemas/<sport>` plus reverting one
# line of tools/gen-publish-schemas.R. No JSON is touched either way.

.DRAFT_SPORTS <- c("basketball", "handball")

.draft_dir <- function() {
  testthat::test_path("..", "..", "config", "publish-schemas")
}

.publish_2dt_fixture_cells <- function(env = parent.frame()) {
  root <- fixture_facts_root(env)
  extracts_root <- file.path(root, "beliefs", "extracts")
  dir.create(extracts_root, recursive = TRUE, showWarnings = FALSE)
  for (sport in .DRAFT_SPORTS) {
    file.copy(
      testthat::test_path("fixtures", "extracts", paste0("sport=", sport)),
      extracts_root,
      recursive = TRUE
    )
  }
  leagues <- load_leagues()
  for (key in c("basketball_iceland", "handball_iceland")) {
    league <- leagues[[key]]
    static <- league[c(
      "sport", "country", "sexes", "active", "stan_model", "data_source"
    )]
    for (sex in c("male", "female")) {
      # validate = FALSE deliberately: the real schemas are not armed, which is
      # the whole point of the draft stage.
      suppressMessages(suppressWarnings(publish_one(
        static, league$betting, key, sex,
        root = root, validate = FALSE, end_date = FIXTURE_END_DATE
      )))
    }
  }
  root
}

test_that("the armed schemas accept every fixture-published bb/hb cell", {
  root <- .publish_2dt_fixture_cells()
  for (sport in .DRAFT_SPORTS) {
    res <- validate_publish_dir(
      file.path(root, "publish", sport),
      schema_dir = .draft_dir(), sport = sport
    )
    expect_length(res$unmatched, 0L)
    expect_equal(
      res$n_files,
      4L * length(sport_publish_profile(sport)$surfaces),
      info = sport
    )
    expect_true(res$ok, info = paste(sport, paste(res$errors, collapse = " | ")))
  }
})

test_that("the generator's source directories are still inert", {
  # The mirror of the pre-arming assertion. _base and _delta ride the rsync to
  # metill-platform as extra files; neither resolver can reach them, because no
  # publish JSON can have one of them as its first path segment and _base's
  # files are named <name>.json rather than <name>.schema.json.
  for (d in c("_base", "_delta", "_draft")) {
    expect_null(.resolve_schema_path(
      testthat::test_path("..", "..", "config", "publish-schemas"),
      d, "meta.json"
    ))
  }
})

test_that("the armed schemas really are in force, not resolving to nothing", {
  # The failure this guards is silent: a schema_dir that resolves nothing
  # returns ok = TRUE with n_files = 0, which reads exactly like success.
  root <- .publish_2dt_fixture_cells()
  for (sport in .DRAFT_SPORTS) {
    res <- validate_publish_dir(
      file.path(root, "publish", sport),
      schema_dir = testthat::test_path("..", "..", "config", "publish-schemas"),
      sport = sport
    )
    expect_gt(res$n_files, 0L)
    expect_length(res$unmatched, 0L)
    expect_true(res$ok, info = sport)
  }
})

test_that("the bb/hb schemas REJECT the football-only placement labels", {
  # D3, enforced in the contract rather than only in a test. Basketball's four
  # cells qualify 8 of 12, 8 of 12, 10 of 10 and 4 of 11 teams for the
  # post-season -- four cells, four structures, one of which takes every team
  # through -- and the league table decides the deildarmeistari, not the
  # Islandsmeistari. A p_top_six or p_winner in a bb/hb payload is a top-six
  # number wearing a playoff label, so the schema refuses it outright.
  for (sport in .DRAFT_SPORTS) {
    for (banned in c("p_top_six", "p_winner")) {
      tmp <- withr::local_tempdir()
      cell <- file.path(tmp, sport, "iceland", "karla-bd")
      dir.create(cell, recursive = TRUE)
      row <- list(team = "A", p_top_of_table = 0.5, p_relegation = 0.1)
      row[[banned]] <- 0.5
      jsonlite::write_json(
        list(
          generated_at = "2100-01-01T00:00:00+0000", season = 2100L,
          n_teams = 4L, basis = "regular_season_table",
          records = list(), summary = list(row)
        ),
        file.path(cell, "final_positions.json"),
        auto_unbox = TRUE, null = "null"
      )
      res <- validate_publish_dir(
        file.path(tmp, sport),
        schema_dir = .draft_dir(), sport = sport
      )
      expect_false(res$ok, info = paste(sport, banned))
    }
  }
})

test_that("the bb/hb schemas require the meta v2 contract", {
  # These cells are new and emit v2 from their first publish, so unlike
  # football they can require it from day one.
  base <- list(
    sport = "basketball", sex = "male", league = "Bonusdeild", division = "BD",
    is_cup = FALSE, season = 2100L, generated_at = "2100-01-01T00:00:00+0000",
    fit_date = "2100-01-01", round = 3L, n_draws = 50L,
    n_rounds = 6L, n_rounds_source = "config",
    units = list(strength = "points", home_advantage = "points", diff_bin_width = 5L),
    points = list(win = 2L, draw = NULL, loss = 0L),
    season_scope = "regular_season",
    postseason = list(name_is = "Urslitakeppni", modelled = FALSE),
    qualify = NULL, relegation = list(slots = NULL)
  )
  check <- function(meta, env = parent.frame()) {
    tmp <- withr::local_tempdir(.local_envir = env)
    cell <- file.path(tmp, "basketball", "iceland", "karla-bd")
    dir.create(cell, recursive = TRUE)
    jsonlite::write_json(meta, file.path(cell, "meta.json"),
      auto_unbox = TRUE, null = "null"
    )
    validate_publish_dir(
      file.path(tmp, "basketball"),
      schema_dir = .draft_dir(), sport = "basketball"
    )$ok
  }
  expect_true(check(base))
  for (k in c("n_rounds", "units", "points", "season_scope", "postseason",
              "qualify", "relegation", "n_rounds_source")) {
    dropped <- base[setdiff(names(base), k)]
    expect_false(check(dropped), info = k)
  }
  # basketball's division code starts with a digit (SC-10).
  d1 <- base
  d1$division <- "1D"
  expect_true(check(d1))
  # football's split-season object has no meaning here and is deleted.
  spl <- base
  spl$split <- list(upper = 6L, lower = 6L)
  expect_true(check(spl))
  # season_scope is narrowed: these tables never cover a full season.
  fs <- base
  fs$season_scope <- "full_season"
  expect_false(check(fs))
  # ...and the sport enum is narrowed to the one sport.
  wrong <- base
  wrong$sport <- "handball"
  expect_false(check(wrong))
})

test_that("home advantage is bounded on the sport's own units (B5)", {
  # The 2DT models are additive in raw points/goals -- there is no link to
  # undo. Applying football's exp() to an additive 4-point parameter published
  # a home edge of 54.6, which every schema in the tree accepted. A range bound
  # on the sport's own scale is what would have caught it. It is a gross-error
  # guard, not a complete B5 detector: exp() of a ~1.5-goal handball edge is
  # ~4.5 and stays inside the bound. test-extract-2dt-home-advantage-units.R
  # is the exact check; this is the contract-level backstop.
  for (sport in .DRAFT_SPORTS) {
    write_ha <- function(median, env = parent.frame()) {
      tmp <- withr::local_tempdir(.local_envir = env)
      cell <- file.path(tmp, sport, "iceland", "karla-bd")
      dir.create(cell, recursive = TRUE)
      jsonlite::write_json(
        list(generated_at = "2100-01-01T00:00:00+0000", records = list(list(
          team = "A", component = "total", median = median,
          coverage = 0.5, lower = median - 0.5, upper = median + 0.5
        ))),
        file.path(cell, "home_advantage.json"),
        auto_unbox = TRUE
      )
      validate_publish_dir(
        file.path(tmp, sport),
        schema_dir = .draft_dir(), sport = sport
      )$ok
    }
    expect_true(write_ha(2.5), info = sport)
    expect_false(write_ha(54.6), info = sport)
  }
})

test_that("each sport's delta file set equals the JSON surfaces it declares", {
  # The delta directory IS the manifest, so this is the seam that stops it
  # drifting from sport_publish_profile()$surfaces. Football's surface list is
  # NOT a file list -- five of its sixteen entries are payload features -- so
  # the comparison is against the surfaces that name a base schema.
  base_names <- sub(
    "[.]json$", "",
    list.files(
      testthat::test_path("..", "..", "config", "publish-schemas", "_base"),
      pattern = "[.]json$"
    )
  )
  for (sport in c("football", .DRAFT_SPORTS)) {
    deltas <- sub("[.]json$", "", list.files(
      testthat::test_path("..", "..", "config", "publish-schemas", "_delta", sport),
      pattern = "[.]json$"
    ))
    expect_setequal(
      deltas,
      intersect(sport_publish_profile(sport)$surfaces, base_names)
    )
  }
  # Neither 2DT sport ingests a knockout cup, and the ABSENCE of a
  # tournament_placements delta is what records that.
  for (sport in .DRAFT_SPORTS) {
    expect_false(file.exists(testthat::test_path(
      "..", "..", "config", "publish-schemas", "_delta", sport,
      "tournament_placements.json"
    )))
  }
})

test_that("basketball and handball schemas are armed", {
  for (sport in .DRAFT_SPORTS) {
    expect_false(
      is.null(.resolve_schema_path(
        testthat::test_path("..", "..", "config", "publish-schemas"),
        sport, "meta.json"
      )),
      info = sport
    )
  }
})

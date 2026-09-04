# THE B4 ACCEPTANCE TEST.
#
# Basketball and handball have never published from CI. publish_one() took the
# extracts branch only for football_iceland; every other league fell through to
# data/beliefs/fits/sport=X/country=Y/sex=Z/fit.rds -- a path .gitignore
# excludes and decide-publish.yml never produces. It warned "No fit at {path}
# -- skipping" and returned invisible(NULL), so the workflow exited 0 with no
# health row and nothing published.
#
# This test reproduces the EXACT CI condition: an extracts tree, and provably
# no fit RDS anywhere under the root. The RDS exists on the dev machine, so its
# absence is asserted rather than assumed -- its presence would mask the very
# failure this file exists to catch.

.B4_CELLS <- list(
  basketball = c("karla-bd", "karla-1d", "kvenna-bd", "kvenna-1d"),
  handball   = c("karla-od", "karla-g66", "kvenna-od", "kvenna-g66")
)

.b4_publish_all <- function(env = parent.frame()) {
  root <- fixture_facts_root(env)
  extracts_root <- file.path(root, "beliefs", "extracts")
  dir.create(extracts_root, recursive = TRUE, showWarnings = FALSE)
  for (sport in names(.B4_CELLS)) {
    file.copy(
      testthat::test_path("fixtures", "extracts", paste0("sport=", sport)),
      extracts_root,
      recursive = TRUE
    )
  }

  # THE RDS-ABSENT CONDITION.
  expect_false(dir.exists(file.path(root, "beliefs", "fits")))
  expect_length(
    list.files(root, pattern = "[.]rds$", recursive = TRUE, ignore.case = TRUE),
    0L
  )

  leagues <- load_leagues()
  for (key in c("basketball_iceland", "handball_iceland")) {
    league <- leagues[[key]]
    static <- league[c(
      "sport", "country", "sexes", "active", "stan_model", "data_source"
    )]
    for (sex in c("male", "female")) {
      suppressMessages(suppressWarnings(publish_one(
        static, league$betting, key, sex,
        root = root, validate = FALSE, end_date = FIXTURE_END_DATE
      )))
    }
  }

  # Still absent afterwards: nothing on the publish path creates or reads one.
  expect_false(dir.exists(file.path(root, "beliefs", "fits")))
  root
}

test_that("all eight bb/hb cells publish from extracts with no fit RDS", {
  root <- .b4_publish_all()
  publish_root <- file.path(root, "publish")

  produced <- character()
  for (sport in names(.B4_CELLS)) {
    sport_dir <- file.path(publish_root, sport, "iceland")
    expect_true(dir.exists(sport_dir), info = sport)
    produced <- c(
      produced,
      file.path(sport, list.files(sport_dir, include.dirs = TRUE))
    )
  }
  expect_setequal(
    produced,
    unlist(lapply(
      names(.B4_CELLS),
      function(s) file.path(s, .B4_CELLS[[s]])
    ), use.names = FALSE)
  )

  # No code path still writes the un-suffixed data/publish/<sport>/iceland/
  # {karla,kvenna}/ shape the stale June JSON lives in.
  expect_true(all(grepl("^(karla|kvenna)-[a-z0-9]+$", basename(produced))))
})

test_that("each bb/hb cell ships exactly the sport's profile surfaces", {
  root <- .b4_publish_all()
  for (sport in names(.B4_CELLS)) {
    expected <- paste0(sort(sport_publish_profile(sport)$surfaces), ".json")
    for (cell in .B4_CELLS[[sport]]) {
      files <- list.files(
        file.path(root, "publish", sport, "iceland", cell),
        pattern = "[.]json$"
      )
      expect_setequal(files, expected)
    }
  }
})

test_that("bb/hb meta.json carries division, is_cup and the configured label", {
  root <- .b4_publish_all()
  slugs <- list(
    basketball = list(
      male = .iceland_division_slugs("basketball_iceland", "male"),
      female = .iceland_division_slugs("basketball_iceland", "female")
    ),
    handball = list(
      male = .iceland_division_slugs("handball_iceland", "male"),
      female = .iceland_division_slugs("handball_iceland", "female")
    )
  )
  labels <- list(
    basketball = list(
      male = .iceland_division_labels("basketball_iceland", "male"),
      female = .iceland_division_labels("basketball_iceland", "female")
    ),
    handball = list(
      male = .iceland_division_labels("handball_iceland", "male"),
      female = .iceland_division_labels("handball_iceland", "female")
    )
  )
  for (sport in names(.B4_CELLS)) {
    for (cell in .B4_CELLS[[sport]]) {
      sex <- if (startsWith(cell, "karla")) "male" else "female"
      slug <- sub("^(karla|kvenna)-", "", cell)
      code <- names(slugs[[sport]][[sex]])[slugs[[sport]][[sex]] == slug]
      expect_length(code, 1L)

      meta <- jsonlite::read_json(
        file.path(root, "publish", sport, "iceland", cell, "meta.json"),
        simplifyVector = FALSE
      )
      expect_equal(meta$sport, sport)
      expect_equal(meta$sex, sex)
      expect_equal(meta$division, code)
      expect_false(meta$is_cup)
      expect_equal(meta$league, unname(labels[[sport]][[sex]][[code]]))
      expect_null(meta$split)
    }
  }
})

test_that("bb/hb next_games uses football's field names, not the 2DT ones", {
  root <- .b4_publish_all()
  required <- c(
    "mean_home_goals", "mean_away_goals", "mean_goal_diff",
    "p_home_win", "p_draw", "p_away_win",
    "division_code", "goal_diff_distribution"
  )
  # The retired bespoke 2DT publisher's names. R7: p_tie is a deliberate
  # contract break -- nothing on metill-platform reads the bb/hb paths.
  retired <- c(
    "mean_home", "mean_away", "mean_diff", "p_home", "p_away", "p_tie",
    "division"
  )

  non_empty <- character()
  for (sport in names(.B4_CELLS)) {
    for (cell in .B4_CELLS[[sport]]) {
      ng <- jsonlite::read_json(
        file.path(root, "publish", sport, "iceland", cell, "next_games.json"),
        simplifyVector = FALSE
      )
      if (length(ng$matches) == 0L) next
      non_empty <- c(non_empty, file.path(sport, cell))
      for (m in ng$matches) {
        expect_named(m, .NEXT_GAMES_COLUMNS, ignore.order = FALSE)
        expect_true(all(required %in% names(m)))
        expect_length(intersect(setdiff(retired, "division"), names(m)), 0L)
        expect_true(is.list(m$goal_diff_distribution))
      }
    }
  }
  expect_true("basketball/karla-bd" %in% non_empty)
  expect_true("handball/karla-od" %in% non_empty)
})

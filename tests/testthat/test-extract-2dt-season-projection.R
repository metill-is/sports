# The 2DT extractor's table covers the whole regular season of each
# division's current season (spec 2026-09-16 §1, §11).
#
# Every simulated game hands out exactly two points (2-0, or 1-1 inside
# handball's tie threshold), so a division's expected points total is
# 2 x its games -- an invariant that tells a full season from a 14-day window
# and a new season from last season's table.

# The fixture's handball men's OD (4 teams) between seasons: the 2100 season
# is complete, the 2101 double round robin is published a week apart from
# FIXTURE_END_DATE + 1, so only its first two games sit inside Stan's window.
#
# `burn` draws that many uniforms between the stub fit and the extract, so the
# extractor starts from a different global RNG state (the stub seeds it).
.preseason_cell <- function(new_team = NULL, drop_pair = NULL, n_draws = 50L,
                            burn = 0L, env = parent.frame()) {
  root <- fixture_facts_root(env = env)
  teams <- fixture_division_teams("handball", "male", "OD")
  season_teams <- c(teams, new_team)
  g <- expand.grid(home_team = season_teams, away_team = season_teams,
                   stringsAsFactors = FALSE)
  g <- g[g$home_team != g$away_team, ]
  if (!is.null(drop_pair)) {
    g <- g[!(g$home_team == drop_pair[1] & g$away_team == drop_pair[2]), ]
  }
  next_season <- tibble::tibble(
    sport = "handball", country = "iceland", sex = "male", season = 2101L,
    match_date = FIXTURE_END_DATE + 7L * seq_len(nrow(g)) - 6L,
    home_team = g$home_team, away_team = g$away_team, division = "OD",
    round = seq_len(nrow(g)), kickoff_time = "19:30"
  )
  sched <- read_table("schedules", root = root)
  keep <- !(sched$sport == "handball" & sched$sex == "male" & sched$division == "OD")
  write_table(dplyr::bind_rows(sched[keep, ], next_season), "schedules", root = root)

  league <- load_leagues()[["handball_iceland"]]
  st <- suppressMessages(local_stub_2dt(league, "male", root = root, n_draws = n_draws))
  stats::runif(burn)
  extracts_root <- file.path(withr::local_tempdir(.local_envir = env), "extracts")
  suppressMessages(extract_handball_iceland(
    fit = st$fit, league = league, sex = "male",
    fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
    root = root, extracts_root = extracts_root, prep = st$prep
  ))
  part <- file.path(
    extracts_root, "sport=handball", "country=iceland", "sex=male",
    paste0("fit_date=", format(FIXTURE_FIT_DATE, "%Y-%m-%d"))
  )
  list(
    teams = teams,
    read = function(ft) {
      d <- arrow::read_parquet(file.path(part, paste0(ft, ".parquet")))
      d[d$division == "OD", ]
    }
  )
}

.expected_total <- function(pd) sum(pd$points * pd$probability)

test_that("before a ball is played the table is the whole new season", {
  cell <- .preseason_cell()
  fp <- cell$read("final_positions")
  expect_setequal(unique(fp$team), cell$teams)
  expect_setequal(unique(fp$placement), 1:4)
  pd <- cell$read("points_distribution")
  # Twelve 2101 fixtures and nothing banked. The old path would have added
  # 2100's 12 banked points to two windowed games: 16.
  expect_equal(.expected_total(pd), 24, tolerance = 1e-9)
  expect_lte(max(pd$points), 12)
})

test_that("a pairing missing from the new schedule is still played out (F16)", {
  cell <- .preseason_cell(drop_pair = c("HAM OD 03", "HAM OD 04"))
  expect_equal(.expected_total(cell$read("points_distribution")), 24, tolerance = 1e-9)
})

test_that("a scheduled team with no history is tabled below the division median (§7)", {
  cell <- .preseason_cell(new_team = "HAM OD NEW", n_draws = 400L)
  fp <- cell$read("final_positions")
  expect_setequal(unique(fp$team), c(cell$teams, "HAM OD NEW"))
  newcomer <- fp[fp$team == "HAM OD NEW", ]
  expect_equal(sum(newcomer$probability), 1, tolerance = 1e-9)
  expect_gt(sum(newcomer$placement * newcomer$probability), 3)
  expect_equal(.expected_total(cell$read("points_distribution")), 40, tolerance = 1e-9)
  # The strength surfaces still describe only teams the fit knows.
  expect_false("HAM OD NEW" %in% cell$read("team_strengths_quantiles")$team)
})

test_that("re-extracting the same fit reproduces its tables", {
  # local_stub_2dt() calls set.seed(), so two plain calls would enter the
  # extractor with the same global RNG state and match whether or not the
  # extractor seeds itself. The burn moves that state for the second call.
  a <- .preseason_cell(burn = 0L)
  b <- .preseason_cell(burn = 7L)
  expect_identical(a$read("final_positions"), b$read("final_positions"))
  expect_identical(a$read("points_distribution"), b$read("points_distribution"))
})

# EURO 2028 qualifying: the publish JSON (data/publish/euro2028/karla/).

e28p_dir <- function(...) testthat::test_path("..", "..", "data", "nt", "euro2028", ...)
e28p_names <- function() e28p_dir("team_names_is.csv")
e28p_stamp <- as.POSIXct("2026-12-06 19:00:00", tz = "UTC")

# A fit with strengths falling by pot, the fixture draw and a short run.
e28p_setup <- function(n_sims = 300L) {
  facts <- nt_euro2028_facts(e28p_dir())
  s <- facts$seeding
  theta <- stats::setNames(c(0.9, 0.5, 0.2, -0.2, -0.7)[s$pot], s$team)
  fit <- nt_test_fit(theta, n_draws = 4L)
  draw <- nt_read_euro2028_draw(
    testthat::test_path("fixtures", "euro2028", "groups.csv"),
    facts_dir = e28p_dir(), fit = fit
  )
  list(fit = fit, draw = draw, odds = nt_euro2028_odds(fit, draw, n_sims = n_sims, seed = 3L))
}

e28p_read <- function(dir, f) jsonlite::read_json(file.path(dir, f), simplifyVector = FALSE)

test_that("every entrant has one Icelandic name", {
  nm <- utils::read.csv(e28p_names(), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  expect_setequal(nm$team, nt_euro2028_facts(e28p_dir())$seeding$team)
  expect_false(anyDuplicated(nm$team) > 0)
  expect_true(all(nzchar(nm$name_is)))
  is <- stats::setNames(nm$name_is, nm$team)
  expect_identical(unname(is[c("Iceland", "Germany", "Faroe Islands")]), c("Ísland", "Þýskaland", "Færeyjar"))
})

test_that("the odds hold both readings from one seed", {
  e <- e28p_setup()
  again <- nt_simulate_euro2028(e$fit, e$draw, n_sims = 300L, seed = 3L)
  expect_identical(e$odds$teams, again$teams)
  expect_identical(e$odds$slots, again$slots)
  expect_equal(e$odds$compare$n_sims, 300L)
  expect_identical(e$odds$fixtures_source, "generated")
  expect_equal(e$odds$fit_n_draws, 4L)
})

test_that("the publish JSON fills 24 places and names every team in Icelandic", {
  e <- e28p_setup()
  out <- withr::local_tempdir()
  publish_euro2028(e$odds, out_dir = out, names_csv = e28p_names(), generated_at = e28p_stamp)
  expect_setequal(list.files(out), c("meta.json", "groups.json", "teams.json"))

  meta <- e28p_read(out, "meta.json")
  expect_identical(meta$generated_at, "2026-12-06T19:00:00Z")
  expect_identical(meta$draw_date, "2026-12-06")
  expect_equal(meta$n_sims, 300L)
  expect_identical(meta$fixtures, "generated")
  expect_identical(meta$article_15d$reading, "repeat")
  expect_false(meta$article_15d$ruled)
  expect_identical(meta$article_15d$note, "moot, not ruled")
  hp <- unlist(meta$host_places)
  expect_equal(sum(hp), 1, tolerance = 1e-3)

  tm <- e28p_read(out, "teams.json")$teams
  expect_length(tm, 54L)
  get <- function(k) vapply(tm, function(r) as.numeric(r[[k]]), numeric(1))
  expect_lt(abs(sum(get("p_qualify")) - 24), 0.01)
  expect_lt(abs(sum(get("p_winner")) - 12), 0.01)
  expect_lt(abs(sum(get("p_qualify_direct")) - 20), 0.01)
  # Sorted by the chance of qualifying.
  expect_false(is.unsorted(rev(get("p_qualify"))))
  ice <- tm[[which(vapply(tm, function(r) r$team, "") == "Iceland")]]
  expect_identical(ice$team_is, "Ísland")
  expect_identical(unlist(ice$groups), "H")
  expect_equal(ice$p_qualify, round(e$odds$teams$p_qualify[e$odds$teams$team == "Iceland"], 4))
  spain <- tm[[which(vapply(tm, function(r) r$team, "") == "Spain")]]
  expect_identical(unlist(spain$groups), c("A", "G"))
  expect_true(spain$quarter_finalist)

  gs <- e28p_read(out, "groups.json")$groups
  expect_identical(vapply(gs, function(g) g$group, ""), LETTERS[1:12])
  expect_identical(vapply(gs, function(g) as.integer(g$size), 1L), rep(c(4L, 5L), each = 6L))
  slots <- unlist(lapply(gs, function(g) g$slots), recursive = FALSE)
  expect_identical(lengths(lapply(slots, function(r) r$p_positions)), rep(c(4L, 5L), c(24L, 30L)))
  expect_length(slots, 54L)
  ph <- Filter(function(r) !is.null(r$placeholder), slots)
  expect_length(ph, 8L)
  qf1w <- Filter(function(r) identical(r$slot, "QF1 winner"), slots)[[1]]
  expect_null(qf1w$team)
  expect_identical(qf1w$placeholder$role, "winner")
  expect_identical(unlist(qf1w$placeholder$teams), c("Spain", "France"))
  expect_identical(unlist(qf1w$placeholder$teams_is), c("Spánn", "Frakkland"))
  qf1d <- Filter(function(r) identical(r$slot, "QF1 defeated"), slots)[[1]]
  expect_identical(qf1d$placeholder$role_is, "taplið")
  sl <- e$odds$slots
  expect_equal(qf1w$p_qualify, round(sl$p_qualify[sl$slot == "QF1 winner"], 4))
  expect_length(nt_check_euro2028_publish(out, stats::setNames(
    utils::read.csv(e28p_names(), fileEncoding = "UTF-8")$name_is,
    utils::read.csv(e28p_names(), fileEncoding = "UTF-8")$team
  )), 0L)
})

test_that("a payload that fails its checks leaves the published files as they were", {
  e <- e28p_setup()
  out <- withr::local_tempdir()
  publish_euro2028(e$odds, out_dir = out, names_csv = e28p_names(), generated_at = e28p_stamp)
  before <- tools::md5sum(file.path(out, c("meta.json", "groups.json", "teams.json")))
  bad <- e$odds
  bad$teams$p_qualify[1] <- 1.5
  expect_error(
    publish_euro2028(bad, out_dir = out, names_csv = e28p_names(), generated_at = Sys.time()),
    "failed its checks"
  )
  expect_identical(tools::md5sum(file.path(out, c("meta.json", "groups.json", "teams.json"))), before)
  # A first publish that fails writes nothing at all.
  fresh <- file.path(withr::local_tempdir(), "karla")
  expect_error(publish_euro2028(bad, out_dir = fresh, names_csv = e28p_names()), "failed its checks")
  expect_false(dir.exists(fresh))
})

test_that("the check catches a mangled Icelandic name and tables that do not add up", {
  e <- e28p_setup()
  out <- withr::local_tempdir()
  publish_euro2028(e$odds, out_dir = out, names_csv = e28p_names(), generated_at = e28p_stamp)
  nm <- utils::read.csv(e28p_names(), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  is <- stats::setNames(nm$name_is, nm$team)
  rewrite <- function(f, edit) {
    x <- e28p_read(out, f)
    jsonlite::write_json(edit(x), file.path(out, f), auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null")
  }
  # What a C locale writes for the letter I-acute.
  rewrite("teams.json", function(x) {
    i <- which(vapply(x$teams, function(r) r$team, "") == "Iceland")
    x$teams[[i]]$team_is <- "<U+00CD>sland"
    x
  })
  expect_match(nt_check_euro2028_publish(out, is), "Icelandic name missing or mangled for Iceland", all = FALSE)
  # Every value in 0..1, yet one team's place is lost: only the total shows it.
  rewrite("teams.json", function(x) {
    x$teams[[1]]$p_winner <- 0
    x
  })
  expect_match(nt_check_euro2028_publish(out, is), "p_winner sums to", all = FALSE)
  rewrite("groups.json", function(x) {
    x$groups[[1]]$slots[[1]]$p_positions[[1]] <- 0
    x
  })
  expect_match(nt_check_euro2028_publish(out, is), "group A: position probabilities", all = FALSE)
  file.remove(file.path(out, "meta.json"))
  expect_identical(nt_check_euro2028_publish(out, is), "meta.json is missing.")
})

test_that("the JSON carries the repeat reading and how often the once reading differs", {
  e <- e28p_setup(n_sims = 40L)
  d <- e$draw
  # Group K plays out the four-way tie on which the readings disagree: Albania
  # finish second under "repeat" and third under "once".
  map <- stats::setNames(d$groups$K, c("A", "B", "C", "D", "E"))
  tie <- nt_four_way_tie()
  key <- paste(d$fixtures$home_team, d$fixtures$away_team)
  hit <- match(paste(map[tie$home_team], map[tie$away_team]), key)
  d$fixtures$home_score[hit] <- tie$home_score
  d$fixtures$away_score[hit] <- tie$away_score
  odds <- nt_euro2028_odds(e$fit, d, n_sims = 40L, seed = 9L)
  expect_equal(odds$compare$any_position, 1)
  out <- withr::local_tempdir()
  publish_euro2028(odds, out_dir = out, names_csv = e28p_names(), generated_at = e28p_stamp)
  tm <- e28p_read(out, "teams.json")$teams
  alb <- tm[[which(vapply(tm, function(r) r$team, "") == "Albania")]]
  expect_equal(unlist(alb$p_positions)[2:3], c(1, 0))
  expect_equal(odds$teams_once$p_pos3[odds$teams_once$team == "Albania"], 1)
  expect_equal(e28p_read(out, "meta.json")$article_15d$p_any_position_differs, 1)
})

# International handball replays: the days a replay forecasts on, and the
# checks each day's files must pass.

replay_fixture <- function() {
  x <- publish_fixture()
  spec <- x$spec
  pf <- spec$prelim
  # Every preliminary match won 30-25 by the listed home side, so each group
  # finishes in its listed order (T01 > T02 > T03 > T04, and so on).
  res <- data.frame(
    stage = "preliminary", match_date = pf$match_date, home_team = pf$home_team, away_team = pf$away_team,
    home_60 = 30L, away_60 = 25L, home_score = 30L, away_score = 25L, pen_home = NA_integer_, pen_away = NA_integer_,
    home_wins = TRUE, row.names = NULL
  )
  standings <- do.call(rbind, lapply(names(spec$groups), function(g) {
    data.frame(stage = "preliminary", group = g, position = 1:4, team = spec$groups[[g]])
  }))
  main <- do.call(rbind, lapply(names(spec$main), function(G) {
    t <- unlist(lapply(spec$main[[G]], function(g) spec$groups[[g]][1:2]))
    data.frame(stage = "main", group = G, position = seq_along(t), team = t)
  }))
  facts <- list(standings = rbind(standings, main), knockout = data.frame(code = character(), winner = character(), loser = character()))
  sim <- ih_simulate_tournament(x$fit, spec, res, n_sims = 400L, seed = 1)
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  ih_publish_tournament(x$fit, sim, spec, res, dir, names = x$names, n_sims_matches = 400L)
  list(spec = spec, res = res, facts = facts, dir = dir, as_of = max(pf$match_date))
}

test_that("a replay forecasts the day before the start, every match day and the day after the last", {
  spec <- publish_fixture()$spec
  res <- data.frame(match_date = as.Date(c("2026-12-03", "2026-12-03", "2026-12-05", "2026-12-20")))
  days <- ih_replay_as_of_dates(spec, res)
  expect_equal(days, as.Date(c("2026-12-02", "2026-12-03", "2026-12-05", "2026-12-20", "2026-12-21")))
  expect_equal(ih_replay_as_of_dates(spec, res, "2026-12-08, 2026-12-01"), as.Date(c("2026-12-01", "2026-12-08")))
})

test_that("a day whose files agree with what happened passes every check", {
  x <- replay_fixture()
  ck <- ih_replay_checks(x$dir, x$spec, x$res, x$as_of, x$facts)
  expect_setequal(ck$check, c(
    "position probabilities sum to 1", "finished groups in their actual order", "eliminated teams 0, qualified teams 1",
    "main round carries the right results", "after the final: champion 1, placements 0/1", "goal-difference bin 0 is p_draw"
  ))
  expect_true(all(ck$ok), info = paste(ck$check[!ck$ok], ck$detail[!ck$ok]))
})

test_that("the checks catch a finished group out of its actual order and a broken probability", {
  x <- replay_fixture()
  wrong <- x$facts
  a <- wrong$standings$stage == "preliminary" & wrong$standings$group == "A"
  wrong$standings$team[a] <- rev(wrong$standings$team[a])
  ck <- ih_replay_checks(x$dir, x$spec, x$res, x$as_of, wrong)
  expect_false(ck$ok[ck$check == "finished groups in their actual order"])

  g <- jsonlite::read_json(file.path(x$dir, "groups.json"))
  g$groups[[1]]$teams[[1]]$p_pos[[1]] <- 0.5
  jsonlite::write_json(g, file.path(x$dir, "groups.json"), auto_unbox = TRUE, digits = NA)
  ck <- ih_replay_checks(x$dir, x$spec, x$res, x$as_of, x$facts)
  expect_false(ck$ok[ck$check == "position probabilities sum to 1"])

  p <- jsonlite::read_json(file.path(x$dir, "predictions.json"))
  p$matches[[1]]$p_draw <- p$matches[[1]]$p_draw + 0.01
  jsonlite::write_json(p, file.path(x$dir, "predictions.json"), auto_unbox = TRUE, digits = NA)
  ck <- ih_replay_checks(x$dir, x$spec, x$res, x$as_of, x$facts)
  expect_false(ck$ok[ck$check == "goal-difference bin 0 is p_draw"])
})

test_that("a team out after the preliminary round is caught when it keeps a main-round chance", {
  x <- replay_fixture()
  pl <- jsonlite::read_json(file.path(x$dir, "tournament_placements.json"))
  i <- which(vapply(pl$records, function(r) r$team == "T04" && r$round_name == "MR", logical(1)))
  pl$records[[i]]$probability <- 0.2
  jsonlite::write_json(pl, file.path(x$dir, "tournament_placements.json"), auto_unbox = TRUE, digits = NA)
  ck <- ih_replay_checks(x$dir, x$spec, x$res, x$as_of, x$facts)
  expect_false(ck$ok[ck$check == "eliminated teams 0, qualified teams 1"])
})

test_that("the replays are not live championships and never publish", {
  expect_length(intersect(names(IH_REPLAYS), names(IH_TOURNAMENTS)), 0L)
  for (id in names(IH_REPLAYS)) expect_equal(IH_REPLAYS[[id]]$publish[1], "replay")
  runner <- readLines(here::here("scripts", "ih", "replay_tournament.R"), warn = FALSE)
  expect_true(any(grepl("A replay never writes under data/publish", runner, fixed = TRUE)))
})

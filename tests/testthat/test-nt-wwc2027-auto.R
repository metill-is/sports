# The automatic refresh of the 2027 Women's World Cup play-off forecast:
# R/nt-wwc2027-auto.R, scripts/nt/wwc2027_auto_playoff.R and
# .github/workflows/wwc2027-playoff.yml. Feeds under fixtures/wwc2027/ are
# UEFA's match feed trimmed to the fields read: round 2 as listed on
# 27 November 2026, round 1 as finished in October 2026, and round 2 with
# simulated finished legs in round 1's shape.

fx <- function(name) {
  jsonlite::read_json(test_path("fixtures", "wwc2027", name), simplifyVector = FALSE)
}
ties_path <- here::here("data", "wc", "structure", "wwc2027_playoff_round2.csv")
fit_path <- here::here("data", "wc", "wwc2027", "playoff_fit.rds")
unplayed <- function() {
  t <- wwc2027_read_ties(ties_path)
  t$leg1_goals_a <- NA_integer_
  t$leg1_goals_b <- NA_integer_
  t$winner <- NA_character_
  t
}

test_that("the listed round 2 parses into 16 unplayed legs, the ties the right way round", {
  legs <- wwc2027_uefa_legs(fx("uefa_round2_upcoming.json"))
  expect_equal(nrow(legs), 16L)
  expect_true(all(legs$status == "UPCOMING"))
  expect_equal(sum(legs$leg == 1L), 8L)
  expect_true(all(is.na(legs$goals_home)))
  ties <- unplayed()
  expect_identical(wwc2027_apply_results(ties, legs), ties)
  # Every UEFA name is the ties table's (martj42) spelling.
  expect_setequal(c(legs$home, legs$away), c(ties$team_a, ties$team_b))
})

test_that("round 1 as it finished: the 90-minute score, and the winner after extra time and penalties", {
  r1 <- fx("uefa_round1_finished.json")
  expect_equal(nrow(wwc2027_uefa_legs(r1)), 0L) # not round 2
  legs <- wwc2027_uefa_legs(r1, round = "WEQ Play-offs Round 1")
  expect_equal(nrow(legs), 32L)
  # Poland v Belgium, 13 October 2026: 2-1 after 90, 3-2 after extra time,
  # Poland on penalties (the extra-time match in nt_extra_time_matches()).
  pb <- legs[legs$home == "Poland" & legs$away == "Belgium", ]
  expect_equal(pb$leg, 2L)
  expect_equal(c(pb$goals_home, pb$goals_away), c(2L, 1L))
  expect_identical(pb$winner, "Poland")
  ties <- data.frame(
    team_a = "Belgium", team_b = "Poland", leg1_goals_a = NA_integer_,
    leg1_goals_b = NA_integer_, winner = NA_character_
  )
  out <- wwc2027_apply_results(ties, legs[legs$home %in% c("Belgium", "Poland") & legs$away %in% c("Belgium", "Poland"), ])
  expect_equal(c(out$leg1_goals_a, out$leg1_goals_b), c(1L, 0L))
  expect_identical(out$winner, "Poland")
})

test_that("finished first legs fill the scores; live and unplayed legs leave the table alone", {
  ties <- unplayed()
  part <- wwc2027_apply_results(ties, wwc2027_uefa_legs(fx("uefa_round2_leg1_partial.json")))
  filled <- !is.na(part$leg1_goals_a)
  expect_setequal(part$team_a[filled], c("Slovenia", "Finland"))
  expect_equal(part$leg1_goals_b[part$team_a == "Slovenia"], 3L)
  expect_true(is.na(part$leg1_goals_a[part$team_a == "Portugal"])) # live, not final
  expect_true(all(is.na(part$winner)))

  leg1 <- wwc2027_apply_results(ties, wwc2027_uefa_legs(fx("uefa_round2_leg1_simulated.json")))
  expect_false(anyNA(leg1$leg1_goals_a))
  expect_equal(unlist(leg1[leg1$team_a == "Portugal", c("leg1_goals_a", "leg1_goals_b")], use.names = FALSE), c(1L, 2L))
  expect_true(all(is.na(leg1$winner)))
  # Applying the same feed again changes nothing (the run's "no news" path).
  expect_identical(wwc2027_apply_results(leg1, wwc2027_uefa_legs(fx("uefa_round2_leg1_simulated.json"))), leg1)
})

test_that("finished second legs set the winners, penalties included; a thin feed never erases a result", {
  leg2 <- wwc2027_apply_results(unplayed(), wwc2027_uefa_legs(fx("uefa_round2_leg2_simulated.json")))
  expect_false(anyNA(leg2$winner))
  expect_identical(leg2$winner[leg2$team_b == "Republic of Ireland"], "Republic of Ireland") # on penalties
  expect_identical(leg2$winner[leg2$team_a == "Portugal"], "Iceland")
  # The feed going back to "upcoming" (or empty) keeps what was recorded.
  expect_identical(wwc2027_apply_results(leg2, wwc2027_uefa_legs(fx("uefa_round2_upcoming.json"))), leg2)
  expect_identical(wwc2027_apply_results(leg2, wwc2027_uefa_legs(list())), leg2)
})

test_that("a match that is not a tie the right way round stops the run", {
  legs <- wwc2027_uefa_legs(fx("uefa_round2_leg1_simulated.json"))
  swapped <- legs
  swapped$leg[swapped$home == "Portugal"] <- 2L
  expect_error(wwc2027_apply_results(unplayed(), swapped), "right way round")
  renamed <- legs
  renamed$away[renamed$away == "Republic of Ireland"] <- "Ireland"
  expect_error(wwc2027_apply_results(unplayed(), renamed), "right way round")
  odd <- wwc2027_uefa_legs(fx("uefa_round2_leg2_simulated.json"))
  odd$winner[odd$home == "Iceland"] <- "Spain"
  expect_error(wwc2027_apply_results(unplayed(), odd), "not in the tie")
})

test_that("the ties table round-trips byte for byte", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  wwc2027_write_ties(wwc2027_read_ties(ties_path), tmp)
  expect_identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(ties_path)))
})

test_that("the committed compact fit predates the legs and reproduces the committed forecast", {
  skip_if_not(file.exists(fit_path))
  saved <- readRDS(fit_path)
  ties <- wwc2027_read_ties(ties_path)
  expect_setequal(saved$fit$teams, c(ties$team_a, ties$team_b))
  expect_equal(nrow(saved$fit$draws$theta), 4000L)
  # A first-leg score reweights the draws: the fit must not have seen it.
  expect_true(saved$cutoff <= as.Date("2026-12-01"))
  expect_true(saved$last_match < as.Date("2026-12-01"))
  committed <- jsonlite::read_json(here::here("data", "publish", "world_cup", "kvenna", "playoff.json"))
  root <- withr::local_tempdir()
  out <- wwc2027_publish_from_saved(saved, ties, root = root, generated_at = committed$generated_at)
  again <- jsonlite::read_json(file.path(out, "playoff.json"))
  expect_identical(again, committed)
})

test_that("a simulated leg night publishes the reweighted forecast", {
  skip_if_not(file.exists(fit_path))
  saved <- readRDS(fit_path)
  ties <- wwc2027_apply_results(unplayed(), wwc2027_uefa_legs(fx("uefa_round2_leg1_simulated.json")))
  root <- withr::local_tempdir()
  out <- wwc2027_publish_from_saved(saved, ties, root = root, generated_at = "2026-12-01T22:00:00Z")
  pl <- jsonlite::read_json(file.path(out, "playoff.json"))
  expect_true(all(vapply(pl$ties, `[[`, "", "status") == "first_leg_played"))
  expect_lt(pl$effective_draws, 4000)
  ice <- pl$ties[[which(vapply(pl$ties, `[[`, "", "team_a") == "Portugal")]]
  expect_true(ice$legs[[1]]$played)
  expect_equal(c(ice$legs[[1]]$goals_home, ice$legs[[1]]$goals_away), c(1L, 2L))
  expect_null(ice$after_first_leg)

  ties2 <- wwc2027_apply_results(ties, wwc2027_uefa_legs(fx("uefa_round2_leg2_simulated.json")))
  out2 <- wwc2027_publish_from_saved(saved, ties2, root = root, generated_at = "2026-12-05T23:00:00Z")
  pl2 <- jsonlite::read_json(file.path(out2, "playoff.json"))
  expect_true(all(vapply(pl2$ties, `[[`, "", "status") == "decided"))
  expect_identical(sort(vapply(pl2$places, function(p) p$p_win, 1)), c(rep(0, 8), rep(1, 8)))
})

wf_path <- here::here(".github", "workflows", "wwc2027-playoff.yml")
wf_steps <- function() yaml::read_yaml(wf_path)$jobs$refresh$steps
wf_step <- function(name) {
  steps <- wf_steps()
  hit <- vapply(steps, function(s) identical(s$name, name), logical(1))
  expect_equal(sum(hit), 1L, info = name)
  steps[[which(hit)]]
}

test_that("the workflow polls only on the leg nights and stops after the play-off", {
  skip_if_not(file.exists(wf_path))
  wf <- yaml::read_yaml(wf_path)
  on <- wf[["on"]] %||% wf[["TRUE"]]
  crons <- vapply(on$schedule, `[[`, "", "cron")
  expect_setequal(crons, c("*/15 15-23 1,5 12 *", "30 0-12/2 2,6 12 *"))
  expect_identical(wf$jobs$refresh$env$LAST_DAY, "2026-12-07")
  expect_match(wf_step("Check the window")$run, '"$GITHUB_EVENT_NAME" == "schedule"', fixed = TRUE)
  expect_false(isTRUE(wf$concurrency$`cancel-in-progress`))
  expect_length(wf$permissions, 0L)
  expect_identical(wf$jobs$refresh$permissions, list(contents = "write"))
  # No refit: neither INLA nor CmdStan is installed.
  txt <- paste(readLines(wf_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("INLA_VERSION|install_cmdstan|wwc2027_fit.R", txt))
})

test_that("only the ties table and the women's folder are committed, and a dry run never commits or dispatches", {
  skip_if_not(file.exists(wf_path))
  commit <- wf_step("Commit the forecast")
  adds <- regmatches(commit$run, gregexpr("git add[^\n]*", commit$run))[[1]]
  expect_identical(adds, "git add -- data/wc/structure/wwc2027_playoff_round2.csv data/publish/world_cup/kvenna/")
  expect_match(commit$`if`, "env.SIMULATE == 'none'", fixed = TRUE)
  expect_match(commit$`if`, "steps.publish.outputs.changed == 'true'", fixed = TRUE)
  expect_match(commit$`if`, "github.ref == 'refs/heads/main'", fixed = TRUE)
  expect_match(commit$run, ".github/scripts/push-with-retry.sh", fixed = TRUE)
  wf <- yaml::read_yaml(wf_path)
  notify <- wf$jobs$notify
  expect_identical(notify$environment, "ih-refresh")
  expect_match(notify$`if`, "needs.refresh.outputs.changed == 'true'", fixed = TRUE)
  expect_match(notify$`if`, "== 'none'", fixed = TRUE)
  expect_match(notify$`if`, "refs/heads/main", fixed = TRUE)
  expect_identical(notify$steps[[1]]$env$GH_TOKEN, "${{ secrets.PLATFORM_DISPATCH_TOKEN }}")
  expect_match(notify$steps[[1]]$run, "gh workflow run pull-sports-data.yml --repo metill-is/metill-platform --ref main", fixed = TRUE)
  # Each simulated feed the dispatch offers exists.
  sims <- setdiff(unlist((wf[["on"]] %||% wf[["TRUE"]])$workflow_dispatch$inputs$simulate$options), "none")
  for (s in sims) {
    expect_true(file.exists(test_path("fixtures", "wwc2027", sprintf("uefa_round2_%s_simulated.json", s))), info = s)
  }
  lines <- readLines(wf_path, warn = FALSE)
  secrets <- unique(unlist(regmatches(lines, gregexpr("secrets\\.[A-Z_]+", lines))))
  expect_setequal(secrets, paste0("secrets.", c("GITHUB_TOKEN", "PLATFORM_DISPATCH_TOKEN")))
})

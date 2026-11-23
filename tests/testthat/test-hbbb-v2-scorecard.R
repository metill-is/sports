# The monthly scorecard of the handball / basketball v2 switch path
# (R/hbbb-v2-scorecard.R, scripts/hbbb_v2_scorecard.R). Local only.

test_that("no GitHub Actions workflow runs the scorecard", {
  yml <- list.files(here::here(".github", "workflows"), pattern = "\\.ya?ml$", full.names = TRUE)
  skip_if(length(yml) == 0L, "no workflow files")
  hits <- unlist(lapply(yml, function(f) {
    x <- readLines(f, warn = FALSE)
    if (any(grepl("hbbb_v2_scorecard|hbbb-v2-scorecard|hbbb_scorecard", x))) basename(f)
  }))
  expect_identical(hits, NULL)
})

# ---- Standard errors ------------------------------------------------------------

# The definition the estimator implements, written pair by pair: the
# covariance of every two matches that share a team-season, whichever side the
# team played on, each match with itself included.
brute_force_se <- function(x, home, away) {
  n <- length(x)
  e <- x - mean(x)
  share <- outer(home, home, "==") | outer(away, away, "==") | outer(home, away, "==") | outer(away, home, "==")
  G <- length(unique(c(home, away)))
  sqrt(G / (G - 1) * sum(outer(e, e) * share) / n^2)
}

test_that("the team-season standard error is the covariance of every two matches sharing a team-season", {
  set.seed(3)
  teams <- paste0("T", 1:6)
  pairs <- expand.grid(h = teams, a = teams, stringsAsFactors = FALSE)
  pairs <- pairs[pairs$h != pairs$a, ]
  home <- paste(pairs$h, 2027)
  away <- paste(pairs$a, 2027)
  # A team's matches carry its own effect on both sides: home and away
  # matches of one team are correlated, which a by-side cluster misses.
  effect <- stats::setNames(stats::rnorm(6, 0, 1), teams)
  x <- effect[pairs$h] - effect[pairs$a] + stats::rnorm(nrow(pairs), 0, 0.3)
  expect_equal(cluster_se_team_season(x, home, away), brute_force_se(x, home, away))
  # Side does not matter: swapping the labels of some matches changes nothing.
  swap <- seq_along(x) %% 3 == 0
  h2 <- ifelse(swap, away, home)
  a2 <- ifelse(swap, home, away)
  expect_equal(cluster_se_team_season(x, h2, a2), cluster_se_team_season(x, home, away))
})

test_that("with no team-season shared the team-season standard error is the iid one, up to the cluster factor", {
  x <- c(0.3, -1.2, 0.8, 2.1, -0.4, 0.05, 1.7)
  home <- paste0("H", seq_along(x))
  away <- paste0("A", seq_along(x))
  G <- 2 * length(x)
  expect_equal(cluster_se_team_season(x, home, away), sqrt(G / (G - 1) * sum((x - mean(x))^2) / length(x)^2))
})

test_that("a single team-season gives no standard error", {
  expect_identical(cluster_se_team_season(c(1, 2, 3), rep("A", 3), rep("A", 3)), NA_real_)
})

# ---- Scoring v2 ------------------------------------------------------------------

one_match <- function(loc_m, s_m, loc_t, s_t, n_draws = 3L) {
  list(
    loc_m = matrix(loc_m, n_draws, length(loc_m), byrow = TRUE), sigma_m = rep(s_m, n_draws),
    loc_t = matrix(loc_t, n_draws, length(loc_t), byrow = TRUE), sigma_t = rep(s_t, n_draws)
  )
}

test_that("handball is scored on the rounded predictive of the margin, the total and the result", {
  comp <- one_match(c(1.2, -0.5), 5, c(55, 60), 6.5)
  obs <- data.frame(home_score = c(30L, 27L), away_score = c(28L, 27L))
  s <- hbbb_v2_score_matches(comp, obs, "handball")
  expect_equal(s$ls_margin, log(v2_integer_pmf(c(2, 0), c(1.2, -0.5), 5)))
  expect_equal(s$ls_total, log(v2_integer_pmf(c(58, 54), c(55, 60), 6.5)))
  o <- v2_outcome_probs(c(1.2, -0.5), 5)
  expect_equal(s$ll_result, log(c(o$p_home[1], o$p_draw[2])))
  expect_equal(c(s$p_home, s$p_draw, s$p_away), c(o$p_home, o$p_draw, o$p_away))
  expect_equal(s$margin, c(2, 0))
  expect_equal(s$mean_m, c(1.2, -0.5))
  expect_equal(s$mean_t, c(55, 60))
})

test_that("basketball is scored on the final result, reached through the level-score shift and overtime", {
  comp <- one_match(c(3, -1), 12, c(165, 150), 16)
  obs <- data.frame(home_score = c(88L, 70L), away_score = c(80L, 74L), home_score_reg = c(80L, 70L), away_score_reg = c(80L, 74L))
  shift <- c(0.3, 0.3)
  ot <- list(mean = 20, sd = 6)
  s <- hbbb_v2_score_matches(comp, obs, "basketball", zero_shift = shift, ot = ot)
  r <- v2_basketball_result_probs(c(3, -1), 12, zero_shift = shift)
  expect_equal(s$ll_result, log(c(r$p_home[1], r$p_away[2])))
  # The final margin's distribution: no level mass, sums to one.
  k <- setdiff(-250:250, 0)
  comp1 <- one_match(3, 12, 165, 16)
  pm <- vapply(k, function(kk) exp(hbbb_v2_score_matches(comp1, data.frame(home_score = 100L + kk, away_score = 100L, home_score_reg = 100L + kk, away_score_reg = 100L), "basketball", zero_shift = shift[1], ot = ot)$ls_margin[1]), numeric(1))
  expect_equal(sum(pm), 1, tolerance = 1e-6)
  # Game 1 went to overtime from a level regulation score: its final margin of
  # 8 has the overtime part only.
  p0 <- r$p_draw_reg[1]
  ot_part <- v2_integer_pmf(8, 3 * 5 / 40, 12 * sqrt(5 / 40), ties = FALSE)
  expect_equal(s$ls_margin[1], log(v2_integer_pmf(8, 3, 12, zero_shift = 0.3) + p0 * ot_part))
  # The final total: regulation alone, or regulation plus one overtime period.
  want_t <- (1 - p0) * v2_integer_pmf(168, 165, 16) + p0 * v2_integer_pmf(168, 165 + 20, sqrt(16^2 + 6^2))
  expect_equal(s$ls_total[1], log(want_t))
  # The expected final total includes the expected overtime points.
  expect_equal(s$mean_t, c(165, 150) + r$p_draw_reg * 20)
  # The regulation 1X2 the bookmaker settles on, with the shift.
  o <- v2_outcome_probs(c(3, -1), 12, zero_shift = shift)
  expect_equal(c(s$p_home_reg, s$p_draw_reg), c(o$p_home, o$p_draw))
  expect_equal(s$margin_reg, c(0, -4))
})

test_that("the final-margin distribution agrees with the result probabilities when the draws differ", {
  # Overtime happens in the draws where the game is close, so its share is
  # weighted by each draw's own level probability. One close draw and one
  # lopsided one tell a weighted average from a plain one.
  comp <- list(loc_m = matrix(c(0.5, 30), 2, 1), sigma_m = c(10, 10), loc_t = matrix(165, 2, 1), sigma_t = c(16, 16))
  k <- setdiff(-250:250, 0)
  pm <- vapply(k, function(kk) {
    exp(hbbb_v2_score_matches(comp, data.frame(home_score = 100L + kk, away_score = 100L, home_score_reg = NA_integer_, away_score_reg = NA_integer_),
      "basketball",
      zero_shift = 0.2, ot = list(mean = 20, sd = 6)
    )$ls_margin)
  }, numeric(1))
  r <- v2_basketball_result_probs(comp$loc_m, comp$sigma_m, zero_shift = 0.2)
  expect_equal(sum(pm[k > 0]), r$p_home, tolerance = 1e-6)
  expect_equal(sum(pm), 1, tolerance = 1e-6)
})

test_that("a stored fit is scored against the current results, each fixture on its own draws", {
  # Three fixtures with very different locations: scoring one fixture's
  # result on another fixture's draws would be far off.
  fx <- data.frame(
    match_date = as.Date(c("2100-03-10", "2100-03-10", "2100-03-11")),
    home_team = c("A", "C", "E"), away_team = c("B", "D", "F")
  )
  fit <- list(
    sport = "handball", fixtures = fx,
    loc_m = matrix(c(8, -8, 0), 3, 3, byrow = TRUE), loc_t = matrix(c(50, 60, 70), 3, 3, byrow = TRUE),
    sigma_m = rep(5, 3), sigma_t = rep(6, 3), zero_shift = c(0, 0, 0), ot = NULL, ot_share = 5 / 40
  )
  # Results in another order; the third game has not been played yet, and a
  # forfeit is never scored.
  res <- data.frame(
    match_date = as.Date(c("2100-03-10", "2100-03-10")), season = 2100L, division = "OD",
    home_team = c("C", "A"), away_team = c("D", "B"), home_score = c(20L, 33L), away_score = c(27L, 25L)
  )
  s <- hbbb_v2_score_fit(fit, res)
  s <- s[order(s$home_team), ]
  expect_identical(s$home_team, c("A", "C"))
  expect_equal(s$margin, c(8, -7))
  expect_equal(s$ls_margin, log(v2_integer_pmf(c(8, -7), c(8, -8), 5)))
  expect_equal(s$ls_total, log(v2_integer_pmf(c(58, 47), c(50, 60), 6)))
  forfeit <- res
  forfeit$home_score[1] <- 0L
  forfeit$away_score[1] <- 10L
  expect_identical(hbbb_v2_score_fit(fit, forfeit)$home_team, "A")
})

# ---- Production's stored forecasts ------------------------------------------------

write_archive_fit <- function(root, sport, fit_date, matches, home_goals, away_goals) {
  n <- length(home_goals)
  rows <- do.call(rbind, lapply(seq_len(nrow(matches)), function(i) {
    data.frame(
      sport = sport, country = "iceland", sex = "male", fit_date = as.Date(fit_date),
      match_date = matches$match_date[i], home_team = matches$home_team[i], away_team = matches$away_team[i],
      draw_id = seq_len(n), home_goals = home_goals, away_goals = away_goals
    )
  }))
  write_table(tibble::as_tibble(rows), "beliefs_archive", root = root)
}

test_that("production's forecast of a match is its newest stored fit dated before the match", {
  root <- withr::local_tempdir()
  m <- data.frame(
    match_date = as.Date(c("2100-03-10", "2100-03-12")), home_team = c("A", "C"), away_team = c("B", "D"),
    home_score = c(30L, 25L), away_score = c(28L, 25L), season = 2100L, division = "OD"
  )
  write_archive_fit(root, "handball", "2100-03-01", m, c(20, 20, 20, 21), c(30, 30, 30, 30))
  write_archive_fit(root, "handball", "2100-03-08", m, c(30, 31, 29, 30, 32), c(28, 28, 28, 30, 27))
  # Dated on the first match's day: too late for it, the newest for the second.
  write_archive_fit(root, "handball", "2100-03-10", m, c(10, 10, 25), c(40, 40, 25))
  p <- hbbb_production_forecasts("handball", "male", m, root = root)
  p <- p[order(p$match_date), ]
  expect_identical(p$fit_date, as.Date(c("2100-03-08", "2100-03-10")))
  diff <- c(30, 31, 29, 30, 32) - c(28, 28, 28, 30, 27)
  expect_equal(p$ls_margin[1], log(v2_draws_pmf(diff, 2)))
  expect_equal(p$mean_m[1], mean(diff))
  expect_equal(p$p_draw[1], mean(abs(diff) <= 0.5))
  expect_equal(p$ll_result[1], log(mean(diff > 0.5)))
  # A level handball result is scored on the draw band.
  expect_equal(p$ll_result[2], log(1 / 3))
  expect_identical(p$lead_days, c(2, 2))
})

test_that("a basketball margin is scored without the level band production's draws can put mass on", {
  root <- withr::local_tempdir()
  m <- data.frame(match_date = as.Date("2100-03-10"), home_team = "A", away_team = "B", home_score = 90L, away_score = 85L, season = 2100L, division = "BD")
  hg <- c(90, 91.2, 80, 85.3, 99)
  ag <- c(85, 91.0, 88, 85.1, 70)
  write_archive_fit(root, "basketball", "2100-03-09", m, hg, ag)
  p <- hbbb_production_forecasts("basketball", "male", m, root = root)
  expect_equal(p$ls_margin, log(v2_draws_pmf(hg - ag, 5, ties = FALSE)))
  expect_equal(p$ll_result, log(mean(hg - ag > 0)))
})

# ---- v2 at production's information date --------------------------------------------

test_that("each match is paired with v2 fitted on the results production's forecast had", {
  # Production's fit dated D ran before D's games, so it saw results to D - 1;
  # v2 for the same matches is fitted with end_date D - 1.
  prod <- data.frame(
    match_date = as.Date(c("2100-03-10", "2100-03-10", "2100-03-12", "2100-03-14")),
    home_team = c("A", "C", "E", "G"), away_team = c("B", "D", "F", "H"),
    fit_date = as.Date(c("2100-03-09", "2100-03-09", "2100-03-09", "2100-03-13"))
  )
  plan <- hbbb_scorecard_plan(prod)
  expect_identical(plan$info_date, as.Date(c("2100-03-08", "2100-03-12")))
  expect_identical(vapply(plan$fixtures, nrow, integer(1)), c(3L, 1L))
  expect_true(all(plan$fixtures[[1]]$match_date > plan$info_date[1]))
})

test_that("the fit's inputs are the results up to the information date, and its fixtures are the paired ones", {
  root <- fixture_hbbb_root()
  league <- load_leagues()[["handball_iceland"]]
  res <- as.data.frame(read_table("results", root = root, filter = list(sport = "handball", country = "iceland", sex = "male")))
  dates <- sort(unique(res$match_date))
  info <- dates[length(dates) - 3L]
  fixtures <- res[res$match_date > info & res$match_date <= dates[length(dates) - 1L], c("match_date", "home_team", "away_team")]
  inp <- hbbb_v2_info_inputs(league, "male", info, fixtures, root = root)
  expect_lte(max(inp$spec$train$match_date), info)
  expect_identical(inp$prep$stan_data$N, sum(res$match_date <= info & !.is_forfeit(res, "handball")))
  got <- inp$prep$pred_d[inp$j, ]
  expect_identical(paste(got$match_date, got$home_team, got$away_team), paste(fixtures$match_date, fixtures$home_team, fixtures$away_team))
})

test_that("the cache key is the fit's exact inputs: it moves with the data and the draw count, and not with row order", {
  root <- fixture_hbbb_root()
  league <- load_leagues()[["handball_iceland"]]
  res <- as.data.frame(read_table("results", root = root, filter = list(sport = "handball", country = "iceland", sex = "male")))
  dates <- sort(unique(res$match_date))
  info <- dates[length(dates) - 3L]
  fixtures <- res[res$match_date > info, c("match_date", "home_team", "away_team")]
  expect_gte(nrow(fixtures), 3L)
  inp <- hbbb_v2_info_inputs(league, "male", info, fixtures, root = root)
  key <- hbbb_v2_fit_key(inp, n_draws = 100L, seed = 1L, previous_sd = NULL)
  expect_identical(hbbb_v2_fit_key(inp, n_draws = 100L, seed = 1L, previous_sd = NULL), key)
  expect_false(identical(hbbb_v2_fit_key(inp, n_draws = 200L, seed = 1L, previous_sd = NULL), key))
  expect_false(identical(hbbb_v2_fit_key(inp, n_draws = 100L, seed = 1L, previous_sd = c(a = 1)), key))
  shuffled <- fixtures[rev(seq_len(nrow(fixtures))), ]
  expect_identical(hbbb_v2_fit_key(hbbb_v2_info_inputs(league, "male", info, shuffled, root = root), 100L, 1L, NULL), key)
  # A corrected score before the information date changes the inputs.
  res2 <- res
  res2$home_score[1] <- res2$home_score[1] + 1L
  root2 <- withr::local_tempdir()
  write_table(tibble::as_tibble(res2), "results", root = root2)
  write_table(read_table("schedules", root = root), "schedules", root = root2)
  inp2 <- hbbb_v2_info_inputs(league, "male", info, fixtures, root = root2)
  expect_false(identical(hbbb_v2_fit_key(inp2, 100L, 1L, NULL), key))
})

test_that("an information date's v2 fit keeps its forecasts and gate, without aborting on a failed gate", {
  skip_if_no_inla()
  root <- fixture_hbbb_root()
  league <- load_leagues()[["handball_iceland"]]
  res <- as.data.frame(read_table("results", root = root, filter = list(sport = "handball", country = "iceland", sex = "male")))
  dates <- sort(unique(res$match_date))
  fixtures <- res[res$match_date == max(dates), c("match_date", "home_team", "away_team")]
  inp <- hbbb_v2_info_inputs(league, "male", dates[length(dates) - 1L], fixtures, root = root)
  f <- suppressMessages(suppressWarnings(hbbb_v2_fit_info(inp, "handball", n_draws = 200L)))
  expect_identical(nrow(f$fixtures), nrow(fixtures))
  expect_identical(dim(f$loc_m), c(200L, nrow(fixtures)))
  expect_true(is.logical(f$gate$passed))
  expect_true(all(c("margin sd: team", "total sd: team") %in% names(f$hyper_sd)))
  s <- hbbb_v2_score_fit(f, res)
  expect_identical(nrow(s), nrow(fixtures))
  expect_true(all(is.finite(s$ls_margin)))
})

# ---- The Lengjan line -------------------------------------------------------------

lengjan_rows <- function(home, away, scraped, odds, outcome = c("home", "draw", "away"), sex = NA_character_, kickoff = NA) {
  data.frame(
    sport = "handball", country = "iceland", sex = sex, match_date = as.Date("2100-03-10"),
    home_team = home, away_team = away, market = "moneyline", outcome = outcome,
    line = NA_real_, odds = odds, scraped_at = as.POSIXct(scraped, tz = "UTC"),
    kickoff_at = as.POSIXct(kickoff, tz = "UTC")
  )
}

test_that("the market line is the last complete 1X2 scrape before kick-off, de-vigged, for the cell's sex", {
  root <- withr::local_tempdir()
  league <- load_leagues()[["handball_iceland"]]
  odds <- rbind(
    lengjan_rows("A", "B", "2100-03-08 10:00:00", c(1.5, 6, 3.5)),
    lengjan_rows("A", "B", "2100-03-10 09:00:00", c(1.6, 6.5, 3.2)),
    # The newest scrape on match day lists only two outcomes: not used.
    lengjan_rows("A", "B", "2100-03-10 12:00:00", c(1.7, 3.0), outcome = c("home", "away")),
    lengjan_rows("A", "B", "2100-03-11 09:00:00", c(9, 9, 9)),
    lengjan_rows("C kv", "D kv", "2100-03-10 09:00:00", c(2, 4, 2.5)),
    # Kick-off known: a scrape after it is not a pre-match price.
    lengjan_rows("E", "F", "2100-03-10 08:00:00", c(2, 3.5, 3), kickoff = "2100-03-10 16:00:00"),
    lengjan_rows("E", "F", "2100-03-10 17:00:00", c(1.1, 9, 15), kickoff = "2100-03-10 16:00:00"),
    # One scrape listed the home price twice: the two are averaged.
    lengjan_rows("G", "H", "2100-03-10 08:00:00", c(2.0, 2.2, 3.4, 3.1), outcome = c("home", "home", "draw", "away"))
  )
  write_table(tibble::as_tibble(odds), "odds", root = root)
  line <- hbbb_market_line(league, "male", root = root)
  line <- line[order(line$home_team), ]
  expect_identical(line$home_team, c("A", "E", "G"))
  devig <- function(o) (1 / o) / sum(1 / o)
  expect_equal(unlist(line[1, c("q_home", "q_draw", "q_away")], use.names = FALSE), devig(c(1.6, 6.5, 3.2)))
  expect_equal(unlist(line[2, c("q_home", "q_draw", "q_away")], use.names = FALSE), devig(c(2, 3.5, 3)))
  expect_equal(unlist(line[3, c("q_home", "q_draw", "q_away")], use.names = FALSE), devig(c(2.1, 3.4, 3.1)))
})

# ---- Comparison, accuracy and gate ------------------------------------------------------------

scored_pairs <- function(n, d_margin = 0, d_total = 0, bias = 0, seed = 1) {
  set.seed(seed)
  noise <- function(n) scale(stats::rnorm(n, 0, 0.1), scale = FALSE)[, 1]
  teams <- paste0("T", 1:10)
  h <- sample(teams, n, replace = TRUE)
  a <- vapply(h, function(x) sample(setdiff(teams, x), 1), character(1))
  prod <- data.frame(ls_margin = stats::rnorm(n, -3.2, 0.5), ls_total = stats::rnorm(n, -3.4, 0.5), ll_result = stats::rnorm(n, -0.7, 0.3))
  data.frame(
    sport = "handball", sex = rep(c("male", "female"), length.out = n), season = 2027L,
    match_date = as.Date("2026-11-17") + seq_len(n) %% 40, home_team = h, away_team = unname(a),
    # WHY centred: the gate's bias check compares the mean error with one se;
    # an uncentred draw would land outside it a third of the time.
    margin = 0.6 + scale(stats::rnorm(n, 0, 5), scale = FALSE)[, 1],
    total = 55 + stats::rnorm(n, 0, 6),
    v2_ls_margin = prod$ls_margin + d_margin + noise(n),
    v2_ls_total = prod$ls_total + d_total + noise(n),
    v2_ll_result = prod$ll_result + noise(n),
    prod_ls_margin = prod$ls_margin, prod_ls_total = prod$ls_total, prod_ll_result = prod$ll_result,
    v2_mean_m = 0.6 + bias, prod_mean_m = 2, v2_mean_t = 55, prod_mean_t = 56
  )
}
gate_fits_log <- function(passed = TRUE) data.frame(sport = "handball", gate = passed, breaches = if (passed) "" else "x")

test_that("the comparison is v2 minus production per match, with team-season-clustered standard errors", {
  x <- scored_pairs(80)
  cmp <- hbbb_scorecard_compare(x)
  row <- cmp[cmp$group == "handball, both sexes" & cmp$measure == "margin log score", ]
  d <- x$v2_ls_margin - x$prod_ls_margin
  expect_equal(row$diff, mean(d))
  expect_equal(row$se, cluster_se_team_season(d, paste(x$home_team, x$season), paste(x$away_team, x$season)))
  expect_equal(row$n, 80L)
  expect_setequal(unique(cmp$group), c("handball male", "handball female", "handball, both sexes"))
})

test_that("the accuracy table shows each forecaster's mean expected margin and root mean square errors", {
  x <- scored_pairs(40)
  acc <- hbbb_scorecard_accuracy(x)
  row <- acc[acc$group == "handball, both sexes", ]
  expect_equal(c(row$mean_m_prod, row$mean_m_v2, row$mean_m_obs), c(2, 0.6, mean(x$margin)))
  expect_equal(row$rmse_m_v2, sqrt(mean((x$margin - 0.6)^2)))
  expect_equal(row$rmse_t_prod, sqrt(mean((x$total - 56)^2)))
})

test_that("the gate needs 150 matches, scores no worse than production by more than one se, an unbiased margin and passing fits", {
  ok <- scored_pairs(200, d_margin = 0.02)
  g <- hbbb_scorecard_gate(hbbb_scorecard_compare(ok), ok, gate_fits_log())
  expect_true(g$passed)

  few <- scored_pairs(120)
  g <- hbbb_scorecard_gate(hbbb_scorecard_compare(few), few, gate_fits_log())
  expect_false(g$passed)
  expect_match(g$reasons, "120 scored matches, fewer than 150")

  worse <- scored_pairs(200, d_total = -0.08)
  g <- hbbb_scorecard_gate(hbbb_scorecard_compare(worse), worse, gate_fits_log())
  expect_false(g$passed)
  expect_match(g$reasons, "total log score.*worse than production by more than one standard error")

  # The bias check at one standard error: just inside passes, just outside fails.
  err <- ok$margin - ok$v2_mean_m
  se <- cluster_se_team_season(err, paste(ok$home_team, ok$season), paste(ok$away_team, ok$season))
  inside <- ok
  inside$v2_mean_m <- inside$v2_mean_m - 0.9 * se
  expect_true(hbbb_scorecard_gate(hbbb_scorecard_compare(inside), inside, gate_fits_log())$passed)
  outside <- ok
  outside$v2_mean_m <- outside$v2_mean_m - 1.1 * se
  g <- hbbb_scorecard_gate(hbbb_scorecard_compare(outside), outside, gate_fits_log())
  expect_false(g$passed)
  expect_match(g$reasons, "expected home margin")

  # A v2 fit that failed the production gate would not have been published.
  g <- hbbb_scorecard_gate(hbbb_scorecard_compare(ok), ok, gate_fits_log(FALSE))
  expect_false(g$passed)
  expect_match(g$reasons, "1 of 1 v2 fits failed the production gate")
})

test_that("the note is written from the tables, one row per group", {
  x <- scored_pairs(200, d_margin = 0.02)
  cmp <- hbbb_scorecard_compare(x)
  lines <- hbbb_scorecard_note(
    meta = list(from = as.Date("2026-11-17"), to = as.Date("2026-12-16"), commit = "abc1234", n_draws = 4000L, minutes = 12),
    gate = hbbb_scorecard_gate(cmp, x, gate_fits_log()), cmp = cmp, accuracy = hbbb_scorecard_accuracy(x),
    market = NULL, fits = gate_fits_log(), paired = transform(x, prod_lead_days = 1)
  )
  expect_true(any(grepl("^\\| handball, both sexes \\| 200 \\| \\+0\\.0200", lines)))
  expect_true(any(grepl("No scored match had a Lengjan 1X2 price", lines)))
  expect_false(any(grepl("NA ±|NaN", lines)))
})

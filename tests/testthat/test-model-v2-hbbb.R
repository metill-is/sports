# The handball / basketball v2 switch path (R/model-v2-hbbb.R). No league is on
# it: test-model-v2.R pins that every league's engine is Stan.

# ---- Spec ---------------------------------------------------------------------

test_that("the spec trains on prepare_data()'s rows and refuses a prep built on other rows", {
  root <- fixture_hbbb_root()
  f <- hbbb_fixture_spec("handball", root = root)
  expect_identical(f$spec$v$N, f$prep$stan_data$N)
  expect_identical(f$spec$v$N_pred, nrow(f$prep$pred_d))
  # Fixture seasons come from the schedules (the fixture's are 2100), not from
  # the calendar year of the date.
  expect_true(all(f$spec$pred$season == 2100L))

  shuffled <- f$prep
  shuffled$stan_data$team1 <- rev(shuffled$stan_data$team1)
  expect_error(
    hbbb_v2_spec(shuffled, f$league, "male", end_date = FIXTURE_END_DATE, root = root),
    "do not match"
  )
})

test_that("basketball trains on regulation time, keeps rows without it out of the likelihood and counts its coverage", {
  root <- fixture_hbbb_root(drop_reg = 4L)
  f <- hbbb_fixture_spec("basketball", root = root)
  s <- f$spec
  res <- as.data.frame(read_table("results", root = root, filter = list(sport = "basketball", country = "iceland", sex = "male")))
  res <- res[order(res$match_date), ]
  res <- res[res$match_date <= FIXTURE_END_DATE, ]
  expect_identical(s$y$margin, as.numeric(res$home_score_reg - res$away_score_reg))
  expect_identical(s$y$total, as.numeric(res$home_score_reg + res$away_score_reg))
  # Every row keeps its indices: a row without regulation scores still has a state.
  expect_identical(s$v$N, nrow(res))
  last <- res$season == max(res$season)
  expect_equal(s$coverage, mean(!is.na(res$home_score_reg[last])))
  expect_lt(s$coverage, 1)
  # Three overtime games per cell: too few, so the overtime points come from
  # a five-minute share of the regulation total.
  expect_identical(s$ot$n, 3L)
  expect_identical(s$ot$source, "share of regulation")
  reg_total <- (res$home_score_reg + res$away_score_reg)[!is.na(res$home_score_reg)]
  expect_equal(s$ot$mean, mean(reg_total) * 5 / 40)

  hb <- hbbb_fixture_spec("handball", root = root)$spec
  expect_true(all(hb$observed))
  expect_identical(hb$coverage, 1)
})

test_that("each team's current state is at its next fixture, else at its latest match of the season", {
  root <- fixture_hbbb_root()
  s <- hbbb_fixture_spec("basketball", root = root)$spec
  v <- s$v
  id <- s$node_id
  for (k in seq_len(v$K)) {
    ahead <- which(v$season_pred == s$s_cur & (v$home_pred == k | v$away_pred == k))
    if (length(ahead)) {
      j <- ahead[1]
      want <- if (v$home_pred[j] == k) id$pred_home[j] else id$pred_away[j]
    } else {
      played <- which(v$season == s$s_cur & (v$home == k | v$away == k))
      i <- played[length(played)]
      want <- if (v$home[i] == k) id$train_home[i] else id$train_away[i]
    }
    expect_identical(id$cur[k], want, info = s$teams[k])
    expect_identical(s$nodes$k[id$cur[k]], k)
  }
  # Basketball has the week term: its nodes carry a team-season and a week.
  expect_false(anyNA(s$nodes$wk[id$cur]))
  # Handball is static: one node per team and season, no week term.
  hb <- hbbb_fixture_spec("handball", root = root)$spec
  expect_true(all(is.na(hb$nodes$ts)) && all(is.na(hb$nodes$wk)))
  expect_null(hb$latent$margin$w_t)
  expect_false(anyDuplicated(hb$nodes[, c("k", "s", "d")]) > 0)
})

test_that("a team's r-th training match addresses its own node, carried forward past its last match", {
  root <- fixture_hbbb_root()
  f <- hbbb_fixture_spec("handball", root = root)
  sd <- f$prep$stan_data
  s <- f$spec
  expect_identical(dim(s$slot), c(as.integer(sd$N_rounds), s$v$K))
  expect_identical(s$slot[cbind(sd$round1, sd$team1)], s$node_id$train_home)
  expect_identical(s$slot[cbind(sd$round2, sd$team2)], s$node_id$train_away)
  n_k <- tabulate(c(sd$team1, sd$team2), nbins = s$v$K)
  short <- which(n_k < sd$N_rounds)
  expect_gt(length(short), 0L)
  for (k in short) {
    expect_true(all(s$slot[(n_k[k] + 1L):sd$N_rounds, k] == s$slot[n_k[k], k]))
  }
})

test_that("a fixture's season is its schedule's, and a new season moves every current state across the break", {
  # WHY: the fixture's January dates fall in the calendar year of their season,
  # so only a relabelled schedule shows that the season is looked up, not
  # derived from the date.
  root <- fixture_hbbb_root(schedule_season = 2101L)
  f <- hbbb_fixture_spec("handball", root = root)
  s <- f$spec
  expect_true(all(s$pred$season == 2101L))
  expect_identical(s$s_cur_label, 2101L)
  expect_identical(s$last_season, 2100L)
  expect_true(all(s$nodes$s[s$node_id$cur] == s$s_cur))
})

# ---- Composition and the adapted fit -------------------------------------------

test_that("fixture locations are the v2 formulas of the drawn components", {
  root <- fixture_hbbb_root()
  s <- hbbb_fixture_spec("basketball", root = root)$spec
  smp <- synthetic_v2_samples(s, "basketball")
  comp <- hbbb_v2_compose(smp$margin, smp$total, s)
  v <- s$v
  col <- function(m, i) m[, as.character(i)]
  for (j in seq_len(v$N_pred)) {
    h <- v$home_pred[j]
    a <- v$away_pred[j]
    st <- function(lat, k, ts) {
      col(lat$b, k) + col(lat$u_s, (k - 1L) * v$NS + v$season_pred[j]) +
        col(lat$w_t, (ts - 1L) * v$W + v$wk_pred[j])
    }
    lm <- smp$margin$latent
    lt <- smp$total$latent
    want_m <- as.vector(lm$hfa) + col(lm$ha, h) + st(lm, h, v$ts_home_pred[j]) - st(lm, a, v$ts_away_pred[j])
    lvl <- lt[[paste0("alpha", v$div_pred[j])]][, 1] + col(lt$s_s, (v$div_pred[j] - 1L) * v$NS + v$season_pred[j])
    want_t <- lvl + st(lt, h, v$ts_home_pred[j]) + st(lt, a, v$ts_away_pred[j])
    expect_equal(comp$loc_m[, j], want_m)
    expect_equal(comp$loc_t[, j], want_t)
  }
})

test_that("the adapted fit carries the Stan path's variable names and shapes", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    f <- hbbb_fixture_spec(sport, root = root)
    fit <- synthetic_hbbb_fit(f$spec, f$prep, sport)
    stan <- stub_2dt_draws(
      teams = f$prep$teams$team, n_pred = nrow(f$prep$pred_d), n_draws = 40L,
      n_rounds = f$prep$stan_data$N_rounds, n_seasons = f$prep$stan_data$N_seasons
    )
    reads <- c(
      "cur_offense_home", "cur_defense_home", "cur_strength_home",
      "cur_offense_away", "cur_defense_away", "cur_strength_away",
      "offense", "defense", "home_advantage_off", "home_advantage_def", "home_advantage_tot",
      "goals1_pred", "goals2_pred", "lp__", "nu", "mean_goals", "delta_mean_goals", "sigma_mean_goals",
      if (sport == "handball") c("rho", "sigma_team", "mean_sigma_team", "scale_sigma_team") else c("sigma", "alpha_rho", "beta_rho", "beta2_rho", "beta3_rho")
    )
    expect_setequal(fit$variables, reads)
    for (var in reads) {
      expect_identical(
        posterior::variables(fit$draws(var)), colnames(stan[[var]]),
        info = paste(sport, var)
      )
    }
    expect_identical(posterior::ndraws(fit$draws("lp__")), 40L)
  }
})

test_that("attack and defence reproduce a fixture's v2 margin and total when it is both teams' next match", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    f <- hbbb_fixture_spec(sport, root = root)
    s <- f$spec
    smp <- synthetic_v2_samples(s, sport)
    comp <- hbbb_v2_compose(smp$margin, smp$total, s)
    fit <- hbbb_v2_as_fit(comp, s, f$prep, sport)
    d <- function(var) posterior::as_draws_matrix(fit$draws(var))
    v <- s$v
    nxt <- which(s$node_id$pred_home == s$node_id$cur[v$home_pred] & s$node_id$pred_away == s$node_id$cur[v$away_pred])
    expect_gt(length(nxt), 0L)
    level <- d("mean_goals")[, s$n_seasons]
    for (j in nxt) {
      h <- v$home_pred[j]
      a <- v$away_pred[j]
      mu_h <- level + d("cur_offense_home")[, h] - d("cur_defense_away")[, a]
      mu_a <- level + d("cur_offense_away")[, a] - d("cur_defense_home")[, h]
      expect_equal(as.vector(mu_h - mu_a), comp$loc_m[, j], info = sport)
      expect_equal(as.vector(mu_h + mu_a), comp$loc_t[, j], info = sport)
    }
    # The strength sum is the margin model's net strength.
    expect_equal(as.vector(d("cur_strength_away")), as.vector(comp$net[, s$node_id$cur]))
  }
})

test_that("the season simulator, fed these inputs, draws the v2 predictive", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    f <- hbbb_fixture_spec(sport, root = root)
    s <- f$spec
    smp <- synthetic_v2_samples(s, sport, n_draws = 4L, const_sigma = TRUE)
    comp <- hbbb_v2_compose(smp$margin, smp$total, s)
    fit <- hbbb_v2_as_fit(comp, s, f$prep, sport)
    inputs <- .extract_sim_inputs_2dt(fit, f$prep$teams, sport, n_seasons = s$n_seasons)
    v <- s$v
    j <- which(s$node_id$pred_home == s$node_id$cur[v$home_pred] & s$node_id$pred_away == s$node_id$cur[v$away_pred])[1]
    team_row <- function(k, home) {
      r <- inputs$team[inputs$team$team == s$teams[k] & inputs$team$.draw == 1L, ]
      out <- data.frame(off = r$cur_offense, def = r$cur_defense)
      if (home) {
        out$off <- out$off + r$home_advantage_off
        out$def <- out$def + r$home_advantage_def
      }
      if (sport == "handball") out$sigma_team <- r$sigma_team
      out[rep(1L, 2e5), , drop = FALSE]
    }
    sc <- inputs$scalar[inputs$scalar$.draw == 1L, ]
    sc$mean_goals <- sc$mean_goals_fit
    sc <- sc[rep(1L, 2e5), , drop = FALSE]
    g <- withr::with_seed(5, .match_fn_2dt(sport)(team_row(v$home_pred[j], TRUE), team_row(v$away_pred[j], FALSE), sc))
    margin <- g$home - g$away
    total <- g$home + g$away
    sig <- if (sport == "basketball") c(12, 16) else c(5, 6)
    # Monte Carlo error of 200,000 draws: about 0.2 % of an sd.
    expect_lt(abs(mean(margin) - comp$loc_m[1, j]) / sig[1], 0.01, label = paste(sport, "margin mean"))
    expect_lt(abs(mean(total) - comp$loc_t[1, j]) / sig[2], 0.01, label = paste(sport, "total mean"))
    expect_equal(stats::sd(margin), sig[1], tolerance = 0.01, label = paste(sport, "margin sd"))
    expect_equal(stats::sd(total), sig[2], tolerance = 0.01, label = paste(sport, "total sd"))
  }
})

test_that("the simulator steps the scoring level into a later season only from the last season with results", {
  # The state already sits in the current season. Stepping it again would
  # count the season-to-season variance twice.
  same <- fixture_hbbb_root()
  later <- fixture_hbbb_root(schedule_season = 2101L)
  step <- function(root) {
    f <- hbbb_fixture_spec("handball", root = root)
    smp <- synthetic_v2_samples(f$spec, "handball")
    comp <- hbbb_v2_compose(smp$margin, smp$total, f$spec)
    fit <- hbbb_v2_as_fit(comp, f$spec, f$prep, "handball", level_sd = 4)
    unique(as.vector(fit$draws("sigma_mean_goals")))
  }
  expect_identical(step(same), 2)
  expect_identical(step(later), 0)
})

test_that("handball scores are whole numbers and basketball's final scores are never level", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    f <- hbbb_fixture_spec(sport, root = root)
    fit <- synthetic_hbbb_fit(f$spec, f$prep, sport, n_draws = 400L)
    h <- posterior::as_draws_matrix(fit$draws("goals1_pred"))
    a <- posterior::as_draws_matrix(fit$draws("goals2_pred"))
    expect_true(all(h == round(h)) && all(a == round(a)) && all(h >= 0) && all(a >= 0), info = sport)
    if (sport == "basketball") expect_false(any(h == a))
  }
})

test_that("the score draws leave the caller's random stream where it was", {
  root <- fixture_hbbb_root()
  f <- hbbb_fixture_spec("basketball", root = root)
  set.seed(9)
  expected <- stats::runif(1)
  set.seed(9)
  synthetic_hbbb_fit(f$spec, f$prep, "basketball")
  expect_identical(stats::runif(1), expected)
})

# ---- Overtime and the level-score shift ----------------------------------------

test_that("overtime settles every level draw, toward the side the regulation predictive favours", {
  n <- 2e5
  home <- matrix(c(rep(80L, n), 90L), ncol = 1L)
  away <- matrix(c(rep(80L, n), 70L), ncol = 1L)
  loc <- matrix(6, nrow(home), 1L)
  out <- withr::with_seed(3, v2_overtime_scores(home, away, loc, 12, ot = list(mean = 20, sd = 6)))
  expect_false(any(out$home == out$away))
  # A draw that was not level is left alone.
  expect_identical(c(out$home[n + 1L], out$away[n + 1L]), c(90L, 70L))
  # One period: margin normal(6 / 8, 12 sqrt(1 / 8)), a level period replayed.
  share <- 5 / 40
  p_home <- 1 - stats::pnorm(0.5, 6 * share, 12 * sqrt(share))
  p_away <- stats::pnorm(-0.5, 6 * share, 12 * sqrt(share))
  expect_equal(mean(out$home[1:n] > out$away[1:n]), p_home / (p_home + p_away), tolerance = 0.01)
  # Points per overtime game: one period's 20, plus the replayed periods.
  p_level <- 1 - p_home - p_away
  expect_equal(mean(out$home[1:n] + out$away[1:n] - 160), 20 / (1 - p_level), tolerance = 0.02)
})

test_that("overtime reads each draw's own margin sd", {
  # The first half of the draws has a near-zero sd: their overtime always goes
  # to the home side, whose expected period margin is 0.75. The second half
  # has a huge one. Only some cells are level, so the level cells' positions
  # are not the draws' positions.
  n <- 4000L
  home <- matrix(80L, n, 2L)
  away <- matrix(80L, n, 2L)
  early <- seq_len(n / 4)
  home[early, 1] <- 90L
  away[early, 1] <- 70L
  loc <- matrix(6, n, 2L)
  scale <- rep(c(0.01, 1000), each = n / 2)
  out <- withr::with_seed(8, v2_overtime_scores(home, away, loc, scale, ot = list(mean = 20, sd = 6)))
  tiny <- setdiff(seq_len(n / 2), early)
  expect_true(all(out$home[tiny, 1] > out$away[tiny, 1]))
  expect_true(all(out$home[seq_len(n / 2), 2] > out$away[seq_len(n / 2), 2]))
  huge <- (n / 2 + 1L):n
  expect_lt(mean(out$home[huge, ] > out$away[huge, ]), 0.6)
})

test_that("the leave-one-out predictive undoes one observation's update", {
  prior_m <- c(-3, 0, 4.5)
  prior_v <- c(2, 0.5, 9)
  s2 <- 25
  y <- c(7, -2, 1)
  post_v <- 1 / (1 / prior_v + 1 / s2)
  post_m <- post_v * (prior_m / prior_v + y / s2)
  loo <- v2_loo_gaussian(post_m, post_v, y, s2)
  expect_equal(loo$mean, prior_m)
  expect_equal(loo$scale, sqrt(prior_v + s2))
})

test_that("the level-score shift is zero for handball, per division for men and single for women", {
  n <- 3000
  set.seed(21)
  div <- rep(1:2, length.out = n)
  loc <- stats::rnorm(n, 0, 5)
  y <- round(stats::rnorm(n, loc, 10))
  extra <- div == 1L & y != 0 & stats::runif(n) < 0.06
  y[extra] <- 0
  fake_fit <- list(fitted = data.frame(mean = loc, sd = rep(1e-3, n)), sigma2 = 100)
  spec <- function(tie_shift) {
    list(
      opts = list(tie_shift = tie_shift), observed = rep(TRUE, n), y = list(margin = y),
      v = list(N_pred = 3L, div = div, div_pred = c(1L, 2L, 3L), divisions = c("BD", "1D", "2D"))
    )
  }
  by_div <- hbbb_v2_zero_shift(fake_fit, spec("division"))
  tab <- by_div$table
  expect_gt(tab$shift[tab$group == "BD"], 0.4)
  expect_lt(abs(tab$shift[tab$group == "1D"]), 0.15)
  # A fixture in a division with no estimate gets the single shift.
  expect_equal(by_div$per_fixture, c(tab$shift[tab$group == "BD"], tab$shift[tab$group == "1D"], tab$shift[tab$group == "all"]))
  single <- hbbb_v2_zero_shift(fake_fit, spec("single"))
  expect_equal(single$per_fixture, rep(tab$shift[tab$group == "all"], 3))
  expect_identical(hbbb_v2_zero_shift(fake_fit, spec("none"))$per_fixture, c(0, 0, 0))
})

# ---- Gate ------------------------------------------------------------------------

gate_fits <- function(sport = "handball") {
  scale <- if (sport == "basketball") 2.5 else 1
  hyper <- function(obs) {
    data.frame(
      parameter = c("sd: the Gaussian observations", "sd: u_s", "rho: u_s", "sd: b", "sd: ha"),
      mean = c(obs, 2 * scale, 0.7, 3 * scale, 0.4 * scale)
    )
  }
  list(
    margin = list(
      mode_status = 0L, hyper_sd = hyper(5.5 * scale), sigma2 = (5.5 * scale)^2,
      fixed = data.frame(mean = 0.7 * scale, row.names = "hfa")
    ),
    total = list(
      mode_status = 0L, hyper_sd = hyper(6.5 * scale)[1:4, ], sigma2 = (6.5 * scale)^2,
      pred = data.frame(mean = if (sport == "basketball") c(160, 170) else c(54, 58))
    )
  )
}
gate_spec <- function(regulation = FALSE, coverage = 1) {
  list(opts = list(regulation = regulation, within = "none"), coverage = coverage, last_season = 2027L)
}

test_that("the INLA gate passes a sound fit and names each kind of failure", {
  ok <- hbbb_v2_gate(gate_fits(), gate_spec(), "handball")
  expect_true(ok$passed)
  expect_identical(c(ok$evaluated, ok$skipped), c(3L, 1L))
  expect_false(ok$jump_checked)

  breach <- function(change, sport = "handball", spec = gate_spec(), ...) {
    f <- change(gate_fits(sport))
    g <- hbbb_v2_gate(f, spec, sport, ...)
    expect_false(g$passed)
    g$breaches
  }
  expect_match(breach(function(f) {
    f$total$mode_status <- 2L
    f
  }), "total model: hyperparameter mode not found", all = FALSE)
  expect_match(breach(function(f) {
    f$margin$mode_status <- NA_integer_
    f
  }), "margin model: hyperparameter mode not found", all = FALSE)
  # The team effects switched off (the degenerate mode R-INLA can settle in):
  # club and season spread both near zero.
  expect_match(breach(function(f) {
    f$margin$hyper_sd$mean[c(2, 4)] <- 1e-4
    f
  }), "team spread.*margin sd: team", all = FALSE)
  expect_match(breach(function(f) {
    f$total$hyper_sd$mean[2] <- 200
    f
  }), "above 5 times its prior bound.*total sd: u_s", all = FALSE)
  # One weakly identified component near zero is not a failure: the club /
  # season split is not unique (report, "Engine risks"), and a men's handball
  # total fit has its club sd at 0.57 with a 90 % interval down to 0.003.
  lone <- gate_fits()
  lone$total$hyper_sd$mean[4] <- 1e-3
  expect_true(hbbb_v2_gate(lone, gate_spec(), "handball")$passed)
  expect_match(breach(function(f) {
    f$margin$fixed["hfa", "mean"] <- 9
    f
  }), "home advantage 9.00", all = FALSE)
  expect_match(breach(function(f) {
    f$total$sigma2 <- 40^2
    f
  }), "observation sds", all = FALSE)
  expect_match(breach(function(f) {
    f$total$pred$mean <- c(150, 160)
    f
  }), "mean predicted total", all = FALSE)

  prev <- hbbb_v2_hyper_sd(gate_fits())
  same <- hbbb_v2_gate(gate_fits(), gate_spec(), "handball", previous_sd = prev)
  expect_true(same$passed)
  expect_identical(c(same$evaluated, same$skipped), c(4L, 0L))
  expect_true(same$jump_checked)
  expect_true(all(c("margin sd: team", "total sd: team") %in% names(prev)))
  moved_b <- prev
  moved_b["total sd: b"] <- moved_b["total sd: b"] * 4
  expect_true(hbbb_v2_gate(gate_fits(), gate_spec(), "handball", previous_sd = moved_b)$passed)
  prev["total sd: the Gaussian observations"] <- prev["total sd: the Gaussian observations"] * 4
  expect_match(breach(identity, previous_sd = prev), "factor 3 since the previous fit: total sd: the Gaussian observations", all = FALSE)
  prev <- hbbb_v2_hyper_sd(gate_fits())
  prev["margin sd: team"] <- prev["margin sd: team"] / 4
  expect_match(breach(identity, previous_sd = prev), "factor 3 since the previous fit: margin sd: team", all = FALSE)

  bb <- hbbb_v2_gate(gate_fits("basketball"), gate_spec(TRUE), "basketball")
  expect_true(bb$passed)
  expect_identical(c(bb$evaluated, bb$skipped), c(4L, 1L))
  expect_match(
    breach(identity, sport = "basketball", spec = gate_spec(TRUE, coverage = 0.4)),
    "regulation-time scores for 40 % of season 2027",
    all = FALSE
  )
})

# ---- fit_league and the extract --------------------------------------------------

test_that("fit_league sends a handball or basketball league on the INLA engine to the v2 fit and records it", {
  for (sport in c("handball", "basketball")) {
    root <- fixture_hbbb_root()
    league <- load_leagues()[[paste0(sport, "_iceland")]]
    league$engine <- "inla"
    called <- character()
    local_mocked_bindings(
      fit_model_v2_inla = function(...) stop("football engine called for ", sport),
      fit_model_v2_hbbb = function(prep, league, sex, end_date, root, from_season, n_draws, seed, previous_sd, accept_jump) {
        called <<- c(called, league$sport)
        f <- hbbb_v2_spec(prep, league, sex, end_date = end_date, root = root)
        synthetic_hbbb_fit(f, prep, league$sport)
      }
    )
    beliefs <- suppressMessages(fit_league(
      league = league, sex = "male", fit_date = FIXTURE_END_DATE, end_date = FIXTURE_END_DATE,
      root = root, chains = 1L, iter_sampling = 40L
    ))
    expect_identical(called, sport)
    expect_gt(nrow(beliefs), 0L)
    diag <- as.data.frame(read_table("fit_diagnostics", root = root, filter = list(sport = sport, country = "iceland", sex = "male")))
    expect_identical(nrow(diag), 1L)
    prep <- suppressMessages(prepare_data(league, "male", end_date = FIXTURE_END_DATE, root = root))
    expect_identical(diag$n_obs, as.integer(prep$stan_data$N))
    expect_identical(diag$engine, "inla")
    expect_identical(diag$model_version, HBBB_V2_VERSION)
    expect_true(diag$passed)
    partition <- file.path(
      root, "beliefs", "extracts", paste0("sport=", sport), "country=iceland", "sex=male",
      paste0("fit_date=", format(FIXTURE_END_DATE))
    )
    expect_true(file.exists(file.path(partition, "fit_meta.parquet")))
    meta <- arrow::read_parquet(file.path(partition, "fit_meta.parquet"))
    expect_identical(meta$engine, "inla")
    expect_true(is.na(meta$stan_model))
  }
})

test_that("the extract writes the Stan path's parquets, in the same columns, from a v2 fit", {
  for (sport in c("handball", "basketball")) {
    root <- fixture_hbbb_root()
    f <- hbbb_fixture_spec(sport, root = root)
    extract_fn <- if (sport == "handball") extract_handball_iceland else extract_basketball_iceland
    run <- function(fit) {
      out <- file.path(withr::local_tempdir(.local_envir = parent.frame(2)), "extracts")
      suppressMessages(extract_fn(
        fit, f$league,
        sex = "male", fit_date = FIXTURE_FIT_DATE, end_date = FIXTURE_END_DATE,
        root = root, extracts_root = out, prep = f$prep
      ))
      file.path(out, paste0("sport=", sport), "country=iceland", "sex=male", paste0("fit_date=", format(FIXTURE_FIT_DATE)))
    }
    v2_dir <- run(synthetic_hbbb_fit(f$spec, f$prep, sport, n_draws = 50L))
    stan_dir <- run(stub_fit(stub_2dt_draws(
      teams = f$prep$teams$team, n_pred = nrow(f$prep$pred_d), n_draws = 50L,
      n_rounds = f$prep$stan_data$N_rounds, n_seasons = f$prep$stan_data$N_seasons,
      level = if (sport == "basketball") 85 else 26
    )))
    files <- list.files(stan_dir)
    expect_setequal(list.files(v2_dir), files)
    for (file in setdiff(files, "fit_meta.parquet")) {
      a <- arrow::read_parquet(file.path(v2_dir, file))
      b <- arrow::read_parquet(file.path(stan_dir, file))
      expect_identical(names(a), names(b), info = paste(sport, file))
      expect_gt(nrow(a), 0L)
    }
    pm <- arrow::read_parquet(file.path(v2_dir, "predicted_matches.parquet"))
    expect_equal(pm$p_home_win + pm$p_draw + pm$p_away_win, rep(1, nrow(pm)), tolerance = 1e-9)
    if (sport == "basketball") expect_true(all(pm$p_draw == 0))
  }
})

test_that("a basketball cell without regulation-time scores is refused before R-INLA is asked", {
  root <- fixture_facts_root()
  league <- load_leagues()[["basketball_iceland"]]
  prep <- suppressMessages(prepare_data(league, "male", end_date = FIXTURE_END_DATE, root = root))
  local_mocked_bindings(fit_v2_inla = function(...) stop("R-INLA was asked"))
  expect_error(
    fit_model_v2_hbbb(prep, league, "male", end_date = FIXTURE_END_DATE, root = root),
    "no regulation-time scores"
  )
})

# ---- R-INLA -------------------------------------------------------------------------

# A simulated league in a data root: 8 teams, three seasons of a double round
# robin, one division; the third season's last two weeks are left to predict.
simulated_hbbb_root <- function(sport, env = parent.frame(), seed = 4) {
  set.seed(seed)
  big <- sport == "basketball"
  teams <- paste0("T", 1:8)
  strength <- stats::rnorm(8, 0, if (big) 5 else 2)
  pace <- stats::rnorm(8, 0, if (big) 4 else 1.5)
  rows <- list()
  for (s in 1:3) {
    season_strength <- strength + stats::rnorm(8, 0, if (big) 3 else 1.5)
    pairs <- expand.grid(h = 1:8, a = 1:8)
    pairs <- pairs[pairs$h != pairs$a, ]
    pairs <- pairs[sample(nrow(pairs)), ]
    dates <- as.Date(sprintf("%d-09-01", 2096 + s)) + 2L * (seq_len(nrow(pairs)) - 1L)
    margin <- round((if (big) 3 else 0.7) + season_strength[pairs$h] - season_strength[pairs$a] +
      stats::rnorm(nrow(pairs), 0, if (big) 12 else 5))
    total <- round((if (big) 165 else 55) + pace[pairs$h] + pace[pairs$a] + stats::rnorm(nrow(pairs), 0, if (big) 16 else 6))
    total <- margin + 2 * round((total - margin) / 2)
    rows[[s]] <- data.frame(
      sport = sport, country = "iceland", sex = "male", season = 2097L + s,
      match_date = dates, home_team = teams[pairs$h], away_team = teams[pairs$a],
      home_score = as.integer((total + margin) / 2), away_score = as.integer((total - margin) / 2),
      division = if (big) "BD" else "OD", round = NA_integer_
    )
  }
  res <- do.call(rbind, rows)
  if (big) {
    level <- res$home_score == res$away_score
    res$home_score_reg <- res$home_score
    res$away_score_reg <- res$away_score
    res$home_score[level] <- res$home_score[level] + 7L
    res$away_score[level] <- res$away_score[level] + 4L
  }
  cut <- max(res$match_date) - 14
  root <- withr::local_tempdir(.local_envir = env)
  write_table(tibble::as_tibble(res[res$match_date <= cut, ]), "results", root = root)
  sched <- res[res$match_date > cut, c("sport", "country", "sex", "season", "match_date", "home_team", "away_team", "division", "round")]
  sched$kickoff_time <- NA_character_
  write_table(tibble::as_tibble(sched), "schedules", root = root)
  list(root = root, end_date = cut)
}

test_that("an R-INLA fit of a simulated league passes the gate and forecasts the v2 predictive", {
  skip_if_no_inla()
  for (sport in c("handball", "basketball")) {
    sim <- simulated_hbbb_root(sport)
    league <- load_leagues()[[paste0(sport, "_iceland")]]
    prep <- suppressMessages(prepare_data(league, "male", end_date = sim$end_date, root = sim$root))
    fit <- suppressMessages(fit_model_v2_hbbb(prep, league, "male", end_date = sim$end_date, root = sim$root, n_draws = 400L, seed = 3L))
    expect_true(fit$gate$passed, info = sport)
    expect_identical(fit$engine, "inla")
    expect_identical(fit$n_obs, as.integer(prep$stan_data$N))
    expect_identical(unname(fit$mode_status), c(0L, 0L))
    h <- posterior::as_draws_matrix(fit$draws("goals1_pred"))
    a <- posterior::as_draws_matrix(fit$draws("goals2_pred"))
    expect_identical(ncol(h), nrow(prep$pred_d))
    total <- mean(h + a)
    expect_gt(total, if (sport == "basketball") 140 else 45)
    expect_lt(total, if (sport == "basketball") 190 else 65)
    if (sport == "basketball") expect_false(any(h == a))
  }
})

test_that("the R-INLA fit of a cell is repeatable", {
  skip_if_no_inla()
  sim <- simulated_hbbb_root("handball")
  league <- load_leagues()[["handball_iceland"]]
  prep <- suppressMessages(prepare_data(league, "male", end_date = sim$end_date, root = sim$root))
  one <- function() {
    f <- suppressMessages(fit_model_v2_hbbb(prep, league, "male", end_date = sim$end_date, root = sim$root, n_draws = 200L, seed = 3L))
    f$draws(c("cur_strength_away", "goals1_pred"))
  }
  expect_identical(one(), one())
})

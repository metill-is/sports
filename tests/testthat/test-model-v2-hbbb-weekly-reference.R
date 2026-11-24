# The weekly Stan reference fit for handball and basketball v2
# (R/model-v2-hbbb-reference.R): once a week each cell is fitted by R-INLA as
# production fits it and by NUTS with the Stan programs of the same model,
# and their predictive probabilities are compared. A breach is a health
# warning.
#
# The INLA and NUTS fitters are arguments, so the orchestration is driven
# here with stand-ins and needs neither engine.

hbbb_stan_dir <- function() do.call(testthat::test_path, as.list(c("..", "..", HBBB_REFERENCE_STAN_DIR)))

# Draws of a fixture's predictive in the fitters' shared layout: the same
# location in every draw, shifted by `shift` on the margin and `shift_t` on
# the total.
.hbbb_fake_fit <- function(shift = 0, shift_t = 0, divergent = c(0L, 0L), rhat = c(1.002, 1.004),
                           mode_status = c(0L, 0L), gate_passed = TRUE, n_draws = 40L) {
  function(spec) {
    n_pred <- spec$v$N_pred
    big <- identical(spec$opts$within, "ar1")
    centre_m <- seq(-3, 3, length.out = n_pred) + shift
    centre_t <- rep(if (big) 165 else 55, n_pred) + shift_t
    list(
      loc_m = matrix(centre_m, n_draws, n_pred, byrow = TRUE),
      loc_t = matrix(centre_t, n_draws, n_pred, byrow = TRUE),
      sigma_m = rep(if (big) 12 else 5, n_draws), sigma_t = rep(if (big) 16 else 6, n_draws),
      mode_status = mode_status, gate_passed = gate_passed,
      diagnostics = list(
        margin = list(n_divergent = divergent[1], n_draws = 4000L, max_rhat = rhat[1]),
        total = list(n_divergent = divergent[2], n_draws = 4000L, max_rhat = rhat[2])
      )
    )
  }
}

.hbbb_reference <- function(sport, root, end_date = FIXTURE_END_DATE, sex = "male", ...) {
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  suppressMessages(hbbb_v2_weekly_reference(league, sex, end_date = end_date, root = root, ...))
}

# The names a Stan program's data block declares, its #include fragments read
# in place.
.stan_data_names <- function(file) {
  lines <- readLines(file, warn = FALSE)
  from <- grep("^data\\s*\\{", lines)
  to <- from + which(grepl("^\\}", lines[-seq_len(from)]))[1]
  body <- lines[(from + 1L):(to - 1L)]
  body <- unlist(lapply(body, function(l) {
    inc <- regmatches(l, regexec("^\\s*#include\\s+(\\S+)", l))[[1]]
    if (length(inc)) readLines(file.path(dirname(file), inc[2]), warn = FALSE) else l
  }))
  body <- trimws(sub("//.*$", "", body))
  decl <- regmatches(body, regexec("([A-Za-z_][A-Za-z0-9_]*)\\s*;$", body))
  unlist(lapply(decl, function(m) if (length(m)) m[2]))
}

# ---- The Stan reference programs and their data -----------------------------------

test_that("the Stan data is the spec R-INLA fits, minus the rows without a response, in the programs' names", {
  root <- fixture_hbbb_root(drop_reg = 3L)
  for (sport in c("handball", "basketball")) {
    s <- hbbb_fixture_spec(sport, root = root)$spec
    priors <- v2_priors(sport)
    v <- s$v
    obs <- s$observed
    if (sport == "basketball") expect_lt(sum(obs), v$N) else expect_true(all(obs))
    for (resp in c("margin", "total")) {
      d <- hbbb_v2_stan_data(s, resp, priors)
      info <- paste(sport, resp)
      expect_setequal(names(d), .stan_data_names(file.path(hbbb_stan_dir(), sprintf("v2_%s.stan", resp))))
      expect_identical(d$N, sum(obs), info = info)
      expect_identical(as.numeric(d$y), s$y[[resp]][obs], info = info)
      for (nm in c("home", "away", "season", "div", "wk", "ts_home", "ts_away")) {
        expect_identical(as.integer(d[[nm]]), v[[nm]][obs], info = paste(info, nm))
        expect_identical(as.integer(d[[paste0(nm, "_pred")]]), v[[paste0(nm, "_pred")]], info = paste(info, nm))
      }
      # The whole grid of states, so a row left out of the likelihood keeps
      # its states, with their prior only, as in R-INLA.
      expect_identical(d[c("K", "NS", "TS", "W", "ND", "N_pred")], v[c("K", "NS", "TS", "W", "ND", "N_pred")])
      # pc.prec(u, 0.01) on a precision is exponential(-log(0.01) / u) on the sd.
      rate <- function(u) -log(0.01) / u
      expect_equal(d$rate_sigma, rate(if (resp == "margin") priors$u_sigma_m else priors$u_sigma_t), info = info)
      expect_equal(c(d$rate_b, d$rate_u), rate(c(priors$u_b, priors$u_u)), info = info)
      expect_equal(c(d$rho_mu, d$rho_sd), c(priors$rho_mu, priors$rho_sd), info = info)
      expect_identical(d$rho_week, s$opts$rho_week)
      if (sport == "basketball") {
        # The within-season AR(1): its sd's scale is u_w_ar1, as fit_v2_inla() uses.
        expect_identical(d$within_mode, 2L)
        expect_equal(d$rate_w, rate(priors$u_w_ar1))
      } else {
        expect_identical(d$within_mode, 0L)
      }
      if (resp == "margin") {
        expect_equal(c(d$rate_ha, d$hfa_sd), c(rate(priors$u_ha), priors$hfa_sd))
      } else {
        expect_equal(c(d$rate_season, d$level_mean, d$level_sd), c(rate(priors$u_season), priors$total_mean, priors$total_mean_sd))
      }
    }
  }
})

test_that("the Stan programs put the priors on the quantities fit_v2_inla() puts them on", {
  model_block <- function(resp) {
    l <- readLines(file.path(hbbb_stan_dir(), sprintf("v2_%s.stan", resp)), warn = FALSE)
    from <- grep("^model\\s*\\{", l)
    to <- from + which(grepl("^\\}", l[-seq_len(from)]))[1]
    gsub("\\s+", " ", trimws(l[(from + 1L):(to - 1L)]))
  }
  shared <- c(
    "sigma ~ exponential(rate_sigma);", "sigma_b ~ exponential(rate_b);", "sigma_u ~ exponential(rate_u);",
    "sigma_w ~ exponential(rate_w);", "theta_rho ~ normal(rho_mu, rho_sd);", "y ~ normal(eta, sigma);"
  )
  expect_true(all(c(shared, "hfa ~ normal(0, hfa_sd);", "sigma_ha ~ exponential(rate_ha);") %in% model_block("margin")))
  expect_true(all(c(shared, "alpha ~ normal(level_mean, level_sd);", "sigma_season ~ exponential(rate_season);") %in% model_block("total")))
  # R-INLA's AR(1) is parameterised by its marginal sd and log((1 + rho) / (1 - rho)).
  states <- readLines(file.path(hbbb_stan_dir(), "_states_v2.stan"), warn = FALSE)
  expect_true(any(grepl("rho = tanh(0.5 * theta_rho)", states, fixed = TRUE)))
  expect_true(any(grepl("sqrt(1 - square(rho)) * sigma_u", states, fixed = TRUE)))
})

test_that("the programs' fingerprint covers the fragments they include", {
  dir <- withr::local_tempdir()
  file.copy(list.files(hbbb_stan_dir(), full.names = TRUE), dir)
  before <- hbbb_v2_stan_md5(dir)
  expect_identical(before, hbbb_v2_stan_md5(hbbb_stan_dir()))
  cat("// edited\n", file = file.path(dir, "_states_v2.stan"), append = TRUE)
  expect_false(identical(hbbb_v2_stan_md5(dir), before))
})

test_that("NUTS is held to the Rhat of what the comparison reads, not of the club / season split", {
  # Handball women on 2026-11-24, as the margin fit gave them: one team's club
  # level and season levels slow along their ridge, the locations converged.
  sm <- data.frame(
    variable = c("sigma", "sigma_b", "sigma_u", "theta_rho", "hfa", "sigma_ha", "b[6]", "u[6,5]", "mu_pred[1]", "mu_pred[2]"),
    rhat = c(1.001, 1.022, 1.024, 1.004, 1.002, 1.003, 1.023, 1.026, 1.003, 1.002)
  )
  g <- hbbb_v2_rhat_groups(sm)
  expect_equal(g$pred, 1.003)
  expect_equal(g$states, 1.026)
  expect_equal(g$hyper, 1.024)
  # The observation sd is read by the comparison too.
  sm$rhat[sm$variable == "sigma"] <- 1.03
  expect_equal(hbbb_v2_rhat_groups(sm)$pred, 1.03)
  # No Rhat for the locations is no evidence.
  expect_true(is.na(hbbb_v2_rhat_groups(sm[!grepl("^(mu_pred|sigma$)", sm$variable), ])$pred))
})

# ---- Fixtures when none lies inside the horizon ------------------------------------

.break_train <- function() {
  data.frame(
    season = c(rep(2027L, 6), 2026L),
    division = c("OD", "OD", "OD", "PO", "G66", "G66", "OD"),
    home_team = c("A", "B", "C", "A", "D", "E", "F"),
    away_team = c("B", "C", "A", "B", "E", "D", "A")
  )
}

test_that("a break inside the season pairs the season's teams within their divisions on the next fixture's date", {
  sched <- data.frame(
    match_date = as.Date(c("2026-12-10", "2026-12-14", "2026-11-20")),
    season = 2027L, home_team = "A", away_team = "B"
  )
  fx <- hbbb_v2_break_fixtures(.break_train(), sched, as.Date("2026-11-24"), division_map = c(PO = "OD"))
  expect_identical(attr(fx, "kind"), "mid-season pairings")
  expect_true(all(fx$match_date == as.Date("2026-12-10")))
  expect_true(all(fx$season == 2027L))
  # Both ways, within each division; the play-off rows count as OD, and F,
  # who played only last season, is not paired.
  expect_setequal(paste(fx$home_team, fx$away_team), c("A B", "B A", "A C", "C A", "B C", "C B", "D E", "E D"))
  expect_identical(fx$division[fx$home_team == "D"], "G66")
  expect_true(all(fx$division[fx$home_team %in% c("A", "B", "C")] == "OD"))
})

test_that("the off-season pairs last season's teams in the next season", {
  fx <- hbbb_v2_break_fixtures(.break_train(), NULL, as.Date("2027-06-20"), division_map = c(PO = "OD"))
  expect_identical(attr(fx, "kind"), "off-season pairings")
  expect_true(all(fx$match_date == as.Date("2027-06-27")))
  expect_true(all(fx$season == 2028L))
  expect_equal(nrow(fx), 8L)
  # A published schedule for the next season dates them at its first fixture.
  sched <- data.frame(match_date = as.Date("2027-09-20"), season = 2028L, home_team = "A", away_team = "B")
  fx <- hbbb_v2_break_fixtures(.break_train(), sched, as.Date("2027-06-20"))
  expect_identical(attr(fx, "kind"), "off-season pairings")
  expect_true(all(fx$match_date == as.Date("2027-09-20") & fx$season == 2028L))
})

test_that("a handball spec with no fixture builds, and pairings stand in for fixtures with their own season", {
  # Every handball men's fixture two months out: none inside the horizon.
  later <- function(s) {
    hb <- s$sport == "handball" & s$sex == "male"
    s$match_date[hb] <- s$match_date[hb] + 60L
    s
  }
  root <- fixture_hbbb_root(schedule_edit = later)
  f <- hbbb_fixture_spec("handball", root = root)
  # The static model's nodes used to be built from a team index of length
  # zero beside a scalar week, which data.frame() refuses.
  expect_identical(f$spec$v$N_pred, 0L)
  expect_identical(length(f$spec$node_id$cur), f$spec$v$K)

  pairs <- hbbb_v2_break_fixtures(f$spec$train, NULL, FIXTURE_END_DATE, division_map = c(PO = "OD"))
  s <- hbbb_v2_spec(f$prep, f$league, "male", end_date = FIXTURE_END_DATE, root = root, pred = pairs)
  expect_identical(s$v$N_pred, nrow(pairs))
  # The pairings' season is a season no schedule holds: taken as given.
  expect_identical(unique(s$pred$season), max(f$spec$train$season) + 1L)
  expect_identical(unique(s$v$season_pred), s$v$NS)
})

# ---- The comparison ----------------------------------------------------------------

test_that("the compared probabilities are the rounded Gaussian's", {
  set.seed(2)
  loc_m <- matrix(stats::rnorm(30, 1, 0.5), 10)
  loc_t <- matrix(stats::rnorm(30, 55, 1), 10)
  sm <- abs(stats::rnorm(10, 5, 0.2))
  st <- abs(stats::rnorm(10, 6, 0.2))
  lines_m <- c(0.5, 1.5, -0.5)
  lines_t <- c(54.5, 55.5, 56.5)
  p <- hbbb_v2_reference_probs(loc_m, sm, loc_t, st, lines_m, lines_t)
  expect_equal(unname(rowSums(p[, c("home", "draw", "away")])), rep(1, 3))
  for (j in 1:3) {
    expect_equal(p[[j, "draw"]], mean(stats::pnorm((0.5 - loc_m[, j]) / sm) - stats::pnorm((-0.5 - loc_m[, j]) / sm)))
    expect_equal(p[[j, "home"]], mean(1 - stats::pnorm((0.5 - loc_m[, j]) / sm)))
    expect_equal(p[[j, "spread"]], mean(1 - stats::pnorm((lines_m[j] - loc_m[, j]) / sm)))
    expect_equal(p[[j, "total"]], mean(1 - stats::pnorm((lines_t[j] - loc_t[, j]) / st)))
  }
})

test_that("the weekly reference compares the two engines fixture by fixture, both halves", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    same <- .hbbb_reference(sport, root, fit_inla = .hbbb_fake_fit(), fit_nuts = .hbbb_fake_fit())
    f <- hbbb_fixture_spec(sport, root = root)
    expect_identical(same$row$fixtures, "upcoming")
    expect_identical(same$row$n_fixtures, f$spec$v$N_pred)
    expect_identical(nrow(same$fixtures), f$spec$v$N_pred)
    expect_identical(same$row$n_train, sum(f$spec$observed))
    expect_identical(same$row$model_version, HBBB_V2_VERSION)
    expect_equal(c(same$row$mean_abs_diff, same$row$spread_mean_abs_diff, same$row$total_mean_abs_diff), c(0, 0, 0))
    expect_true(same$row$passed, info = sport)
    # As write_table() stores it: the script adds run_sports.
    expect_no_error(validate_against_schema(fill_optional_columns(same$row, "engine_reference"), "engine_reference"))

    # NUTS half a point higher on the margin: the result and the spread move,
    # the total does not.
    apart <- .hbbb_reference(sport, root, fit_inla = .hbbb_fake_fit(), fit_nuts = .hbbb_fake_fit(shift = 0.5))
    fx <- apart$fixtures
    expect_equal(
      apart$row$mean_abs_diff,
      mean(abs(as.matrix(fx[c("inla_home", "inla_draw", "inla_away")]) - as.matrix(fx[c("nuts_home", "nuts_draw", "nuts_away")])))
    )
    expect_equal(apart$row$spread_mean_abs_diff, mean(abs(fx$inla_spread - fx$nuts_spread)))
    expect_gt(apart$row$mean_abs_diff, HBBB_REFERENCE_LIMITS$in_season[["result"]])
    expect_equal(apart$row$total_mean_abs_diff, 0)
    expect_false(apart$row$passed)
    # The lines are the half points nearest the two engines' mean expectation.
    expect_equal(fx$line_margin, floor((fx$inla_mean_margin + fx$nuts_mean_margin) / 2) + 0.5)

    # A total two points apart fails on its own line.
    total <- .hbbb_reference(sport, root, fit_inla = .hbbb_fake_fit(), fit_nuts = .hbbb_fake_fit(shift_t = 2))
    expect_equal(total$row$mean_abs_diff, 0)
    expect_gt(total$row$total_mean_abs_diff, HBBB_REFERENCE_LIMITS$in_season[["line"]])
    expect_false(total$row$passed)
  }
})

test_that("an unsound NUTS half, or an INLA fit without a mode or past its gate, does not pass", {
  root <- fixture_hbbb_root()
  run <- function(...) .hbbb_reference("handball", root, ...)$row
  ok <- .hbbb_fake_fit()
  # The health layer's bar for a Stan fit, held by each half: divergences
  # under 0.5 % of the transitions, Rhat at most 1.02.
  expect_true(run(fit_inla = ok, fit_nuts = .hbbb_fake_fit(divergent = c(1L, 19L), rhat = c(1.019, 1.01)))$passed)
  expect_false(run(fit_inla = ok, fit_nuts = .hbbb_fake_fit(divergent = c(0L, 21L)))$passed)
  bad_rhat <- run(fit_inla = ok, fit_nuts = .hbbb_fake_fit(rhat = c(1.002, 1.05)))
  expect_false(bad_rhat$passed)
  expect_equal(bad_rhat$nuts_max_rhat, 1.05)
  # A half with no Rhat is no evidence its chains agree.
  no_rhat <- run(fit_inla = ok, fit_nuts = .hbbb_fake_fit(rhat = c(1.002, NA_real_)))
  expect_false(no_rhat$passed)
  expect_true(is.na(no_rhat$nuts_max_rhat))
  expect_identical(run(fit_inla = ok, fit_nuts = .hbbb_fake_fit(divergent = c(2L, 3L)))$nuts_divergences, 5L)
  # Either half's mode search failing is recorded by its status.
  mode <- run(fit_inla = .hbbb_fake_fit(mode_status = c(0L, 2L)), fit_nuts = ok)
  expect_false(mode$passed)
  expect_identical(mode$inla_mode_status, 2L)
  expect_true(is.na(run(fit_inla = .hbbb_fake_fit(mode_status = c(NA_integer_, 0L)), fit_nuts = ok)$inla_mode_status))
  expect_false(run(fit_inla = .hbbb_fake_fit(gate_passed = FALSE), fit_nuts = ok)$passed)
})

test_that("with no fixture inside the horizon the weekly reference compares the season's pairings", {
  later <- function(s) {
    bb <- s$sport == "basketball" & s$sex == "male"
    s$match_date[bb] <- s$match_date[bb] + 60L
    s
  }
  root <- fixture_hbbb_root(schedule_edit = later)
  res <- .hbbb_reference("basketball", root, fit_inla = .hbbb_fake_fit(), fit_nuts = .hbbb_fake_fit())
  expect_identical(res$row$fixtures, "mid-season pairings")
  sched <- as.data.frame(read_table("schedules", root = root, filter = list(sport = "basketball", country = "iceland", sex = "male")))
  expect_true(all(res$fixtures$match_date == min(sched$match_date[sched$match_date > FIXTURE_END_DATE])))
  expect_gt(res$row$n_fixtures, 0L)
  expect_true(res$row$passed)
})

test_that("across the season break the looser limits apply, and the row says so", {
  # Half a goal apart on the total: about 0.033 on the total line, past the
  # in-season limit and inside the season-break one.
  apart <- .hbbb_fake_fit(shift_t = 0.5)
  within <- .hbbb_reference("handball", fixture_hbbb_root(), fit_inla = .hbbb_fake_fit(), fit_nuts = apart)$row
  expect_identical(within$fixtures, "upcoming")
  expect_gt(within$total_mean_abs_diff, HBBB_REFERENCE_LIMITS$in_season[["line"]])
  expect_lt(within$total_mean_abs_diff, HBBB_REFERENCE_LIMITS$across_break[["line"]])
  expect_false(within$passed)
  expect_identical(c(within$threshold, within$line_threshold), unname(HBBB_REFERENCE_LIMITS$in_season[c("result", "line")]))

  # The same fixtures in a season the cell has no match of yet: pre-season.
  root <- fixture_hbbb_root(schedule_season = 2101L)
  across <- .hbbb_reference("handball", root, fit_inla = .hbbb_fake_fit(), fit_nuts = apart)$row
  expect_identical(across$fixtures, "upcoming, across the season break")
  expect_true(across$passed)
  expect_identical(c(across$threshold, across$line_threshold), unname(HBBB_REFERENCE_LIMITS$across_break[c("result", "line")]))
})

test_that("a spec crosses the season break when a fixture's season has no training match", {
  root <- fixture_hbbb_root()
  expect_false(hbbb_v2_across_break(hbbb_fixture_spec("handball", root = root)$spec))
  expect_true(hbbb_v2_across_break(hbbb_fixture_spec("handball", root = fixture_hbbb_root(schedule_season = 2101L))$spec))
})

test_that("with nothing scheduled the weekly reference pairs last season's teams in the next season", {
  gone <- function(s) s[!(s$sport == "handball" & s$sex == "male"), ]
  root <- fixture_hbbb_root(schedule_edit = gone)
  res <- .hbbb_reference("handball", root, fit_inla = .hbbb_fake_fit(), fit_nuts = .hbbb_fake_fit())
  expect_identical(res$row$fixtures, "off-season pairings, across the season break")
  expect_true(all(res$fixtures$match_date == FIXTURE_END_DATE + 7L))
  expect_identical(res$row$line_threshold, HBBB_REFERENCE_LIMITS$across_break[["line"]])
})

# ---- The INLA side is fit_league()'s fit of that day --------------------------------

test_that("the INLA side fits as fit_league() would on the day: its seed, 4,000 draws, full integration, its gate", {
  root <- fixture_hbbb_root()
  f <- hbbb_fixture_spec("handball", root = root)
  captured <- NULL
  testthat::local_mocked_bindings(hbbb_v2_fit_halves = function(spec, sport, n_draws, seed, int_strategy) {
    captured <<- list(sport = sport, n_draws = n_draws, seed = seed, int_strategy = int_strategy)
    stop("captured")
  })
  expect_error(hbbb_v2_reference_inla(f$league, "male", FIXTURE_END_DATE, root = root)(f$spec), "captured")
  expect_identical(captured$sport, "handball")
  expect_identical(captured$n_draws, 4000L)
  expect_identical(captured$int_strategy, "auto")
  expect_identical(captured$seed, .fit_seed("handball", "iceland", "male", format(FIXTURE_END_DATE)))
})

test_that("the INLA side pairs the halves as production does and records the gate", {
  root <- fixture_hbbb_root()
  for (sport in c("handball", "basketball")) {
    f <- hbbb_fixture_spec(sport, root = root)
    testthat::local_mocked_bindings(fit_v2_inla = fake_v2_inla_fit)
    side <- hbbb_v2_reference_inla(f$league, "male", FIXTURE_END_DATE, root = root, n_draws = 40L)(f$spec)
    seed <- .fit_seed(sport, "iceland", "male", format(FIXTURE_END_DATE))
    fits <- hbbb_v2_fit_halves(f$spec, sport, n_draws = 40L, seed = seed)
    comp <- hbbb_v2_pair(fits, f$spec, seed = seed)
    expect_identical(side$loc_m, comp$loc_m, info = sport)
    expect_identical(side$loc_t, comp$loc_t, info = sport)
    expect_identical(side$sigma_t, comp$sigma_t, info = sport)
    expect_identical(dim(side$loc_m), c(40L, f$spec$v$N_pred))
    expect_identical(unname(side$mode_status), c(0L, 0L))
    expect_identical(side$gate_passed, hbbb_v2_gate(fits, f$spec, sport)$passed)
  }
})

# ---- Health -------------------------------------------------------------------------

.hbbb_ref_row <- function(sport = "handball", sex = "male", run_date = as.Date("2026-11-30"), mad = 0.003,
                          spread = 0.004, total = 0.003, passed = TRUE, run_sports = "football,handball,basketball") {
  tibble::tibble(
    sport = sport, country = "iceland", sex = sex, run_date = run_date,
    run_at = as.POSIXct(paste(run_date, "10:00:00"), tz = "UTC"),
    fixtures = "upcoming", n_fixtures = 20L, n_train = 1300L,
    mean_abs_diff = mad, max_abs_diff = mad * 3, threshold = HBBB_REFERENCE_LIMITS$in_season[["result"]],
    nuts_divergences = 0L, nuts_max_rhat = 1.004, inla_mode_status = 0L,
    inla_gate_passed = TRUE, passed = passed, inla_version = "25.6.7", cmdstan_version = "2.38.0",
    stan_md5 = "abc", model_version = HBBB_V2_VERSION, code_commit = "sha",
    spread_mean_abs_diff = spread, spread_max_abs_diff = spread * 3,
    total_mean_abs_diff = total, total_max_abs_diff = total * 3,
    line_threshold = HBBB_REFERENCE_LIMITS$in_season[["line"]], run_sports = run_sports
  )
}

test_that("a sport no run has covered is paused; a covered cell with no row warns", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-12-01 12:00:00", tz = "UTC")
  th <- health_thresholds()
  status <- function() {
    h <- check_engine_reference(root, now, th)
    stats::setNames(h$status, h$scope)
  }
  # Football's rows from before this check covered football alone (no
  # run_sports): handball and basketball have not been run, not stopped.
  football <- tibble::tibble(
    sport = "football", country = "iceland", sex = c("male", "female"), run_date = as.Date("2026-11-30"),
    run_at = as.POSIXct("2026-11-30 10:00:00", tz = "UTC"), fixtures = "upcoming", n_fixtures = 10L,
    n_train = 2900L, mean_abs_diff = 0.003, max_abs_diff = 0.01, threshold = 0.01, nuts_divergences = 0L,
    nuts_max_rhat = 1.003, inla_mode_status = 0L, inla_gate_passed = TRUE, passed = TRUE,
    inla_version = "25.6.7", cmdstan_version = "2.38.0", stan_md5 = "abc", model_version = "2.0", code_commit = "sha"
  )
  write_table(football, "engine_reference", root = root)
  s <- status()
  expect_length(s, 6L)
  expect_true(all(s[c("football/iceland/male", "football/iceland/female")] == "OK"))
  expect_true(all(s[grepl("^(handball|basketball)/", names(s))] == "PAUSED"))

  # A run that covered every sport but wrote only handball men's row: the
  # three silent cells warn.
  write_table(.hbbb_ref_row("handball", "male"), "engine_reference", root = root)
  s <- status()
  expect_identical(unname(s["handball/iceland/male"]), "OK")
  expect_true(all(s[c("handball/iceland/female", "basketball/iceland/male", "basketball/iceland/female")] == "WARN"))
})

test_that("a handball or basketball breach on either line, or a stale reference, warns", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-12-01 12:00:00", tz = "UTC")
  th <- health_thresholds()
  status_of <- function(scope, at = now) {
    h <- check_engine_reference(root, at, th)
    h$status[h$scope == scope]
  }
  write_table(.hbbb_ref_row("basketball", "female"), "engine_reference", root = root)
  expect_identical(status_of("basketball/iceland/female"), "OK")
  h <- check_engine_reference(root, now, th)
  expect_match(h$value[h$scope == "basketball/iceland/female"], "spread line", fixed = TRUE)

  # The newest row decides.
  write_table(.hbbb_ref_row("basketball", "female", run_date = as.Date("2026-12-01"), total = 0.03), "engine_reference", root = root)
  expect_identical(status_of("basketball/iceland/female"), "WARN")
  write_table(.hbbb_ref_row("basketball", "female", run_date = as.Date("2026-12-01"), spread = 0.03), "engine_reference", root = root)
  expect_identical(status_of("basketball/iceland/female"), "WARN")
  write_table(.hbbb_ref_row("basketball", "female", run_date = as.Date("2026-12-01"), passed = FALSE), "engine_reference", root = root)
  expect_identical(status_of("basketball/iceland/female"), "WARN")
  write_table(.hbbb_ref_row("basketball", "female", run_date = as.Date("2026-12-01")), "engine_reference", root = root)
  expect_identical(status_of("basketball/iceland/female"), "OK")

  later <- now + as.difftime(th$engine_reference_max_age_days + 1, units = "days")
  expect_identical(status_of("basketball/iceland/female", later), "WARN")
})

test_that("a handball or basketball row is judged by the limits it carries", {
  root <- withr::local_tempdir()
  now <- as.POSIXct("2026-12-01 12:00:00", tz = "UTC")
  status_of <- function() {
    h <- check_engine_reference(root, now, health_thresholds())
    h$status[h$scope == "handball/iceland/female"]
  }
  brk <- HBBB_REFERENCE_LIMITS$across_break
  # Pre-season: a total line 0.04 apart passes the season-break limit.
  row <- .hbbb_ref_row("handball", "female", total = 0.04)
  row$threshold <- brk[["result"]]
  row$line_threshold <- brk[["line"]]
  row$fixtures <- "off-season pairings, across the season break"
  write_table(row, "engine_reference", root = root)
  expect_identical(status_of(), "OK")
  # A row that carries no limits is held to the in-season ones.
  row$run_date <- as.Date("2026-12-01")
  row$line_threshold <- NA_real_
  write_table(row, "engine_reference", root = root)
  expect_identical(status_of(), "WARN")
})

# ---- The script ---------------------------------------------------------------------

test_that("the weekly reference script fits handball and basketball as production does and records what it covered", {
  src <- readLines(testthat::test_path("..", "..", "scripts", "engine_reference.R"), warn = FALSE)
  body <- paste(src[!grepl("^\\s*#", src)], collapse = "\n")
  expect_match(body, 'c("football", "handball", "basketball")', fixed = TRUE)
  expect_match(body, "hbbb_v2_weekly_reference(", fixed = TRUE)
  expect_match(body, "hbbb_v2_reference_inla(league, sex, end_date", fixed = TRUE)
  expect_match(body, "hbbb_v2_reference_nuts(sport, hbbb_stan_dir", fixed = TRUE)
  expect_match(body, "HBBB_REFERENCE_STAN_DIR", fixed = TRUE)
  expect_match(body, "res$row$run_sports <- run_sports", fixed = TRUE)
})

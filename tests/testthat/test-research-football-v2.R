# Football v2 research prototype (research/football-v2/): the Stan reference
# engine, scoring and evaluation. Not package code, so its files are sourced
# here; the specification they build on lives in R/model-v2-spec.R.

fv2_dir <- testthat::test_path("..", "..", "research", "football-v2")
skip_if_not(dir.exists(fv2_dir), "research/football-v2 not present")
skip_if_not_installed("Matrix")
fv2 <- new.env()
for (f in c("engines.R", "score.R", "evaluate.R", "data.R")) {
  sys.source(file.path(fv2_dir, f), envir = fv2)
}

test_that("scores match a direct Poisson calculation", {
  lam_h <- matrix(c(1.2, 1.8, 0.7, 0.9), 2, byrow = TRUE)
  lam_a <- matrix(c(0.9, 1.1, 2.0, 2.4), 2, byrow = TRUE)
  sc <- fv2$fv2_score(lam_h, lam_a, c(2L, 0L), c(1L, 0L))
  expect_equal(sc$p_home + sc$p_draw + sc$p_away, c(1, 1))
  grid <- function(i) {
    g <- 0:fv2$FV2_MAX_GOALS
    m <- Reduce(`+`, lapply(1:2, function(d) outer(dpois(g, lam_h[i, d]), dpois(g, lam_a[i, d])))) / 2
    m / sum(m)
  }
  g1 <- grid(1)
  expect_equal(sc$ls_score[1], log(g1[3, 2]))
  expect_equal(sc$p_home[1], sum(g1[lower.tri(g1)]))
  expect_equal(sc$ls_1x2[1], log(sc$p_home[1]))
  g2 <- grid(2)
  expect_equal(sc$ls_1x2[2], log(sum(diag(g2))))
  expect_equal(sc$xg_home, rowMeans(lam_h))
  expect_true(all(sc$rps >= 0 & sc$rps <= 1))
  expect_equal(
    fv2$fv2_log_score_1x2(0.5, 0.3, 0.2, c(1, 1, 0), c(0, 1, 2)),
    log(c(0.5, 0.3, 0.2))
  )
})

test_that("a home win is certain when the home rate dwarfs the away rate", {
  sc <- fv2$fv2_score(matrix(8, 1, 3), matrix(0.01, 1, 3), 5L, 0L)
  expect_gt(sc$p_home, 0.99)
  expect_lt(sc$rps, 1e-3)
})

test_that("de-vigged odds sum to one under both methods", {
  odds <- rbind(c(1.25, 5.92, 7.23), c(2.4, 3.3, 2.9))
  prop <- fv2$fv2_devig(odds)
  pow <- fv2$fv2_devig(odds, "power")
  expect_equal(rowSums(prop), c(1, 1))
  expect_equal(rowSums(pow), c(1, 1))
  expect_equal(prop[1, ] / prop[1, 1], (1 / odds[1, ]) / (1 / odds[1, 1]), ignore_attr = TRUE)
  # The power method takes more of the margin from the long shots.
  expect_lt(pow[1, "away"], prop[1, "away"])
  expect_gt(pow[1, "home"], prop[1, "home"])
  fair <- 1 / c(0.5, 0.3, 0.2)
  expect_equal(as.numeric(fv2$fv2_devig(rbind(fair), "power")), c(0.5, 0.3, 0.2))
  expect_equal(as.numeric(fv2$fv2_devig(rbind(fair))), c(0.5, 0.3, 0.2))
})

test_that("season phase follows the round, play-offs are late, cups borrow", {
  m <- data.frame(
    match_date = as.Date("2025-04-07") + 7 * c(0:17, 18, 1, 16),
    season = 2025L,
    round = c(1:18, 1, 1, 4),
    division = c(rep("BD", 18), "BD_UPPER_PO", "CUP", "CUP"),
    league_division = c(rep("BD", 18), "BD", NA, NA)
  )
  ph <- as.character(fv2$fv2_phase(m))
  expect_identical(ph[1:18], rep(c("early", "middle", "late"), c(5, 7, 6)))
  expect_identical(ph[19], "late")
  expect_identical(ph[20:21], c("early", "late"))
})

test_that("the paired interval is the usual one without clusters", {
  d <- c(0.03, -0.01, 0.02, 0.05, -0.04, 0.01)
  out <- fv2$fv2_paired(d)
  expect_equal(out$mean, mean(d))
  expect_equal(out$se, stats::sd(d) / sqrt(length(d)))
  expect_equal(out$hi - out$lo, 2 * stats::qnorm(0.975) * out$se)
  expect_identical(out$n, 6L)
})

test_that("clustering widens the interval when matches move together", {
  d <- c(0.04, 0.05, 0.03, -0.04, -0.05, -0.03)
  cl <- rep(1:2, each = 3)
  plain <- fv2$fv2_paired(d)
  clustered <- fv2$fv2_paired(d, cl)
  expect_identical(clustered$n_cluster, 2L)
  sums <- tapply(d - mean(d), cl, sum)
  expect_equal(clustered$se, sqrt(2 / 1 * sum(sums^2)) / 6)
  expect_gt(clustered$se, plain$se)
  expect_identical(fv2$fv2_paired(c(d, NA), c(cl, 2L))$n, 6L)
})

test_that("the Stan programs compile and read the spec", {
  skip_on_ci()
  skip_if(Sys.getenv("FV2_TEST_STAN") != "1", "set FV2_TEST_STAN=1 to compile and sample")
  skip_if_not_installed("cmdstanr")
  sim <- simulate_league()
  tmp <- withr::local_tempdir()
  stan_dir <- file.path(fv2_dir, "stan")
  run <- function(spec) {
    fv2$fv2_fit_stan(
      spec, fv2$fv2_stan_file(spec, stan_dir), tmp, tmp,
      chains = 2L, iter_warmup = 150L, iter_sampling = 100L, refresh = 0L
    )
  }
  ad <- fv2_build(sim, clock = "break")
  f <- run(ad)
  expect_identical(dim(f$draws$att), c(200L, ad$S))
  expect_identical(colnames(f$draws$hyper)[1:3], c("tau_att", "sigma_att", "kappa_att"))
  # CmdStan writes six significant figures, so "zero" is about 1e-7.
  expect_lt(max(abs(rowSums(f$draws$att[, ad$first_slot]))), 1e-6)

  sq <- fv2_build(sim, param = "sq", style = "static")
  expect_identical(basename(fv2$fv2_stan_file(sq, stan_dir)), "fv2_sq.stan")
  g <- run(sq)
  expect_identical(colnames(g$draws$hyper), c("tau_str", "sigma_str", "tau_sty", "omega_att", "omega_def"))
  # Attack minus defence is twice the style: fixed over a team's slots.
  style <- (g$draws$att - g$draws$def) / 2
  a <- sq$slots$slot[sq$slots$team == 1L]
  expect_lt(max(abs(style[, a] - style[, a[1]])), 1e-6)
  expect_lt(max(abs(rowSums(style[, sq$first_slot]))), 1e-6)

  div <- unique(data.frame(
    team = c(sim$home_team, sim$away_team), season = rep(sim$season, 2),
    division = "L", rank = 1L
  ))
  rev <- fv2_build(sim, clock = "break", rho = 0.8, divisions = div)
  h <- fv2$fv2_fit_stan(
    rev, fv2$fv2_stan_file(rev, stan_dir, est_rho = TRUE), tmp, tmp,
    chains = 2L, iter_warmup = 150L, iter_sampling = 100L, refresh = 0L, est_rho = TRUE
  )
  expect_identical(
    colnames(h$draws$hyper),
    c("tau_att", "sigma_att", "kappa_att", "tau_def", "sigma_def", "kappa_def", "omega_att", "omega_def", "rho")
  )
  expect_true(all(h$draws$hyper[, "rho"] > 0 & h$draws$hyper[, "rho"] < 1))
  expect_identical(dim(h$draws$att), c(200L, rev$S))

  lvl <- fv2_build(
    sim,
    clock = "break", rho = 0.8, param = "sq", style = "static",
    level_walk = TRUE, divisions = div
  )
  k <- run(lvl)
  expect_identical(
    colnames(k$draws$hyper),
    c("tau_str", "sigma_str", "kappa_str", "tau_sty", "omega_att", "omega_def", "sigma_lvl_goal", "sigma_lvl_ha")
  )
  expect_identical(dim(k$draws$lvl_goal), c(200L, length(lvl$weeks)))
  expect_lt(max(abs(rowSums(k$draws$lvl_goal))), 1e-5)
  expect_lt(max(abs(rowSums(k$draws$lvl_ha))), 1e-5)
})

test_that("the Stan program's recursion is the R twin, so the same prior as INLA's precision", {
  skip_on_ci()
  skip_if(Sys.getenv("FV2_TEST_STAN") != "1", "set FV2_TEST_STAN=1 to compile and sample")
  skip_if_not_installed("cmdstanr")
  # A league where a team sits out a season and another has no new-season slot:
  # the cases where the two engines used to pick different reversion groups.
  matches <- rbind(
    stayers_matches(2021),
    data.frame(
      match_date = stayers_matches(2022)$match_date[1], home_team = "B", away_team = "C",
      home_score = 0L, away_score = 1L, season = 2022L, cell = "L1@2022"
    ),
    stayers_matches(2023)
  )
  spec <- fv2_build(matches, clock = "break", rho = 0.6, divisions = stayers_divisions(seasons = 2021:2023))
  seg <- fv2_segments(spec)
  expect_gt(seg$n_group, 0L)
  tmp <- withr::local_tempdir()
  file.copy(file.path(fv2_dir, "stan", "fv2_rev.stan"), tmp)
  mod <- cmdstanr::cmdstan_model(file.path(tmp, "fv2_rev.stan"), compile_standalone = TRUE)
  set.seed(4)
  for (i in 1:3) {
    lvl <- stats::rnorm(spec$K)
    z <- stats::rnorm(spec$S)
    z_brk <- stats::rnorm(seg$n_seg)
    stan <- mod$functions$walk(
      lvl, z, z_brk, 0.05, 0.25, 0.6, seg$step_in, seg$seg_of_slot, seg$seg_first, seg$seg_last,
      seg$seg_team, seg$seg_prev, seg$seg_stay, seg$seg_group, seg$seg_brk_sd, seg$seg_order,
      as.array(seg$wave_start), as.array(seg$wave_end), seg$member_seg,
      as.array(seg$group_first), as.array(seg$group_last)
    )
    expect_equal(as.numeric(stan), fv2_walk(seg, lvl, z, z_brk, 0.05, 0.25, 0.6), tolerance = 1e-10)
  }
})

toy_wf <- function() {
  one <- function(variant, shift) {
    data.frame(
      sex = rep(c("male", "female"), each = 3), variant = variant,
      match_date = as.Date("2025-05-01") + c(0, 1, 7, 0, 1, 7),
      home_team = c("A", "C", "A", "E", "G", "E"), away_team = c("B", "D", "C", "F", "H", "G"),
      home_score = c(2, 0, 1, 3, 1, 0), away_score = c(1, 0, 2, 0, 1, 2),
      cutoff = as.Date("2025-04-27") + c(0, 0, 7, 0, 0, 7),
      phase = factor(c("early", "early", "late", "early", "early", "late"), c("early", "middle", "late")),
      ls_1x2 = c(-0.9, -1.2, -1.0, -0.6, -1.3, -1.1) + shift,
      ls_score = c(-3.1, -2.8, -3.3, -3.6, -2.9, -3.2) + shift,
      rps = 0.2, p_home = c(0.5, 0.4, 0.45, 0.6, 0.35, 0.5)
    )
  }
  d <- rbind(one("base", 0), one("alt", c(0.02, 0.01, 0.03, -0.01, 0.02, 0.01)))
  d$key <- fv2$fv2_match_key(d)
  d
}

test_that("score tables report losses and the home-win rate per group", {
  d <- toy_wf()
  tab <- fv2$fv2_score_table(d, c("variant", "sex"))
  expect_identical(nrow(tab), 4L)
  row <- tab[tab$variant == "base" & tab$sex == "male", ]
  expect_identical(row$n, 3L)
  expect_equal(row$logloss_1x2, mean(c(0.9, 1.2, 1.0)))
  expect_equal(row$scoreline, mean(c(3.1, 2.8, 3.3)))
  expect_equal(row$obs_home, 1 / 3)
  expect_equal(row$p_home, mean(c(0.5, 0.4, 0.45)))
})

test_that("paired comparisons use only the matches both variants predicted", {
  d <- toy_wf()
  cmp <- fv2$fv2_compare(d, "base")
  shift <- c(0.02, 0.01, 0.03, -0.01, 0.02, 0.01)
  expect_identical(cmp$n, 6L)
  expect_equal(cmp$diff, mean(shift))
  expect_equal(cmp$se, stats::sd(shift) / sqrt(6))
  by_sex <- fv2$fv2_compare(d, "base", "sex")
  expect_equal(by_sex$diff[by_sex$group == "female"], mean(shift[4:6]))
  dropped <- d[!(d$variant == "alt" & d$home_team == "A"), ]
  expect_identical(fv2$fv2_compare(dropped, "base")$n, 4L)
  expect_equal(fv2$fv2_compare(d, "base", metric = "ls_score")$diff, mean(shift))
})

test_that("an outside forecast is scored on the matches it shares", {
  d <- toy_wf()
  ext <- data.frame(
    key = d$key[d$variant == "base"][1:2],
    line_home = c(0.5, 0.2), line_draw = c(0.3, 0.5), line_away = c(0.2, 0.3)
  )
  b <- fv2$fv2_benchmark(d, ext, "line", "base")
  expect_identical(nrow(b), 2L)
  expect_equal(b$ls_line, log(c(0.5, 0.5)))
  expect_equal(b$ls_model, c(-0.9, -1.2))
  expect_identical(b$phome_line, c(0.5, 0.2))
})

test_that("production fits the variant the walk-forward recommended", {
  # The evidence in the report is for the registry's `rev80_sq_static_lvl`.
  # If the options the package fits drift from it, the report no longer
  # describes what production runs.
  sys.source(file.path(fv2_dir, "variants.R"), envir = fv2)
  expect_identical(football_v2_options(), fv2$fv2_variants()[["rev80_sq_static_lvl"]])
})

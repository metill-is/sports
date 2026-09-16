# Calibrate the tolerances in tests/testthat/test-simulate-2dt-equivalence.R.
#
#   NOT_CRAN=true LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 \
#     Rscript tools/calibrate-2dt-equivalence.R [out_dir]
#
# Run from the repository root. Needs CmdStan; a few minutes (two ~40 s fits,
# then pure R replays).
#
# WHAT IT MEASURES. The equivalence test fits both 2DT Stan models on a noised
# copy of the synthetic facts fixture, replays Stan's own prediction fixtures
# through the R match generator from the SAME posterior draws, and asserts per
# fixture that the two agree: mean (z < 5), IQR ratio (EQUIV_IQR_TOL), Spearman
# correlation (EQUIV_SPEARMAN_TOL), P(home win) (0.06) and a per-draw PIT
# against the Stan GQ block (EQUIV_PIT_MIN_P); and that the league table built
# from the replay matches the pre-2026-09-16 table built from Stan's
# goals*_pred (0.06). This script reuses the test's own helpers and fits, and
# prints for each sport:
#
#   1. every statistic at the test's seeds (20260917 replay, 20260918 table),
#      i.e. exactly what the test sees;
#   2. the spread of each per-fixture statistic over 200 replay seeds against
#      the same fixed Stan draws (max, 99th and 95th percentile; for the PIT,
#      the smallest p and the share below 0.001 / 0.01);
#   3. the largest table difference over 50 seeds.
#
# The tolerances sit above (2) and (3) with room to spare. Measured 2026-09-16
# (CmdStan on macOS arm64), largest over 200 seeds and both sports: mean z 3.6,
# IQR 0.14, Spearman 0.11, P(home win) 0.046, smallest replay-PIT p 0.0013;
# table 0.035 over 50 seeds.
# Re-run after a CmdStan, model or platform change moves the draws, and when a
# check in that test starts failing. Results are also saved as
# calibrate-2dt-equivalence.rds in `out_dir` (default: a temp directory).

if (!file.exists("DESCRIPTION") ||
  !file.exists(file.path("tests", "testthat", "test-simulate-2dt-equivalence.R"))) {
  stop("Run tools/calibrate-2dt-equivalence.R from the sports repository root.")
}
# The test's fit helper starts with skip_on_cran(); outside a test that skip
# would abort the script.
Sys.setenv(NOT_CRAN = "true")

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1]] else tempdir()
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

suppressMessages(devtools::load_all(".", quiet = TRUE))
suppressMessages(library(testthat))
invisible(testthat::source_test_helpers("tests/testthat", env = globalenv()))
# The test file's top-level assignments: its constants and helpers
# (.equivalence_fit, .fixture_sides, .oracle_2dt, .pit_2dt), not its tests.
for (e in parse(file.path("tests", "testthat", "test-simulate-2dt-equivalence.R"))) {
  if (is.call(e) && identical(e[[1]], as.name("<-"))) eval(e, globalenv())
}
options(width = 250)

N_SEEDS <- 200L
N_TABLE_SEEDS <- 50L
se <- function(a, b) sqrt(stats::var(a) / length(a) + stats::var(b) / length(b))
iqr_dev <- function(a, b) abs(stats::IQR(a) / stats::IQR(b) - 1)
spearman <- function(a, b) stats::cor(a, b, method = "spearman")
ks_p <- function(u) stats::ks.test(u, "punif")$p.value

measure_sport <- function(sport) {
  # env = this frame: the noised facts root is removed when the function ends.
  f <- .equivalence_fit(sport, env = environment())
  d <- f$fit$diagnostic_summary(quiet = TRUE)
  cat(
    "\n=====", sport, "=====\nreturn codes:", f$fit$return_codes(),
    " divergent:", d$num_divergent, " max_treedepth:", d$num_max_treedepth,
    " ebfmi:", round(d$ebfmi, 2), "\n"
  )
  allsum <- f$fit$summary()
  cat(
    "max rhat (all vars):", max(allsum$rhat, na.rm = TRUE),
    " min ess_bulk:", min(allsum$ess_bulk, na.rm = TRUE), "\n"
  )

  # The same inputs, in the same order, as .expect_generator_matches_stan().
  si <- .extract_sim_inputs_2dt(
    f$fit, f$prep$teams, sport,
    n_seasons = f$prep$stan_data$N_seasons
  )
  si$scalar$z_level <- 0
  si$scalar <- .season_level_2dt(si$scalar, 0L)
  cat(
    "n draws:", nrow(si$scalar), " nu<4 share:", mean(si$scalar$nu < 4),
    " nu<2 share:", mean(si$scalar$nu < 2), "\n"
  )
  mf <- .match_fn_2dt(sport)
  pg <- .compute_posterior_goals_2dt(f$fit, f$prep$pred_d)
  games <- unique(pg$game_nr)
  prep_g <- lapply(games, function(g) {
    stan <- pg[pg$game_nr == g, ]
    stan <- stan[order(stan$.draw), ]
    sides <- .fixture_sides(si, mf, stan$home_team[1], stan$away_team[1])
    list(stan = stan, sides = sides, oracle = .oracle_2dt(sport, sides, si$scalar))
  })
  stat_row <- function(x, r) {
    s <- x$stan
    c(
      zh = abs(mean(r$home) - mean(s$home_score)) / se(r$home, s$home_score),
      za = abs(mean(r$away) - mean(s$away_score)) / se(r$away, s$away_score),
      iqr_h = iqr_dev(r$home, s$home_score),
      iqr_a = iqr_dev(r$away, s$away_score),
      sp = abs(spearman(r$home, r$away) - spearman(s$home_score, s$away_score)),
      pw = abs(mean(r$home > r$away) - mean(s$home_score > s$away_score)),
      pit_r = ks_p(.pit_2dt(r$home, r$away, x$oracle))
    )
  }

  # (1) At the test's seed, in the test loop's RNG order.
  rows <- list()
  withr::with_seed(20260917L, {
    for (i in seq_along(games)) {
      x <- prep_g[[i]]
      s <- x$stan
      r <- mf(x$sides$home, x$sides$away, si$scalar)
      rows[[i]] <- data.frame(
        game = games[i],
        fixture = paste(s$home_team[1], "v", s$away_team[1]),
        mh_S = mean(s$home_score), mh_R = mean(r$home),
        ma_S = mean(s$away_score), ma_R = mean(r$away),
        pw_S = mean(s$home_score > s$away_score), pw_R = mean(r$home > r$away),
        pitp_S = ks_p(.pit_2dt(s$home_score, s$away_score, x$oracle)),
        t(stat_row(x, r)),
        rho_min = min(x$oracle$rho), rho_max = max(x$oracle$rho)
      )
    }
  })
  obs <- do.call(rbind, rows)
  cat("\nAt the test's seed:\n")
  print(obs, digits = 3)

  # (2) Spread over N_SEEDS replay seeds, Stan side fixed.
  spread <- array(
    NA_real_, c(N_SEEDS, length(games), 7L),
    dimnames = list(
      NULL, games, c("zh", "za", "iqr_h", "iqr_a", "sp", "pw", "pit_r")
    )
  )
  for (k in seq_len(N_SEEDS)) {
    withr::with_seed(k, for (i in seq_along(games)) {
      x <- prep_g[[i]]
      spread[k, i, ] <- stat_row(x, mf(x$sides$home, x$sides$away, si$scalar))
    })
  }
  cat("\n", N_SEEDS, "-seed spread (max over seeds x fixtures):\n", sep = "")
  for (st in dimnames(spread)[[3]]) {
    v <- spread[, , st]
    if (st == "pit_r") {
      cat(sprintf(
        "  %-6s min p %.2e  share p<0.001 %.4f  share p<0.01 %.4f\n",
        st, min(v), mean(v < 0.001), mean(v < 0.01)
      ))
    } else {
      cat(sprintf(
        "  %-6s max %.4f  q99 %.4f  q95 %.4f  mean %.4f\n",
        st, max(v), stats::quantile(v, .99), stats::quantile(v, .95), mean(v)
      ))
    }
  }

  # (3) The table: Stan's window through the old path against the same
  # fixtures through the simulator, as in the test.
  div <- .iceland_division_codes(paste0(sport, "_iceland"), "male")[[1]]
  results <- read_table("results", root = f$root)
  played <- results[
    results$sport == sport & results$sex == "male" &
      results$season == 2100L & results$division == div, ,
    drop = FALSE
  ]
  tp <- .tie_params_pfi(sport)
  base_old <- .compute_base_points_2dt(
    played,
    has_ties = tp$has_ties, tie_threshold = tp$tie_threshold
  )
  old <- .compute_final_positions_2dt(
    pg, div, base_old, tp$has_ties, tp$tie_threshold,
    current_top_teams = NULL
  )
  window <- unique(pg[pg$division == div, c("game_nr", "home_team", "away_team")])
  window <- window[order(window$game_nr), c("home_team", "away_team")]
  base_new <- tibble::tibble(
    team = base_old$team, base_points = base_old$base_points,
    base_gd = as.integer(base_old$base_diff), base_gf = 0L
  )
  table_at <- function(seed) {
    new <- withr::with_seed(seed, simulate_league_season(
      si$team, si$scalar, window, base_new,
      match_fn = mf,
      points_fn = .points_fn_2dt(tp$has_ties, tp$tie_threshold),
      tie_break = "jitter"
    ))$final_positions
    dplyr::inner_join(
      old, new,
      by = c("team", "placement"), suffix = c("_stan", "_r")
    )
  }
  both <- table_at(20260918L)
  both$d <- abs(both$probability_stan - both$probability_r)
  cat("\nTable at the test's seed:\n")
  print(as.data.frame(both), digits = 3)
  cat("max table diff at the test's seed:", max(both$d), "\n")
  tmax <- vapply(seq_len(N_TABLE_SEEDS), function(k) {
    max(with(table_at(k), abs(probability_stan - probability_r)))
  }, numeric(1))
  cat(
    N_TABLE_SEEDS, "-seed table max diff: max ", max(tmax),
    "  q95 ", stats::quantile(tmax, .95), "\n",
    sep = ""
  )
  list(obs = obs, spread = spread, table = both, tmax = tmax)
}

out <- lapply(c(basketball = "basketball", handball = "handball"), measure_sport)
path <- file.path(out_dir, "calibrate-2dt-equivalence.rds")
saveRDS(out, path)
cat("\nSaved:", path, "\n")

#' Benchmark: CVXR vs SLSQP on real league posteriors + Busseti RC-Kelly
#'
#' Run from Sports/ directory:
#'   Rscript R/bets/bench_cvxr.R [league_key]
#'
#' For each +EV match in the named league, reports:
#'   - SLSQP vs CVXR solution agreement (max |f diff|)
#'   - Growth-rate difference
#'   - Per-solve wall time
#'   - Risk-constrained stake + growth at λ ∈ {0.5, 2, 5}
#'
#' Requires CVXR; exits cleanly if CVXR isn't installed.

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

if (!requireNamespace("CVXR", quietly = TRUE)) {
  cat("CVXR not installed; install.packages('CVXR') to run this benchmark.\n")
  quit(status = 0)
}

box::use(
  . / kelly_joint[
    build_return_matrix, collect_match_bets, get_kelly_joint, parse_handicap
  ],
  . / kelly_cvxr[get_kelly_cvxr],
  . / odds[load_odds],
  readr[read_csv, read_file],
  yaml[yaml.load],
  dplyr[filter, distinct, mutate]
)
`%||%` <- function(a, b) if (is.null(a)) b else a

bench <- function(league_key) {
  cat(sprintf("\n══════ %s ══════\n", league_key))
  leagues <- yaml.load(read_file(here::here("config", "leagues.yml")))
  league <- leagues[[league_key]]
  if (is.null(league)) {
    cat("  (not in leagues.yml)\n")
    return(invisible(NULL))
  }
  league_dir <- here::here(league$dir)
  cfg <- yaml.load(read_file(file.path(league_dir, "config", "bets.yml")))
  global <- yaml.load(read_file(here::here("config", "bankroll.yml")))
  merged <- global
  merged[names(cfg$bankroll)] <- cfg$bankroll
  cfg$bankroll <- merged
  cfg$league_dir <- league_dir
  cfg$sport <- league$sport
  cfg$country <- league$country

  sex <- cfg$sex[[1]]
  post_path <- file.path(league_dir, "results", sex, "posterior_goals.csv")
  if (!file.exists(post_path)) {
    cat("  (no posterior)\n")
    return(invisible(NULL))
  }
  post <- read_csv(post_path, show_col_types = FALSE)
  if (!"division" %in% names(post)) post$division <- "1"
  post$date <- as.Date(post$date)

  odds <- tryCatch(
    load_odds(cfg, sport_dir = league_dir, sex = sex),
    error = function(e) {
      cat(sprintf("  (odds load: %s)\n", conditionMessage(e)))
      NULL
    }
  )
  if (is.null(odds)) {
    cat("  (odds load failed)\n")
    return(invisible(NULL))
  }

  odds_hc <- odds$handicap
  if (!is.null(odds_hc) && nrow(odds_hc) > 0 && is.character(odds_hc$change)) {
    odds_hc <- odds_hc |> mutate(change = parse_handicap(change))
  }

  matches <- post |> distinct(date, division, home, away)
  max_stake <- cfg$bankroll$max_match_kelly %||% 1.0
  ev_thr <- cfg$bankroll$ev_threshold %||% 0.0

  n <- 0
  for (m_idx in seq_len(nrow(matches))) {
    match <- matches[m_idx, ]
    draws <- post |>
      filter(date == match$date, home == match$home, away == match$away)
    if (nrow(draws) == 0) next

    pick <- function(tbl, enabled) {
      if (!isTRUE(enabled) || is.null(tbl) || nrow(tbl) == 0) {
        return(NULL)
      }
      out <- tbl |>
        filter(date == match$date, home == match$home, away == match$away)
      if (nrow(out) == 0) NULL else out
    }
    m_1x2 <- pick(odds$outcome, cfg$markets$outcome)
    m_hc <- pick(odds_hc, cfg$markets$handicap)
    m_tot <- pick(odds$totals, cfg$markets$totals)

    bets <- collect_match_bets(m_1x2, m_hc, m_tot, cfg)
    if (is.null(bets) || nrow(bets) == 0) next
    R <- build_return_matrix(draws, bets)
    p_hat <- colMeans(R > 0)
    keep <- p_hat - 1 / bets$o > ev_thr
    if (!any(keep)) next

    bets <- bets[keep, ]
    R <- R[, keep, drop = FALSE]

    t0 <- Sys.time()
    fit_s <- get_kelly_joint(net_return = R, max_stake = max_stake)
    t_s <- as.numeric(Sys.time() - t0) * 1000
    t0 <- Sys.time()
    fit_c <- get_kelly_cvxr(R, max_stake = max_stake)
    t_c <- as.numeric(Sys.time() - t0) * 1000
    max_diff <- max(abs(fit_s$solution - fit_c$solution))

    rc_stakes <- sapply(c(0.5, 2.0, 5.0), function(lam) {
      fit <- get_kelly_cvxr(R, max_stake = max_stake, risk_lambda = lam)
      c(sum_f = sum(fit$solution), growth = fit$diagnostics$growth_rate)
    })
    colnames(rc_stakes) <- paste0("λ=", c(0.5, 2.0, 5.0))

    cat(sprintf(
      "\n  %s vs %s  [%d +EV bets]\n",
      match$home, match$away, ncol(R)
    ))
    cat(sprintf(
      "    SLSQP  f_sum=%.4f  G=%+.5f  %.0f ms\n",
      sum(fit_s$solution), fit_s$diagnostics$growth_rate, t_s
    ))
    cat(sprintf(
      "    CVXR   f_sum=%.4f  G=%+.5f  %.0f ms   max|df|=%.2e\n",
      sum(fit_c$solution), fit_c$diagnostics$growth_rate, t_c, max_diff
    ))
    for (j in seq_len(ncol(rc_stakes))) {
      cat(sprintf(
        "    %s  f_sum=%.4f  G=%+.5f\n",
        colnames(rc_stakes)[j], rc_stakes["sum_f", j], rc_stakes["growth", j]
      ))
    }
    n <- n + 1
    if (n >= 5) break
  }
  if (n == 0) cat("  (no +EV matches)\n")
  invisible(NULL)
}

args <- commandArgs(trailingOnly = TRUE)
leagues <- if (length(args) > 0) {
  args
} else {
  c(
    "basketball_iceland", "handball_iceland", "football_iceland"
  )
}
for (k in leagues) bench(k)

# Synthetic scenario — always runs so the benchmark produces a readable
# report even when no active league has +EV bets today.
cat("\n══════ synthetic: football-style match with known edges ══════\n")

box::use(dplyr[tibble])
set.seed(202604)
S <- 4000
draws <- tibble(
  home_goals = rpois(S, 1.45),
  away_goals = rpois(S, 1.10)
)
bets_syn <- tibble(
  bet_type = c("1x2_home", "1x2_tie", "1x2_away", "over", "under"),
  o = c(2.10, 3.40, 3.80, 1.85, 2.00),
  change = NA_real_,
  limit = c(NA, NA, NA, 2.5, 2.5),
  tie_threshold = 0,
  hc_threshold = NA_real_,
  market_variant = NA_character_
)
R_syn <- build_return_matrix(draws, bets_syn)

t0 <- Sys.time()
fit_s <- get_kelly_joint(net_return = R_syn, max_stake = 1.0)
t_s <- as.numeric(Sys.time() - t0) * 1000
t0 <- Sys.time()
fit_c <- get_kelly_cvxr(R_syn, max_stake = 1.0)
t_c <- as.numeric(Sys.time() - t0) * 1000
max_diff <- max(abs(fit_s$solution - fit_c$solution))

cat(sprintf(
  "\n  5 bets (1x2 + O/U), Poisson(1.45, 1.10), %d draws\n", S
))
cat(sprintf(
  "  SLSQP  f = (%s)  f_sum=%.4f  G=%+.5f  %.0f ms\n",
  paste(sprintf("%.4f", fit_s$solution), collapse = ", "),
  sum(fit_s$solution), fit_s$diagnostics$growth_rate, t_s
))
cat(sprintf(
  "  CVXR   f = (%s)  f_sum=%.4f  G=%+.5f  %.0f ms   max|df|=%.2e\n",
  paste(sprintf("%.4f", fit_c$solution), collapse = ", "),
  sum(fit_c$solution), fit_c$diagnostics$growth_rate, t_c, max_diff
))
for (lam in c(0.5, 2.0, 5.0, 10.0)) {
  fit <- get_kelly_cvxr(R_syn, max_stake = 1.0, risk_lambda = lam)
  cat(sprintf(
    "  λ=%-5.1f f = (%s)  f_sum=%.4f  G=%+.5f  worst=%.4f\n",
    lam,
    paste(sprintf("%.4f", fit$solution), collapse = ", "),
    sum(fit$solution), fit$diagnostics$growth_rate,
    fit$diagnostics$worst_case_wealth
  ))
}

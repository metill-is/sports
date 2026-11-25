# The owner's legacy international handball model, run for the backtest.
#
# Program: _legacy/sports/handball/international/Stan/2d_student_t.stan
# (bivariate Student-t scores, per-team random walks over each team's own
# matches, team home advantages, a friendly effect, a season scoring level).
# Its data preparation (_legacy/.../R/common/prep_data.R and process_data.R)
# is reproduced here on the same match table the v2 model reads:
# * competitions and editions as configured in _legacy/.../R/common/leagues.R:
#   women the EURO (with its qualifiers), the world championship, the Olympics,
#   the African and Asian championships and friendlies; men the EURO, the
#   world championship, the Olympics and friendlies; from 2015 (the EURO and
#   world championship editions listed there);
# * teams kept: those that played the EURO, the world championship or the
#   Olympics and whose last match falls in the two years before the cutoff
#   (the legacy rule "a major tournament and a match in 2025 or later", run in
#   2026), both sides of every match in that set;
# * friendlies are the listed home side's home match and carry the friendly
#   effect; every championship match is neutral, except that a host is put
#   first and gets its home advantage in the matches predicted.
# Two departures, both in the legacy model's favour: its women's division
# codes flag the Asian championship as the friendly competition (division 2)
# where friendlies were meant; here friendlies are flagged. And its dates took
# the year from the edition's name, so qualifiers played the year before were
# dated a year late; here every match keeps its own date.

LEGACY_COMPETITIONS <- list(
  female = list(
    EURO = c(2018L, 2020L, 2022L, 2024L, 2026L), WC = c(2019L, 2021L, 2023L, 2025L),
    OG = c(2016L, 2020L, 2021L, 2024L), ASIA = 2015:2026, AFR = 2015:2026, FRI = 2015:2026
  ),
  male = list(
    EURO = seq(2012L, 2026L, 2L), WC = c(2017L, 2019L, 2021L, 2023L, 2025L),
    OG = c(2016L, 2020L, 2021L, 2024L), FRI = 2015:2026
  )
)

#' The legacy model's training rows at a cutoff
legacy_training <- function(matches, sex, as_of) {
  comp <- LEGACY_COMPETITIONS[[sex]]
  keep <- mapply(function(c, e) !is.null(comp[[c]]) && e %in% comp[[c]], matches$competition, matches$edition)
  d <- matches[keep & matches$match_date < as_of & matches$match_date >= as.Date("2015-01-01") &
    !is.na(matches$home_score) & !matches$awarded, , drop = FALSE]
  major <- d$competition %in% c("EURO", "WC", "OG")
  long <- data.frame(team = c(d$home_team, d$away_team), date = c(d$match_date, d$match_date), major = c(major, major))
  last <- tapply(long$date, long$team, max)
  any_major <- tapply(long$major, long$team, any)
  ok <- names(last)[any_major & as.Date(last, origin = "1970-01-01") >= as_of - 730]
  d <- d[d$home_team %in% ok & d$away_team %in% ok, , drop = FALSE]
  d$friendly <- d$kind == "friendly"
  d
}

#' Stan data in the legacy layout (prepare_handball_data())
legacy_stan_data <- function(d, pred) {
  d <- d[order(d$match_date, d$match_id), , drop = FALSE]
  d$game_nr <- seq_len(nrow(d))
  teams <- sort(unique(c(d$home_team, d$away_team)))
  k <- length(teams)
  long <- data.frame(
    game_nr = rep(d$game_nr, 2), date = rep(as.numeric(d$match_date), 2),
    side = rep(c("home", "away"), each = nrow(d)), team = c(d$home_team, d$away_team)
  )
  long <- long[order(long$team, long$game_nr), ]
  long$round <- stats::ave(long$game_nr, long$team, FUN = seq_along)
  long$timediff <- stats::ave(long$date, long$team, FUN = function(x) c(NA, diff(x)))
  long$timediff[is.na(long$timediff)] <- 7
  long$timediff <- pmin(long$timediff, 100)
  n_rounds <- max(long$round)
  tbm <- matrix(0, k, n_rounds)
  tbm[cbind(match(long$team, teams), long$round)] <- long$timediff
  get <- function(side, col) long[[col]][long$side == side][order(long$game_nr[long$side == side])]
  latest <- tapply(long$date, long$team, max)

  pred <- pred[order(pred$match_date), , drop = FALSE]
  pl <- data.frame(
    i = rep(seq_len(nrow(pred)), 2), side = rep(c("home", "away"), each = nrow(pred)),
    team = c(pred$home_team, pred$away_team), date = rep(as.numeric(pred$match_date), 2)
  )
  pl <- pl[order(pl$team, pl$date, pl$i), ]
  pl$prev <- stats::ave(pl$date, pl$team, FUN = function(x) c(NA, x[-length(x)]))
  pl$prev[is.na(pl$prev)] <- latest[pl$team[is.na(pl$prev)]]
  pl$td <- pmin(pl$date - pl$prev, 50)
  td <- function(side) pl$td[pl$side == side][order(pl$i[pl$side == side])]
  top <- sort(unique(c(pred$home_team, pred$away_team)))
  first_pred <- tapply(pl$date, pl$team, min)

  list(
    K = k, N = nrow(d), N_pred = nrow(pred), N_rounds = n_rounds,
    N_seasons = length(unique(format(d$match_date, "%Y"))),
    season = as.numeric(as.factor(format(d$match_date, "%Y"))),
    team1 = match(d$home_team, teams), team2 = match(d$away_team, teams),
    team1_home = as.integer(d$friendly),
    round1 = get("home", "round"), round2 = get("away", "round"),
    time_between_matches = tbm,
    goals1 = d$home_score, goals2 = d$away_score,
    division = as.integer(factor(d$competition)), casual = as.integer(d$friendly),
    team1_pred = match(pred$home_team, teams), team2_pred = match(pred$away_team, teams),
    team1_home_pred = as.integer(pred$host_home),
    pred_timediff1 = td("home"), pred_timediff2 = td("away"),
    pred_division = rep(1L, nrow(pred)),
    time_to_next_games = as.numeric(first_pred[top] - latest[top]),
    top_teams = match(top, teams), N_top_teams = length(top),
    teams = teams
  )
}

#' Fit the legacy program and return its score draws for the predicted matches
#'
#' @param pred Matches to predict: `match_date`, `home_team`, `away_team`,
#'   `host` (TRUE when one side is a host nation).
#' @return list(home, away): draws-by-matches matrices in `pred`'s order and
#'   orientation, plus `diagnostics`.
legacy_predict <- function(matches, sex, as_of, pred, hosts, exe_dir, chains = 4L, iter = 1000L, seed = 1L) {
  d <- legacy_training(matches, sex, as_of)
  teams <- unique(c(d$home_team, d$away_team))
  if (!all(c(pred$home_team, pred$away_team) %in% teams)) {
    miss <- setdiff(c(pred$home_team, pred$away_team), teams)
    warning("legacy training set lacks: ", paste(miss, collapse = ", "))
  }
  swap <- pred$away_team %in% hosts & !pred$home_team %in% hosts
  p2 <- pred
  p2$home_team <- ifelse(swap, pred$away_team, pred$home_team)
  p2$away_team <- ifelse(swap, pred$home_team, pred$away_team)
  p2$host_home <- p2$home_team %in% hosts
  p2$orig <- seq_len(nrow(pred))
  ok <- p2$home_team %in% teams & p2$away_team %in% teams
  sd <- legacy_stan_data(d, p2[ok, ])
  ord <- order(p2$match_date[ok])
  sd_teams <- sd$teams
  sd$teams <- NULL
  dir.create(exe_dir, recursive = TRUE, showWarnings = FALSE)
  stan_file <- file.path(exe_dir, "2d_student_t.stan")
  if (!file.exists(stan_file)) {
    file.copy(here::here("_legacy", "sports", "handball", "international", "Stan", "2d_student_t.stan"), stan_file)
  }
  mod <- cmdstanr::cmdstan_model(stan_file)
  fit <- mod$sample(
    data = sd, chains = chains, parallel_chains = chains, iter_warmup = iter, iter_sampling = iter,
    init = 0, refresh = 0, seed = seed, show_messages = FALSE
  )
  g1 <- fit$draws("goals1_pred", format = "draws_matrix")
  g2 <- fit$draws("goals2_pred", format = "draws_matrix")
  diag <- fit$diagnostic_summary(quiet = TRUE)
  rhat <- suppressWarnings(max(fit$summary(c("sigma_off", "sigma_def", "nu", "rho"), "rhat")$rhat, na.rm = TRUE))
  # Stan's rows are in date order of the kept matches; map back to `pred`.
  idx <- which(ok)[ord]
  home <- away <- matrix(NA_real_, nrow(g1), nrow(pred))
  h <- unclass(g1)
  a <- unclass(g2)
  home[, idx] <- ifelse(rep(swap[idx], each = nrow(h)), a, h)
  away[, idx] <- ifelse(rep(swap[idx], each = nrow(h)), h, a)
  list(
    home = home, away = away, n_train = nrow(d), n_teams = length(sd_teams), n_rounds = sd$N_rounds,
    divergences = sum(diag$num_divergent), max_treedepth = sum(diag$num_max_treedepth), rhat_max = rhat,
    secs = fit$time()$total
  )
}

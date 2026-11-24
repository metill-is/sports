# The monthly scorecard of the handball / basketball v2 switch path: the
# comparisons of docs/reports/2026-11-15-hb-bb-v2/report.md ("Against
# production's stored forecasts", "Against the Lengjan line"), rerun on the
# matches played since the shadow period began. Local only:
# scripts/hbbb_v2_scorecard.R runs it, and no workflow may
# (test-hbbb-v2-scorecard.R).
#
# Leak-free and like for like: a match is scored where production has a
# stored forecast of it (its newest fit dated strictly before the match), and
# v2 forecasts it from the same information: production's fit dated D ran
# before D's games, so v2 is fitted on the results dated up to D - 1. The fit
# uses the switch path's own fitting and pairing (`hbbb_v2_fit_halves()`,
# `hbbb_v2_pair()`), so the specification is the frozen one.

#' First day of the shadow period the scorecard scores.
#' @noRd
HBBB_SCORECARD_START <- as.Date("2026-11-17")

#' The per-sport gate of the report's shadow period: at least `min_matches`
#' scored matches; pooled margin and total log scores not worse than
#' production's by more than `worse_se` standard errors; the mean expected
#' home margin within `bias_se` standard errors of the observed one; and every
#' v2 fit through the production gate.
#' @noRd
HBBB_SCORECARD_GATE <- list(min_matches = 150L, worse_se = 1, bias_se = 1)

#' Standard error of a mean over matches, clustered by team-season.
#'
#' A match belongs to two team-seasons, its home side's and its away side's,
#' and a team-season's matches are correlated on either side. The variance of
#' the mean is the sum of the covariances of every two matches that share a
#' team-season (each match with itself included):
#' `G / (G - 1) * (sum_c S_c^2 - sum_d S_d^2) / n^2`, where `S_c` sums the
#' deviations of every match team-season `c` played, home or away, and `S_d`
#' those of the matches between the same two team-seasons, which `S_c` counts
#' twice. A non-positive sum, possible in small samples, falls back to the
#' iid variance.
#'
#' @param x Per-match values.
#' @param home,away Each match's two team-seasons.
#' @return One standard error; `NA` with fewer than two team-seasons.
#' @noRd
cluster_se_team_season <- function(x, home, away) {
  n <- length(x)
  e <- x - mean(x)
  s_c <- tapply(c(e, e), c(home, away), sum)
  g <- length(s_c)
  if (g < 2L) {
    return(NA_real_)
  }
  pair <- ifelse(home < away, paste(home, away, sep = "\r"), paste(away, home, sep = "\r"))
  s_d <- tapply(e, pair, sum)
  v <- g / (g - 1) * (sum(s_c^2) - sum(s_d^2)) / n^2
  if (!is.finite(v) || v <= 0) v <- sum(e^2) / (n * (n - 1))
  sqrt(v)
}

#' Score v2 forecasts against played results.
#'
#' As in the report's walk-forward. Handball: the rounded predictive of the
#' margin and of the total, and the W/D/L likelihood of the plain rounded
#' margin (no shift; decision D4 is open). Basketball: the final result,
#' reached from the regulation-time predictive through the level-score shift
#' and overtime (`v2_basketball_result_probs()`): the final margin has no
#' level mass, the final total is the regulation total or, after a level
#' regulation score, that plus one overtime period. The regulation-time 1X2
#' the bookmaker settles on is returned too (`*_reg`), for the market
#' comparison.
#'
#' @param comp Draws for the matches: `loc_m`, `loc_t` (draws by matches) and
#'   `sigma_m`, `sigma_t` (one per draw).
#' @param obs Played results, one row per match in the same order:
#'   `home_score`, `away_score`, and for basketball `home_score_reg`,
#'   `away_score_reg` (may be `NA`).
#' @param sport `"handball"` or `"basketball"`.
#' @param zero_shift Basketball's level-score shift, one per match.
#' @param ot `list(mean, sd)` of one overtime period's points.
#' @return Data frame, one row per match: `margin`, `total`, `ls_margin`,
#'   `ls_total`, `ll_result`, `p_home`, `p_draw`, `p_away`, `margin_reg`,
#'   `p_home_reg`, `p_draw_reg`, `p_away_reg`, `ll_result_reg`, `mean_m` (the
#'   expected regulation margin; for basketball overtime moves it by at most a
#'   few hundredths), `mean_t` (the expected final total).
#' @noRd
hbbb_v2_score_matches <- function(comp, obs, sport, zero_shift = 0, ot = NULL, ot_share = 5 / 40) {
  sport <- match.arg(sport, c("handball", "basketball"))
  loc_m <- comp$loc_m
  loc_t <- comp$loc_t
  sm <- comp$sigma_m
  st <- comp$sigma_t
  stopifnot(nrow(obs) == ncol(loc_m), identical(dim(loc_m), dim(loc_t)))
  m <- obs$home_score - obs$away_score
  t <- obs$home_score + obs$away_score
  pick3 <- function(k, o) log(ifelse(k > 0, o$p_home, ifelse(k < 0, o$p_away, o$p_draw)))
  base <- data.frame(margin = m, total = t, mean_m = colMeans(loc_m), mean_t = colMeans(loc_t))
  if (sport == "handball") {
    o <- v2_outcome_probs(loc_m, sm)
    ll <- pick3(m, o)
    return(cbind(base, data.frame(
      ls_margin = log(v2_integer_pmf(m, loc_m, sm)),
      ls_total = log(v2_integer_pmf(t, loc_t, st)),
      ll_result = ll, p_home = o$p_home, p_draw = o$p_draw, p_away = o$p_away,
      margin_reg = m, p_home_reg = o$p_home, p_draw_reg = o$p_draw, p_away_reg = o$p_away,
      ll_result_reg = ll
    )))
  }
  stopifnot(!is.null(ot))
  zero_shift <- rep_len(zero_shift, ncol(loc_m))
  r <- v2_basketball_result_probs(loc_m, sm, zero_shift = zero_shift, ot_share = ot_share)
  # The level regulation mass goes to overtime, shared out over the draws in
  # proportion to each draw's own level probability, as the result
  # probabilities share it (v2_basketball_result_probs()).
  zero <- rep(0, ncol(loc_m))
  p0_d <- v2_cdf_draws(zero + 0.5, loc_m, sm) - v2_cdf_draws(zero - 0.5, loc_m, sm)
  s_ot <- sm * sqrt(ot_share)
  l_ot <- loc_m * ot_share
  ot_d <- (v2_cdf_draws(m + 0.5, l_ot, s_ot) - v2_cdf_draws(m - 0.5, l_ot, s_ot)) /
    (1 - (v2_cdf_draws(zero + 0.5, l_ot, s_ot) - v2_cdf_draws(zero - 0.5, l_ot, s_ot)))
  w <- colSums(p0_d)
  p_ot <- ifelse(w > 0, colSums(p0_d * ot_d) / w, colMeans(ot_d))
  reg_part <- ifelse(m == 0, 0, v2_integer_pmf(m, loc_m, sm, zero_shift = zero_shift))
  p_t <- (1 - r$p_draw_reg) * v2_integer_pmf(t, loc_t, st) +
    r$p_draw_reg * v2_integer_pmf(t, loc_t + ot$mean, sqrt(st^2 + ot$sd^2))
  m_reg <- obs$home_score_reg - obs$away_score_reg
  o <- v2_outcome_probs(loc_m, sm, zero_shift = zero_shift)
  base$mean_t <- base$mean_t + r$p_draw_reg * ot$mean
  cbind(base, data.frame(
    ls_margin = log(reg_part + r$p_draw_reg * p_ot),
    ls_total = log(p_t),
    ll_result = log(ifelse(m > 0, r$p_home, r$p_away)),
    p_home = r$p_home, p_draw = 0, p_away = r$p_away,
    margin_reg = m_reg, p_home_reg = o$p_home, p_draw_reg = o$p_draw, p_away_reg = o$p_away,
    ll_result_reg = pick3(m_reg, o)
  ))
}

.hbbb_key <- function(d) paste(format(as.Date(d$match_date)), d$home_team, d$away_team)

#' Score a stored v2 fit against the results as they stand now.
#'
#' Kept apart from the fit so that a cached fit is always scored on today's
#' results: a game scored late, a corrected score or a regulation score that
#' arrived after the first run counts. Fixtures not yet played, and forfeits,
#' are left out.
#'
#' @param fit Output of `hbbb_v2_fit_info()`.
#' @param results The cell's results.
#' @return One row per played fixture: the match keys, `season`, `division`,
#'   `info_date` and the columns of `hbbb_v2_score_matches()`.
#' @noRd
hbbb_v2_score_fit <- function(fit, results) {
  res <- as.data.frame(results)
  res <- res[!is.na(res$home_score) & !is.na(res$away_score), , drop = FALSE]
  res <- res[!.is_forfeit(res, fit$sport), , drop = FALSE]
  at <- match(.hbbb_key(fit$fixtures), .hbbb_key(res))
  j <- which(!is.na(at))
  if (!length(j)) {
    return(NULL)
  }
  obs <- res[at[j], , drop = FALSE]
  if (!"home_score_reg" %in% names(obs)) {
    obs$home_score_reg <- NA_integer_
    obs$away_score_reg <- NA_integer_
  }
  comp <- list(
    loc_m = fit$loc_m[, j, drop = FALSE], loc_t = fit$loc_t[, j, drop = FALSE],
    sigma_m = fit$sigma_m, sigma_t = fit$sigma_t
  )
  scores <- hbbb_v2_score_matches(comp, obs, fit$sport,
    zero_shift = fit$zero_shift[j], ot = fit$ot, ot_share = fit$ot_share
  )
  keys <- obs[, c("match_date", "season", "division", "home_team", "away_team"), drop = FALSE]
  cbind(keys, info_date = fit$info_date %||% as.Date(NA), scores)
}

#' Production's stored forecasts of played matches, scored as in the report.
#'
#' A match's forecast is the newest archived fit (`beliefs/archive`) dated
#' strictly before it. Its draws are continuous: a margin's probability is
#' the share of draws within half a point (`v2_draws_pmf()`, half a count
#' added); a basketball final margin sets the draws within half a point of
#' level aside, since a final score cannot be level. The W/D/L split uses the
#' published one: the ±0.5 band for handball, the sign for basketball.
#' `p_home_band` / `p_draw_band` are the ±0.5 band for both sports (the
#' market comparison's regulation 1X2).
#'
#' @param matches Played matches: `match_date`, `home_team`, `away_team`,
#'   `home_score`, `away_score`.
#' @param lookback_days Oldest fit read, in days before the first match.
#' @return One row per match with a stored forecast: the match keys,
#'   `fit_date`, `lead_days`, `n_draws`, `ls_margin`, `ls_total`, `ll_result`,
#'   `p_home`, `p_draw`, `p_away`, `p_home_band`, `p_draw_band`, `mean_m`,
#'   `mean_t`.
#' @noRd
hbbb_production_forecasts <- function(sport, sex, matches, root = here::here("data"), lookback_days = 60L) {
  dir <- file.path(root, "beliefs", "archive", paste0("sport=", sport), "country=iceland", paste0("sex=", sex))
  parts <- list.files(dir, pattern = "^fit_date=", full.names = TRUE)
  fd <- as.Date(sub("^fit_date=", "", basename(parts)))
  keep <- fd < max(matches$match_date) & fd >= min(matches$match_date) - lookback_days
  parts <- parts[keep]
  fd <- fd[keep]
  want <- .hbbb_key(matches)
  draws <- lapply(seq_along(parts), function(i) {
    a <- as.data.frame(dplyr::collect(arrow::open_dataset(parts[i])))
    a <- a[.hbbb_key(a) %in% want & a$match_date > fd[i], c("match_date", "home_team", "away_team", "home_goals", "away_goals"), drop = FALSE]
    if (nrow(a)) a$fit_date <- fd[i]
    a
  })
  draws <- do.call(rbind, draws)
  if (is.null(draws) || !nrow(draws)) {
    return(NULL)
  }
  draws$k <- .hbbb_key(draws)
  newest <- tapply(as.numeric(draws$fit_date), draws$k, max)
  draws <- draws[as.numeric(draws$fit_date) == newest[draws$k], , drop = FALSE]
  basketball <- identical(sport, "basketball")
  rows <- lapply(split(draws, draws$k), function(d) {
    obs <- matches[match(d$k[1], want), ]
    diff <- d$home_goals - d$away_goals
    tot <- d$home_goals + d$away_goals
    m <- obs$home_score - obs$away_score
    t <- obs$home_score + obs$away_score
    band <- if (basketball) 0 else 0.5
    p_home <- mean(diff > band)
    p_away <- mean(diff < -band)
    p_draw <- 1 - p_home - p_away
    data.frame(
      match_date = obs$match_date, home_team = obs$home_team, away_team = obs$away_team,
      fit_date = d$fit_date[1], lead_days = as.numeric(obs$match_date - d$fit_date[1]), n_draws = nrow(d),
      ls_margin = log(v2_draws_pmf(diff, m, ties = !basketball)),
      ls_total = log((sum(abs(tot - t) < 0.5) + 0.5) / (nrow(d) + 1)),
      ll_result = log(if (m > 0) p_home else if (m < 0) p_away else p_draw),
      p_home = p_home, p_draw = p_draw, p_away = p_away,
      p_home_band = mean(diff > 0.5), p_draw_band = mean(abs(diff) <= 0.5),
      mean_m = mean(diff), mean_t = mean(tot)
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Which v2 fits to make: one per information date of production's forecasts.
#'
#' Production's fit dated D ran before D's games and saw results to D - 1, so
#' the v2 fit for the matches it forecast is made on the results dated up to
#' D - 1.
#'
#' @param prod Output of `hbbb_production_forecasts()`.
#' @return `list(info_date, fixtures)`: the information dates in order, and
#'   for each the fixtures (`match_date`, `home_team`, `away_team`) to
#'   forecast from it.
#' @noRd
hbbb_scorecard_plan <- function(prod) {
  info <- as.Date(prod$fit_date) - 1L
  dates <- sort(unique(info))
  list(
    info_date = dates,
    fixtures = lapply(dates, function(d) {
      f <- prod[info == d, c("match_date", "home_team", "away_team"), drop = FALSE]
      rownames(f) <- NULL
      f
    })
  )
}

#' Inputs of a v2 fit at an information date: `prepare_data()` and the spec on
#' the results dated up to `info_date`, and the given fixtures' columns among
#' the prediction rows (`j`; fixtures the fit cannot forecast, a team with no
#' result yet, are dropped).
#' @noRd
hbbb_v2_info_inputs <- function(league, sex, info_date, fixtures, root = here::here("data")) {
  info_date <- as.Date(info_date)
  horizon <- max(1L, as.integer(max(as.Date(fixtures$match_date)) - info_date))
  prep <- prepare_data(league, sex, end_date = info_date, root = root, schedule_horizon_days = horizon)
  spec <- hbbb_v2_spec(prep, league, sex, end_date = info_date, root = root)
  j <- match(.hbbb_key(fixtures), .hbbb_key(prep$pred_d))
  keep <- !is.na(j)
  list(
    prep = prep, spec = spec, info_date = info_date, j = j[keep],
    fixtures = as.data.frame(fixtures)[keep, c("match_date", "home_team", "away_team"), drop = FALSE]
  )
}

#' Cache key of a v2 fit: its exact inputs (the model's index data and
#' responses, the options, the overtime summary, the fixtures), the draw
#' count, the seed, the previous fit's sds (the gate's jump check), the code
#' of the fitting and composing files and the R-INLA version. The fixtures are
#' sorted, so their order does not matter.
#' @noRd
hbbb_v2_fit_key <- function(inputs, n_draws, seed, previous_sd, int_strategy = "auto") {
  code <- unname(tools::md5sum(here::here("R", c(
    "model-v2-hbbb.R", "v2-margin-total-inla.R", "v2-margin-total-predict.R",
    "v2-margin-total-data.R", "v2-margin-total-adapter.R"
  ))))
  inla <- if (requireNamespace("INLA", quietly = TRUE)) as.character(utils::packageVersion("INLA")) else NA_character_
  rlang::hash(.hbbb_canon(list(
    HBBB_V2_VERSION, inputs$spec$v, inputs$spec$y, inputs$spec$opts, inputs$spec$ot,
    sort(inputs$j), as.integer(n_draws), as.integer(seed), previous_sd, int_strategy, code, inla
  )))
}

# WHY: rlang::hash() hashes R's serialisation, and two identical() vectors can
# serialise differently (a compact 1:6 or a sorted wrapper against a plain
# vector), which made the key move with the order the fixtures came in. Plain
# text, numbers at full precision and names kept, is what gets hashed.
.hbbb_canon <- function(x) {
  if (is.list(x)) {
    return(lapply(x, .hbbb_canon))
  }
  if (is.null(x)) {
    return("NULL")
  }
  v <- if (is.double(x) && !inherits(x, "Date")) sprintf("%.17g", x) else paste0(x)
  if (!is.null(names(x))) v <- paste0(names(x), "=", v)
  paste0(class(x)[1], ":", v)
}

#' A v2 fit at an information date, as the production engine fits it.
#'
#' Runs the production gate and records it rather than aborting: the
#' scorecard counts what production would have refused.
#'
#' @param inputs Output of `hbbb_v2_info_inputs()`.
#' @return A list the scorer reads (`hbbb_v2_score_fit()`): `sport`,
#'   `info_date`, `fixtures`, `loc_m`, `loc_t` (draws by fixtures), `sigma_m`,
#'   `sigma_t`, `zero_shift`, `ot`, `ot_share`, and the record: `gate`,
#'   `hyper_sd`, `mode_status`, `zero_shift_table`.
#' @noRd
hbbb_v2_fit_info <- function(inputs, sport, n_draws = 4000L, seed = 1L, previous_sd = NULL,
                             int_strategy = "auto") {
  spec <- inputs$spec
  fits <- hbbb_v2_fit_halves(spec, sport, n_draws = n_draws, seed = seed, int_strategy = int_strategy)
  gate <- hbbb_v2_gate(fits, spec, sport, previous_sd = previous_sd)
  shift <- hbbb_v2_zero_shift(fits$margin, spec)
  comp <- hbbb_v2_pair(fits, spec, seed = seed)
  j <- inputs$j
  list(
    sport = sport, info_date = inputs$info_date, fixtures = inputs$fixtures,
    loc_m = comp$loc_m[, j, drop = FALSE], loc_t = comp$loc_t[, j, drop = FALSE],
    sigma_m = comp$sigma_m, sigma_t = comp$sigma_t, zero_shift = shift$per_fixture[j],
    ot = spec$ot, ot_share = spec$opts$ot_share,
    gate = gate, hyper_sd = hbbb_v2_hyper_sd(fits),
    mode_status = c(margin = fits$margin$mode_status, total = fits$total$mode_status),
    zero_shift_table = shift$table
  )
}

#' The Lengjan 1X2 line of a cell, as in the report: proportionally de-vigged
#' (`v2_devig()`), Lengjan names mapped to the federation's
#' (`normalise_lengjan_team_names()`).
#'
#' A price is pre-match when it was scraped before the kick-off where that is
#' known (`kickoff_at`), else on or before the match day. The line is the last
#' pre-match scrape that lists all three outcomes; an outcome listed twice in
#' one scrape gets the mean of its prices. A row is the cell's when its `sex`
#' says so, or, without one, when its team names carry (female) or lack
#' (male) Lengjan's " kv" suffix.
#'
#' @return Data frame: `match_date`, `home_team`, `away_team`, `q_home`,
#'   `q_draw`, `q_away`, `overround`; `NULL` when nothing is priced.
#' @noRd
hbbb_market_line <- function(league, sex, root = here::here("data")) {
  o <- tryCatch(
    as.data.frame(read_table("odds", root = root, filter = list(sport = league$sport, country = league$country))),
    error = function(e) NULL
  )
  if (is.null(o) || !nrow(o)) {
    return(NULL)
  }
  o <- o[o$market == "moneyline" & !is.na(o$odds), , drop = FALSE]
  kv <- grepl(" kv$", o$home_team) | grepl(" kv$", o$away_team)
  keep <- if (sex == "female") (is.na(o$sex) & kv) | o$sex %in% "female" else (is.na(o$sex) & !kv) | o$sex %in% "male"
  o <- o[keep, , drop = FALSE]
  if (!nrow(o)) {
    return(NULL)
  }
  o <- suppressMessages(normalise_lengjan_team_names(o, league, sex))
  kick <- if ("kickoff_at" %in% names(o)) o$kickoff_at else rep(as.POSIXct(NA), nrow(o))
  pre <- ifelse(is.na(kick), as.Date(o$scraped_at) <= o$match_date, o$scraped_at < kick)
  o <- o[pre %in% TRUE, , drop = FALSE]
  if (!nrow(o)) {
    return(NULL)
  }
  w <- o |>
    dplyr::group_by(.data$match_date, .data$home_team, .data$away_team, .data$scraped_at, .data$outcome) |>
    dplyr::summarise(odds = mean(.data$odds), .groups = "drop") |>
    tidyr::pivot_wider(names_from = "outcome", values_from = "odds")
  need <- c("home", "draw", "away")
  if (!all(need %in% names(w))) {
    return(NULL)
  }
  w <- as.data.frame(w[stats::complete.cases(w[, need]), , drop = FALSE])
  w <- w[order(.hbbb_key(w), w$scraped_at), , drop = FALSE]
  w <- w[!duplicated(.hbbb_key(w), fromLast = TRUE), , drop = FALSE]
  if (!nrow(w)) {
    return(NULL)
  }
  q <- v2_devig(w[, need])
  out <- data.frame(
    match_date = w$match_date, home_team = w$home_team, away_team = w$away_team,
    q_home = q[, 1], q_draw = q[, 2], q_away = q[, 3], overround = attr(q, "overround")
  )
  rownames(out) <- NULL
  out
}

.hbbb_groups <- function(x) {
  sports <- sort(unique(x$sport))
  c(
    split(seq_len(nrow(x)), paste(x$sport, x$sex), drop = TRUE),
    stats::setNames(lapply(sports, function(s) which(x$sport == s)), paste0(sports, ", both sexes"))
  )
}

.hbbb_se <- function(d, x, ix) {
  cluster_se_team_season(d, paste(x$home_team[ix], x$season[ix]), paste(x$away_team[ix], x$season[ix]))
}

#' v2 against production, per match, in each sex and pooled per sport.
#'
#' @param x One row per match with both forecasts: `sport`, `sex`, `season`,
#'   `home_team`, `away_team` and `v2_` / `prod_` columns `ls_margin`,
#'   `ls_total`, `ll_result`.
#' @return Data frame: `group`, `sport`, `measure`, `n`, `v2`, `production`,
#'   `diff` (v2 minus production; positive favours v2), `se` (clustered by
#'   team-season), `se_iid`.
#' @noRd
hbbb_scorecard_compare <- function(x) {
  measures <- c("margin log score" = "ls_margin", "total log score" = "ls_total", "result log likelihood" = "ll_result")
  groups <- .hbbb_groups(x)
  rows <- list()
  for (g in names(groups)) {
    ix <- groups[[g]]
    for (lab in names(measures)) {
      v <- x[[paste0("v2_", measures[[lab]])]][ix]
      p <- x[[paste0("prod_", measures[[lab]])]][ix]
      d <- v - p
      rows[[length(rows) + 1L]] <- data.frame(
        group = g, sport = x$sport[ix[1]], measure = lab, n = length(ix),
        v2 = mean(v), production = mean(p), diff = mean(d), se = .hbbb_se(d, x, ix),
        se_iid = stats::sd(d) / sqrt(length(d))
      )
    }
  }
  do.call(rbind, rows)
}

#' Each forecaster's mean expected home margin against the observed one, and
#' the root mean square errors of the expected margin and total, as the
#' report's production table shows them.
#' @noRd
hbbb_scorecard_accuracy <- function(x) {
  groups <- .hbbb_groups(x)
  rmse <- function(a, b) sqrt(mean((a - b)^2))
  do.call(rbind, lapply(names(groups), function(g) {
    ix <- groups[[g]]
    data.frame(
      group = g, n = length(ix),
      mean_m_prod = mean(x$prod_mean_m[ix]), mean_m_v2 = mean(x$v2_mean_m[ix]), mean_m_obs = mean(x$margin[ix]),
      rmse_m_prod = rmse(x$margin[ix], x$prod_mean_m[ix]), rmse_m_v2 = rmse(x$margin[ix], x$v2_mean_m[ix]),
      rmse_t_prod = rmse(x$total[ix], x$prod_mean_t[ix]), rmse_t_v2 = rmse(x$total[ix], x$v2_mean_t[ix])
    )
  }))
}

#' The scorecard's gate, per sport (both sexes pooled).
#'
#' @param cmp Output of `hbbb_scorecard_compare()`.
#' @param x The scored matches: `sport`, `season`, `home_team`, `away_team`,
#'   `margin` (observed) and `v2_mean_m`.
#' @param fits One row per v2 fit: `sport`, `gate` (`TRUE`, `FALSE`, or `NA`
#'   for a fit that failed outright).
#' @return Data frame: `sport`, `n`, `bias` (expected minus observed), `bias_se`,
#'   `passed`, `reasons`.
#' @noRd
hbbb_scorecard_gate <- function(cmp, x, fits, rules = HBBB_SCORECARD_GATE) {
  sports <- sort(unique(c(x$sport, fits$sport)))
  out <- lapply(sports, function(sp) {
    xs <- x[x$sport == sp, , drop = FALSE]
    n <- nrow(xs)
    reasons <- character()
    if (n < rules$min_matches) {
      reasons <- c(reasons, sprintf("%d scored matches, fewer than %d", n, rules$min_matches))
    }
    fs <- fits[fits$sport == sp, , drop = FALSE]
    failed <- sum(!(fs$gate %in% TRUE))
    if (failed > 0L) {
      reasons <- c(reasons, sprintf("%d of %d v2 fits failed the production gate", failed, nrow(fs)))
    }
    pooled <- cmp[cmp$group == paste0(sp, ", both sexes"), , drop = FALSE]
    for (lab in c("margin log score", "total log score")) {
      r <- pooled[pooled$measure == lab, , drop = FALSE]
      if (!nrow(r) || !is.finite(r$se)) {
        reasons <- c(reasons, sprintf("%s: no standard error against production", lab))
      } else if (r$diff < -rules$worse_se * r$se) {
        reasons <- c(reasons, sprintf(
          "%s: v2 minus production %+.4f (se %.4f), worse than production by more than one standard error",
          lab, r$diff, r$se
        ))
      }
    }
    bias <- NA_real_
    bias_se <- NA_real_
    if (n > 0L) {
      err <- xs$v2_mean_m - xs$margin
      bias <- mean(err)
      bias_se <- .hbbb_se(err, xs, seq_len(n))
      if (!is.finite(bias_se) || abs(bias) > rules$bias_se * bias_se) {
        reasons <- c(reasons, sprintf(
          "mean expected home margin %.2f against %.2f observed: off by %+.2f (se %.2f), more than one standard error",
          mean(xs$v2_mean_m), mean(xs$margin), bias, bias_se
        ))
      }
    }
    data.frame(sport = sp, n = n, bias = bias, bias_se = bias_se, passed = length(reasons) == 0L, reasons = paste(reasons, collapse = "; "))
  })
  do.call(rbind, out)
}

#' The scorecard's note, as markdown lines.
#'
#' @param meta `list(from, to, commit, n_draws, minutes)`.
#' @param market Per-cell market comparison (`group`, `n`, `period`,
#'   `market`, `production`, `v2`, `diff`, `se`) or `NULL`.
#' @param fits One row per v2 fit (`gate`).
#' @param paired The scored matches with `prod_lead_days`.
#' @noRd
hbbb_scorecard_note <- function(meta, gate, cmp, accuracy, market, fits, paired) {
  f2 <- function(x) formatC(x, format = "f", digits = 2)
  f4 <- function(x) formatC(x, format = "f", digits = 4)
  pm <- function(d, se) paste0(sprintf("%+.4f", d), " ± ", ifelse(is.finite(se), f4(se), "n/a"))
  lines <- c(
    sprintf("# Handball and basketball v2: scorecard, %s to %s", format(meta$from), format(meta$to)),
    "",
    sprintf(
      "Run %s UTC at commit %s with `Rscript scripts/hbbb_v2_scorecard.R --from %s --to %s --draws %d`; %d v2 fits, %.0f minutes (cached fits are not refitted).",
      format(Sys.time(), tz = "UTC", "%Y-%m-%d %H:%M"), meta$commit, format(meta$from), format(meta$to),
      as.integer(meta$n_draws), nrow(fits), meta$minutes
    ),
    "",
    paste(
      "A match is scored where production has a stored forecast of it (its newest fit dated before the match).",
      "v2 forecasts it from the same information: production's fit dated D saw the results to D - 1, and v2 is fitted on them,",
      "with the switch path's frozen specification. Scored as the report scored it; positive differences favour v2.",
      "Standard errors are clustered by team-season (every two matches sharing a team-season, on either side); the iid ones are in `comparison.csv`."
    ),
    "",
    "## Gate",
    "",
    "| Sport | Scored matches | Mean expected minus observed home margin (se) | Passed | Why not |",
    "|---|---|---|---|---|",
    sprintf(
      "| %s | %d | %s (%s) | %s | %s |", gate$sport, gate$n, ifelse(is.finite(gate$bias), sprintf("%+.2f", gate$bias), "n/a"),
      ifelse(is.finite(gate$bias_se), f2(gate$bias_se), "n/a"), gate$passed, gate$reasons
    ),
    "",
    "## Against production's stored forecasts",
    ""
  )
  if (is.null(cmp) || !nrow(cmp)) {
    lines <- c(lines, "No match had a stored production forecast.", "")
  } else {
    cell <- function(g, lab) {
      r <- cmp[cmp$group == g & cmp$measure == lab, ]
      pm(r$diff, r$se)
    }
    groups <- unique(cmp$group)
    n <- vapply(groups, function(g) cmp$n[cmp$group == g][1], integer(1))
    lines <- c(
      lines,
      "| Group | Matches | Margin log score: v2 − production | Total log score | Result log likelihood |",
      "|---|---|---|---|---|",
      sprintf(
        "| %s | %d | %s | %s | %s |", groups, n,
        vapply(groups, cell, character(1), lab = "margin log score"),
        vapply(groups, cell, character(1), lab = "total log score"),
        vapply(groups, cell, character(1), lab = "result log likelihood")
      ),
      "",
      "| Group | Mean expected margin: production / v2 / observed | Margin RMSE: production / v2 | Total RMSE: production / v2 |",
      "|---|---|---|---|",
      sprintf(
        "| %s | %s / %s / %s | %s / %s | %s / %s |", accuracy$group,
        f2(accuracy$mean_m_prod), f2(accuracy$mean_m_v2), f2(accuracy$mean_m_obs),
        f2(accuracy$rmse_m_prod), f2(accuracy$rmse_m_v2), f2(accuracy$rmse_t_prod), f2(accuracy$rmse_t_v2)
      ),
      "",
      sprintf("Production's forecast was %s days old at the median; v2 had the same information.", format(stats::median(paired$prod_lead_days))),
      ""
    )
  }
  lines <- c(lines, "## Against the Lengjan line", "")
  if (is.null(market) || !nrow(market)) {
    lines <- c(lines, "No scored match had a Lengjan 1X2 price.", "")
  } else {
    lines <- c(
      lines,
      "Proportionally de-vigged 1X2, the last complete scrape before kick-off. Handball is full time; basketball is regulation time, with v2's level-score shift and production's ±0.5 band.",
      "", "| Cell | Priced matches | Period | 1X2 log likelihood: market / production / v2 | v2 − market |", "|---|---|---|---|---|",
      sprintf(
        "| %s | %d | %s | %s / %s / %s | %s |", market$group, market$n, market$period,
        f4(market$market), f4(market$production), f4(market$v2), pm(market$diff, market$se)
      ), ""
    )
  }
  c(
    lines, "## Fits", "",
    sprintf(
      "%d v2 fits: %d passed the production gate, %d did not, %d failed outright. Forecasts from a fit that did not pass are not scored (production would not have published them); `fits.csv` lists each fit's breaches.",
      nrow(fits), sum(fits$gate %in% TRUE), sum(fits$gate %in% FALSE), sum(is.na(fits$gate))
    ),
    ""
  )
}

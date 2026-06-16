# R/backtest-pit.R
#' Joint-distribution forecast diagnostics (randomised PIT, draw rate,
#' scoreline residuals) over the saved posterior-predictive extracts.
#' @importFrom rlang .data
NULL

#' Derive a scalar match marginal from a home/away goal pair.
#' @param home,away Integer (or numeric) goal counts (vectorised).
#' @param marginal One of "total" (h+a), "diff" (h-a, the Skellam), "home", "away".
#' @return Numeric vector of the chosen marginal.
#' @noRd
bt_marginal_value <- function(home, away, marginal = c("total", "diff", "home", "away")) {
  marginal <- match.arg(marginal)
  switch(marginal,
    total = home + away,
    diff = home - away,
    home = home,
    away = away
  )
}

#' Discrete PIT band [F(y-1), F(y)] of an integer outcome under a pmf.
#' @param values Integer support points of the predictive pmf.
#' @param weights Non-negative weights (e.g. posterior-draw counts) per value.
#' @param y Observed integer outcome.
#' @return Length-2 numeric `c(lo, hi)`; `c(NA, NA)` if total weight is 0.
#' @noRd
bt_pit_bounds <- function(values, weights, y) {
  tot <- sum(weights)
  if (!isTRUE(tot > 0)) {
    return(c(NA_real_, NA_real_))
  }
  lo <- sum(weights[values <= y - 1]) / tot
  hi <- sum(weights[values <= y]) / tot
  c(lo, hi)
}

#' Randomised PIT for a discrete outcome (Czado-Gneiting-Held 2009).
#'
#' `u = F(y-1) + U * [F(y) - F(y-1)]`. Under a correctly specified predictive,
#' `u ~ Uniform(0, 1)`; a U-shaped histogram of `u` over many matches signals an
#' under-dispersed (over-confident) predictive, a hump signals over-dispersion.
#' @param values,weights Predictive pmf (support + draw counts).
#' @param y Observed integer outcome.
#' @param u Uniform(0,1) draw for the randomisation; injectable for tests.
#' @return Scalar randomised PIT value in `[0, 1]`.
#' @export
bt_rpit <- function(values, weights, y, u = stats::runif(1)) {
  b <- bt_pit_bounds(values, weights, y)
  b[1] + u * (b[2] - b[1])
}

#' Empty predicted-matches tibble (loader schema).
#' @noRd
bt_predicted_empty <- function() {
  tibble::tibble(
    home_team = character(), away_team = character(),
    match_date = as.Date(character()),
    home_goals = integer(), away_goals = integer(), count = integer(),
    division = character(), sex = character(), fit_date = as.Date(character())
  )
}

#' Load every saved football_iceland predicted-matches extract for a sex.
#'
#' Reads each `beliefs/extracts/.../sex=<s>/fit_date=<F>/predicted_matches.parquet`
#' (the posterior-predictive score histogram) and row-binds them, attaching `sex`
#' and `fit_date` from the hive path. Read-only.
#' @param root Data root holding `beliefs/extracts/`.
#' @param sex Character vector of sexes to load. Default both.
#' @param season Optional integer year; filters fit_dates to that season.
#' @return Tibble of all fit_dates' predicted matches, or the empty schema.
#' @export
bt_load_predicted <- function(root = here::here("data"),
                              sex = c("male", "female"), season = NULL) {
  base <- file.path(root, "beliefs", "extracts", "sport=football", "country=iceland")
  out <- list()
  for (s in sex) {
    ext_dir <- file.path(base, paste0("sex=", s))
    if (!dir.exists(ext_dir)) next
    fds <- sub("fit_date=", "", list.files(ext_dir))
    fds <- fds[grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", fds)]
    if (!is.null(season)) fds <- fds[substr(fds, 1, 4) == as.character(season)]
    for (fd in fds) {
      p <- file.path(ext_dir, paste0("fit_date=", fd), "predicted_matches.parquet")
      if (!file.exists(p)) next
      pm <- arrow::read_parquet(p)
      if (nrow(pm) == 0L) next
      pm$sex <- s
      pm$fit_date <- as.Date(fd)
      out[[length(out) + 1L]] <- pm
    }
  }
  if (length(out) == 0L) {
    return(bt_predicted_empty())
  }
  dplyr::bind_rows(out)
}

#' Restrict predicted matches to each match's leak-free as-of fit.
#'
#' Per `(sex, match)`, keeps only the rows from the most recent `fit_date`
#' STRICTLY before `match_date` -- the freshest forecast that could not have seen
#' the result. Matches with no pre-match fit are dropped.
#' @param predicted Output of [bt_load_predicted()].
#' @return `predicted` filtered to the as-of fit per match.
#' @noRd
bt_pit_asof <- function(predicted) {
  if (nrow(predicted) == 0L) {
    return(predicted)
  }
  key <- c("sex", "home_team", "away_team", "match_date")
  pre <- dplyr::filter(predicted, .data$fit_date < .data$match_date)
  if (nrow(pre) == 0L) {
    return(predicted[0, , drop = FALSE])
  }
  chosen <- pre |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::summarise(fit_date = max(.data$fit_date), .groups = "drop")
  predicted |>
    dplyr::inner_join(chosen, by = c(key, "fit_date"))
}

#' Stable per-match key (federation names, clean between extracts and results).
#' @noRd
bt_match_key <- function(d) {
  paste(d$sex, d$match_date, d$home_team, d$away_team, sep = "\r")
}

#' Empty PIT-values tibble.
#' @noRd
bt_pit_empty <- function() {
  tibble::tibble(
    sex = character(), match_date = as.Date(character()),
    home_team = character(), away_team = character(),
    division = character(), marginal = character(),
    observed = numeric(), u = numeric()
  )
}

#' Per-match leak-free randomised PIT over a chosen score marginal.
#'
#' For each match, builds the as-of predictive pmf of the marginal (`total`,
#' `diff`, `home`, `away`) from the posterior-draw counts, looks up the observed
#' value from `results`, and computes the randomised PIT ([bt_rpit()]). The match
#' key is the federation-name `(sex, match_date, home_team, away_team)`, clean
#' between extracts and results.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store (`home_score`, `away_score`, key cols).
#' @param marginal Score marginal to transform.
#' @param seed RNG seed for the PIT randomisation (reproducible).
#' @return Tibble `(sex, match_date, home_team, away_team, division, marginal,
#'   observed, u)`, one row per scored match.
#' @export
bt_pit_values <- function(predicted, results,
                          marginal = c("total", "diff", "home", "away"),
                          seed = 1L) {
  marginal <- match.arg(marginal)
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(bt_pit_empty())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  asof$mval <- bt_marginal_value(asof$home_goals, asof$away_goals, marginal)
  pmf <- asof |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "division", "mval")))) |>
    dplyr::summarise(weight = sum(.data$count), .groups = "drop")

  obs <- results
  obs$observed <- bt_marginal_value(obs$home_score, obs$away_score, marginal)
  obs <- obs[, c(key, "observed"), drop = FALSE]

  matches <- pmf |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key, "division")))) |>
    dplyr::inner_join(obs, by = key)
  if (nrow(matches) == 0L) {
    return(bt_pit_empty())
  }

  pmf$.k <- bt_match_key(pmf)
  pmf_by <- split(pmf, pmf$.k)
  mk <- bt_match_key(matches)
  withr::with_seed(seed, {
    matches$u <- vapply(seq_len(nrow(matches)), function(i) {
      sub <- pmf_by[[mk[i]]]
      bt_rpit(sub$mval, sub$weight, matches$observed[i])
    }, numeric(1))
  })
  matches$marginal <- marginal
  tibble::as_tibble(matches[, c(key, "division", "marginal", "observed", "u")])
}

#' Uniformity summary of PIT values (KS test against Uniform(0,1)).
#'
#' A calibrated predictive yields `u ~ Uniform(0,1)`; a small `ks_p` (or a
#' visibly U-/hump-shaped histogram) flags miscalibration of the predictive
#' distribution's shape.
#' @param pit Tibble with a numeric `u` column (e.g. from [bt_pit_values()]).
#' @param by Optional grouping columns.
#' @return One row (or per group) of `(n, ks_stat, ks_p, mean_u)`.
#' @export
bt_pit_uniformity <- function(pit, by = NULL) {
  one <- function(d) {
    u <- d$u[is.finite(d$u)]
    if (length(u) < 2L) {
      return(tibble::tibble(n = length(u), ks_stat = NA_real_, ks_p = NA_real_, mean_u = mean(u)))
    }
    k <- suppressWarnings(stats::ks.test(u, "punif"))
    tibble::tibble(n = length(u), ks_stat = unname(k$statistic), ks_p = k$p.value, mean_u = mean(u))
  }
  if (nrow(pit) == 0L) {
    return(tibble::tibble(n = integer(), ks_stat = numeric(), ks_p = numeric(), mean_u = numeric()))
  }
  if (is.null(by)) {
    return(one(pit))
  }
  pit |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::group_modify(~ one(.x)) |>
    dplyr::ungroup()
}

#' Predicted vs observed draw rate, with the (observed - predicted) gap.
#'
#' Predicted draw probability per match = as-of `P(home_goals == away_goals)`
#' from the draw counts; observed = the realised draw indicator. A persistent
#' positive gap (model under-predicts draws) is the canonical signal for a
#' Dixon-Coles low-score correction or a bivariate-Poisson correlation term.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store.
#' @param by Optional grouping columns (e.g. `c("sex", "division")`).
#' @return `(<by..>, n, predicted_draw_rate, observed_draw_rate, gap)`.
#' @export
bt_draw_rate <- function(predicted, results, by = "sex") {
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(tibble::tibble())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  per_match <- asof |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, "division")))) |>
    dplyr::summarise(
      p_draw = sum(.data$count[.data$home_goals == .data$away_goals]) / sum(.data$count),
      .groups = "drop"
    )
  obs <- results
  obs$obs_draw <- as.numeric(obs$home_score == obs$away_score)
  obs <- obs[, c(key, "obs_draw"), drop = FALSE]
  joined <- dplyr::inner_join(per_match, obs, by = key)
  if (nrow(joined) == 0L) {
    return(tibble::tibble())
  }
  joined |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted_draw_rate = mean(.data$p_draw),
      observed_draw_rate = mean(.data$obs_draw),
      gap = mean(.data$obs_draw) - mean(.data$p_draw),
      .groups = "drop"
    )
}

#' Observed-minus-predicted scoreline frequencies on the (home, away) goal grid.
#'
#' Predicted cell frequency = mean over matches of (per-match `count / total`);
#' observed = the share of matches that landed exactly on that cell. Off-diagonal
#' vs diagonal structure in the residual separates a correlation fault
#' (Dixon-Coles / lambda3) from a marginal/dispersion one.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store.
#' @param by Optional grouping columns.
#' @param max_goals Cap the grid (scores above fold into the top cell). Default 6.
#' @return `(<by..>, home_goals, away_goals, predicted_freq, observed_freq, residual)`.
#' @export
bt_scoreline_residuals <- function(predicted, results, by = "sex", max_goals = 6L) {
  asof <- bt_pit_asof(predicted)
  if (nrow(asof) == 0L) {
    return(tibble::tibble())
  }
  key <- c("sex", "match_date", "home_team", "away_team")
  cap <- function(x) pmin(as.integer(x), max_goals)
  matched_keys <- dplyr::inner_join(
    dplyr::distinct(asof, dplyr::across(dplyr::all_of(key))),
    dplyr::distinct(results[, key, drop = FALSE]),
    by = key
  )
  pm <- dplyr::semi_join(asof, matched_keys, by = key)
  res <- dplyr::semi_join(results, matched_keys, by = key)
  if (nrow(pm) == 0L) {
    return(tibble::tibble())
  }
  pm$home_goals <- cap(pm$home_goals)
  pm$away_goals <- cap(pm$away_goals)
  pred <- pm |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(key, by, "home_goals", "away_goals")))) |>
    dplyr::summarise(cell_count = sum(.data$count), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key))) |>
    dplyr::mutate(cell_p = .data$cell_count / sum(.data$cell_count)) |>
    dplyr::ungroup() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "home_goals", "away_goals")))) |>
    dplyr::summarise(predicted_freq = mean(.data$cell_p), .groups = "drop")
  res$home_goals <- cap(res$home_score)
  res$away_goals <- cap(res$away_score)
  obs <- res |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(.n = dplyr::n()) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "home_goals", "away_goals")))) |>
    dplyr::summarise(observed_freq = dplyr::n() / dplyr::first(.data$.n), .groups = "drop")
  dplyr::full_join(pred, obs, by = c(by, "home_goals", "away_goals")) |>
    dplyr::mutate(
      predicted_freq = dplyr::coalesce(.data$predicted_freq, 0),
      observed_freq = dplyr::coalesce(.data$observed_freq, 0),
      residual = .data$observed_freq - .data$predicted_freq
    )
}

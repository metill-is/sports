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

#' Load every saved predicted-matches extract for one sport/country and sex.
#'
#' Reads each
#' `beliefs/extracts/sport=<sport>/country=<country>/sex=<s>/fit_date=<F>/predicted_matches.parquet`
#' (the posterior-predictive score histogram) and row-binds them, attaching `sex`
#' and `fit_date` from the hive path. Read-only.
#'
#' Every diagnostic in this file is built on the LONG-FORM extract shape: one row
#' per `(home_goals, away_goals)` cell carrying a posterior-draw `count`, i.e. a
#' joint score pmf. Football writes that shape. The 2DT sports (handball,
#' basketball) currently write a PRUNED extract holding only
#' `goal_diff_distribution` plus outcome probabilities, from which no scoreline
#' marginal can be formed -- so pointing this loader at them is a data gap, not a
#' code path. Abort naming the file rather than row-bind a foreign schema and
#' fail three calls later on a missing `home_goals`.
#' @param root Data root holding `beliefs/extracts/`.
#' @param sex Character vector of sexes to load. Default both.
#' @param season Optional integer year; filters fit_dates to that season.
#' @param sport,country Scalar hive partition the extracts sit under. The
#'   defaults keep every existing caller on `football`/`iceland`.
#' @return Tibble of all fit_dates' predicted matches, or the empty schema.
#' @export
bt_load_predicted <- function(root = here::here("data"),
                              sex = c("male", "female"), season = NULL,
                              sport = "football", country = "iceland") {
  if (length(sport) != 1L || length(country) != 1L) {
    cli::cli_abort("{.arg sport} and {.arg country} must each name one partition.")
  }
  base <- file.path(
    root, "beliefs", "extracts",
    paste0("sport=", sport), paste0("country=", country)
  )
  need <- c("home_goals", "away_goals", "count")
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
      absent <- setdiff(need, names(pm))
      if (length(absent) > 0L) {
        cli::cli_abort(c(
          "{.path {p}} is not a long-form predicted-matches extract.",
          x = "Missing column{?s}: {.field {absent}}.",
          i = "The PIT, draw-rate and scoreline diagnostics need the \\
               per-scoreline posterior-draw histogram; the pruned 2DT extract \\
               carries only a goal-difference distribution."
        ))
      }
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
    observed = numeric(), lo = numeric(), hi = numeric(), u = numeric()
  )
}

#' Per-match leak-free randomised PIT over a chosen score marginal.
#'
#' For each match, builds the as-of predictive pmf of the marginal (`total`,
#' `diff`, `home`, `away`) from the posterior-draw counts, looks up the observed
#' value from `results`, and returns BOTH the deterministic PIT band
#' `[lo, hi] = [F(y-1), F(y)]` ([bt_pit_bounds()]) and one randomised draw from
#' inside it ([bt_rpit()]). The match key is the federation-name
#' `(sex, match_date, home_team, away_team)`, clean between extracts and results.
#'
#' `u` is a single seeded realisation, fit for plotting a histogram but NOT for
#' deciding a verdict: a pass/fail read off one randomisation is decided by
#' `seed`, not by the model. `lo`/`hi` carry the randomisation-free content so
#' [bt_pit_uniformity()] can average its statistics over many draws instead.
#' @param predicted Output of [bt_load_predicted()].
#' @param results Results store (`home_score`, `away_score`, key cols).
#' @param marginal Score marginal to transform.
#' @param seed RNG seed for the displayed `u` randomisation (reproducible).
#' @return Tibble `(sex, match_date, home_team, away_team, division, marginal,
#'   observed, lo, hi, u)`, one row per scored match.
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
  # Carry the band, not just a point inside it. The band is a fact about the
  # predictive; which point of it `u` lands on is a coin toss, and a verdict
  # computed from one toss is a property of `seed`. Downstream re-randomises
  # from (lo, hi) many times and averages -- see bt_pit_uniformity().
  bands <- vapply(seq_len(nrow(matches)), function(i) {
    sub <- pmf_by[[mk[i]]]
    bt_pit_bounds(sub$mval, sub$weight, matches$observed[i])
  }, numeric(2))
  matches$lo <- bands[1L, ]
  matches$hi <- bands[2L, ]
  # One runif(n) draws the same stream as n successive runif(1) calls, so the
  # displayed `u` is unchanged from the per-match bt_rpit() loop this replaces.
  withr::with_seed(seed, {
    matches$u <- matches$lo + stats::runif(nrow(matches)) * (matches$hi - matches$lo)
  })
  matches$marginal <- marginal
  tibble::as_tibble(
    matches[, c(key, "division", "marginal", "observed", "lo", "hi", "u")]
  )
}

#' Empty uniformity-summary tibble (the verdict schema).
#' @noRd
bt_pit_uniformity_empty <- function() {
  tibble::tibble(
    n = integer(), ks_stat = numeric(), ks_p = numeric(), mean_u = numeric(),
    var_u = numeric(), var_z = numeric(), var_p = numeric(),
    n_rand = integer(), verdict = character()
  )
}

#' Uniformity verdict for PIT values (KS + a dispersion test, min-n guarded).
#'
#' A calibrated predictive yields `u ~ Uniform(0,1)`. This is the only numeric
#' calibration verdict the dashboard ships, so it reports three complementary
#' statistics rather than one, and never returns a bare `NA` that a reader can
#' mistake for "fine":
#'
#' * `mean_u` — location. Detects a biased predictive (systematically too high
#'   or too low), around 0.5 when unbiased.
#' * `ks_stat` / `ks_p` — Kolmogorov-Smirnov against `Uniform(0,1)`. General
#'   purpose, but driven by the largest CDF gap, so it is comparatively weak
#'   against a symmetric U-shape whose CDF tracks the diagonal in the middle.
#' * `var_u` / `var_z` / `var_p` — dispersion, and the statistic with real power
#'   against precisely the over-confidence this file exists to detect. Under
#'   uniformity `Var(u) = 1/12` and the sample variance is asymptotically normal
#'   with `sd = sqrt(1 / (180 n))` (from `mu4 - sigma^4 = 1/80 - 1/144 = 1/180`).
#'   `var_z > 0` means the PIT histogram is U-shaped: too much mass in the
#'   tails, i.e. an UNDER-dispersed, over-confident predictive. `var_z < 0`
#'   means a central hump, i.e. an over-dispersed one.
#'
#' Randomisation: the discrete PIT only pins each match to a band
#' `[lo, hi]`, so any statistic computed from one draw inside those bands is
#' itself random. When `pit` carries `lo`/`hi` (as [bt_pit_values()] emits) the
#' bands are re-randomised `n_rand` times, seeded and so reproducible, without
#' the verdict being decided by the seed. Each statistic is summarised over those
#' draws in the way that keeps it self-consistent:
#'
#' * `mean_u` is not randomised at all. The limit the draws converge on is the
#'   mean of the band midpoints, so that is reported directly -- exact, and free
#'   of both seed and Monte Carlo error.
#' * `var_u` is the MEAN over draws, with Monte Carlo error falling like
#'   `1 / sqrt(n_rand)`; `var_z`/`var_p` are derived FROM it, so the trio agrees
#'   by construction.
#' * `ks_stat`/`ks_p` come from ONE representative draw: the realisation whose
#'   statistic sits nearest the median. Averaging the two independently would
#'   report a p-value that is not the p-value of the reported statistic -- the
#'   old pair could not both be true. Since the KS p-value is a strictly
#'   decreasing function of the statistic at fixed `n`, a draw central in the
#'   statistic is equally central in the p-value, so this pair is a genuine test
#'   result AND both halves are still a central summary over the draws (with an
#'   even `n_rand` it is one of the two middle order statistics rather than the
#'   interpolated median). Far steadier than the `n_rand = 1` seed lottery.
#'
#' Given only a `u` column the function degenerates to that single realisation
#' (`n_rand = 1`) and the old seed-dependence applies; prefer passing the bands.
#'
#' @param pit Tibble with a numeric `u` column, and ideally the `lo`/`hi` PIT
#'   band columns from [bt_pit_values()].
#' @param by Optional grouping columns.
#' @param min_n Minimum usable PIT values before any test statistic is reported.
#'   Below it the row comes back flagged `"insufficient_data"` with the KS and
#'   dispersion statistics `NA`, rather than silently unstable numbers. `mean_u`
#'   still reports: it is a plain location summary, unbiased for 0.5 under
#'   uniformity at any `n`, and needs none of the large-sample approximation the
#'   others rest on. Default 30.
#' @param n_rand Randomisation draws to summarise over when `lo`/`hi` are
#'   present. Default 50. Reported back as `n_rand`, which is 0 on the
#'   `"insufficient_data"` row because no draw was needed there.
#' @param alpha Two-sided level at which `ks_p`/`var_p` set `verdict`.
#'   Default 0.05.
#' @param seed RNG seed for the randomisation draws (reproducible).
#' @return One row (or per group) of `(n, ks_stat, ks_p, mean_u, var_u, var_z,
#'   var_p, n_rand, verdict)`. `verdict` is `"insufficient_data"` (n below
#'   `min_n`), `"flagged"` (some test rejects uniformity at `alpha`) or `"ok"`
#'   -- `"ok"` meaning no evidence AGAINST calibration at this sample size, not
#'   evidence of calibration.
#' @export
bt_pit_uniformity <- function(pit, by = NULL, min_n = 30L, n_rand = 50L,
                              alpha = 0.05, seed = 1L) {
  min_n <- max(2L, as.integer(min_n))
  one <- function(d) {
    # Represent the single-`u` fallback as a degenerate band lo == hi == u, so
    # both paths run identical code and the fallback reproduces the old result
    # exactly (a draw inside a zero-width band is its endpoint).
    if (all(c("lo", "hi") %in% names(d))) {
      keep <- is.finite(d$lo) & is.finite(d$hi)
      lo <- d$lo[keep]
      hi <- d$hi[keep]
      draws <- max(1L, as.integer(n_rand))
    } else {
      lo <- hi <- d$u[is.finite(d$u)]
      draws <- 1L
    }
    n <- length(lo)

    # `mean_u` needs no randomisation: E[u_i] is the band midpoint, so averaging
    # mean(u) over draws just converges on the mean of the midpoints. Report that
    # limit directly -- exact, seed-free, and on the degenerate lo == hi fallback
    # identical to mean(u).
    mean_u <- if (n > 0L) mean((lo + hi) / 2) else NA_real_

    # A calibration verdict is the evidence that any of these forecasts are any
    # good; on a thin sample there IS no verdict. Returning NA here let a reader
    # take "no answer" for "nothing wrong" -- name the reason instead. `mean_u`
    # survives the guard: it is a moment, not a test, unbiased for 0.5 under
    # uniformity at any n, whereas the KS test and the whole variance arm lean on
    # a large-n approximation that a thin sample does not support.
    if (n < min_n) {
      cli::cli_alert_warning(
        "bt_pit_uniformity: {n} usable PIT value{?s} (< min_n = {min_n}); \\
         reporting verdict = insufficient_data (mean_u only)."
      )
      return(tibble::tibble(
        n = n, ks_stat = NA_real_, ks_p = NA_real_, mean_u = mean_u,
        var_u = NA_real_, var_z = NA_real_, var_p = NA_real_,
        n_rand = 0L, verdict = "insufficient_data"
      ))
    }

    stats_r <- withr::with_seed(seed, {
      vapply(seq_len(draws), function(r) {
        u <- lo + stats::runif(n) * (hi - lo)
        k <- suppressWarnings(stats::ks.test(u, "punif"))
        c(ks_stat = unname(k$statistic), ks_p = k$p.value, var_u = stats::var(u))
      }, numeric(3))
    })

    # Report the KS pair from ONE realisation, not as two independent means: a
    # mean p-value is not the p-value of a mean statistic, and the old pair could
    # not both be true. The KS p-value is strictly decreasing in the statistic at
    # fixed n, so the draw central in the statistic is equally central in p --
    # the pair is internally consistent AND both halves stay a central summary
    # over `draws`, far steadier than the n_rand = 1 seed lottery.
    ks_r <- stats_r["ks_stat", ]
    j <- which.min(abs(ks_r - stats::median(ks_r)))
    ks_stat <- ks_r[[j]]
    ks_p <- stats_r["ks_p", j]

    # The dispersion arm keeps the mean: var_u is a moment, and var_z/var_p are
    # computed FROM the averaged var_u rather than averaged alongside it.
    var_u <- mean(stats_r["var_u", ])
    var_z <- (var_u - 1 / 12) / sqrt(1 / (180 * n))
    var_p <- 2 * stats::pnorm(-abs(var_z))
    # na.rm + isTRUE so a degenerate KS p-value cannot turn the comparison into
    # NA and abort the whole dashboard render inside `if`.
    p_min <- suppressWarnings(min(c(ks_p, var_p), na.rm = TRUE))
    tibble::tibble(
      n = n, ks_stat = ks_stat, ks_p = ks_p,
      mean_u = mean_u, var_u = var_u,
      var_z = var_z, var_p = var_p, n_rand = draws,
      verdict = if (isTRUE(p_min < alpha)) "flagged" else "ok"
    )
  }
  if (nrow(pit) == 0L) {
    return(bt_pit_uniformity_empty())
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

# Helpers for the handball / basketball v2 switch path (R/model-v2-hbbb.R).

# The committed facts fixture, with regulation-time scores on the basketball
# rows: equal to the final score, except `ot_rows` rows per cell that went to
# overtime (level after regulation, five points short of the lower final
# score) and rows whose final score is level, which a real basketball result
# cannot be and which therefore get none. `drop_reg` removes the regulation
# scores of that many of each cell's latest rows. `schedule_season` relabels
# every fixture's season (a season the training rows have not reached);
# `schedule_edit` takes and returns the schedules data frame.
fixture_hbbb_root <- function(env = parent.frame(), ot_rows = 3L, drop_reg = 0L,
                              schedule_season = NULL, schedule_edit = identity) {
  tmp <- withr::local_tempdir(.local_envir = env)
  results <- as.data.frame(arrow::read_parquet(testthat::test_path("fixtures", "facts", "results.parquet")))
  schedules <- arrow::read_parquet(testthat::test_path("fixtures", "facts", "schedules.parquet"))
  results$home_score_reg <- NA_integer_
  results$away_score_reg <- NA_integer_
  bb <- which(results$sport == "basketball")
  ok <- bb[results$home_score[bb] != results$away_score[bb]]
  results$home_score_reg[ok] <- as.integer(results$home_score[ok])
  results$away_score_reg[ok] <- as.integer(results$away_score[ok])
  for (cell in split(ok, results$sex[ok])) {
    cell <- cell[order(results$match_date[cell])]
    ot <- cell[seq_len(min(ot_rows, length(cell)))]
    level <- pmax(pmin(results$home_score[ot], results$away_score[ot]) - 5L, 0L)
    results$home_score_reg[ot] <- as.integer(level)
    results$away_score_reg[ot] <- as.integer(level)
    if (drop_reg > 0L) {
      gone <- utils::tail(cell, drop_reg)
      results$home_score_reg[gone] <- NA_integer_
      results$away_score_reg[gone] <- NA_integer_
    }
  }
  write_table(tibble::as_tibble(results), "results", root = tmp)
  if (!is.null(schedule_season)) schedules$season <- as.integer(schedule_season)
  schedules <- tibble::as_tibble(schedule_edit(as.data.frame(schedules)))
  write_table(schedules, "schedules", root = tmp)
  tmp
}

# prepare_data() and the v2 spec for one fixture cell.
hbbb_fixture_spec <- function(sport, sex = "male", root) {
  league <- load_leagues()[[paste0(sport, "_iceland")]]
  prep <- suppressMessages(prepare_data(league, sex, end_date = FIXTURE_END_DATE, root = root))
  spec <- hbbb_v2_spec(prep, league, sex, end_date = FIXTURE_END_DATE, root = root)
  list(league = league, prep = prep, spec = spec)
}

# Joint draws in the shape fit_v2_inla(latent =) returns, for every node the
# spec asks for: centred near a plausible league (scoring level by sport, home
# advantage 1), sds `sigma` (margin, total). `const_sigma` pins them.
synthetic_v2_samples <- function(spec, sport, n_draws = 40L, seed = 11L,
                                 sigma = if (sport == "basketball") c(12, 16) else c(5, 6),
                                 const_sigma = FALSE) {
  v <- spec$v
  level <- if (sport == "basketball") 165 else 55
  size <- function(nm) {
    switch(nm,
      u_s = v$K * v$NS,
      b = v$K,
      ha = v$K,
      w_t = v$TS * v$W,
      s_s = v$ND * v$NS,
      1L
    )
  }
  withr::with_seed(seed, list(
    margin = synthetic_half(v, spec$latent$margin, n_draws, level, sigma[1], const_sigma),
    total = synthetic_half(v, spec$latent$total, n_draws, level, sigma[2], const_sigma)
  ))
}

# One half's draws: every node `sel` names (0 = all of a component).
synthetic_half <- function(v, sel, n_draws, level, sigma, const_sigma = FALSE) {
  size <- function(nm) {
    switch(nm,
      u_s = v$K * v$NS, b = v$K, ha = v$K, w_t = v$TS * v$W, s_s = v$ND * v$NS,
      1L
    )
  }
  lat <- lapply(names(sel), function(nm) {
    nodes <- if (all(sel[[nm]] == 0L)) seq_len(size(nm)) else as.integer(sel[[nm]])
    centre <- if (startsWith(nm, "alpha")) level else if (nm == "hfa") 1 else 0
    m <- matrix(centre + stats::rnorm(n_draws * length(nodes), 0, 1.5), n_draws)
    colnames(m) <- nodes
    m
  })
  names(lat) <- names(sel)
  list(latent = lat, sigma = if (const_sigma) rep(sigma, n_draws) else abs(stats::rnorm(n_draws, sigma, 0.2)))
}

# A stand-in for fit_v2_inla() with fit_v2_inla()'s arguments, returning what
# it returns from synthetic draws, so the code around R-INLA runs without it.
# The sport is read off the priors' scoring level.
fake_v2_inla_fit <- function(v, response, priors, ..., n_samples = 0L, latent = NULL, seed = 1L) {
  big <- priors$total_mean > 100
  level <- if (big) 165 else 55
  sigma <- if (response == "margin") (if (big) 12 else 5) else (if (big) 16 else 6)
  smp <- withr::with_seed(seed, synthetic_half(v, latent, as.integer(n_samples), level, sigma))
  sds <- c("sd: the Gaussian observations" = sigma, "sd: u_s" = 2, "rho: u_s" = 0.7, "sd: b" = 3)
  if (!is.null(latent$w_t)) sds <- c(sds, "sd: w_t" = 5)
  sds <- c(sds, if (response == "margin") c("sd: ha" = 0.4) else c("sd: s_s" = 1))
  centre <- if (response == "margin") 1 else level
  list(
    response = response, mode_status = 0L,
    hyper_sd = data.frame(parameter = names(sds), mean = unname(sds), q05 = unname(sds) / 2, q95 = unname(sds) * 2),
    sigma2 = sigma^2,
    fixed = if (response == "margin") data.frame(mean = 1, row.names = "hfa") else data.frame(mean = rep(level, v$ND)),
    pred = data.frame(mean = rep(centre, v$N_pred), sd = rep(1, v$N_pred)),
    fitted = data.frame(mean = rep(centre, v$N), sd = rep(1, v$N)),
    samples = smp
  )
}

# An adapted fit from synthetic draws, with the record fit_model_v2_hbbb()
# attaches, so fit_league() and the extractors can run on it without R-INLA.
synthetic_hbbb_fit <- function(spec, prep, sport, n_draws = 40L, seed = 11L) {
  s <- synthetic_v2_samples(spec, sport, n_draws = n_draws, seed = seed)
  comp <- hbbb_v2_compose(s$margin, s$total, spec)
  fit <- hbbb_v2_as_fit(comp, spec, prep, sport, seed = seed)
  meta <- list(
    engine = "inla", version = HBBB_V2_VERSION,
    gate = list(passed = TRUE, breaches = character(), evaluated = 3L, skipped = 1L, jump_checked = FALSE),
    hyper_sd = c("margin sd: b" = 2, "total sd: b" = 3),
    n_obs = as.integer(prep$stan_data$N),
    n_obs_regulation = if (sport == "basketball") sum(spec$observed) else NA_integer_
  )
  fit[names(meta)] <- meta
  fit
}

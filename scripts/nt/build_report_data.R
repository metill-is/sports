#!/usr/bin/env Rscript
# Collect the national-team experiment outputs into one compact file that the
# report (docs/reports/2026-11-15-national-team-v2.qmd) renders from.
#
# Usage: Rscript scripts/nt/build_report_data.R <experiment out dir> <results.rds>

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
out_dir <- args[[1]]
target <- args[[2]]
dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
read_if <- function(path) if (file.exists(path)) readRDS(path) else NULL

spec <- nt_tournaments()
matches <- list()
scores <- list()
fits <- list()
production <- list()
for (key in spec$key) {
  ev <- read_if(file.path(out_dir, "eval", paste0("eval_", key, ".rds")))
  if (is.null(ev)) next
  m <- ev$matches
  matches[[key]] <- tibble::tibble(
    key = key, label = spec$label[spec$key == key], match_id = seq_len(nrow(m)),
    match_date = m$match_date, home_team = m$home_team, away_team = m$away_team,
    stage = m$stage, home_90 = m$home_90, away_90 = m$away_90,
    home_score = m$home_score, away_score = m$away_score,
    venue = as.integer(!m$neutral), extra_time = m$extra_time
  )
  for (mdl in names(ev$scores)) {
    sc <- ev$scores[[mdl]]
    scores[[paste(key, mdl)]] <- tibble::tibble(
      key = key, match_id = seq_len(nrow(m)), model = mdl, sc
    )
  }
  for (mdl in setdiff(names(ev$fits), "production")) {
    f <- ev$fits[[mdl]]
    fits[[paste(key, mdl)]] <- tibble::tibble(
      key = key, model = mdl, mlik = f$mlik, kappa = f$kappa,
      seconds = f$total_seconds, n_matches = f$settings$n_matches,
      n_teams = f$settings$n_teams, n_periods = f$settings$n_periods,
      hyper = list(f$hyper), fixed = list(f$fixed)
    )
  }
  if (!is.null(ev$fits$production)) {
    p <- ev$fits$production
    production[[key]] <- tibble::tibble(
      key = key, n_train = p$n_train, window_start = p$window_start,
      n_divergent = p$metrics$n_divergent, max_rhat = p$metrics$max_rhat,
      min_ess_bulk = p$metrics$min_ess_bulk
    )
  }
}
matches <- do.call(rbind, matches)
scores <- do.call(rbind, scores)

# Extra time: what the three tournaments show against the simulator's recipe.
ko <- merge(
  matches[matches$stage == "knockout", ],
  scores[scores$model == "base", c("key", "match_id", "p_draw", "eg_home", "eg_away")]
)
level_prob <- function(a, b) .nt_poisson_compare(a, b)$draw
et <- ko[ko$extra_time, ]
extra_time <- list(
  n_knockout = nrow(ko),
  n_extra_time = nrow(et),
  expected_level_at_90 = sum(ko$p_draw),
  n_shootout = sum(et$home_score == et$away_score),
  expected_shootouts_third = sum(ko$p_draw * level_prob(ko$eg_home / 3, ko$eg_away / 3)),
  expected_shootouts_given_et = sum(level_prob(et$eg_home / 3, et$eg_away / 3)),
  et_goals = sum(et$home_score + et$away_score - et$home_90 - et$away_90),
  expected_et_goals_third = sum(et$eg_home + et$eg_away) / 3
)

# The panel may have been run as several workers, one output file each.
wf_files <- list.files(
  file.path(out_dir, "wf"), pattern = "^walk_forward\\.rds$",
  recursive = TRUE, full.names = TRUE
)
wf_raw <- NULL
for (f in wf_files) {
  part <- readRDS(f)
  for (ck in names(part)) {
    if (is.null(part[[ck]]$matches)) next
    if (is.null(wf_raw[[ck]])) {
      wf_raw[[ck]] <- part[[ck]]
    } else {
      stopifnot(identical(
        wf_raw[[ck]]$matches[, c("match_date", "home_team", "away_team")],
        part[[ck]]$matches[, c("match_date", "home_team", "away_team")]
      ))
      new_models <- setdiff(names(part[[ck]]$scores), names(wf_raw[[ck]]$scores))
      wf_raw[[ck]]$scores[new_models] <- part[[ck]]$scores[new_models]
      wf_raw[[ck]]$hyper[new_models] <- part[[ck]]$hyper[new_models]
    }
  }
}
wf <- NULL
wf_hyper <- NULL
if (!is.null(wf_raw)) {
  wf <- do.call(rbind, lapply(names(wf_raw), function(ck) {
    x <- wf_raw[[ck]]
    if (is.null(x$matches)) {
      return(NULL)
    }
    do.call(rbind, lapply(names(x$scores), function(mdl) {
      s <- x$scores[[mdl]]
      tibble::tibble(
        cutoff = as.Date(ck), match_date = x$matches$match_date,
        tournament = x$matches$tournament,
        friendly = x$matches$tournament == "Friendly",
        in_target = x$matches$in_target, neutral = x$matches$neutral,
        home_score = x$matches$home_score, away_score = x$matches$away_score,
        model = mdl,
        p_home = s$p_home, p_draw = s$p_draw, p_away = s$p_away,
        eg_home = s$eg_home, eg_away = s$eg_away,
        log_1x2 = s$log_1x2, rps = s$rps
      )
    }))
  }))
  wf_hyper <- do.call(rbind, lapply(names(wf_raw), function(ck) {
    h <- wf_raw[[ck]]$hyper$base
    if (is.null(h)) {
      return(NULL)
    }
    data.frame(cutoff = as.Date(ck), h)
  }))
}

# The men's tournament fits used the qualifier-only team filter; the filter was
# later broadened for the women's file. Where the two differ (Vanuatu, before
# 2025) the base fit was repeated to confirm the scores do not move.
filter_check <- do.call(rbind, lapply(c("wc2022", "euro2024"), function(key) {
  wide <- read_if(file.path(out_dir, "eval_wide", paste0("eval_", key, ".rds")))
  narrow <- read_if(file.path(out_dir, "eval", paste0("eval_", key, ".rds")))
  if (is.null(wide) || is.null(wide$scores$base)) {
    return(NULL)
  }
  a <- narrow$scores$base
  b <- wide$scores$base
  data.frame(
    key = key,
    log_1x2_qualifier_rule = mean(a$log_1x2), log_1x2_broad_rule = mean(b$log_1x2),
    max_abs_prob_diff = max(abs(as.matrix(a[, 1:3]) - as.matrix(b[, 1:3]))),
    n_teams_qualifier_rule = narrow$fits$base$settings$n_teams,
    n_teams_broad_rule = wide$fits$base$settings$n_teams
  )
}))

stan <- list(
  base = read_if(file.path(out_dir, "stan", "stan_reference_wc2026_base.rds")),
  damped = read_if(file.path(out_dir, "stan", "stan_reference_wc2026_damped.rds"))
)
stan <- lapply(stan, function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x$summary <- as.data.frame(x$summary[!grepl("^theta_now", x$summary$variable), ])
  x
})

res <- list(
  built_at = Sys.time(),
  matches = matches,
  scores = scores,
  fits = do.call(rbind, fits),
  production = do.call(rbind, production),
  extra_time = extra_time,
  walk_forward = wf,
  walk_forward_hyper = wf_hyper,
  simulation = read_if(file.path(out_dir, "sim_compare_wc2026.rds")),
  production_forecast = read_if(file.path(out_dir, "prod_forecast_wc2026.rds")),
  women = read_if(file.path(out_dir, "women_check.rds")),
  filter_check = filter_check,
  current = list(
    men = read_if(file.path(out_dir, "current_men.rds")),
    women = read_if(file.path(out_dir, "current_women.rds"))
  ),
  stan = stan
)
saveRDS(res, target, compress = "xz")
cat(sprintf(
  "wrote %s (%.0f kB): %d matches, models: %s; walk-forward rows: %s\n",
  target, file.size(target) / 1024, nrow(matches),
  paste(unique(scores$model), collapse = ", "),
  if (is.null(wf)) "none" else nrow(wf)
))

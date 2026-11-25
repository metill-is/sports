# How large an error in one engine's margin would breach the limits?
# docs/evidence/hbbb-engine-reference/README.md, "What the limits catch".
# From INLA's own probabilities in the evidence fixtures: each fixture's
# effective predictive sd from P(home) and P(away) of the rounded Gaussian
# (1 / (qnorm(1 - p_home) - qnorm(p_away))). One engine's margin is shifted
# -- every fixture by delta, or one team's fixtures by delta (+ at home, -
# away) -- and the smallest delta (step 0.05) that breaches is found, with
# the means' limits alone and with the single-fixture limits too. The
# engines' measured gap is left out, so a real error breaches sooner.
# Run from the repository root:
#   Rscript docs/evidence/hbbb-engine-reference/diagnostics/detection.R
options(width = 160)
ev <- file.path("docs", "evidence", "hbbb-engine-reference")
lim <- list(
  in_season = c(result = 0.01, line = 0.025, result_max = 0.04, line_max = 0.075),
  across_break = c(result = 0.02, line = 0.05, result_max = 0.06, line_max = 0.14)
)
probs <- function(mu, s, line) {
  ph <- 1 - pnorm((0.5 - mu) / s)
  pa <- pnorm((-0.5 - mu) / s)
  cbind(ph, 1 - ph - pa, pa, 1 - pnorm((line - mu) / s))
}
breach <- function(fx, shift, regime, use_max) {
  l <- lim[[regime]]
  mu0 <- fx$inla_mean_margin
  s <- fx$sd
  mu1 <- mu0 + shift
  line <- floor((mu0 + mu1) / 2) + 0.5
  d <- abs(probs(mu0, s, line) - probs(mu1, s, line))
  res <- d[, 1:3, drop = FALSE]
  sp <- d[, 4]
  b <- mean(res) > l[["result"]] || mean(sp) > l[["line"]]
  if (use_max) b <- b || max(res) > l[["result_max"]] || max(sp) > l[["line_max"]]
  b
}
min_shift <- function(fx, shift_of, regime, use_max) {
  for (delta in seq(0.05, 40, by = 0.05)) if (breach(fx, shift_of(delta), regime, use_max)) {
    return(delta)
  }
  NA
}
cells <- list(
  c("2026-03-16", "in_season"), c("2026-10-09", "in_season"), c("2026-11-24", "in_season"),
  c("2026-07-01", "across_break")
)
out <- list()
for (cl in cells) {
  for (f in list.files(file.path(ev, cl[1]), "^fixtures_(handball|basketball)_", full.names = TRUE)) {
    fx <- read.csv(f)
    cell <- sub("^fixtures_(.*)\\.csv$", "\\1", basename(f))
    # Women's basketball at 2026-10-09 had one team without a match of the new
    # season: across the break by hbbb_v2_across_break().
    regime <- if (grepl("basketball_female", cell) && cl[1] == "2026-10-09") "across_break" else cl[2]
    fx$sd <- 1 / (qnorm(1 - fx$inla_home) - qnorm(fx$inla_away))
    all_mean <- min_shift(fx, function(d) rep(d, nrow(fx)), regime, FALSE)
    all_max <- min_shift(fx, function(d) rep(d, nrow(fx)), regime, TRUE)
    teams <- sort(unique(c(fx$home_team, fx$away_team)))
    one <- sapply(teams, function(tm) {
      sh <- function(d) d * ((fx$home_team == tm) - (fx$away_team == tm))
      c(mean_only = min_shift(fx, sh, regime, FALSE), with_max = min_shift(fx, sh, regime, TRUE))
    })
    out[[length(out) + 1]] <- data.frame(
      date = cl[1], cell = cell, regime = regime, n = nrow(fx), median_sd = round(median(fx$sd), 1),
      all_mean_only = all_mean, all_with_max = all_max,
      team_mean_only_med = median(one["mean_only", ], na.rm = TRUE), team_mean_only_worst = max(one["mean_only", ], na.rm = TRUE),
      team_with_max_med = median(one["with_max", ], na.rm = TRUE), team_with_max_worst = max(one["with_max", ], na.rm = TRUE)
    )
  }
}
res <- do.call(rbind, out)
print(res, row.names = FALSE)
write.csv(res, file.path(ev, "diagnostics", "detection.csv"), row.names = FALSE)

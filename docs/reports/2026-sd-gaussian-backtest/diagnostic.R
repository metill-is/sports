#!/usr/bin/env Rscript
# Read-only second-moment diagnostic for the (S, D) = (total, signed diff)
# football model (design: docs/superpowers/specs/2026-06-29-sd-gaussian-backtest-design.md).
#
# Tests whether the mean-induced Poisson backbone holds:
#   Var(S|match) = E[S],  Var(D|match) = E[S],  Cov(S,D|match) = E[D].
# Conditional means come from a lightweight per-season independent-Poisson
# (Maher) fit; we then measure departures (overdispersion phi, covariance
# multiplier psi, tail kurtosis) to set the covariance form + priors.
#
# Run from the repo root:  Rscript docs/reports/2026-sd-gaussian-backtest/diagnostic.R

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
options(width = 120)
set.seed(1)

ROOT <- here::here()
OUT <- here::here("docs", "reports", "2026-sd-gaussian-backtest")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

theme_set(
  tryCatch(metill::theme_metill(), error = function(e) theme_minimal(base_size = 12))
)

results <- open_dataset(file.path(ROOT, "data/facts/results")) |>
  filter(sport == "football", country == "iceland") |>
  select(sex, season, match_date, home_team, away_team, home_score, away_score) |>
  collect() |>
  filter(!is.na(home_score), !is.na(away_score)) |>
  mutate(season = as.integer(season))

# Per-season independent-Poisson (Maher) conditional means lambda_h, lambda_a.
fit_lambdas <- function(df) {
  out <- vector("list", 0L)
  for (yr in sort(unique(df$season))) {
    d <- df[df$season == yr, , drop = FALSE]
    if (nrow(d) < 30L) next
    long <- bind_rows(
      tibble(scored = d$home_score, att = d$home_team, def = d$away_team, home = 1, .mi = seq_len(nrow(d)), side = "h"),
      tibble(scored = d$away_score, att = d$away_team, def = d$home_team, home = 0, .mi = seq_len(nrow(d)), side = "a")
    )
    m <- glm(scored ~ att + def + home, family = poisson(), data = long)
    long$lam <- fitted(m)
    wide <- long |>
      select(.mi, side, lam) |>
      pivot_wider(names_from = side, values_from = lam)
    d$lambda_h <- wide$h[match(seq_len(nrow(d)), wide$.mi)]
    d$lambda_a <- wide$a[match(seq_len(nrow(d)), wide$.mi)]
    out[[as.character(yr)]] <- d
  }
  bind_rows(out)
}

analyse <- function(df, sex_label) {
  d <- fit_lambdas(df) |>
    mutate(
      S = home_score + away_score,
      D = home_score - away_score,
      ES = lambda_h + lambda_a, # Poisson: E[S]
      ED = lambda_h - lambda_a, # Poisson: E[D]
      rS = S - ES,
      rD = D - ED
    )

  # Overall Pearson-style scalars (Poisson null = 1 for each).
  phi_S <- sum(d$rS^2) / sum(d$ES) # Var(S) vs E[S]
  phi_D <- sum(d$rD^2) / sum(d$ES) # Var(D) vs E[S]  (Poisson predicts E[S], NOT E[D])
  psi <- sum(d$rS * d$rD) / sum(d$ED) # Cov(S,D) vs E[D]
  zS <- d$rS / sqrt(d$ES)
  zD <- d$rD / sqrt(d$ES)
  kurt <- function(x) mean((x - mean(x))^4) / (mean((x - mean(x))^2))^2 # 3 = normal

  cat(sprintf(
    "\n================  %s  (N = %d, %d seasons)  ================\n",
    sex_label, nrow(d), length(unique(d$season))
  ))
  cat(sprintf("  phi_S = Var(S)/E[S]   = %.3f   (Poisson null 1.0)\n", phi_S))
  cat(sprintf("  phi_D = Var(D)/E[S]   = %.3f   (Poisson null 1.0)\n", phi_D))
  cat(sprintf("  psi   = Cov(S,D)/E[D] = %.3f   (Poisson null 1.0)\n", psi))
  cat(sprintf("  resid corr(S,D)       = %.3f\n", cor(d$rS, d$rD)))
  cat(sprintf("  kurtosis  z_S = %.2f   z_D = %.2f   (normal = 3.0)\n", kurt(zS), kurt(zD)))
  cat(sprintf("  P(draw) observed      = %.3f\n", mean(d$D == 0)))

  binstat <- function(d, xvar, nb = 10) {
    d$.x <- d[[xvar]]
    d$.b <- dplyr::ntile(d$.x, nb)
    d |>
      group_by(.b) |>
      summarise(
        x = mean(.x), var_S = mean(rS^2), var_D = mean(rD^2),
        cov_SD = mean(rS * rD), n = dplyr::n(), .groups = "drop"
      )
  }
  bS <- binstat(d, "ES")
  bD <- binstat(d, "ED")

  pois_line <- geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50")
  fit_line <- geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, colour = "#c0392b")
  p1 <- ggplot(bS, aes(x, var_S)) +
    pois_line +
    fit_line +
    geom_point(aes(size = n)) +
    labs(title = "Var(S | match) vs E[S]", subtitle = "dashed = Poisson (y=x); red = phi_S", x = "E[S]", y = "mean r_S^2") +
    guides(size = "none")
  p2 <- ggplot(bS, aes(x, var_D)) +
    pois_line +
    fit_line +
    geom_point(aes(size = n)) +
    labs(title = "Var(D | match) vs E[S]", subtitle = "dashed = Poisson; red = phi_D", x = "E[S]", y = "mean r_D^2") +
    guides(size = "none")
  p3 <- ggplot(bD, aes(x, cov_SD)) +
    pois_line +
    fit_line +
    geom_point(aes(size = n)) +
    labs(title = "Cov(S,D | match) vs E[D]", subtitle = "dashed = Poisson (Cov=E[D]); red = psi", x = "E[D]", y = "mean r_S * r_D") +
    guides(size = "none")
  qqdf <- bind_rows(
    tibble(theo = qnorm(ppoints(length(zS))), samp = sort(zS), which = "z_S"),
    tibble(theo = qnorm(ppoints(length(zD))), samp = sort(zD), which = "z_D")
  )
  p4 <- ggplot(qqdf, aes(theo, samp, colour = which)) +
    geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
    geom_point(size = 0.5, alpha = 0.5) +
    labs(title = "Normal QQ of standardised residuals", subtitle = "tails off the line = heavier-than-Gaussian", x = "theoretical normal quantile", y = "standardised residual", colour = NULL)

  panel <- patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2) +
    patchwork::plot_annotation(title = sprintf("(S, D) second-moment diagnostic - football iceland %s", sex_label))
  fn <- file.path(OUT, sprintf("sd_diag_%s.png", sex_label))
  ragg::agg_png(fn, width = 1400, height = 1000, res = 130)
  print(panel)
  dev.off()
  cat(sprintf("  [plot] %s\n", fn))

  tibble(
    sex = sex_label, n = nrow(d), phi_S = phi_S, phi_D = phi_D, psi = psi,
    resid_corr = cor(d$rS, d$rD), kurt_S = kurt(zS), kurt_D = kurt(zD), draw = mean(d$D == 0)
  )
}

summ <- bind_rows(
  analyse(results[results$sex == "male", ], "male"),
  analyse(results[results$sex == "female", ], "female")
)
cat("\n================  SUMMARY  ================\n")
print(as.data.frame(summ), digits = 3)

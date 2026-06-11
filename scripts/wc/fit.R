#!/usr/bin/env Rscript
# Fit the (unchanged) bivariate-Poisson football model on international results
# for the 2026 World Cup forecast. Exploratory run: diagnostics observed, not
# gated, so we can see how the RW model behaves on sparse international data.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(dplyr))
options(width = 120)

league <- list(sport = "football", country = "world")
prep <- prepare_data(league,
  sex = "male", end_date = Sys.Date(),
  schedule_horizon_days = 0L
)
sd <- prep$stan_data
cat(sprintf(
  "stan_data: K=%d  N=%d  N_rounds=%d  N_pred=%d\n",
  sd$K, sd$N, sd$N_rounds, sd$N_pred
))

stan_path <- here::here(
  "Stan", "football_iceland", "bivariate_poisson_no_inflation.stan"
)

t0 <- Sys.time()
fit <- fit_model(
  sd, stan_path,
  method = "sample",
  chains = 4L, parallel_chains = 4L,
  iter_warmup = 750L, iter_sampling = 750L,
  adapt_delta = 0.95, seed = 42L,
  check_diagnostics = FALSE, show_progress = TRUE
)
cat(sprintf("fit time: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

cat("\n--- diagnostics ---\n")
m <- extract_stan_metrics(fit)
print(m[c(
  "n_divergent", "div_frac", "max_rhat", "min_ess_bulk",
  "min_ess_tail", "treedepth_frac", "min_ebfmi"
)])

fit_dir <- here::here("data", "wc", "fit")
dir.create(fit_dir, showWarnings = FALSE, recursive = TRUE)
fit$save_object(file.path(fit_dir, "fit.rds"))
saveRDS(prep$teams, file.path(fit_dir, "teams.rds"))

cat("\n--- team ratings (offence + defence, neutral) ---\n")
si <- .extract_sim_inputs_pfi(fit, prep$teams)
ratings <- si$team |>
  group_by(team) |>
  summarise(
    rating = mean(cur_offense + cur_defense),
    offence = mean(cur_offense),
    defence = mean(cur_defense),
    .groups = "drop"
  ) |>
  arrange(desc(rating))
cat("TOP 16:\n")
print(as.data.frame(head(ratings, 16)), digits = 3)
cat("\nBOTTOM 10:\n")
print(as.data.frame(tail(ratings, 10)), digits = 3)
saveRDS(si, file.path(fit_dir, "sim_inputs.rds"))
cat("\nSaved fit + sim_inputs to", fit_dir, "\n")

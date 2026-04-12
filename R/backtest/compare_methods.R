#### Inference Method Comparison ####
#
# Compares MCMC (sample), Pathfinder, and Variational inference for a given
# league. Evaluates posterior quality, prediction accuracy, and betting impact.
#
# Usage:
#   cd Sports
#   Rscript R/backtest/compare_methods.R --league basketball_iceland --sex male
#   Rscript R/backtest/compare_methods.R --league handball_iceland --sex male --iter 500
#   Rscript R/backtest/compare_methods.R --league football_iceland --sex male

library(tidyverse)
library(here)
library(cmdstanr)
library(posterior)
here::i_am(".here")

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

#### CLI Arguments ####
args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default) {
  idx <- match(flag, args)
  if (is.na(idx) || idx >= length(args)) {
    return(default)
  }
  args[idx + 1]
}

league_key <- get_arg("--league", "basketball_iceland")
sex <- get_arg("--sex", "male")
iter <- as.integer(get_arg("--iter", "1000"))
chains <- as.integer(get_arg("--chains", "4"))
num_paths <- as.integer(get_arg("--paths", "4"))
draws <- as.integer(get_arg("--draws", "4000"))

cat(sprintf("\n=== Inference Method Comparison ===\n"))
cat(sprintf("League: %s | Sex: %s\n", league_key, sex))
cat(sprintf(
  "MCMC: %d chains x %d iter | Pathfinder: %d paths x %d draws\n\n",
  chains, iter, num_paths, draws
))

#### Load league config and prepare data ####
box::use(R / pipeline / config[load_leagues])
leagues <- load_leagues(here("config", "leagues.yml"), include_inactive = TRUE)
league <- leagues[[league_key]]

if (is.null(league)) stop("Unknown league: ", league_key)

# Capture sports_dir before any withr::with_dir changes here context
sports_dir <- here()

# Prepare stan data based on pipeline type
stan_data <- switch(league$pipeline,
  shared = {
    config_path <- file.path(sports_dir, league$config_module)
    env <- new.env(parent = globalenv())
    source(config_path, local = env)
    config <- env$get_config()
    env2 <- new.env(parent = globalenv())
    source(file.path(sports_dir, "R", "shared", "prep_data.R"), local = env2)
    suppressMessages(env2$prepare_data(config, sex, Sys.Date()))
  },
  football = {
    withr::with_dir(file.path(sports_dir, league$dir), {
      suppressMessages(here::i_am(league$rproj %||% ".here"))
      env <- new.env(parent = globalenv())
      source(file.path(sports_dir, "R", "shared", "prep_data_football.R"), local = env)
      suppressMessages(env$prepare_football_data(sex))
    })
  },
  handball_other = {
    withr::with_dir(file.path(sports_dir, "handball", "other"), {
      suppressMessages(here::i_am("handball_other.Rproj"))
      env <- new.env(parent = globalenv())
      source(here::here("R", "utils", "prep_data.R"), local = env)
      suppressMessages(env$prepare_data(league$country, sex, Sys.Date()))
    })
  },
  stop("Unknown pipeline: ", league$pipeline)
)

# Reset here context
here::i_am(".here")

# Resolve Stan model path
stan_model_path <- switch(league$pipeline,
  shared = here(league$dir, "Stan", league$stan_model),
  football = here(league$dir, "Stan", league$stan_model),
  handball_other = here("handball", "other", "Stan", "2d_student_t.stan")
)

cat(sprintf("Stan model: %s\n", basename(stan_model_path)))
cat(sprintf(
  "Data: N=%d matches, K=%d teams, N_pred=%d predictions\n\n",
  stan_data$N, stan_data$K, stan_data$N_pred
))

#### Compile model (shared across methods) ####
cat("Compiling model... ")
model <- cmdstan_model(stan_model_path)
cat("OK\n\n")

#### Run each method ####
methods <- c("sample", "pathfinder", "variational")
results <- list()
timings <- list()

for (m in methods) {
  cat(sprintf("--- Running %s ---\n", toupper(m)))
  t0 <- proc.time()

  tryCatch(
    {
      if (m == "sample") {
        fit <- model$sample(
          data = stan_data,
          chains = chains,
          parallel_chains = chains,
          iter_warmup = iter,
          iter_sampling = iter,
          init = 0,
          refresh = 0,
          show_messages = FALSE,
          show_exceptions = FALSE
        )
        results[[m]] <- fit
      } else if (m == "pathfinder") {
        approx <- model$pathfinder(
          data = stan_data,
          num_paths = num_paths,
          draws = draws,
          init = 0
        )
        results[[paste0(m, "_raw")]] <- approx
        pf_draws <- posterior::as_draws_matrix(approx$draws())
        gq <- model$generate_quantities(
          fitted_params = pf_draws,
          data = stan_data
        )
        # Validate GQ produced usable draws
        test_draws <- gq$draws("goals1_pred")
        results[[m]] <- gq
      } else if (m == "variational") {
        approx <- model$variational(
          data = stan_data,
          algorithm = "fullrank",
          draws = draws,
          init = 0
        )
        results[[paste0(m, "_raw")]] <- approx
        gq <- model$generate_quantities(
          fitted_params = approx,
          data = stan_data
        )
        test_draws <- gq$draws("goals1_pred")
        results[[m]] <- gq
      }
      elapsed <- (proc.time() - t0)[["elapsed"]]
      timings[[m]] <- elapsed
      cat(sprintf("  Completed in %.1fs\n\n", elapsed))
    },
    error = function(e) {
      elapsed <- (proc.time() - t0)[["elapsed"]]
      timings[[m]] <<- NA
      cat(sprintf("  FAILED after %.1fs: %s\n\n", elapsed, conditionMessage(e)))
    }
  )
}

#### Extract predictions ####
cat("=== Extracting posterior predictions ===\n\n")

extract_preds <- function(fit, method_name) {
  pred_draws <- as_draws_df(fit$draws(c("goals1_pred", "goals2_pred")))

  pred_draws |>
    as_tibble() |>
    pivot_longer(
      -c(.draw, .chain, .iteration),
      names_to = "parameter",
      values_to = "value"
    ) |>
    mutate(
      type = if_else(str_detect(parameter, "goals1"), "home_goals", "away_goals"),
      game_nr = str_match(parameter, "\\[(.*)\\]$")[, 2] |> as.numeric()
    ) |>
    select(.draw, type, game_nr, value) |>
    pivot_wider(names_from = type, values_from = value) |>
    mutate(method = method_name)
}

all_preds <- list()
for (m in methods) {
  if (!is.null(results[[m]])) {
    all_preds[[m]] <- extract_preds(results[[m]], m)
  }
}

if (length(all_preds) < 2) {
  cat("Only MCMC produced valid predictions. Approximate methods failed.\n")
  cat("This model's posterior geometry is incompatible with normal approximations.\n\n")

  # Still report timing
  cat("=== Timing Results ===\n\n")
  cat(sprintf("   %-15s %10s %10s\n", "Method", "Seconds", "Status"))
  cat(sprintf("   %s\n", strrep("-", 37)))
  for (m in methods) {
    t <- timings[[m]]
    status <- if (m %in% names(all_preds)) "OK" else if (!is.na(t)) "GQ failed" else "FAILED"
    cat(sprintf("   %-15s %10s %10s\n", m, if (!is.na(t)) sprintf("%.1f", t) else "-", status))
  }

  # Report Pathfinder diagnostics if available
  if (!is.null(results[["pathfinder_raw"]])) {
    cat("\n   Pathfinder Pareto k: likely >> 0.7 (approximation unreliable)\n")
    cat("   The model's covariance constraints cannot be captured by normal approximations.\n")
  }

  cat("\nDone.\n")
  quit(status = 0)
}

preds <- bind_rows(all_preds)

#### Comparison metrics ####
cat("=== Comparison Results ===\n\n")

# 1. Timing
cat("1. TIMING\n")
cat(sprintf("   %-15s %10s %10s\n", "Method", "Seconds", "Speedup"))
cat(sprintf("   %s\n", strrep("-", 37)))
mcmc_time <- timings[["sample"]]
for (m in methods) {
  t <- timings[[m]]
  if (!is.na(t)) {
    speedup <- if (m == "sample") "1.0x" else sprintf("%.1fx", mcmc_time / t)
    cat(sprintf("   %-15s %10.1f %10s\n", m, t, speedup))
  } else {
    cat(sprintf("   %-15s %10s %10s\n", m, "FAILED", "-"))
  }
}
cat("\n")

# 2. Prediction means and SDs per match
cat("2. PREDICTION ACCURACY (mean goals per match)\n")
pred_summary <- preds |>
  group_by(method, game_nr) |>
  summarise(
    mean_home = mean(home_goals),
    mean_away = mean(away_goals),
    sd_home = sd(home_goals),
    sd_away = sd(away_goals),
    .groups = "drop"
  )

mcmc_summary <- pred_summary |> filter(method == "sample")

for (m in setdiff(methods, "sample")) {
  if (!m %in% names(all_preds)) next
  m_summary <- pred_summary |> filter(method == m)
  comp <- inner_join(mcmc_summary, m_summary, by = "game_nr", suffix = c("_mcmc", paste0("_", m)))

  mean_diff_home <- mean(abs(comp[[paste0("mean_home_", m)]] - comp$mean_home_mcmc))
  mean_diff_away <- mean(abs(comp[[paste0("mean_away_", m)]] - comp$mean_away_mcmc))
  sd_ratio_home <- mean(comp[[paste0("sd_home_", m)]] / comp$sd_home_mcmc)
  sd_ratio_away <- mean(comp[[paste0("sd_away_", m)]] / comp$sd_away_mcmc)

  cat(sprintf("   %s vs sample:\n", m))
  cat(sprintf("     Mean abs diff (home): %.3f goals\n", mean_diff_home))
  cat(sprintf("     Mean abs diff (away): %.3f goals\n", mean_diff_away))
  cat(sprintf("     SD ratio (home): %.3f (1.0 = equal, <1 = underestimates)\n", sd_ratio_home))
  cat(sprintf("     SD ratio (away): %.3f\n", sd_ratio_away))
  cat("\n")
}

# 3. Win/draw/loss probabilities
cat("3. OUTCOME PROBABILITIES\n")
outcome_probs <- preds |>
  group_by(method, game_nr) |>
  summarise(
    p_home_win = mean(home_goals > away_goals),
    p_draw = mean(abs(home_goals - away_goals) < 0.5),
    p_away_win = mean(home_goals < away_goals),
    .groups = "drop"
  )

mcmc_probs <- outcome_probs |> filter(method == "sample")

for (m in setdiff(methods, "sample")) {
  if (!m %in% names(all_preds)) next
  m_probs <- outcome_probs |> filter(method == m)
  comp <- inner_join(mcmc_probs, m_probs, by = "game_nr", suffix = c("_mcmc", paste0("_", m)))

  max_diff_hw <- max(abs(comp[[paste0("p_home_win_", m)]] - comp$p_home_win_mcmc))
  max_diff_d <- max(abs(comp[[paste0("p_draw_", m)]] - comp$p_draw_mcmc))
  max_diff_aw <- max(abs(comp[[paste0("p_away_win_", m)]] - comp$p_away_win_mcmc))
  mean_diff_hw <- mean(abs(comp[[paste0("p_home_win_", m)]] - comp$p_home_win_mcmc))

  cat(sprintf("   %s vs sample:\n", m))
  cat(sprintf("     Mean |diff| home win prob: %.4f (%.1f pp)\n", mean_diff_hw, mean_diff_hw * 100))
  cat(sprintf("     Max  |diff| home win:  %.4f (%.1f pp)\n", max_diff_hw, max_diff_hw * 100))
  cat(sprintf("     Max  |diff| draw:      %.4f (%.1f pp)\n", max_diff_d, max_diff_d * 100))
  cat(sprintf("     Max  |diff| away win:  %.4f (%.1f pp)\n", max_diff_aw, max_diff_aw * 100))

  # Check acceptance criteria
  pass <- max(max_diff_hw, max_diff_d, max_diff_aw) < 0.02
  cat(sprintf("     Acceptance (<2pp): %s\n", if (pass) "PASS" else "FAIL"))
  cat("\n")
}

# 4. Pathfinder PSIS diagnostics
if (!is.null(results[["pathfinder_raw"]])) {
  cat("4. PATHFINDER DIAGNOSTICS\n")
  pf <- results[["pathfinder_raw"]]
  tryCatch(
    {
      lp_draws <- as_draws_df(pf$draws("lp_approx__"))
      # Check for Pareto k via the diagnostic_summary if available
      if ("diagnostic_summary" %in% names(pf)) {
        diag <- pf$diagnostic_summary()
        cat(sprintf("   Diagnostic summary:\n"))
        print(diag)
      }

      # Manual PSIS check on log-weight ratios
      if ("lp__" %in% pf$metadata()$stan_variables &&
        "lp_approx__" %in% pf$metadata()$stan_variables) {
        lp <- as.numeric(as_draws_df(pf$draws("lp__"))$lp__)
        lp_approx <- as.numeric(as_draws_df(pf$draws("lp_approx__"))$lp_approx__)
        log_ratios <- lp - lp_approx
        psis_result <- loo::psis(log_ratios, r_eff = 1)
        pareto_k <- psis_result$diagnostics$pareto_k
        cat(sprintf("   PSIS Pareto k: %.3f\n", pareto_k))
        cat(sprintf(
          "   Interpretation: %s\n",
          if (pareto_k < 0.5) {
            "GOOD (reliable)"
          } else if (pareto_k < 0.7) {
            "OK (marginally reliable)"
          } else {
            "BAD (unreliable — consider MCMC)"
          }
        ))
        cat(sprintf("   Acceptance (k < 0.7): %s\n", if (pareto_k < 0.7) "PASS" else "FAIL"))
      }
    },
    error = function(e) {
      cat(sprintf("   Could not extract diagnostics: %s\n", conditionMessage(e)))
    }
  )
  cat("\n")
}

# 5. Posterior interval coverage (check SD calibration via prediction intervals)
cat("5. PREDICTION INTERVAL WIDTHS\n")
interval_widths <- preds |>
  group_by(method, game_nr) |>
  summarise(
    q05_home = quantile(home_goals, 0.05),
    q95_home = quantile(home_goals, 0.95),
    q05_away = quantile(away_goals, 0.05),
    q95_away = quantile(away_goals, 0.95),
    .groups = "drop"
  ) |>
  mutate(
    width_home = q95_home - q05_home,
    width_away = q95_away - q05_away
  )

width_summary <- interval_widths |>
  group_by(method) |>
  summarise(
    mean_width_home = mean(width_home),
    mean_width_away = mean(width_away),
    .groups = "drop"
  )

mcmc_widths <- width_summary |> filter(method == "sample")
cat(sprintf("   %-15s %15s %15s\n", "Method", "90% CI home", "90% CI away"))
cat(sprintf("   %s\n", strrep("-", 47)))
for (m in methods) {
  w <- width_summary |> filter(method == m)
  if (nrow(w) == 0) next
  suffix <- ""
  if (m != "sample") {
    ratio_h <- w$mean_width_home / mcmc_widths$mean_width_home
    ratio_a <- w$mean_width_away / mcmc_widths$mean_width_away
    suffix <- sprintf("  (%.2f / %.2f ratio)", ratio_h, ratio_a)
  }
  cat(sprintf(
    "   %-15s %15.2f %15.2f%s\n",
    m, w$mean_width_home, w$mean_width_away, suffix
  ))
}
cat("\n")

# 6. Overall verdict
cat("=== VERDICT ===\n\n")
for (m in setdiff(methods, "sample")) {
  if (!m %in% names(all_preds)) {
    cat(sprintf("   %s: FAILED (did not produce results)\n", m))
    next
  }
  speedup <- if (!is.na(timings[[m]])) sprintf("%.1fx faster", mcmc_time / timings[[m]]) else "N/A"
  cat(sprintf("   %s: %s than MCMC\n", m, speedup))
  cat(sprintf("   Review the metrics above to determine if quality is acceptable.\n"))
  cat(sprintf(
    "   To use: Rscript run.R --league %s --method %s --step fit,results\n\n",
    league_key, m
  ))
}

cat("Done.\n")

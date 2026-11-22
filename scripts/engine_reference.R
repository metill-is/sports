#!/usr/bin/env Rscript
# scripts/engine_reference.R -- the weekly Stan reference fit (decision 5 of
# the football cutover plan, docs/reports/2026-11-15-football-v2-proposal.qmd).
#
# For each football cell: the v2.0 specification on the latest data, as a
# production fit builds it, fitted by R-INLA as fit_league() would fit it that
# day (its seed, full integration, one thread, then its gate; on one thread a
# refit of the same data reproduces the production fit) and by NUTS with the
# Stan reference program
# (research/football-v2/stan/), then the two engines' outcome probabilities
# compared fixture by fixture (R/model-v2-reference.R). One summary row per
# cell goes to data/health/engine_reference/, which check_engine_reference()
# reads; a mean absolute difference above 0.01 there is a health warning.
#
# Usage:
#   Rscript scripts/engine_reference.R
#   Rscript scripts/engine_reference.R --sex female --out /tmp/reference
#
# --out writes each cell's fixture-by-fixture comparison and summary as CSV
# (the workflow uploads them). Exits non-zero when a cell could not be fitted;
# a cell that was fitted but did not pass is a health warning, not an error.
# Needs R-INLA, sn, cmdstanr and CmdStan. Run from the repo root.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("research/football-v2/engines.R"))

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i)) args[[i + 1L]] else default
}
sexes <- flag("sex", c("male", "female"))
out_dir <- flag("out")
end_date <- as.Date(flag("end-date", format(Sys.Date())))
if (!is.null(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

league <- load_leagues()[["football_iceland"]]
stan_dir <- here::here("research", "football-v2", "stan")
stan_file <- file.path(stan_dir, fv2_stan_program(list(opts = football_v2_options())))
exe_dir <- file.path(tempdir(), "fv2-reference-exe")
dir.create(exe_dir, showWarnings = FALSE)
seed <- as.integer(format(end_date, "%Y%m%d"))

provenance <- list(
  inla_version = as.character(utils::packageVersion("INLA")),
  cmdstan_version = as.character(cmdstanr::cmdstan_version()),
  stan_md5 = unname(tools::md5sum(stan_file))
)

# As fit_league() fits on that day: its seed (keyed on the cell and the fit
# date), full integration, one thread, 4,000 draws, then its gate against the
# cell's previous INLA fit, when there is one.
fit_inla_for <- function(sex) {
  seed <- .fit_seed(league$sport, league$country, sex, format(end_date))
  function(spec) {
    f <- suppressWarnings(fv2_fit_inla(spec, int_strategy = "auto", n_samp = 4000L, threads = "1:1", seed = seed))
    gate <- fv2_inla_gate(f, spec, previous_sd = fv2_previous_hyper(league, sex, end_date, root = here::here("data")))
    f$gate_passed <- isTRUE(gate$passed)
    if (!f$gate_passed) cli::cli_alert_warning("{sex}: INLA gate: {paste(gate$breaches, collapse = '; ')}")
    f
  }
}
fit_nuts <- function(spec) {
  csv_dir <- tempfile("fv2-reference-csv-")
  dir.create(csv_dir)
  on.exit(unlink(csv_dir, recursive = TRUE), add = TRUE)
  fv2_fit_stan(spec, fv2_stan_file(spec, stan_dir), exe_dir, csv_dir, seed = seed, refresh = 0L)
}

cli::cli_h1("Weekly Stan reference fit, football v2.0, data to {end_date}")
failed <- character()
for (sex in sexes) {
  # One cell's failure, in the fits or the write, must not cost the other.
  res <- tryCatch(
    {
      res <- fv2_weekly_reference(
        league, sex,
        end_date = end_date, root = here::here("data"),
        fit_inla = fit_inla_for(sex), fit_nuts = fit_nuts, provenance = provenance
      )
      write_table(res$row, "engine_reference", root = here::here("data"))
      res
    },
    error = function(e) {
      cli::cli_alert_danger("{sex}: {conditionMessage(e)}")
      NULL
    }
  )
  if (is.null(res)) {
    failed <- c(failed, sex)
    next
  }
  if (!is.null(out_dir)) {
    utils::write.csv(res$fixtures, file.path(out_dir, sprintf("fixtures_%s.csv", sex)), row.names = FALSE)
    utils::write.csv(res$row, file.path(out_dir, sprintf("summary_%s.csv", sex)), row.names = FALSE)
  }
  r <- res$row
  cli::cli_alert_info(sprintf(
    "%s: mean |p_INLA - p_NUTS| = %.4f, largest %.4f over %d fixtures (%s); NUTS %d divergences, Rhat %.3f; INLA mode status %d",
    sex, r$mean_abs_diff, r$max_abs_diff, r$n_fixtures, r$fixtures,
    r$nuts_divergences, r$nuts_max_rhat, r$inla_mode_status
  ))
  if (!isTRUE(r$passed)) {
    # A GitHub annotation on the run; the health check carries the warning.
    cat(sprintf("::warning::football %s: the weekly reference did not pass (mean |p_INLA - p_NUTS| = %.4f, threshold %.2f)\n", sex, r$mean_abs_diff, r$threshold))
  }
}

if (length(failed)) {
  cli::cli_alert_danger("No reference for: {failed}")
  quit(save = "no", status = 1L)
}

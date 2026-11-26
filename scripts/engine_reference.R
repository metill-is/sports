#!/usr/bin/env Rscript
# scripts/engine_reference.R -- the weekly Stan reference fit: decision 5 of
# the football cutover plan (docs/reports/2026-11-15-football-v2-proposal.qmd)
# and the shadow period's reference check for handball and basketball v2
# (docs/reports/2026-11-15-hb-bb-v2/report.md).
#
# For each cell: the v2 inputs on the latest data, as a production fit builds
# them, fitted by R-INLA as fit_league() would fit them that day (its seed,
# full integration, one thread, then its gate; on one thread a refit of the
# same data reproduces the production fit) and by NUTS with the Stan reference
# programs (football: research/football-v2/stan/; handball and basketball:
# docs/reports/2026-11-15-hb-bb-v2/stan/), then the two engines' outcome
# probabilities compared fixture by fixture (R/model-v2-reference.R,
# R/model-v2-hbbb-reference.R). One summary row per cell goes to
# data/health/engine_reference/, which check_engine_reference() reads; a run
# that did not pass is a health warning there.
#
# Usage:
#   Rscript scripts/engine_reference.R
#   Rscript scripts/engine_reference.R --sport handball,basketball --sex female --out /tmp/reference
#   Rscript scripts/engine_reference.R --end-date 2026-03-15 --no-write --out /tmp/reference
#
# --out writes each cell's fixture-by-fixture comparison and summary as CSV
# (the workflow uploads them); --no-write leaves the store alone (a
# measurement on past data). Exits non-zero when a cell could not be fitted; a
# cell that was fitted but did not pass is a health warning, not an error.
# Needs R-INLA, sn, cmdstanr and CmdStan. Run from the repo root.

invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
suppressPackageStartupMessages(devtools::load_all(here::here(), quiet = TRUE))
source(here::here("research/football-v2/engines.R"))

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name, default = NULL) {
  i <- which(args == paste0("--", name))
  if (length(i)) args[[i + 1L]] else default
}
split_flag <- function(name, default) {
  x <- flag(name)
  if (is.null(x)) default else trimws(strsplit(x, ",", fixed = TRUE)[[1]])
}
sports <- split_flag("sport", c("football", "handball", "basketball"))
sexes <- split_flag("sex", c("male", "female"))
stopifnot(all(sports %in% c("football", "handball", "basketball")), all(sexes %in% c("male", "female")))
out_dir <- flag("out")
end_date <- as.Date(flag("end-date", format(Sys.Date())))
write_rows <- !"--no-write" %in% args
if (!is.null(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
# Recorded on every row: the health check tells a cell this run never covered
# (PAUSED) from one it covered and wrote nothing for (WARN).
run_sports <- paste(sports, collapse = ",")

leagues <- load_leagues()
exe_dir <- file.path(tempdir(), "engine-reference-exe")
dir.create(exe_dir, showWarnings = FALSE)
seed <- as.integer(format(end_date, "%Y%m%d"))
inla_version <- as.character(utils::packageVersion("INLA"))
cmdstan_version <- as.character(cmdstanr::cmdstan_version())

# ---- Football v2.0 ---------------------------------------------------------------

football <- leagues[["football_iceland"]]
fv2_stan_dir <- here::here("research", "football-v2", "stan")
fv2_stan <- file.path(fv2_stan_dir, fv2_stan_program(list(opts = football_v2_options())))

# As fit_league() fits on that day: its seed (keyed on the cell and the fit
# date), full integration, one thread, 4,000 draws, then its gate against the
# cell's previous INLA fit, when there is one.
fit_inla_for <- function(league, sex) {
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
  # WHY: 2,000 draws per chain. At 1,000 the men's strength-walk sd (sigma[1])
  # mixed slowly (Rhat 1.031, bulk ESS 147, data of 2026-11-23) though every
  # forecast was at Rhat 1.007; at 2,000 it was 1.0026 (ESS 329).
  fv2_fit_stan(spec, fv2_stan_file(spec, fv2_stan_dir), exe_dir, csv_dir,
    seed = seed, refresh = 0L, iter_sampling = 2000L
  )
}
football_cell <- function(sex) {
  fv2_weekly_reference(
    football, sex,
    end_date = end_date, root = here::here("data"),
    fit_inla = fit_inla_for(football, sex), fit_nuts = fit_nuts,
    provenance = list(
      inla_version = inla_version, cmdstan_version = cmdstan_version,
      stan_md5 = unname(tools::md5sum(fv2_stan))
    )
  )
}

# ---- Handball and basketball v2 ----------------------------------------------------

hbbb_stan_dir <- do.call(here::here, as.list(HBBB_REFERENCE_STAN_DIR))
hbbb_cell <- function(sport, sex) {
  league <- leagues[[paste0(sport, "_iceland")]]
  hbbb_v2_weekly_reference(
    league, sex,
    end_date = end_date, root = here::here("data"),
    # As fit_league() fits on that day (hbbb_v2_reference_inla()): its seed,
    # 4,000 draws, full integration, one thread, its gate.
    fit_inla = hbbb_v2_reference_inla(league, sex, end_date, root = here::here("data")),
    fit_nuts = hbbb_v2_reference_nuts(sport, hbbb_stan_dir, exe_dir, seed = seed),
    provenance = list(
      inla_version = inla_version, cmdstan_version = cmdstan_version,
      stan_md5 = hbbb_v2_stan_md5(hbbb_stan_dir)
    )
  )
}

# ---- Every cell ------------------------------------------------------------------

cli::cli_h1("Weekly Stan reference fit ({run_sports}), data to {end_date}")
failed <- character()
for (sport in sports) {
  for (sex in sexes) {
    cell <- paste(sport, sex)
    t0 <- Sys.time()
    # One cell's failure, in the fits or the write, must not cost the others.
    res <- tryCatch(
      {
        res <- if (sport == "football") football_cell(sex) else hbbb_cell(sport, sex)
        res$row$run_sports <- run_sports
        if (write_rows) write_table(res$row, "engine_reference", root = here::here("data"))
        res
      },
      error = function(e) {
        cli::cli_alert_danger("{cell}: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(res)) {
      failed <- c(failed, cell)
      next
    }
    secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (!is.null(out_dir)) {
      stem <- sprintf("%s_%s", sport, sex)
      utils::write.csv(res$fixtures, file.path(out_dir, sprintf("fixtures_%s.csv", stem)), row.names = FALSE)
      utils::write.csv(cbind(res$row, seconds = round(secs)), file.path(out_dir, sprintf("summary_%s.csv", stem)), row.names = FALSE)
      # Handball / basketball: per half, the NUTS divergences, Rhat by group and
      # time, and the INLA mode status and gate, to read a failed week by.
      if (!is.null(res$diagnostics)) {
        utils::write.csv(res$diagnostics, file.path(out_dir, sprintf("diagnostics_%s.csv", stem)), row.names = FALSE)
      }
    }
    r <- res$row
    lines <- if (sport == "football") "" else sprintf(", spread line %.4f, total line %.4f", r$spread_mean_abs_diff, r$total_mean_abs_diff)
    cli::cli_alert_info(sprintf(
      "%s: mean |p_INLA - p_NUTS| = %.4f (largest %.4f)%s over %d fixtures (%s); NUTS %d divergences, Rhat %.3f; INLA mode status %d; %.0f s",
      cell, r$mean_abs_diff, r$max_abs_diff, lines, r$n_fixtures, r$fixtures,
      r$nuts_divergences, r$nuts_max_rhat, r$inla_mode_status, secs
    ))
    if (!isTRUE(r$passed)) {
      # A GitHub annotation on the run; the health check carries the warning.
      cat(sprintf("::warning::%s: the weekly reference did not pass (mean |p_INLA - p_NUTS| = %.4f, limit %.2f%s)\n", cell, r$mean_abs_diff, r$threshold, lines))
    }
  }
}

if (length(failed)) {
  cli::cli_alert_danger("No reference for: {failed}")
  quit(save = "no", status = 1L)
}

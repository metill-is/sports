# Writes the stored NUTS reference that tests/testthat/test-model-v2-reference.R
# checks the INLA engine against (proposal, "A reference test that needs no
# Stan run"). Run once, from the repo root, on a machine with CmdStan, R-INLA
# and sn:
#   Rscript research/football-v2/make_reference.R
# Regenerate it whenever football_v2_options(), fv2_priors() or the Stan
# reference program change on purpose; the test fails as "stale" until then.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/engines.R")
source("tests/testthat/helper-football-v2.R")

sex <- "female"
end_date <- as.Date("2026-07-31")
seed <- 20261122L
out_file <- "tests/testthat/fixtures/football-v2-reference.rds"
league <- load_leagues()[["football_iceland"]]
root <- here::here("data")

# The specification's inputs exactly as the production fit builds them:
# fit_model_v2_inla() is run up to its call of fv2_build(), whose arguments
# are kept, and stopped there.
prep <- suppressMessages(prepare_data(league, sex, end_date = end_date, root = root))
inputs <- NULL
recorder <- function(train, pred = NULL, carry = NULL, ..., divisions = NULL) {
  inputs <<- list(train = train, pred = pred, carry = carry, divisions = divisions)
  stop(structure(class = c("inputs_captured", "error", "condition"), list(message = "inputs captured", call = NULL)))
}
tryCatch(
  testthat::with_mocked_bindings(
    fit_model_v2_inla(prep, league, sex, end_date = end_date, root = root),
    fv2_build = recorder, .package = "sports"
  ),
  inputs_captured = function(e) NULL
)
stopifnot(!is.null(inputs), nrow(inputs$pred) > 0L)
spec <- do.call(fv2_build, c(inputs, football_v2_options()))
cat(sprintf("%s to %s: %d matches, %d teams, %d states, %d fixtures\n", sex, end_date, spec$N, spec$K, spec$S, nrow(spec$pred)))

stan_file <- fv2_stan_file(spec, "research/football-v2/stan")
exe_dir <- file.path(tempdir(), "exe")
csv_dir <- file.path(tempdir(), "csv")
dir.create(exe_dir, showWarnings = FALSE)
dir.create(csv_dir, showWarnings = FALSE)
f <- fv2_fit_stan(spec, stan_file, exe_dir, csv_dir, seed = seed, refresh = 0L)
d <- f$diagnostics
max_rhat <- max(c(f$summary$rhat, d$state_max_rhat), na.rm = TRUE)
cat(sprintf(
  "NUTS: %.0f s, divergences %d, max treedepth hits %d, largest Rhat %.4f, smallest state ESS %.0f\n",
  d$wall_secs, d$n_divergent, d$n_max_treedepth, max_rhat, d$state_min_ess
))

ref <- list(
  inputs = inputs,
  opts = football_v2_options(),
  priors = fv2_priors(),
  summary = reference_summary(f$draws, spec),
  provenance = list(
    sex = sex, end_date = end_date, seed = seed, chains = 4L, iter_warmup = 1000L, iter_sampling = 1000L,
    n_draws = nrow(f$draws$att), stan_program = basename(stan_file), stan_md5 = unname(tools::md5sum(stan_file)),
    cmdstan = cmdstanr::cmdstan_version(), n_divergent = as.integer(d$n_divergent),
    n_max_treedepth = as.integer(d$n_max_treedepth), max_rhat = max_rhat, min_state_ess = d$state_min_ess,
    commit = system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE),
    made = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
)
saveRDS(ref, out_file, compress = "xz")
cat(sprintf("saved %s (%.0f KB)\n", out_file, file.size(out_file) / 1024))

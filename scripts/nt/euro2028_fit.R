#!/usr/bin/env Rscript
# National-team v2 fit for the EURO 2028 qualifying odds: the owner-approved
# specification (nt_prepare() and nt_fit_inla() defaults: 12-year window,
# quarterly grid, estimated random walk, pooled venue effects, no friendly
# term), fitted repeatably (one thread, R-INLA's online optimisation off,
# seeded draws) on every match before the cutoff.
#
# Usage:
#   Rscript scripts/nt/euro2028_fit.R <results.csv> <out_fit.rds> [cutoff=YYYY-MM-DD]
#       [seed=2028] [n_draws=2000] [source=<martj42 commit>]
# Without a cutoff, the day after the last played match in the file.
# Heavy (about two minutes of R-INLA): run it as `ml run -- Rscript ...`.

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 150)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: euro2028_fit.R <results.csv> <out_fit.rds> [key=value ...]")
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}

results <- nt_read_results(args[[1]])
played <- results[!is.na(results$home_score), ]
cutoff <- as.Date(opt("cutoff", as.character(max(played$match_date) + 1)))
prep <- nt_prepare(results, cutoff)
fit <- nt_fit_inla(
  prep,
  threads = "1:1", reproducible = TRUE,
  seed = as.integer(opt("seed", "2028")), n_draws = as.integer(opt("n_draws", "2000"))
)
fit$source <- list(
  results = normalizePath(args[[1]]),
  results_md5 = unname(tools::md5sum(args[[1]])),
  martj42_commit = opt("source", NA_character_),
  last_match = max(prep$matches$match_date),
  fitted_at = format(Sys.time(), "%Y-%m-%d %H:%M %Z"),
  inla_version = as.character(utils::packageVersion("INLA"))
)
saveRDS(fit, args[[2]])

seeding <- nt_euro2028_facts()$seeding
absent <- setdiff(seeding$team, fit$teams)
if (length(absent) > 0L) stop("Entrants missing from the fit: ", paste(absent, collapse = ", "))
strength <- colMeans(fit$draws$theta[, seeding$team])
tab <- data.frame(team = seeding$team, pot = seeding$pot, strength = round(strength, 3))
tab <- tab[order(-tab$strength), ]
tab$rank <- seq_len(nrow(tab))
cat(sprintf(
  "Fit to %s (last match %s): %d matches, %d teams, %.0f s fit + %.0f s draws, INLA %s.\n",
  cutoff, fit$source$last_match, fit$settings$n_matches, fit$settings$n_teams,
  fit$seconds[["fit"]], fit$seconds[["draws"]], fit$source$inla_version
))
print(fit$hyper, digits = 3)
cat("\nThe 54 EURO 2028 entrants by posterior mean strength:\n")
print(tab, row.names = FALSE)

#!/usr/bin/env Rscript
# Generate data/publish/world_cup/karla/head_to_head.json from the saved fit,
# standalone (without re-running the full forecast/publish). The daily
# world-cup.yml cron produces this file via forecast.R + publish_world_cup();
# this script is the manual fallback + the local-dev generator for vendoring
# into metill-platform.

suppressMessages(devtools::load_all(quiet = TRUE))

s <- wc_structure()
teams <- readRDS(here::here("data", "wc", "fit", "teams.rds"))
wc_validate_teams(s, teams$team)
si <- readRDS(here::here("data", "wc", "fit", "sim_inputs.rds"))
fx <- wc_group_fixtures(s)

cat(sprintf("group fixtures: %d (%d played)\n", nrow(fx), sum(fx$played)))

# Condition on played knockout results, mirroring forecast.R: pinned matches
# collapse settled pairs to 0/1 instead of re-simulating eliminated teams.
kres <- wc_knockout_results(s)
sw <- wc_shootout_winners()
cat(sprintf("played knockout results: %d\n", nrow(kres)))

cat("computing team-vs-team head-to-head (joint MC)...\n")
t0 <- Sys.time()
h2h <- wc_head_to_head(si$team, si$scalar, fx, s,
  k_replays = 400L, knockout_results = kres, shootout_winners = sw
)
cat(sprintf("  done in %.0fs\n", as.numeric(Sys.time() - t0, units = "secs")))

is_name <- .wc_country_namer(
  here::here("data", "wc", "structure", "country_names_is.csv")
)
generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
payload <- wc_head_to_head_payload(h2h, is_name, generated_at, Sys.Date())

out_dir <- here::here("data", "publish", "world_cup", "karla")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_path <- file.path(out_dir, "head_to_head.json")
jsonlite::write_json(payload, out_path,
  auto_unbox = TRUE, matrix = "rowmajor", digits = 6
)
cat(sprintf(
  "wrote %s (%d pairs, %.0f KB)\n", out_path, length(payload$pairs),
  file.info(out_path)$size / 1024
))

# ---- spot check: Portugal vs Argentina ----
# The "standalone" reference values (0.2657 / 0.1123 / 0.1307 / 0.3555) are the
# pre-knockout prototype's UNPINNED numbers — they only apply while no knockout
# result involving either team has been played. Once a knockout pin settles the
# pair, expect 0/1-degenerate values instead.
tv <- payload$teams
iP <- match("Portugal", tv) - 1L
iA <- match("Argentina", tv) - 1L
lo <- min(iP, iA)
hi <- max(iP, iA)
pr <- Filter(function(x) x$i == lo && x$j == hi, payload$pairs)[[1]]
por_is_i <- (iP == lo)
p_por_farther <- if (por_is_i) pr$p_i_farther else 1 - pr$p_i_farther - pr$p_tie
p_por_wins <- if (is.null(pr$p_i_wins_if_meet)) {
  NA_real_
} else if (por_is_i) {
  pr$p_i_wins_if_meet
} else {
  1 - pr$p_i_wins_if_meet
}
cat("\n== Portugal vs Argentina spot check ==\n")
cat(sprintf("  P(Portugal farther)  = %.4f\n", p_por_farther))
cat(sprintf("  P(tie)               = %.4f\n", pr$p_tie))
cat(sprintf("  P(they meet)         = %.4f\n", pr$p_meet))
cat(sprintf("  P(Portugal wins|meet)= %.4f\n", p_por_wins))
cat(sprintf(
  "  meet_by_round: R16 %.3f  QF %.3f  SF %.3f  Final %.3f\n",
  pr$meet_by_round$R16, pr$meet_by_round$QF, pr$meet_by_round$SF, pr$meet_by_round$Final
))

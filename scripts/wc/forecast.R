#!/usr/bin/env Rscript
# Post-fit World Cup forecast pipeline: load the saved fit -> extract per-draw
# strengths -> simulate the tournament -> publish JSON -> render the HTML page.
# Assumes scripts/wc/fit.R has produced data/wc/fit/{fit.rds,teams.rds}.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(dplyr))
options(width = 120)

s <- wc_structure()
teams <- readRDS(here::here("data", "wc", "fit", "teams.rds"))
wc_validate_teams(s, teams$team)

si_path <- here::here("data", "wc", "fit", "sim_inputs.rds")
if (file.exists(si_path)) {
  si <- readRDS(si_path)
} else {
  fit <- readRDS(here::here("data", "wc", "fit", "fit.rds"))
  si <- .extract_sim_inputs_pfi(fit, teams)
}

fx <- wc_group_fixtures(s)
cat(sprintf("group fixtures: %d (%d played)\n", nrow(fx), sum(fx$played)))

# Condition the forecast on played knockout results (Phase 2): pin each decided
# match so the winner advances w.p. 1 and the loser 0, instead of re-simulating
# the bracket from the group stage and showing eliminated teams with a chance.
# Empty during the group stage -> the call is identical to before.
kres <- wc_knockout_results(s)
sw <- wc_shootout_winners()
cat(sprintf("played knockout results: %d\n", nrow(kres)))

out <- simulate_world_cup(si$team, si$scalar, fx, s,
  pairing_seed = 2026L, knockout_results = kres, shootout_winners = sw
)

# Knockout-stage match cards: predict the scheduled-but-unplayed knockout
# fixtures (empty during the group stage) and append them to the group cards so
# predictions.json carries the upcoming round once the bracket is set.
kfx <- wc_knockout_fixtures(s)
cat(sprintf("knockout fixtures: %d\n", nrow(kfx)))
if (nrow(kfx) > 0L) {
  kpred <- wc_knockout_predictions(kfx, si$team, si$scalar, s, out$bracket_model$W)
  out$predictions <- dplyr::bind_rows(out$predictions, kpred)
}

# Joint team-vs-team head-to-head pass (separate from the marginal bracket model
# above) — powers the page's "Einvígi" section. ~1.5 min extra on the cron.
cat("computing team-vs-team head-to-head (joint MC)...\n")
h2h <- wc_head_to_head(si$team, si$scalar, fx, s, k_replays = 400L)
publish_world_cup(out, si$team, s, fx, fit_date = Sys.Date(), head_to_head = h2h)
page <- wc_render_html()

cat("\n=== CHAMPION PROBABILITIES (top 20) ===\n")
champ <- out$placement_probs |>
  filter(.data$round_name == "Champion") |>
  arrange(desc(.data$probability))
print(as.data.frame(head(champ, 20)), digits = 3)

cat("\n=== GROUP-WINNER FAVOURITES (P(advance) extremes) ===\n")
gp <- out$group_probs |> arrange(.data$group, desc(.data$p_advance))
print(as.data.frame(gp |> group_by(group) |> slice_head(n = 1) |> ungroup() |>
  select(group, team, p_first, p_advance)), digits = 3)

cat("\nForecast page:", page, "\n")

# Backfill v3 (Student-t, free nu) upcoming-match posterior draws into the variant archive.
#
# Reads fit_student_t_v3_free_nu.rds, extracts total_goals_pred / goal_diff_pred
# (the upcoming-match posterior-predictive arrays), joins with pred_d.csv for
# match metadata, writes to:
#   store/predictions_archive/sport=football/country=iceland/sex=male/variant=v3_free_nu/fit_date=2026-04-21/predictions.parquet
#
# The schema is intentionally a minimal superset of the BVP archive so
# read_predictions_archive() unions both variants cleanly. Columns:
#   iteration, game_nr, division, date, home, away, home_goals, away_goals
# where for v3 we round total_goals to integer home_goals + away_goals and
# reconstruct (home_goals, away_goals) from (S, D) = (home+away, home-away)
# via (home = (S+D)/2, away = (S-D)/2). For v3 these are NOT exact integers -
# the downstream market_probs module recomputes (total_goals, goal_diff)
# from them so the rounding-back only matters for display.
#
# Invocation: Rscript R/backtest/betting_pnl/backfill_v3_archive.R

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(posterior)
})

source(here::here("R", "storage", "store.R"))

sports_dir <- here::here()
audit_dir <- file.path(
  sports_dir, "football", "iceland", "results", "male",
  "audits", "student_t"
)
fit_path <- file.path(audit_dir, "fit_student_t_v3_free_nu.rds")
pred_d_path <- file.path(sports_dir, "football", "iceland", "results", "male", "pred_d.csv")

if (!file.exists(fit_path)) stop("v3 audit fit not found: ", fit_path)
if (!file.exists(pred_d_path)) stop("pred_d.csv not found: ", pred_d_path)

# Fit date: the audit session was 2026-04-20 late night; results dated to 2026-04-21 per
# commit a778047 and the student-t-revisit plan doc.
v3_fit_date <- "2026-04-21"

cat("Reading v3 fit:", fit_path, "\n")
fit <- readRDS(fit_path)

cat("Reading pred_d:", pred_d_path, "\n")
pred_d <- readr::read_csv(pred_d_path, show_col_types = FALSE)
N_pred <- nrow(pred_d)
cat("N_pred =", N_pred, "\n")

# Extract pred arrays.
draws <- fit$draws(
  variables = c("total_goals_pred", "goal_diff_pred"),
  format = "draws_df"
)

total_long <- draws |>
  select(.draw, starts_with("total_goals_pred")) |>
  pivot_longer(cols = -.draw, names_to = "raw", values_to = "total_goals") |>
  mutate(game_nr = as.integer(sub(".*\\[(\\d+)\\]$", "\\1", raw))) |>
  select(-raw)

diff_long <- draws |>
  select(.draw, starts_with("goal_diff_pred")) |>
  pivot_longer(cols = -.draw, names_to = "raw", values_to = "goal_diff") |>
  mutate(game_nr = as.integer(sub(".*\\[(\\d+)\\]$", "\\1", raw))) |>
  select(-raw)

joined <- inner_join(total_long, diff_long, by = c(".draw", "game_nr"))

cat("Draws x matches:", nrow(joined), "\n")

# Reconstruct home_goals, away_goals from (S, D) so the archive schema matches BVP.
# For Student-t, (S, D) are continuous - home/away will also be continuous.
archive_df <- joined |>
  left_join(pred_d |> select(game_nr, date, home, away, division), by = "game_nr") |>
  mutate(
    home_goals = (total_goals + goal_diff) / 2,
    away_goals = (total_goals - goal_diff) / 2,
    iteration = as.integer(.draw)
  ) |>
  select(iteration, game_nr, division, date, home, away, home_goals, away_goals)

cat(
  "Archive frame:", nrow(archive_df), "rows;", length(unique(archive_df$game_nr)),
  "unique matches x", length(unique(archive_df$iteration)), "iterations\n"
)

archive_predictions(
  df = archive_df,
  sport = "football", country = "iceland", sex = "male",
  sports_dir = sports_dir,
  variant = "v3_free_nu",
  fit_date = v3_fit_date
)

cat("v3 archive backfill complete.\n")

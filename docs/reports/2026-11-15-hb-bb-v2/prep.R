# Build the data for one (cell, cutoff): production stan_data, the lean-model data
# (appearance slots + the v2 team-season / week grid) and the hold-out set.
# Usage: Rscript docs/reports/2026-11-15-hb-bb-v2/prep.R <cell> <cutoff> <horizon_end>
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
args <- commandArgs(trailingOnly = TRUE)
cell <- args[[1]]
cutoff <- as.Date(args[[2]])
horizon_end <- as.Date(args[[3]])
ci <- CELLS[[cell]]
league <- load_leagues()[[ci$league]]

p <- suppressMessages(prepare_data(league, ci$sex,
  end_date = cutoff, root = DATA_ROOT,
  schedule_horizon_days = 0L
))
sd <- p$stan_data
teams <- p$teams

res_all <- cell_results(cell)
train <- as.data.frame(suppressMessages(model_training_results(res_all, league, end_date = cutoff)))
stopifnot(nrow(train) == sd$N)
train$home_nr <- match(train$home_team, teams$team)
train$away_nr <- match(train$away_team, teams$team)
stopifnot(
  identical(as.integer(train$home_nr), sd$team1),
  identical(as.integer(train$home_score), sd$goals1)
)

hold <- played_only(res_all, ci$sport)
hold <- hold[hold$match_date > cutoff & hold$match_date <= horizon_end &
  hold$home_team %in% teams$team & hold$away_team %in% teams$team, ]
hold <- hold[order(hold$match_date), ]
hold$home_nr <- match(hold$home_team, teams$team)
hold$away_nr <- match(hold$away_team, teams$team)

# Appearance slots: one per (team, appearance), sorted by team then date.
long <- rbind(
  data.frame(m = seq_len(nrow(train)), team = train$home_nr, date = train$match_date, side = "home"),
  data.frame(m = seq_len(nrow(train)), team = train$away_nr, date = train$match_date, side = "away")
)
long <- long[order(long$team, long$date, long$m), ]
long$slot <- seq_len(nrow(long))
prev <- ave(as.numeric(long$date), long$team, FUN = function(x) c(NA, head(x, -1)))
dt <- as.numeric(long$date) - prev
long$sdt <- ifelse(is.na(dt), 0, sqrt(pmin(dt, 100))) # production's 100-day cap
first_slot <- tapply(long$slot, long$team, min)
last_slot <- tapply(long$slot, long$team, max)
home_slot <- long$slot[long$side == "home"][order(long$m[long$side == "home"])]
away_slot <- long$slot[long$side == "away"][order(long$m[long$side == "away"])]
season_levels <- sort(unique(train$season))
slots <- list(
  K = nrow(teams), N = nrow(train), S = nrow(long), NS = length(season_levels),
  slot_team = as.integer(long$team), slot_sdt = long$sdt,
  first_slot = as.integer(first_slot), last_slot = as.integer(last_slot),
  home_slot = as.integer(home_slot), away_slot = as.integer(away_slot),
  home_team = as.integer(train$home_nr), away_team = as.integer(train$away_nr),
  season = as.integer(match(train$season, season_levels)),
  y_home = as.numeric(train$home_score), y_away = as.numeric(train$away_score),
  N_hold = nrow(hold), hold_home = as.integer(hold$home_nr), hold_away = as.integer(hold$away_nr)
)
stopifnot(
  all(slots$slot_team[slots$home_slot] == train$home_nr),
  all(slots$slot_team[slots$away_slot] == train$away_nr),
  all(hold$season %in% season_levels)
)

v2 <- build_v2_data(train, hold, teams$team,
  season_start = season_starts(res_all), n_weeks = season_weeks(res_all),
  division_map = DIVISION_MAP[[ci$sport]]
)

saveRDS(
  list(
    cell = cell, cutoff = cutoff, horizon_end = horizon_end, prod = sd, slots = slots, v2 = v2,
    teams = teams, train = train, hold = hold, season_levels = season_levels
  ),
  file.path(OUT, "out", sprintf("data_%s_%s.rds", cell, cutoff))
)
cat(sprintf(
  "%s cutoff %s: K=%d N=%d N_rounds=%d slots=%d prior-only share=%.3f seasons=%d | hold=%d (%s) | v2: TS=%d W=%d\n",
  cell, cutoff, sd$K, sd$N, sd$N_rounds, slots$S, 1 - slots$S / (sd$K * sd$N_rounds), slots$NS, nrow(hold),
  paste(names(table(hold$division)), table(hold$division), collapse = ", "), v2$TS, v2$W
))

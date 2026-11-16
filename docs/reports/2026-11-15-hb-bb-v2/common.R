# Shared setup for the handball / basketball v2 experiments.
# Run every script from the repo root: load_leagues() resolves config via here::here().
options(width = 180)
suppressMessages(devtools::load_all(".", quiet = TRUE))

EXP_DIR <- "docs/reports/2026-11-15-hb-bb-v2"
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
# WHY an override: the stored results were computed against the data of the branch
# point; a re-run from a later checkout names that data here instead of reading newer rows.
DATA_ROOT <- Sys.getenv("HBBB_DATA", "data")

CELLS <- list(
  hb_m = list(league = "handball_iceland", sport = "handball", sex = "male"),
  hb_f = list(league = "handball_iceland", sport = "handball", sex = "female"),
  bb_m = list(league = "basketball_iceland", sport = "basketball", sex = "male"),
  bb_f = list(league = "basketball_iceland", sport = "basketball", sex = "female")
)

# Scale constants per sport: every prior in the lean models is stated in these units
# (goals for handball, points for basketball).
SPORT_SCALE <- list(
  handball = list(
    score_mean = 28, score_mean_sd = 10, total_mean = 55, total_mean_sd = 20,
    u_sigma_m = 15, u_sigma_t = 20, u_b = 10, u_u = 8, u_w = 1, u_ha = 3, u_season = 5,
    hfa_sd = 5, strength0_sd = 10, score_sd_prior = c(5, 5), rw_sd_u = 1
  ),
  basketball = list(
    score_mean = 85, score_mean_sd = 20, total_mean = 165, total_mean_sd = 40,
    u_sigma_m = 30, u_sigma_t = 40, u_b = 25, u_u = 20, u_w = 2.5, u_ha = 6, u_season = 10,
    hfa_sd = 10, strength0_sd = 10, score_sd_prior = c(10, 5), rw_sd_u = 2.5
  )
)

cell_results <- function(cell) {
  ci <- CELLS[[cell]]
  r <- read_table("results",
    root = DATA_ROOT,
    filter = list(sport = ci$sport, country = "iceland", sex = ci$sex)
  )
  as.data.frame(r)
}

# WHY: the walkover scores (20-0, 10-0) are not played results; production drops them
# from training, so they are dropped from every scored hold-out set too.
played_only <- function(res, sport) {
  res <- res[!is.na(res$home_score) & !is.na(res$away_score), , drop = FALSE]
  res[!.is_forfeit(res, sport), , drop = FALSE]
}

# The tie treatment proposed for regulation-time basketball, per cell: one level-score
# shift per division for men, a single shift for women. Every table that shows "v2" for
# basketball uses this variant.
REG_SHIFT <- c(bb_m = "shift_div", bb_f = "shift")

# WHY: handball's PO rows are the top division's play-off bracket, one short block
# per season, so they share OD's scoring level instead of getting their own.
DIVISION_MAP <- list(handball = c(PO = "OD"), basketball = NULL)

# The season calendar (first match date, number of weeks) is taken from every stored row.
# The results table holds played matches only, so a season in progress is as long as it
# has been played so far, plus the two spare weeks below.
season_starts <- function(res) {
  s <- tapply(as.numeric(res$match_date), res$season, min)
  stats::setNames(as.Date(as.numeric(s), origin = "1970-01-01"), names(s))
}
season_weeks <- function(res) {
  s <- season_starts(res)
  max(floor((as.numeric(res$match_date) - as.numeric(s[as.character(res$season)])) / 7)) + 2L
}

#' Run joint Kelly diagnostics for a league
#'
#' Usage:
#'   cd Sports && Rscript R/bets/diagnose_joint.R [league_key]
#'   e.g.: Rscript R/bets/diagnose_joint.R football_italy

Sys.setlocale("LC_ALL", "is_IS.UTF-8")

box::use(
  ./diagnostics_joint[run_diagnostics, print_diagnostics],
  ./odds[load_odds],
  readr[read_csv, read_file],
  yaml[yaml.load],
  dplyr[filter]
)

`%||%` <- function(a, b) if (is.null(a)) b else a

args <- commandArgs(trailingOnly = TRUE)
league_key <- if (length(args) >= 1) args[1] else "football_italy"

sports_dir <- here::here()

# Load config
leagues <- yaml.load(read_file(file.path(sports_dir, "config", "leagues.yml")))
league <- leagues[[league_key]]
league_dir <- file.path(sports_dir, league$dir)
bets_yml <- file.path(league_dir, "config", "bets.yml")
cfg <- yaml.load(read_file(bets_yml))

# Merge global bankroll
bankroll_yml <- file.path(sports_dir, "config", "bankroll.yml")
if (file.exists(bankroll_yml)) {
  global <- yaml.load(read_file(bankroll_yml))
  merged <- global
  merged[names(cfg$bankroll)] <- cfg$bankroll
  cfg$bankroll <- merged
}

sex <- cfg$sex[[1]]

# Load posterior
pred_path <- cfg$predictions$path
pattern <- file.path(league_dir, pred_path, sex, "*", "posterior_goals.csv")
candidates <- Sys.glob(pattern)
if (length(candidates) > 0) {
  post_path <- candidates[order(basename(dirname(candidates)), decreasing = TRUE)[1]]
} else {
  post_path <- file.path(league_dir, pred_path, sex, "posterior_goals.csv")
}

post <- read_csv(post_path, show_col_types = FALSE) |>
  filter(date >= Sys.Date())

# Load odds
odds <- load_odds(cfg, league_dir, sex = sex)

# Run diagnostics
diag <- run_diagnostics(post, odds, cfg)
print_diagnostics(diag, cfg)

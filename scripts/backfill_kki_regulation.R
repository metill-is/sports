# One-off backfill: Baskethotel game ids and regulation-time scores for every past KKI
# season in data/facts/results, from each game's card.
#
# Polite: one card request at a time, one second apart (kki_add_regulation_scores()), with
# the per-run cap lifted. Results only (ingest_league(schedule = FALSE)): the schedule
# belongs to the daily ingest.
#
# The current season is left to the daily ingest (scrape-results.yml), which fetches the
# cards of the games played since its last run, up to 200 a run. Its partitions are the
# ones that job rewrites every day, so a commit of them from here would collide with it.
#
# Afterwards the script checks that every stored row is still there with its final score,
# and prints the coverage of regulation-time scores per sex, season and division.
#
# Usage (repository root):
#   Rscript scripts/backfill_kki_regulation.R                      # every past season
#   Rscript scripts/backfill_kki_regulation.R --seasons 2025,2026
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
current <- kki_current_season()
seasons <- if (length(args) >= 2L && identical(args[[1]], "--seasons")) {
  as.integer(strsplit(args[[2]], ",", fixed = TRUE)[[1]])
} else {
  sort(unique(as.integer(unlist(lapply(KKI_SEASON_IDS, function(s) lapply(s, names))))))
}
seasons <- seasons[seasons < current]
stopifnot(length(seasons) > 0L)

league <- load_leagues()[["basketball_iceland"]]
key_cols <- c("sex", "season", "division", "match_date", "home_team", "away_team")
read_kki <- function() {
  x <- read_table("results", filter = list(sport = "basketball", country = "iceland"))
  x$season <- as.integer(x$season)
  x[x$season %in% seasons, , drop = FALSE]
}
key <- function(x) do.call(paste, c(lapply(x[, key_cols, drop = FALSE], as.character), sep = "\r"))

before <- read_kki()
cat(sprintf(
  "Backfill of seasons %s: %d stored rows; current season %d left to the daily ingest\n",
  paste(seasons, collapse = ", "), nrow(before), current
))

# The daily step's per-run cap and time budget are lifted; its stop after three failed
# requests in a row stays, so an outage ends the run instead of trying every card.
options(sports.kki_max_cards = Inf, sports.kki_card_budget_secs = Inf)
for (sex in league$sexes) {
  cat(sprintf("\n== %s\n", sex))
  ingest_league(league, sex, seasons = seasons, schedule = FALSE)
}

after <- read_kki()
lost <- setdiff(key(before), key(after))
added <- setdiff(key(after), key(before))
at <- match(key(before), key(after))
changed <- !is.na(at) & (before$home_score != after$home_score[at] | before$away_score != after$away_score[at])
changed[is.na(changed)] <- FALSE
cat(sprintf(
  "\nRows before %d, after %d; stored rows missing after: %d; rows added: %d; final scores changed: %d\n",
  nrow(before), nrow(after), length(lost), length(added), sum(changed)
))
if (length(lost)) cat("  missing:", utils::head(lost, 5), sep = "\n    ")
if (length(added)) cat("  added:", utils::head(added, 5), sep = "\n    ")

played <- after[!is.na(after$home_score), , drop = FALSE]
cov <- stats::aggregate(
  cbind(
    games = 1L, with_reg = !is.na(played$home_score_reg),
    overtime = !is.na(played$home_score_reg) & played$home_score_reg == played$away_score_reg
  ) ~ sex + season + division,
  data = played, FUN = sum
)
cov$share <- sprintf("%.1f %%", 100 * cov$with_reg / cov$games)
cat("\nCoverage of regulation-time scores among played games\n")
print(cov[order(cov$sex, cov$season, cov$division), ], row.names = FALSE)
cat(sprintf(
  "\nTotal: %d of %d played games (%.1f %%), %d of them overtime games\n",
  sum(cov$with_reg), sum(cov$games), 100 * sum(cov$with_reg) / sum(cov$games), sum(cov$overtime)
))

# GISKO optimal predictions. Reads the WC posterior cache and emits, per
# upcoming match, the expected-points-optimal scoreline + a per-round joker.
# Read-only; never on CI. Usage:
#   Rscript scripts/wc/gisko.R                 # upcoming group fixtures
#   Rscript scripts/wc/gisko.R --match "Spain vs France @neutral #R16" \
#                              --match "Brazil vs Argentina @neutral #R16"
suppressMessages(devtools::load_all(here::here()))
options(width = 120)

args <- commandArgs(trailingOnly = TRUE)
max_goals <- 8L

parse_match <- function(s) {
  rnd <- if (grepl("#", s)) trimws(sub(".*#", "", s)) else "KO"
  s2 <- sub("\\s*#.*$", "", s)
  venue <- if (grepl("@", s2)) trimws(sub(".*@\\s*", "", s2)) else "neutral"
  s3 <- sub("\\s*@.*$", "", s2)
  parts <- trimws(strsplit(s3, "\\s+vs\\s+")[[1]])
  tibble::tibble(
    home = parts[1], away = parts[2], venue = venue, round = rnd,
    label = paste(parts[1], "v", parts[2])
  )
}

if (any(args == "--match")) {
  vals <- args[which(args == "--match") + 1L]
  matches <- do.call(rbind, lapply(vals, parse_match))
} else {
  s <- wc_structure()
  fx <- wc_group_fixtures(s)
  up <- fx[!fx$played, , drop = FALSE]
  if (nrow(up) == 0L) {
    cli::cli_alert_warning(
      "No upcoming group fixtures. Pass knockout pairings, e.g. --match \"Spain vs France @neutral #R16\"."
    )
    quit(status = 0L)
  }
  raw <- utils::read.csv(
    here::here("data", "wc", "structure", "wc2026_schedule.csv"),
    check.names = FALSE, colClasses = "character", encoding = "UTF-8"
  )
  rk <- .wc_pair_key(
    sports:::.wc_alias(trimws(raw[["Home Team"]])),
    sports:::.wc_alias(trimws(raw[["Away Team"]]))
  )
  round_of_pair <- stats::setNames(raw[["Round Number"]], rk)
  pk <- .wc_pair_key(up$home_team, up$away_team)
  rn <- unname(round_of_pair[pk])
  matches <- tibble::tibble(
    home = up$home_team, away = up$away_team, venue = up$venue,
    round = ifelse(is.na(rn), "G?", paste0("G", rn)),
    label = paste(up$home_team, "v", up$away_team)
  )
}

si <- readRDS(here::here("data", "wc", "fit", "sim_inputs.rds"))

out <- lapply(split(matches, matches$round), function(mr) {
  res <- gisko_optimise_round(mr, si, max_goals = max_goals)
  res$round <- mr$round[1]
  res
})

for (res in out) {
  cli::cli_h2("Round {res$round}")
  tab <- res$picks
  tab$pick <- paste0(tab$opt_home, "-", tab$opt_away)
  print(tab[, c("label", "pick", "exp_points", "p_exact", "differs_from_modal")])
  cli::cli_alert_info(
    "Joker: {tab$label[res$joker]} (E[points] {round(tab$exp_points[res$joker], 2)})"
  )
  print(res$summary)
}

dir.create(here::here("data", "wc", "gisko"), showWarnings = FALSE, recursive = TRUE)
payload <- lapply(out, function(res) {
  list(
    round = res$round, joker_label = res$picks$label[res$joker],
    picks = res$picks, summary = res$summary
  )
})
jsonlite::write_json(
  payload, here::here("data", "wc", "gisko", "optimal_picks.json"),
  auto_unbox = TRUE, pretty = TRUE, dataframe = "rows"
)
cli::cli_alert_success("Wrote data/wc/gisko/optimal_picks.json")

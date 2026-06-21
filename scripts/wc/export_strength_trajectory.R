#!/usr/bin/env Rscript
# Posterior-EXTRACTION (not a re-fit): pull the time-dynamic offence/defence
# random-walk trajectory out of the existing 2026 World Cup fit, per (team,
# round), and write a tidy file the metill-platform Rosling reel consumes.
#
# The bivariate-Poisson football model (Stan/football_iceland/...) parameterises
# each team's offence/defence as a per-team random walk over its OWN match
# sequence:  offense[i, k] = state of team k at its i-th match (round i).
# The published JSON only ever exports offense[N_rounds] (the *current* slice);
# the full offense[, k] / defense[, k] arrays live in the saved fit's posterior.
# This script summarises them and joins each (team, round) back to the calendar
# date of that match, so the reel can animate teams on a shared time axis.
#
#   Reads : data/wc/fit/{fit.rds, teams.rds}  (995MB fit, written by scripts/wc/fit.R)
#           data/facts/results/.../country=world   (the round->date source)
#           data/wc/structure/country_names_is.csv  (Icelandic display names)
#   Writes: out/wc_strength_trajectory.json
#
# Run: Rscript scripts/wc/export_strength_trajectory.R
suppressPackageStartupMessages({
  library(cmdstanr)
  library(posterior)
  library(arrow)
  library(dplyr)
  library(jsonlite)
})

# The fit was produced with end_date = Sys.Date() on the fit day (2026-06-11);
# reproduce its training window so reconstructed rounds align with offense[i,k].
END_DATE <- as.Date("2026-06-11")
HIGHLIGHT <- "Iceland" # the one team that gets rigorous credible bands
FIT_DIR <- here::here("data", "wc", "fit")
OUT <- here::here("out", "wc_strength_trajectory.json")
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)

cat("== loading teams + facts ==\n")
teams <- readRDS(file.path(FIT_DIR, "teams.rds")) # team, team_nr (== k)
stopifnot(HIGHLIGHT %in% teams$team)

res <- open_dataset(
  here::here("data", "facts", "results", "sport=football", "country=world", "sex=male")
) |>
  collect() |>
  filter(.data$match_date <= Sys.Date(), !is.na(.data$home_score))

# Reconstruct the per-team round index EXACTLY as prepare_data() does
# (model-prepare.R:212-216): order each team's matches by date, round = row_number.
# The date for (team, round) is tie-robust (ties share a date), so this is an
# exact recovery of the calendar date behind offense[round, k] / defense[round, k].
long <- bind_rows(
  transmute(res, match_date = .data$match_date, team = .data$home_team),
  transmute(res, match_date = .data$match_date, team = .data$away_team)
) |>
  arrange(.data$team, .data$match_date) |>
  group_by(.data$team) |>
  mutate(round = row_number()) |>
  ungroup()

round_date <- long |> distinct(.data$team, .data$round, .data$match_date)
recon_rounds <- max(long$round)
cat(sprintf(
  "reconstructed: %d teams, max round = %d, window %s -> %s\n",
  length(unique(long$team)), recon_rounds,
  as.character(min(long$match_date)), as.character(max(long$match_date))
))

cat("== loading fit.rds (995MB; ~1 min) ==\n")
fit <- readRDS(file.path(FIT_DIR, "fit.rds"))

# --- all-team posterior MEANS (the grey context cloud + each mean path) -------
parse_ik <- function(v) {
  m <- regmatches(v, regexec("\\[(\\d+),(\\d+)\\]", v))
  data.frame(
    round = as.integer(vapply(m, `[`, "", 2)),
    k = as.integer(vapply(m, `[`, "", 3))
  )
}
cat("== summarising offense/defense means ==\n")
so <- fit$summary("offense", "mean")
gc()
sd_ <- fit$summary("defense", "mean")
gc()
ha_o <- fit$summary("home_advantage_off", "mean")
ha_d <- fit$summary("home_advantage_def", "mean")

fit_rounds <- max(parse_ik(so$variable)$round)
off_long <- cbind(parse_ik(so$variable), off_raw = so$mean)
def_long <- cbind(parse_ik(sd_$variable), def_raw = sd_$mean)

# Each team's TRUE round count, recovered from the posterior flat tail: beyond a
# team's last real match the RW innovation has delta_t = 0, so offense[i] ==
# offense[i-1] bit-for-bit in every draw -> a posterior-mean diff of EXACTLY 0.
# Count the contiguous trailing zero-run to get n_fit(k) WITHOUT guessing the
# training cutoff. martj42 only appends matches, so rounds 1..n_fit always carry
# stable, correct dates regardless of when the facts store is read.
n_fit_tbl <- off_long |>
  arrange(.data$k, .data$round) |>
  group_by(.data$k) |>
  summarise(
    n_fit = {
      z <- rev(diff(.data$off_raw) == 0) # TRUE for trailing flat-tail diffs
      run <- match(FALSE, z)
      fit_rounds - (if (is.na(run)) length(z) else run - 1L)
    },
    .groups = "drop"
  )
stopifnot(max(n_fit_tbl$n_fit) == fit_rounds)
cat(sprintf(
  "fit N_rounds = %d; n_fit recovered per team (busiest = %d, sanity vs recon max %d)\n",
  fit_rounds, max(n_fit_tbl$n_fit), recon_rounds
))
k2team <- setNames(teams$team, teams$team_nr)
ha <- data.frame(
  k = as.integer(regmatches(ha_o$variable, regexpr("\\d+", ha_o$variable))),
  ha_off = ha_o$mean,
  ha_def = ha_d$mean[match(
    as.integer(regmatches(ha_d$variable, regexpr("\\d+", ha_d$variable))),
    as.integer(regmatches(ha_o$variable, regexpr("\\d+", ha_o$variable)))
  )]
)

means <- off_long |>
  inner_join(def_long, by = c("round", "k")) |>
  inner_join(n_fit_tbl, by = "k") |>
  filter(.data$round <= .data$n_fit) |> # keep real rounds, drop the flat tail
  mutate(team = k2team[as.character(.data$k)]) |>
  inner_join(ha, by = "k") |>
  inner_join(round_date, by = c("team", "round")) |> # attach calendar date
  # neutral venue, matching the published cur_offense = offense[N] + home_adv/2
  mutate(
    off = .data$off_raw + .data$ha_off / 2,
    def = .data$def_raw + .data$ha_def / 2
  ) |>
  arrange(.data$team, .data$round)

# Icelandic display names (fallback to English)
nm_is <- readr::read_csv(here::here("data", "wc", "structure", "country_names_is.csv"),
  show_col_types = FALSE
)
is_of <- setNames(nm_is$name_is, nm_is$team)
team_is <- function(t) ifelse(is.na(is_of[t]), t, is_of[t])

teams_out <- lapply(split(means, means$team), function(d) {
  d <- d[order(d$round), ]
  list(
    team = d$team[1], team_is = unname(team_is(d$team[1])), n = nrow(d),
    points = lapply(seq_len(nrow(d)), function(i) {
      list(
        date = as.character(d$match_date[i]), round = d$round[i],
        off = round(d$off[i], 5), def = round(d$def[i], 5)
      )
    })
  )
})
names(teams_out) <- NULL

# --- HIGHLIGHT: rigorous neutral-venue credible bands from the draws ----------
cat(sprintf("== highlight bands: %s (per-draw neutral) ==\n", HIGHLIGHT))
hk <- teams$team_nr[teams$team == HIGHLIGHT]
n_h <- n_fit_tbl$n_fit[n_fit_tbl$k == hk]
hd <- round_date |>
  filter(.data$team == HIGHLIGHT) |>
  arrange(.data$round)
vn <- c(
  sprintf("offense[%d,%d]", seq_len(n_h), hk),
  sprintf("defense[%d,%d]", seq_len(n_h), hk),
  sprintf("home_advantage_off[%d]", hk),
  sprintf("home_advantage_def[%d]", hk)
)
dd <- as_draws_df(fit$draws(variables = vn))
hao <- dd[[sprintf("home_advantage_off[%d]", hk)]]
had <- dd[[sprintf("home_advantage_def[%d]", hk)]]
qs <- function(x) stats::quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
hi_points <- lapply(seq_len(n_h), function(r) {
  o <- dd[[sprintf("offense[%d,%d]", r, hk)]] + hao / 2 # neutral, per draw
  d <- dd[[sprintf("defense[%d,%d]", r, hk)]] + had / 2
  qo <- qs(o)
  qd <- qs(d)
  list(
    date = as.character(hd$match_date[r]), round = r,
    off = round(mean(o), 5), off_lo = round(qo[1], 5), off_lo50 = round(qo[2], 5),
    off_hi50 = round(qo[4], 5), off_hi = round(qo[5], 5),
    def = round(mean(d), 5), def_lo = round(qd[1], 5), def_lo50 = round(qd[2], 5),
    def_hi50 = round(qd[4], 5), def_hi = round(qd[5], 5)
  )
})

out <- list(
  metadata = list(
    source = "martj42 international results -> bivariate-Poisson RW fit (data/wc/fit/fit.rds)",
    extracted = "offense[round,team] / defense[round,team] posterior, neutral venue (+home_adv/2)",
    fit_date = as.character(END_DATE),
    window_start = as.character(min(means$match_date)),
    window_end = as.character(max(means$match_date)),
    n_rounds = fit_rounds, n_teams = length(teams_out),
    scale_note = "log-rate scale; higher off = better attack, higher def = better defence (up-and-right = elite)",
    off_range = round(range(means$off), 5), def_range = round(range(means$def), 5),
    highlight = HIGHLIGHT
  ),
  teams = teams_out,
  highlight = list(
    team = HIGHLIGHT, team_is = unname(team_is(HIGHLIGHT)),
    n = n_h, points = hi_points
  )
)
write_json(out, OUT, auto_unbox = TRUE, digits = 6)
cat(sprintf("\nWROTE %s  (%.0f KB)\n", OUT, file.info(OUT)$size / 1024))

# --- eyeball the story: Iceland first vs last ---
ip <- hi_points
cat(sprintf(
  "\n%s arc:  round 1 (%s)  off=%+.3f def=%+.3f   ->   round %d (%s)  off=%+.3f def=%+.3f\n",
  HIGHLIGHT, ip[[1]]$date, ip[[1]]$off, ip[[1]]$def,
  n_h, ip[[n_h]]$date, ip[[n_h]]$off, ip[[n_h]]$def
))
peak <- which.max(vapply(ip, function(p) p$off + p$def, 0))
cat(sprintf(
  "%s peak (off+def): round %d (%s)  off=%+.3f def=%+.3f\n",
  HIGHLIGHT, peak, ip[[peak]]$date, ip[[peak]]$off, ip[[peak]]$def
))

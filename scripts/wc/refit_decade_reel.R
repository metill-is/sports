#!/usr/bin/env Rscript
# Isolated 2012->2026 re-fit FOR THE METILL ROSLING REEL ONLY.
#
# The live WC forecast fits internationals on a 2022+ window, where strengths
# barely move (a 4.5y RW with sparse data is near-static). A Hans-Rosling
# "sokn x vorn on the move" reel needs a DECADE so teams actually sweep across
# the plane (Japan's rise, Iceland's post-Euro-2016 fall, etc.).
#
# This re-ingests the SAME martj42 CSV with window_start = 2012 and re-fits the
# SAME bivariate-Poisson RW model into an ISOLATED data root (data_reel/), so the
# production facts store + live fit (data/wc/fit/) are never touched. It then
# extracts the per-(team, round) offence/defence trajectory in-session (no 3GB
# fit.rds round-trip) and writes the tidy reel JSON.
#
# Run (background; heavier than the live fit):
#   Rscript scripts/wc/refit_decade_reel.R
suppressPackageStartupMessages({
  devtools::load_all(quiet = TRUE)
  library(dplyr)
  library(arrow)
  library(jsonlite)
})
options(width = 120)

WINDOW_START <- as.Date("2012-01-01")
MIN_TEAM_MATCHES <- 20L # trim micro-nations -> faster fit + cleaner cloud
HIGHLIGHTS <- c("Japan", "Iceland") # rigorous credible bands for these
REEL_ROOT <- here::here("data_reel")
CSV <- here::here("data", "wc", "raw", "results.csv")
OUT <- here::here("out", "wc_strength_trajectory_decade.json")
STAN <- here::here("Stan", "football_iceland", "bivariate_poisson_no_inflation.stan")
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
stopifnot(file.exists(CSV))

cat("== ingest martj42 (2012+) into isolated root ==\n")
wc_ingest_internationals(
  csv_path = CSV, window_start = WINDOW_START,
  min_team_matches = MIN_TEAM_MATCHES, root = REEL_ROOT
)

league <- list(sport = "football", country = "world")
prep <- prepare_data(league,
  sex = "male", end_date = Sys.Date(),
  root = REEL_ROOT, schedule_horizon_days = 0L
)
sd <- prep$stan_data
teams <- prep$teams
cat(sprintf("stan_data: K=%d  N=%d  N_rounds=%d\n", sd$K, sd$N, sd$N_rounds))
for (h in HIGHLIGHTS) {
  cat(sprintf(
    "  %s in fit: %s (team_nr %s)\n", h, h %in% teams$team,
    paste(teams$team_nr[teams$team == h], collapse = "")
  ))
}

cat("== fit (4 chains x 500+500) ==\n")
t0 <- Sys.time()
fit <- fit_model(
  sd, STAN,
  method = "sample",
  chains = 4L, parallel_chains = 4L,
  iter_warmup = 500L, iter_sampling = 500L,
  adapt_delta = 0.9, seed = 42L,
  check_diagnostics = FALSE, show_progress = TRUE
)
cat(sprintf("fit time: %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
m <- extract_stan_metrics(fit)
print(m[c("n_divergent", "max_rhat", "min_ess_bulk", "min_ess_tail")])

# ---- round -> date (same session, same isolated facts, same cutoff) ----------
res <- open_dataset(
  fs::path(REEL_ROOT, "facts", "results", "sport=football", "country=world", "sex=male")
) |>
  collect() |>
  filter(.data$match_date <= Sys.Date(), !is.na(.data$home_score))
long <- bind_rows(
  transmute(res, match_date = .data$match_date, team = .data$home_team),
  transmute(res, match_date = .data$match_date, team = .data$away_team)
) |>
  arrange(.data$team, .data$match_date) |>
  group_by(.data$team) |>
  mutate(round = row_number()) |>
  ungroup()
round_date <- long |> distinct(.data$team, .data$round, .data$match_date)

# ---- posterior means (grey cloud + each mean path) --------------------------
parse_ik <- function(v) {
  m <- regmatches(v, regexec("\\[(\\d+),(\\d+)\\]", v))
  data.frame(
    round = as.integer(vapply(m, `[`, "", 2)),
    k = as.integer(vapply(m, `[`, "", 3))
  )
}
so <- fit$summary("offense", "mean")
gc()
sd2 <- fit$summary("defense", "mean")
gc()
ha_o <- fit$summary("home_advantage_off", "mean")
ha_d <- fit$summary("home_advantage_def", "mean")
fit_rounds <- max(parse_ik(so$variable)$round)
off_long <- cbind(parse_ik(so$variable), off_raw = so$mean)
def_long <- cbind(parse_ik(sd2$variable), def_raw = sd2$mean)

# flat-tail -> true round count per team (delta_t=0 => exact-0 mean diff)
n_fit_tbl <- off_long |>
  arrange(.data$k, .data$round) |>
  group_by(.data$k) |>
  summarise(n_fit = {
    z <- rev(diff(.data$off_raw) == 0)
    run <- match(FALSE, z)
    fit_rounds - (if (is.na(run)) length(z) else run - 1L)
  }, .groups = "drop")
stopifnot(max(n_fit_tbl$n_fit) == fit_rounds)

k2team <- setNames(teams$team, teams$team_nr)
ha_k <- function(s) as.integer(regmatches(s, regexpr("\\d+", s)))
ha <- data.frame(k = ha_k(ha_o$variable), ha_off = ha_o$mean)
ha$ha_def <- ha_d$mean[match(ha$k, ha_k(ha_d$variable))]

means <- off_long |>
  inner_join(def_long, by = c("round", "k")) |>
  inner_join(n_fit_tbl, by = "k") |>
  filter(.data$round <= .data$n_fit) |>
  mutate(team = k2team[as.character(.data$k)]) |>
  inner_join(ha, by = "k") |>
  inner_join(round_date, by = c("team", "round")) |>
  mutate(
    off = .data$off_raw + .data$ha_off / 2,
    def = .data$def_raw + .data$ha_def / 2
  ) |>
  arrange(.data$team, .data$round)

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

# ---- highlight bands (per-draw neutral) -------------------------------------
qs <- function(x) stats::quantile(x, c(0.05, 0.25, 0.5, 0.75, 0.95), names = FALSE)
hi_out <- lapply(HIGHLIGHTS, function(H) {
  hk <- teams$team_nr[teams$team == H]
  n_h <- n_fit_tbl$n_fit[n_fit_tbl$k == hk]
  hd <- round_date |>
    filter(.data$team == H) |>
    arrange(.data$round)
  vn <- c(
    sprintf("offense[%d,%d]", seq_len(n_h), hk),
    sprintf("defense[%d,%d]", seq_len(n_h), hk),
    sprintf("home_advantage_off[%d]", hk),
    sprintf("home_advantage_def[%d]", hk)
  )
  dd <- posterior::as_draws_df(fit$draws(variables = vn))
  hao <- dd[[sprintf("home_advantage_off[%d]", hk)]]
  had <- dd[[sprintf("home_advantage_def[%d]", hk)]]
  pts <- lapply(seq_len(n_h), function(r) {
    o <- dd[[sprintf("offense[%d,%d]", r, hk)]] + hao / 2
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
  list(team = H, team_is = unname(team_is(H)), n = n_h, points = pts)
})

out <- list(
  metadata = list(
    source = "martj42 international results (2012+) -> bivariate-Poisson RW re-fit (ISOLATED, reel-only)",
    extracted = "offense[round,team]/defense[round,team] posterior, neutral venue (+home_adv/2)",
    window_start = as.character(min(means$match_date)),
    window_end = as.character(max(means$match_date)),
    n_rounds = fit_rounds, n_teams = length(teams_out),
    min_team_matches = MIN_TEAM_MATCHES,
    scale_note = "log-rate scale; higher off = better attack, higher def = better defence (up-and-right = elite)",
    off_range = round(range(means$off), 5), def_range = round(range(means$def), 5),
    highlights = HIGHLIGHTS
  ),
  teams = teams_out,
  highlights = hi_out
)
write_json(out, OUT, auto_unbox = TRUE, digits = 6)
cat(sprintf("\nWROTE %s  (%.0f KB)\n", OUT, file.info(OUT)$size / 1024))

for (H in hi_out) {
  p <- H$points
  n <- length(p)
  peak <- which.max(vapply(p, function(x) x$off + x$def, 0))
  cat(sprintf(
    "%s: r1 %s off=%+.2f def=%+.2f -> r%d %s off=%+.2f def=%+.2f | peak r%d %s off=%+.2f def=%+.2f\n",
    H$team, p[[1]]$date, p[[1]]$off, p[[1]]$def, n, p[[n]]$date, p[[n]]$off, p[[n]]$def,
    peak, p[[peak]]$date, p[[peak]]$off, p[[peak]]$def
  ))
}

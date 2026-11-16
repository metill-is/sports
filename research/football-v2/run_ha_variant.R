# Which of pooling, sign freedom and common volatility removes the
# home-advantage inflation? Fits one rung of the isolation ladder
# (stan/ha_*.stan) on the pilot's per-appearance layout and reports the
# home-match-weighted total home effect against the raw log goal ratio.
#
#   Rscript research/football-v2/run_ha_variant.R <sex> <variant> [cutoff] [parallel_chains]
#   variant: ha_a_unpooled_positive | ha_b_unpooled_signfree |
#            ha_c_pooled_positive | ha_d_common_volatility
# Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
sex <- args[[1]]
variant <- args[[2]]
cutoff <- as.Date(if (length(args) >= 3L) args[[3]] else "2026-07-31")
par_chains <- if (length(args) >= 4L) as.integer(args[[4]]) else 4L
out <- fv2_out_dir()

league <- load_leagues()[["football_iceland"]]
sp <- fv2_split(fv2_results(sex), league, cutoff, horizon_end = as.Date("2026-10-31"))
train <- sp$train
teams <- sort(unique(c(train$home_team, train$away_team)))
train$home_nr <- match(train$home_team, teams)
train$away_nr <- match(train$away_team, teams)
n <- nrow(train)
long <- rbind(
  data.frame(m = seq_len(n), team = train$home_nr, date = train$match_date, side = "home"),
  data.frame(m = seq_len(n), team = train$away_nr, date = train$match_date, side = "away")
)
long <- long[order(long$team, long$date, long$m), ]
long$slot <- seq_len(nrow(long))
gap <- as.numeric(long$date) - stats::ave(as.numeric(long$date), long$team, FUN = function(x) c(NA, head(x, -1)))
# WHY: the 100-day cap on drift is production's, kept so this ladder isolates
# the home-advantage structure and nothing else.
long$sdt <- ifelse(is.na(gap), 0, sqrt(pmin(gap, 100)))
stan_data <- list(
  K = length(teams), N = n, S = nrow(long),
  slot_team = long$team, slot_sdt = long$sdt,
  first_slot = as.integer(tapply(long$slot, long$team, min)),
  last_slot = as.integer(tapply(long$slot, long$team, max)),
  home_slot = long$slot[long$side == "home"][order(long$m[long$side == "home"])],
  away_slot = long$slot[long$side == "away"][order(long$m[long$side == "away"])],
  home_team = train$home_nr,
  goals_home = as.integer(train$home_score), goals_away = as.integer(train$away_score)
)

mod <- cmdstanr::cmdstan_model(
  file.path("research/football-v2/stan", paste0(variant, ".stan")),
  dir = file.path(dirname(out), "exe"), quiet = TRUE
)
csv_dir <- file.path(out, sprintf("csv_%s_%s", variant, sex))
dir.create(csv_dir, showWarnings = FALSE)
t0 <- Sys.time()
fit <- mod$sample(
  data = stan_data, chains = 4, parallel_chains = par_chains,
  iter_warmup = 1000, iter_sampling = 1000, adapt_delta = 0.95, max_treedepth = 10,
  init = 0, seed = 20261115, refresh = 500, output_dir = csv_dir,
  show_messages = FALSE, show_exceptions = FALSE
)
wall <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
mat <- function(v) unclass(posterior::as_draws_matrix(fit$draws(v)))
hao <- mat("home_advantage_off")
had <- mat("home_advantage_def")
n_home <- tabulate(train$home_nr, length(teams))
total_draws <- as.numeric((hao + had) %*% n_home) / sum(n_home)
dg <- fit$diagnostic_summary(quiet = TRUE)
sp_diag <- fit$sampler_diagnostics(format = "df")

hold <- sp$pred
h <- match(hold$home_team, teams)
a <- match(hold$away_team, teams)
att <- mat("att_last")
def <- mat("def_last")
c0 <- as.numeric(mat("mean_log_goals"))
thin <- seq(1, nrow(att), by = 4)
eta_home <- c0[thin] + att[thin, h] + hao[thin, h] - def[thin, a]
eta_away <- c0[thin] + att[thin, a] - def[thin, h] - had[thin, h]
sc <- fv2_score(t(exp(eta_home)), t(exp(eta_away)), hold$home_score, hold$away_score)

res <- list(
  sex = sex, variant = variant, cutoff = cutoff, n = n, K = length(teams),
  raw_log_ratio = log(sum(train$home_score) / sum(train$away_score)),
  total_ha = c(mean = mean(total_draws), sd = stats::sd(total_draws), stats::quantile(total_draws, c(0.05, 0.95))),
  team_mean_total = mean(colMeans(hao + had)),
  team_ha_off = colMeans(hao), team_ha_def = colMeans(had),
  team_ha_off_sd = apply(hao, 2, stats::sd), team_ha_def_sd = apply(had, 2, stats::sd),
  wall_secs = wall, par_chains = par_chains,
  n_divergent = sum(dg$num_divergent), n_max_treedepth = sum(dg$num_max_treedepth),
  mean_leapfrog = mean(sp_diag$n_leapfrog__),
  holdout = c(n = nrow(hold), scoreline = mean(sc$ls_score), logloss_1x2 = -mean(sc$ls_1x2),
              p_home = mean(sc$p_home), obs_home = mean(hold$home_score > hold$away_score)),
  scores = cbind(hold[c("match_date", "home_team", "away_team")], sc)
)
saveRDS(res, file.path(out, sprintf("ha_%s_%s.rds", variant, sex)))
unlink(csv_dir, recursive = TRUE)
cat(sprintf(
  "DONE %s %s: total home effect %.3f (sd %.3f) vs raw log goal ratio %.3f | wall %.0fs, div %d, leapfrog %.0f | holdout p(home) %.3f vs %.3f observed, 1X2 %.4f, scoreline %.4f\n",
  sex, variant, res$total_ha[["mean"]], res$total_ha[["sd"]], res$raw_log_ratio, wall, res$n_divergent,
  res$mean_leapfrog, res$holdout[["p_home"]], res$holdout[["obs_home"]], res$holdout[["logloss_1x2"]], res$holdout[["scoreline"]]
))

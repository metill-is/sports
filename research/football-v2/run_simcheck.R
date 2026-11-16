# Do season-simulation outputs agree between engines? Fits one spec with NUTS,
# INLA (full integration) and INLA (empirical Bayes) at a mid-season date,
# feeds each set of draws through the adapter into production's own
# extract_football_iceland() (which runs the season simulation), and compares
# the final-position tables. Two halves of the NUTS draws give the size of
# the Monte Carlo noise. Everything is written to an isolated tree.
#
#   Rscript research/football-v2/run_simcheck.R <sex> <as_of> <variant> [n_draws]
# Run from the repo root.
options(width = 160)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
source("research/football-v2/variants.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
sex <- args[[1]]
as_of <- as.Date(args[[2]])
variant <- args[[3]]
n_draws <- if (length(args) >= 4L) as.integer(args[[4]]) else 2000L

root <- here::here("data")
out <- fv2_out_dir()
sim_root <- file.path(dirname(out), "simcheck", sprintf("%s_%s_%s", variant, sex, as_of))
unlink(sim_root, recursive = TRUE)
dir.create(sim_root, recursive = TRUE)
league <- load_leagues()[["football_iceland"]]

prep <- prepare_data(league, sex, end_date = as_of, root = root, schedule_horizon_days = 200L)
res <- fv2_results(sex, sort = FALSE)
train <- as.data.frame(model_training_results(res, league, end_date = as_of))
pred <- fv2_pred_from_prep(prep$pred_d, cell_of = fv2_cell)
spec <- do.call(fv2_build, c(
  list(train, pred, divisions = fv2_division_table(res, fv2_rank_order(league))),
  fv2_variants()[[variant]]
))
cat(sprintf(
  "%s %s as of %s: %d training matches, %d teams, %d fixtures ahead\n",
  variant, sex, as_of, spec$N, spec$K, nrow(pred)
))

csv_dir <- file.path(sim_root, "csv")
dir.create(csv_dir)
stan <- fv2_fit_stan(
  spec, fv2_stan_file(spec, "research/football-v2/stan"), file.path(dirname(out), "exe"), csv_dir,
  refresh = 0L
)
unlink(csv_dir, recursive = TRUE)
cat(sprintf("NUTS: %d divergences, %.0f leapfrogs per iteration\n", stan$diagnostics$n_divergent, stan$diagnostics$mean_leapfrog))
full <- fv2_fit_inla(spec, int_strategy = "auto", n_samp = n_draws, threads = "4:1")
eb <- fv2_fit_inla(spec, int_strategy = "eb", n_samp = n_draws, threads = "4:1")

take <- function(d, rows) {
  lapply(d, function(x) if (is.matrix(x)) x[rows, , drop = FALSE] else x[rows])
}
half <- length(stan$draws$c_home) / 2
sets <- list(
  nuts_a = take(stan$draws, which(stan$chain_id <= 2L)[seq_len(min(n_draws, half))]),
  nuts_b = take(stan$draws, which(stan$chain_id > 2L)[seq_len(min(n_draws, half))]),
  inla_full = full$draws, inla_eb = eb$draws
)
tables <- lapply(names(sets), function(nm) {
  fit <- fv2_as_fit(sets[[nm]], spec, prep)
  ex_root <- file.path(sim_root, nm)
  suppressMessages(extract_football_iceland(
    fit, league, sex,
    fit_date = as_of, end_date = as_of, root = root, prep = prep, extracts_root = ex_root
  ))
  part <- file.path(ex_root, "sport=football", "country=iceland", paste0("sex=", sex), paste0("fit_date=", as_of))
  cat("  simulated:", nm, "\n")
  cbind(set = nm, as.data.frame(arrow::read_parquet(file.path(part, "final_positions.parquet"))))
})
pos <- do.call(rbind, tables)
wide <- stats::reshape(pos, idvar = c("team", "placement", "division"), timevar = "set", direction = "wide")
wide[is.na(wide)] <- 0
size <- stats::ave(wide$placement, wide$division, FUN = max)
wide$kind <- ifelse(wide$placement == 1L, "first place", ifelse(wide$placement > size - 2L, "bottom two", "other"))

nuts <- (wide$probability.nuts_a + wide$probability.nuts_b) / 2
cmp <- function(label, x, y, rows = rep(TRUE, nrow(wide))) {
  d <- abs(x - y)[rows]
  data.frame(comparison = label, cells = sum(rows), mean_abs = mean(d), max_abs = max(d))
}
kinds <- list(`all places` = rep(TRUE, nrow(wide)), `first place` = wide$kind == "first place", `bottom two` = wide$kind == "bottom two")
summ <- do.call(rbind, lapply(names(kinds), function(k) {
  cbind(sex = sex, outcome = k, rbind(
    cmp("NUTS half against NUTS half", wide$probability.nuts_a, wide$probability.nuts_b, kinds[[k]]),
    cmp("INLA full against NUTS", wide$probability.inla_full, nuts, kinds[[k]]),
    cmp("INLA empirical Bayes against NUTS", wide$probability.inla_eb, nuts, kinds[[k]])
  ))
}))
print(format(summ, digits = 3), row.names = FALSE)

top <- wide[wide$division == "BD" & wide$kind != "other", ]
top <- stats::aggregate(
  cbind(nuts = (probability.nuts_a + probability.nuts_b) / 2, inla_full = probability.inla_full, inla_eb = probability.inla_eb) ~ team + kind,
  top, sum
)
top <- top[order(top$kind, -top$nuts), ]
print(format(top[top$nuts > 0.01 | top$inla_full > 0.01, ], digits = 3), row.names = FALSE)

saveRDS(
  list(
    sex = sex, as_of = as_of, variant = variant, n_draws = n_draws, summary = summ, top_division = top,
    nuts_divergences = stan$diagnostics$n_divergent, fixtures = nrow(pred),
    dirty = system2("git", c("status", "--porcelain", "--", "data"), stdout = TRUE)
  ),
  file.path(out, sprintf("simcheck_%s_%s_%s.rds", variant, sex, as_of))
)
cat("SIMCHECK DONE\n")

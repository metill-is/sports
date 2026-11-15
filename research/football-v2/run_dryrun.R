# End-to-end dry run of the cutover path: production's prepare_data(), the
# INLA engine, the adapter, then production's own extract and publish layers,
# all writing into an isolated tree. Nothing under data/ is written.
#
#   Rscript research/football-v2/run_dryrun.R <sex> <as_of> <variant> [n_samp] [threads]
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
n_samp <- if (length(args) >= 4L) as.integer(args[[4]]) else 2000L
threads <- if (length(args) >= 5L) args[[5]] else "4:1"

root <- here::here("data")
dry <- file.path(dirname(fv2_out_dir()), "dryrun", sprintf("%s_%s_%s", variant, sex, as_of))
unlink(dry, recursive = TRUE)
dir.create(dry, recursive = TRUE)
league <- load_leagues()[["football_iceland"]]
stamp <- function(label, t0) cat(sprintf("  %-28s %.1fs\n", label, as.numeric(difftime(Sys.time(), t0, units = "secs"))))

t0 <- Sys.time()
prep <- prepare_data(league, sex, end_date = as_of, root = root, schedule_horizon_days = 200L)
res <- fv2_results(sex, sort = FALSE)
train <- as.data.frame(model_training_results(res, league, end_date = as_of))
pred <- fv2_pred_from_prep(prep$pred_d, cell_of = fv2_cell)
spec <- do.call(fv2_build, c(
  list(train, pred, divisions = fv2_division_table(res, fv2_rank_order(league))),
  fv2_variants()[[variant]]
))
cat(sprintf(
  "%s %s as of %s: %d training matches, %d teams, %d fixtures ahead, %d states per field\n",
  variant, sex, as_of, spec$N, spec$K, nrow(pred), spec$S
))
stamp("prepare", t0)

t0 <- Sys.time()
f <- fv2_fit_inla(spec, int_strategy = "eb", n_samp = n_samp, threads = threads)
stamp("INLA fit + draws", t0)
fit <- fv2_as_fit(f$draws, spec, prep)

t0 <- Sys.time()
beliefs <- extract_posteriors(fit, prep$pred_d, league, sex, fit_date = as_of)
stamp("extract_posteriors", t0)
cat(sprintf("  beliefs: %d rows = %d fixtures x %d draws\n", nrow(beliefs), nrow(prep$pred_d), n_samp))

extracts_root <- file.path(dry, "extracts")
t0 <- Sys.time()
extract_football_iceland(
  fit, league, sex,
  fit_date = as_of, end_date = as_of, root = root, prep = prep, extracts_root = extracts_root
)
stamp("extract_football_iceland", t0)
part <- file.path(extracts_root, "sport=football", "country=iceland", paste0("sex=", sex), paste0("fit_date=", as_of))
cat("  extract files:", paste(list.files(part), collapse = ", "), "\n")

t0 <- Sys.time()
extracted <- read_extracted_iceland(league, sex = sex, fit_date = as_of, extracts_root = extracts_root)
publish_iceland_league(
  extracted = extracted, league = league, sex = sex, end_date = as_of, root = root,
  output_root = file.path(dry, "publish"), extracts_root = extracts_root,
  archive_root = file.path(dry, "archive"),
  round_predictions_history_root = file.path(dry, "round_predictions_history")
)
stamp("publish_iceland_league", t0)
json <- list.files(file.path(dry, "publish"), recursive = TRUE, pattern = "\\.json$")
cat(sprintf("  published %d JSON files under %s\n", length(json), file.path(dry, "publish")))

prod_part <- file.path(root, "beliefs", "extracts", "sport=football", "country=iceland", paste0("sex=", sex), paste0("fit_date=", as_of))
cmp <- NULL
if (file.exists(file.path(prod_part, "predicted_matches.parquet"))) {
  outcome <- function(path) {
    d <- as.data.frame(arrow::read_parquet(file.path(path, "predicted_matches.parquet")))
    d$w <- d$n / stats::ave(d$n, d$match_date, d$home_team, d$away_team, FUN = sum)
    key <- paste(d$match_date, d$home_team, d$away_team)
    data.frame(
      key = sort(unique(key)),
      p_home = as.numeric(tapply(d$w * (d$home_goals > d$away_goals), key, sum)),
      p_draw = as.numeric(tapply(d$w * (d$home_goals == d$away_goals), key, sum)),
      xg_home = as.numeric(tapply(d$w * d$home_goals, key, sum)),
      xg_away = as.numeric(tapply(d$w * d$away_goals, key, sum))
    )
  }
  a <- tryCatch(outcome(part), error = function(e) NULL)
  b <- tryCatch(outcome(prod_part), error = function(e) NULL)
  if (!is.null(a) && !is.null(b)) {
    cmp <- merge(a, b, by = "key", suffixes = c("_v2", "_prod"))
    cat(sprintf(
      "  against production's stored extract (%d fixtures): mean P(home) v2 %.3f, production %.3f; mean abs diff %.3f; mean xG home v2 %.2f / production %.2f, away %.2f / %.2f\n",
      nrow(cmp), mean(cmp$p_home_v2), mean(cmp$p_home_prod), mean(abs(cmp$p_home_v2 - cmp$p_home_prod)),
      mean(cmp$xg_home_v2), mean(cmp$xg_home_prod), mean(cmp$xg_away_v2), mean(cmp$xg_away_prod)
    ))
  }
}
saveRDS(
  list(
    sex = sex, as_of = as_of, variant = variant, n_samp = n_samp,
    dims = c(N = spec$N, K = spec$K, fixtures = nrow(pred), S = spec$S),
    extract_files = list.files(part), json = json, compare = cmp,
    dirty = system2("git", c("status", "--porcelain", "--", "data"), stdout = TRUE)
  ),
  file.path(fv2_out_dir(), sprintf("dryrun_%s_%s_%s.rds", variant, sex, as_of))
)
cat("DRYRUN DONE\n")

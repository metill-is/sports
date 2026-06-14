#!/usr/bin/env Rscript
# One-off / recovery: rebuild the WC accountability snapshot log from the git
# history of predictions.json, then regenerate results.json. Use to seed the
# accountability surface mid-tournament (recovering pre-match predictions for
# matches already played) or to recover a lost log. No Stan fit required — reads
# the committed facts store + git history only.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

root <- here::here("data")

n <- wc_backfill_snapshots(root = root, ref = "HEAD")
cat(sprintf("folded in %d prediction-snapshot commit(s) from git history\n", n))

s <- wc_structure()
fx <- wc_group_fixtures(s, root = root)
cat(sprintf("group fixtures: %d (%d played)\n", nrow(fx), sum(fx$played %in% TRUE)))

is_name <- .wc_country_namer(
  here::here("data", "wc", "structure", "country_names_is.csv")
)
res <- wc_build_results(fx, root = root, is_name = is_name)

generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
out_path <- here::here("data", "publish", "world_cup", "karla", "results.json")
jsonlite::write_json(
  c(list(generated_at = generated_at), res),
  out_path,
  auto_unbox = TRUE, pretty = TRUE
)

cat(sprintf(
  "\nresults.json: %d played match(es) accounted, %d hit, hit-rate %.0f%%\n",
  res$summary$n_played, res$summary$n_hit, 100 * res$summary$hit_rate
))
if (res$summary$n_played > 0) {
  cat("\n=== accountability (most recent first) ===\n")
  for (m in res$matches) {
    cat(sprintf(
      "  %s  %-14s %d-%d %-14s | Metill gaf %2.0f%%  %s\n",
      m$match_date, m$home_is, m$home_score, m$away_score, m$away_is,
      100 * m$p_outcome, if (isTRUE(m$hit)) "rett" else "X"
    ))
  }
}

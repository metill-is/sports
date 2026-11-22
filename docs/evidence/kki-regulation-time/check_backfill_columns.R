# Do the partitions the backfill rewrote differ from HEAD only in the three new columns?
# Usage (repository root, before committing the backfill):
#   Rscript docs/evidence/kki-regulation-time/check_backfill_columns.R
options(width = 160)
files <- system("git diff --name-only HEAD -- data/facts/results", intern = TRUE)
new_cols <- c("game_id", "home_score_reg", "away_score_reg")
key <- c("match_date", "division", "home_team", "away_team")
for (f in files) {
  tmp <- tempfile(fileext = ".parquet")
  system2("git", c("show", sprintf("HEAD:%s", f)), stdout = tmp)
  a <- as.data.frame(arrow::read_parquet(tmp))
  b <- as.data.frame(arrow::read_parquet(f))
  a <- a[do.call(order, a[key]), , drop = FALSE]
  b <- b[do.call(order, b[key]), , drop = FALSE]
  rownames(a) <- rownames(b) <- NULL
  common <- intersect(names(a), names(b))
  same <- identical(a[common], b[common])
  cat(sprintf("%-75s rows %4d -> %4d | old columns identical: %s | added: %s\n", sub("data/facts/results/", "", f), nrow(a), nrow(b), same,
              paste(setdiff(names(b), names(a)), collapse = ",")))
  unlink(tmp)
}

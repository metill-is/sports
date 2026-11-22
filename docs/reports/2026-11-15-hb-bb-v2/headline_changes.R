# Every headline number, the second run against the final one. check_headlines.R prints
# each headline number on a line of its own; this compares its output for the second run
# (kept as logs/check_headlines.second-run.log) with its output now, line by line within
# each block, and keeps the lines whose text differs.
# A log written before check_headlines.R put two spaces after every label (22 November 2026)
# loses the labels longer than its padding: they are cut at their first space and then
# dropped as duplicates.
# The blocks about the runs themselves (wf_status.rds, run_identity.rds) are left out: they
# describe different comparisons in the two states, not the same number twice.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/headline_changes.R")'
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
read_blocks <- function(f) {
  x <- readLines(f, warn = FALSE)
  x <- x[nzchar(trimws(x))]
  block <- cumsum(startsWith(x, "== "))
  out <- split(x[!startsWith(x, "== ")], sub("^== ([^:]+):.*", "\\1", x[startsWith(x, "== ")])[block[!startsWith(x, "== ")]])
  out[!grepl("^(wf_status|run_identity)", names(out))]
}
split_line <- function(l) {
  l <- trimws(l)
  cut <- regexpr(" {2,}", l)
  if (cut < 0) cut <- regexpr(" ", l)
  c(label = trimws(substr(l, 1, cut)), value = trimws(substr(l, cut, nchar(l))))
}
old <- read_blocks(file.path(OUT, "logs", "check_headlines.second-run.log"))
new <- read_blocks(file.path(OUT, "logs", "check_headlines.log"))
rows <- list()
n_lines <- 0L
for (b in intersect(names(old), names(new))) {
  a <- do.call(rbind, lapply(old[[b]], split_line))
  z <- do.call(rbind, lapply(new[[b]], split_line))
  # WHY by label: a block can gain a line between the two states; a label that occurs
  # once in both is the same number.
  both <- intersect(a[!duplicated(a[, "label"]) & !duplicated(a[, "label"], fromLast = TRUE), "label"],
                    z[!duplicated(z[, "label"]) & !duplicated(z[, "label"], fromLast = TRUE), "label"])
  n_lines <- n_lines + length(both)
  for (l in both) {
    va <- a[a[, "label"] == l, "value"]
    vz <- z[z[, "label"] == l, "value"]
    if (!identical(va, vz)) rows[[length(rows) + 1L]] <- data.frame(block = b, label = l, second = va, final = vz)
  }
}
changed <- if (length(rows)) do.call(rbind, rows) else data.frame(block = character(), label = character(), second = character(), final = character())
cat(sprintf("%d headline lines compared, %d differ\n", n_lines, nrow(changed)))
print(changed[, -1], right = FALSE, row.names = FALSE)
saveRDS(list(n_lines = n_lines, changed = changed), file.path(OUT, "out", "headline_changes.rds"))

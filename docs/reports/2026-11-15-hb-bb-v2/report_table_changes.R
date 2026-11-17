# Every table row of the report, an earlier build against the current one. Rows are matched
# by section, table, first cell and their order among rows sharing that first cell; a row
# is listed when any cell's text differs. The appendix on corrections, the table of result
# files and the section on the runs themselves are left out: they describe the runs and
# are not the same number twice.
# For a cell holding "value +- standard error" pairs the move is also given in units of
# the newer standard error.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/report_table_changes.R")' <earlier report.md> [current report.md]
options(width = 250)
args <- commandArgs(trailingOnly = TRUE)
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
old_file <- if (length(args) >= 1) args[[1]] else file.path(OUT, "out", "second-run", "report.md")
new_file <- if (length(args) >= 2) args[[2]] else "docs/reports/2026-11-15-hb-bb-v2/report.md"
left_out <- c("The R-INLA fits, run four times", "The walk-forward, run twice", "Corrected after review", "Where each number comes from")
MINUS <- "\u2212"
PM <- "\u00b1"

read_rows <- function(path) {
  x <- readLines(path, encoding = "UTF-8", warn = FALSE)
  h2 <- h3 <- ""
  tno <- 0L
  in_table <- FALSE
  header <- NULL
  out <- list()
  for (l in x) {
    if (startsWith(l, "## ")) {
      h2 <- substring(l, 4)
      h3 <- ""
      tno <- 0L
    } else if (startsWith(l, "### ")) {
      h3 <- substring(l, 5)
      tno <- 0L
    }
    if (!startsWith(l, "|")) {
      in_table <- FALSE
      next
    }
    cells <- trimws(strsplit(sub("[|][[:space:]]*$", "", sub("^[|]", "", l)), " | ", fixed = TRUE)[[1]])
    # Markdown escapes (a backslash before a bracket, a pipe, a star) are not part of the text.
    cells <- gsub("\\\\(.)", "\\1", cells)
    if (!in_table) {
      tno <- tno + 1L
      in_table <- TRUE
      header <- cells
      next
    }
    if (all(grepl("^-+$", cells))) next
    out[[length(out) + 1L]] <- list(section = if (nzchar(h3)) paste(h2, h3, sep = " / ") else h2, h3 = h3, table = tno, header = header, cells = cells)
  }
  key <- vapply(out, function(r) paste(r$section, r$table, r$cells[1], sep = " :: "), "")
  nth <- stats::ave(seq_along(key), key, FUN = seq_along)
  names(out) <- paste(key, nth, sep = " #")
  out
}
# Every "value +- se" pair in a cell.
pairs <- function(txt) {
  txt <- gsub(MINUS, "-", gsub(",", "", txt), fixed = TRUE)
  m <- regmatches(txt, gregexpr(paste0("[+-]?[0-9]+[.]?[0-9]* ", PM, " [0-9]+[.]?[0-9]*"), txt))[[1]]
  if (!length(m)) {
    return(NULL)
  }
  cbind(value = as.numeric(sub(" .*", "", m)), se = as.numeric(sub(".* ", "", m)))
}
move_se <- function(a, b) {
  pa <- pairs(a)
  pb <- pairs(b)
  if (is.null(pa) || is.null(pb) || nrow(pa) != nrow(pb) || any(pb[, "se"] == 0)) {
    return(NA_real_)
  }
  max(abs(pb[, "value"] - pa[, "value"]) / pb[, "se"])
}
not_a_result <- function(header) grepl("time|seconds|slot|wall", header, ignore.case = TRUE)

old <- read_rows(old_file)
new <- read_rows(new_file)
keep <- function(r) !(r$h3 %in% left_out)
old <- old[vapply(old, keep, TRUE)]
new <- new[vapply(new, keep, TRUE)]
both <- intersect(names(old), names(new))
changed <- do.call(rbind, lapply(both, function(k) {
  a <- old[[k]]
  b <- new[[k]]
  if (length(a$cells) != length(b$cells)) {
    return(data.frame(key = k, section = b$section, first = b$cells[1], row = b$cells[1], column = "(row shape)", old = paste(a$cells, collapse = " | "), new = paste(b$cells, collapse = " | "), result = TRUE, move_se = NA_real_))
  }
  i <- which(a$cells != b$cells)
  if (!length(i)) {
    return(NULL)
  }
  data.frame(
    key = k, section = b$section, first = b$cells[1], row = paste(b$cells[seq_len(min(2L, i[1] - 1L))], collapse = ", "), column = b$header[i], old = a$cells[i], new = b$cells[i],
    result = !not_a_result(b$header[i]), move_se = mapply(move_se, a$cells[i], b$cells[i])
  )
}))
rownames(changed) <- NULL
res <- changed[changed$result, ]
rows_changed <- length(unique(res$key))
top <- res[which.max(res$move_se), ]
cat(sprintf(
  "%d table rows before, %d now (appendix and run section left out); %d cells in %d rows differ in a result, %d cells in a time or machine record; rows in one build only: %d / %d\n",
  length(old), length(new), nrow(res), rows_changed, sum(!changed$result), length(setdiff(names(old), names(new))), length(setdiff(names(new), names(old)))
))
cat(sprintf("cells with a standard error: %d; moved by more than one standard error: %d; largest move %.2f (%s, %s, %s: %s -> %s)\n",
  sum(!is.na(res$move_se)), sum(res$move_se > 1, na.rm = TRUE), top$move_se, top$section, top$row, top$column, top$old, top$new))
print(res[order(-res$move_se), !names(res) %in% c("key", "first")][seq_len(min(12L, nrow(res))), ], right = FALSE, row.names = FALSE)
saveRDS(
  list(old_file = old_file, rows_old = length(old), rows_new = length(new), changed = changed, only_old = setdiff(names(old), names(new)), only_new = setdiff(names(new), names(old))),
  file.path(OUT, "out", "report_table_changes.rds")
)
# A pipe inside a cell would end the cell in a Markdown table.
bar <- function(x) gsub("|", "\\|", x, fixed = TRUE)
md <- c(
  "# Every table cell of the report that differs between the second run's build and the current one", "",
  sprintf("Earlier build: `%s`. Left out: %s.", old_file, paste(sprintf("\"%s\"", left_out), collapse = ", ")), "",
  "| Section | Row | Column | Second run | Now | A result? | Move, in standard errors |", "| --- | --- | --- | --- | --- | --- | --- |",
  sprintf("| %s | %s | %s | %s | %s | %s | %s |", bar(changed$section), bar(changed$row), bar(changed$column), bar(changed$old), bar(changed$new), ifelse(changed$result, "yes", "time or machine record"),
          ifelse(is.na(changed$move_se), "", sprintf("%.2f", changed$move_se)))
)
writeLines(md, file.path(OUT, "logs", "report_table_changes.md"), useBytes = TRUE)

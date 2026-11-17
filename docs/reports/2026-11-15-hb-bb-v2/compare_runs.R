# Are two complete runs of every R-INLA result identical? Compares every stored number of
# run a against run b, file by file, in the order stored. Timings, the record of what else
# the machine was doing and data-frame row names are the only things left out, because they
# are not results.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/compare_runs.R")' <results of run a> <results of run b> [name]
#   each folder holds the *.rds files as the experiment scripts write them; the comparison
#   is stored as out/<name>.rds (default run_identity) with, for every file that differs,
#   which stored components differ, in how many values and by how much.
options(width = 200)
args <- commandArgs(trailingOnly = TRUE)
dir_a <- args[[1]]
dir_b <- args[[2]]
name <- if (length(args) >= 3) args[[3]] else "run_identity"
OUT <- Sys.getenv("HBBB_OUT", "/Users/brynjolfurjonsson/.claude-reviews/sports-hb-bb-v2")
not_results <- c("secs", "secs_m", "secs_t", "cpu", "machine")

# Drop timing fields at any depth, so that what is compared is results only.
strip <- function(x) {
  if (is.data.frame(x)) {
    return(x[, setdiff(names(x), not_results), drop = FALSE])
  }
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[!names(x) %in% not_results]
    return(lapply(x, strip))
  }
  x
}
# Largest absolute difference over every numeric value two stripped objects hold.
max_diff <- function(a, b) {
  if (is.list(a) && is.list(b) && length(a) == length(b)) {
    d <- unlist(mapply(max_diff, a, b))
    if (!length(d)) {
      return(0)
    }
    # NA marks a component that cannot be compared value by value (different shape or type).
    return(if (all(is.na(d))) NA_real_ else max(d, na.rm = TRUE))
  }
  if (is.numeric(a) && is.numeric(b) && length(a) == length(b)) {
    d <- abs(a - b)
    return(if (all(is.na(d))) 0 else max(d, na.rm = TRUE))
  }
  if (identical(a, b)) 0 else NA_real_
}
count_numbers <- function(x) {
  if (is.list(x)) {
    return(sum(vapply(x, count_numbers, 0)))
  }
  if (is.numeric(x)) length(x) else 0
}
# Every stored component in which two stripped objects differ: where it sits, how many
# values it holds, how many of them differ and the largest absolute difference.
differing <- function(a, b, path = "") {
  if (identical(a, b)) {
    return(NULL)
  }
  if (is.list(a) && is.list(b) && length(a) == length(b)) {
    nm <- if (is.null(names(a))) seq_along(a) else names(a)
    return(do.call(rbind, lapply(seq_along(a), function(i) differing(a[[i]], b[[i]], paste0(path, "$", nm[i])))))
  }
  if (is.numeric(a) && is.numeric(b) && length(a) == length(b)) {
    d <- abs(a - b)
    return(data.frame(component = path, values = length(a), differ = sum(d > 0 | is.na(a) != is.na(b), na.rm = TRUE), max_abs_diff = max(d, na.rm = TRUE)))
  }
  data.frame(component = path, values = length(a), differ = NA_integer_, max_abs_diff = NA_real_)
}

# The files the R-INLA fits write: the eight walk-forwards, the descriptive fits, the
# fixed-cutoff fits and the weekly-correlation fits. Files derived from them are not compared.
is_fit_file <- function(f) grepl("^(wf_[hb]b_[mf]_|descriptives|inla_validate_|within_rho).*[.]rds$", f)
in_a <- list.files(dir_a)[is_fit_file(list.files(dir_a))]
in_b <- list.files(dir_b)[is_fit_file(list.files(dir_b))]
files <- sort(intersect(in_a, in_b))
only_one <- setdiff(union(in_a, in_b), files)
leaves <- list()
tab <- do.call(rbind, lapply(files, function(f) {
  a <- strip(readRDS(file.path(dir_a, f)))
  b <- strip(readRDS(file.path(dir_b, f)))
  # WHY the row names go: they are the positions the rows had in the results table as it
  # was read, and that order is not fixed between processes (the two runs' row names
  # differ). They are labels, not results. The rows are not sorted here: the order in which
  # they are stored is compared along with their values.
  if (grepl("^wf_", f)) {
    rownames(a$pred) <- rownames(b$pred) <- NULL
  }
  if (f == "descriptives.rds") {
    for (cl in names(a)) {
      rownames(a[[cl]]$d) <- rownames(b[[cl]]$d) <- NULL
    }
  }
  w <- differing(a, b)
  if (!is.null(w)) leaves[[f]] <<- cbind(file = f, w)
  data.frame(file = f, numbers = count_numbers(a), identical = identical(a, b), max_abs_diff = max_diff(a, b))
}))
leaves <- do.call(rbind, leaves)
rownames(leaves) <- NULL
print(tab, row.names = FALSE)
if (!is.null(leaves)) {
  cat("\nStored components that differ:\n")
  print(leaves, row.names = FALSE)
}
cat(sprintf(
  "\n%d files, %s numbers compared; identical files: %d; files in only one run: %s\n", nrow(tab), format(sum(tab$numbers), big.mark = ","),
  sum(tab$identical), if (length(only_one)) paste(only_one, collapse = ", ") else "none"
))
saveRDS(list(files = tab, differing = leaves, only_one = only_one, run_a = dir_a, run_b = dir_b), file.path(OUT, "out", paste0(name, ".rds")))

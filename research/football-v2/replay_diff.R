# Compare two trees of published JSON file by file and field by field.
#
#   Rscript research/football-v2/replay_diff.R <tree_a> <tree_b> <out.rds>
#
# Records inside an array are matched by what they are (team, component,
# placement, ...), not by where they sit, so a reordering is not a difference
# and a changed value is reported against the record it belongs to.
options(width = 200)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 3L)
tree_a <- normalizePath(args[[1]])
tree_b <- normalizePath(args[[2]])
out_file <- args[[3]]

# Fields that identify a record, in the order they are tried. A record's key is
# the shortest prefix of the fields it has that tells its records apart, so
# `points` identifies a row of a points distribution (team, points) but stays
# a value in a league table (team alone is enough there). The prefix is chosen
# for the two trees together: a history with one date in one tree and forty in
# the other must be keyed by date in both, or the same row gets two names.
ID_ORDER <- c(
  "team", "home_team", "home", "away_team", "away", "match_date", "date", "match_no",
  "component", "location", "coverage", "placement", "round_name", "as_of", "fit_date",
  "season", "round", "points", "diff", "home_goals", "away_goals", "goals"
)

is_record_array <- function(x) {
  is.list(x) && length(x) > 0L && is.null(names(x)) &&
    all(vapply(x, function(e) is.list(e) && !is.null(names(e)), logical(1)))
}

record_ids <- function(recs, min_fields = 0L) {
  shared <- Reduce(intersect, lapply(recs, names))
  key <- rep("", length(recs))
  used <- 0L
  for (f in intersect(ID_ORDER, shared)) {
    v <- vapply(recs, function(r) {
      z <- r[[f]]
      if (is.null(z) || is.list(z) || length(z) != 1L) NA_character_ else as.character(z)
    }, character(1))
    key <- paste(key, paste0(f, "=", v), sep = if (used == 0L) "" else ",")
    used <- used + 1L
    if (!anyDuplicated(key) && used >= min_fields) {
      return(list(ids = key, used = used))
    }
  }
  list(ids = paste0("#", seq_along(recs)), used = used)
}

key_fields <- new.env()
array_name <- function(record, field) gsub("\\[[^]]*\\]", "", paste0(record, "/", field))

# `learn = TRUE` only records how many key fields each array needs.
flatten_json <- function(file, surface, learn = FALSE) {
  x <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  chunks <- list()
  emit <- function(record, field, value, num) {
    chunks[[length(chunks) + 1L]] <<- data.frame(record = record, field = field, value = value, num = num)
  }
  leaf <- function(v) {
    if (is.null(v) || length(v) == 0L) {
      return(list(value = "<null>", num = NA_real_))
    }
    list(value = as.character(v), num = if (is.numeric(v)) as.numeric(v) else NA_real_)
  }
  walk <- function(x, record, field) {
    if (!is.list(x)) {
      l <- leaf(x)
      emit(record, field, l$value, l$num)
    } else if (length(x) == 0L) {
      emit(record, field, "<empty>", NA_real_)
    } else if (!is.null(names(x))) {
      inner <- if (nzchar(field)) paste(record, field, sep = "/") else record
      for (n in names(x)) walk(x[[n]], inner, n)
    } else if (is_record_array(x)) {
      slot <- paste(surface, array_name(record, field))
      k <- record_ids(x, if (learn) 0L else get0(slot, key_fields, ifnotfound = 0L))
      if (learn) assign(slot, max(k$used, get0(slot, key_fields, ifnotfound = 0L)), envir = key_fields)
      ids <- paste0(record, "/", field, "[", k$ids, "]")
      for (f in unique(unlist(lapply(x, names)))) {
        vals <- lapply(x, `[[`, f)
        if (all(vapply(vals, function(v) !is.list(v), logical(1)))) {
          l <- lapply(vals, leaf)
          emit(ids, f, vapply(l, `[[`, character(1), "value"), vapply(l, `[[`, numeric(1), "num"))
        } else {
          for (i in seq_along(x)) if (!is.null(vals[[i]])) walk(vals[[i]], ids[i], f)
        }
      }
    } else {
      for (i in seq_along(x)) walk(x[[i]], paste0(record, "/", field, "[", i, "]"), paste0(field, "[]"))
    }
  }
  walk(x, "", "")
  do.call(rbind, chunks)
}

json_files <- function(tree) sort(list.files(tree, pattern = "\\.json$", recursive = TRUE))
files_a <- json_files(tree_a)
files_b <- json_files(tree_b)
common <- intersect(files_a, files_b)

for (f in common) {
  for (tree in c(tree_a, tree_b)) flatten_json(file.path(tree, f), basename(f), learn = TRUE)
}
leaves <- do.call(rbind, lapply(common, function(f) {
  a <- flatten_json(file.path(tree_a, f), basename(f))
  b <- flatten_json(file.path(tree_b, f), basename(f))
  m <- merge(a, b, by = c("record", "field"), all = TRUE, suffixes = c("_a", "_b"))
  m$file <- f
  m
}))

both <- !is.na(leaves$value_a) & !is.na(leaves$value_b)
numeric_pair <- both & !is.na(leaves$num_a) & !is.na(leaves$num_b)
leaves$diff <- ifelse(numeric_pair, leaves$num_b - leaves$num_a, NA_real_)
leaves$status <- ifelse(
  !both, ifelse(is.na(leaves$value_b), "only_a", "only_b"),
  ifelse(
    leaves$value_a == leaves$value_b | (numeric_pair & abs(leaves$diff) < 1e-12), "equal",
    ifelse(numeric_pair, "number", "text")
  )
)
leaves$surface <- basename(leaves$file)
leaves$division <- basename(dirname(leaves$file))

by_field <- do.call(rbind, lapply(split(leaves, list(leaves$surface, leaves$field), drop = TRUE), function(d) {
  changed <- d$status == "number"
  data.frame(
    surface = d$surface[1], field = d$field[1], n = nrow(d),
    equal = sum(d$status == "equal"), number = sum(changed), text = sum(d$status == "text"),
    only_a = sum(d$status == "only_a"), only_b = sum(d$status == "only_b"),
    mean_abs = if (any(changed)) mean(abs(d$diff[changed])) else NA_real_,
    max_abs = if (any(changed)) max(abs(d$diff[changed])) else NA_real_,
    mean_abs_all = mean(abs(d$diff), na.rm = TRUE)
  )
}))
rownames(by_field) <- NULL
by_field <- by_field[order(by_field$surface, by_field$field), ]

result <- list(
  tree_a = tree_a, tree_b = tree_b,
  only_in_a = setdiff(files_a, files_b), only_in_b = setdiff(files_b, files_a),
  n_files = length(common), n_leaves = nrow(leaves),
  by_field = by_field,
  changed = leaves[leaves$status != "equal", c("division", "surface", "record", "field", "value_a", "value_b", "diff", "status")]
)
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(result, out_file)

cat(sprintf(
  "%d files in both trees, %d only in A, %d only in B; %d values compared, %d differ\n",
  length(common), length(result$only_in_a), length(result$only_in_b), nrow(leaves), nrow(result$changed)
))
show <- by_field[by_field$equal < by_field$n, ]
show$mean_abs <- signif(show$mean_abs, 3)
show$max_abs <- signif(show$max_abs, 3)
show$mean_abs_all <- NULL
print(show, row.names = FALSE)

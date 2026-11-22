# Can headline_changes.R read back every label check_headlines.R prints? Builds one line per
# literal label the way check_headlines.R's say() prints it, splits it with
# headline_changes.R's split_line(), and lists the labels that do not come back whole.
# Usage (repository root): Rscript docs/reports/2026-11-15-hb-bb-v2/evidence/check_headline_labels.R
D <- "docs/reports/2026-11-15-hb-bb-v2"
fn <- function(file, name) {
  ex <- parse(file.path(D, file), keep.source = FALSE)
  for (e in ex) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name(name))) return(eval(e[[3]]))
  }
  stop(name, " not found in ", file)
}
say_src <- fn("check_headlines.R", "say")
split_line <- fn("headline_changes.R", "split_line")
# Every say("literal", ...) in check_headlines.R.
calls <- list()
walk <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1]], as.name("say")) && length(e) >= 2 && is.character(e[[2]])) calls[[length(calls) + 1L]] <<- e[[2]]
    for (a in as.list(e)[-1]) if (!missing(a)) walk(a)
  }
}
for (e in parse(file.path(D, "check_headlines.R"), keep.source = FALSE)) walk(e)
labels <- unique(unlist(calls))
lines <- vapply(labels, function(l) trimws(capture.output(say_src(l, "%s", "1.23 / 4.56 (n 7)")), "right"), "")
back <- vapply(lines, function(l) split_line(l)[["label"]], "")
bad <- labels[back != trimws(labels)]
cat(sprintf("%d literal labels; %d do not come back whole\n", length(labels), length(bad)))
if (length(bad)) for (b in bad) cat("  ", b, " -> ", back[labels == b][1], "\n", sep = "")

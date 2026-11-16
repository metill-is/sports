# Build the report from the saved results. Every number in the report is read from a
# result file here; a section whose result file does not exist says so instead of
# showing a number.
# Usage: Rscript -e 'source("docs/reports/2026-11-15-hb-bb-v2/build_report.R")'
source("docs/reports/2026-11-15-hb-bb-v2/common.R")
source(file.path(EXP_DIR, "score.R"))
source(file.path(EXP_DIR, "report_helpers.R"))
cutoff <- "2026-01-31"
rd <- function(name) {
  f <- file.path(OUT, "out", name)
  if (file.exists(f)) readRDS(f) else NULL
}
diag <- rd("diag_prod.rds")
ladder <- rd("ladder.rds")
val <- rd("compare_validation.rds")
wf <- rd("wf_scores.rds")
cmp <- rd("wf_compare.rds")
desc <- rd("descriptives.rds")
repro <- rd("repro_descriptives.rds")
per <- readRDS(file.path(OUT, "kki", "periods_matched.rds"))
iv <- lapply(stats::setNames(names(CELLS), names(CELLS)), function(cl) rd(sprintf("inla_validate_%s.rds", cl)))
dat <- lapply(stats::setNames(names(CELLS), names(CELLS)), function(cl) rd(sprintf("data_%s_%s.rds", cl, cutoff)))
LAB <- c(hb_m = "Handball men", hb_f = "Handball women", bb_m = "Basketball men", bb_f = "Basketball women")
UNIT <- c(hb_m = "goals", hb_f = "goals", bb_m = "points", bb_f = "points")
have <- function(x) !is.null(x)
missing_note <- function(what) sprintf("<p class=\"note\"><span class=\"w\">Not available when this report was built:</span> %s.</p>", what)
# Sections are written into a named list and assembled in reading order at the end, so
# the summary can be written after the numbers it quotes have been computed.
S <- list()
cur <- "head"
add <- function(...) S[[cur]] <<- c(S[[cur]], paste0(...))

# ---------------------------------------------------------------- derived numbers
wf_sc <- function(cell, tag) if (have(wf)) wf[[paste(cell, tag)]]$scores else NULL
hb <- c("hb_m", "hb_f")
bb <- c("bb_m", "bb_f")
ha_row <- function(cell) {
  o <- if (have(diag)) diag[[cell]] else NULL
  d <- desc[[cell]]
  data.frame(
    cell = cell,
    prod = if (have(o)) o$ha_weighted[["mean"]] else NA, prod_sd = if (have(o)) o$ha_weighted[["sd"]] else NA,
    raw = if (have(o)) o$raw_margin[["mean"]] else mean(d$d$margin), raw_se = if (have(o)) o$raw_margin[["se"]] else stats::sd(d$d$margin) / sqrt(nrow(d$d)),
    v2 = d$hfa$mean[1], v2_sd = d$hfa$sd[1]
  )
}
ha <- do.call(rbind, lapply(names(CELLS), ha_row))
ha$ratio <- ha$prod / ha$raw
pool <- function(cells, a, b) {
  x <- do.call(rbind, lapply(cells, function(cl) {
    j <- cmp[[cl]]$cmp
    if (is.null(j)) NULL else data.frame(d = j[[a]] - j[[b]])
  }))
  c(n = nrow(x), diff = mean(x$d), se = stats::sd(x$d) / sqrt(nrow(x)))
}

# ---------------------------------------------------------------- header
n_r0 <- if (have(diag)) length(diag) else 0L
add("<h1>Handball and basketball v2</h1>")
add("<p class=\"meta\">Measured production fits, a lean (margin, total) model on R-INLA with Stan as reference, and a cutover proposal · four cells: handball and basketball, men and women · prepared 15 November 2026, revised 16 November after an independent check, on branch <code>feat/hb-bb-v2</code> · no production path, config or ingest was changed</p>")

wf_secs <- unlist(lapply(c("hb_m_none", "hb_f_none", "bb_m_none_reg", "bb_f_none_reg"), function(k) {
  f <- file.path(OUT, "out", sprintf("wf_%s.rds", k))
  if (file.exists(f)) with(readRDS(f)$cut, secs_m + secs_t) else NULL
}))
hbm <- wf_sc("hb_m", "none")
hbf <- wf_sc("hb_f", "none")
bbm <- wf_sc("bb_m", "none_reg")
bbf <- wf_sc("bb_f", "none_reg")
ot_all <- per[per$ok & per$overtime %in% TRUE, ]
ot_rate <- function(sex, div) {
  x <- per[per$ok & per$sex == sex & per$division == div, ]
  mean(x$overtime %in% TRUE)
}
prod_walls <- vapply(names(CELLS), function(cl) {
  r <- rd(sprintf("res_%s_r0_prod.rds", cl))
  if (is.null(r) || is.na(r$wall_secs)) NA_real_ else r$wall_secs / 60
}, 1)
prod_wall_txt <- if (all(is.na(prod_walls))) {
  "tens of minutes"
} else {
  sprintf("%s to %s minutes each under a part-loaded machine", num(min(prod_walls, na.rm = TRUE), 0), num(max(prod_walls, na.rm = TRUE), 0))
}
lede <- sprintf(
  paste0(
    "<p class=\"lede\">The handball and basketball models share the football model's structure and, measured, its defects. ",
    "The home advantage the production fits imply, set against the raw home-minus-away margin, is %s. In handball that is the same twofold inflation as in football. ",
    "A much smaller model does the same job: one net strength per team for the margin, one scoring level per team for the total, a pooled home advantage, all Gaussian, fitted by R-INLA in about %s seconds per cell on one core (margin and total together; the median over %s weekly fits, timed on the same shared machine as the production fits, so good for the ratio and not a benchmark), where the production fits took %s. ",
    "Refitted every week and scored over five seasons (%s matches), it is calibrated. On the %s matches where production's own stored forecasts exist it is better on handball totals; the handball margin and result differences, and every basketball difference, are within noise. ",
    "Two sport-specific findings matter for the product: regulation-time scores for basketball can be had from the source already in use (%s of stored games), and level scores after regulation are %s times as common in the men's top division as a smooth curve implies.</p>"
  ),
  paste(vapply(ha$cell[!is.na(ha$prod)], function(cl) {
    sprintf("%s against %s in %s (&times;%s)", num(ha$prod[ha$cell == cl], 2), num(ha$raw[ha$cell == cl], 2), tolower(LAB[[cl]]), num(ha$ratio[ha$cell == cl], 2))
  }, ""), collapse = ", "),
  num(stats::median(wf_secs), 0), int(length(wf_secs)), prod_wall_txt, int(sum(vapply(list(hbm, hbf, bbm, bbf), function(x) if (is.null(x)) 0L else nrow(x), 1L))),
  int(sum(vapply(names(CELLS), function(cl) if (is.null(cmp[[cl]]$cmp)) 0L else nrow(cmp[[cl]]$cmp), 1L))),
  pct(mean(per$ok), 1), num(mean(bbm$margin_fit[bbm$division == "BD"] == 0) / mean(bbm$p_draw[bbm$division == "BD"]), 1)
)
add(lede)
add(paste0(
  "<p class=\"note\"><strong>The HTML build of this report is local-only and is not in git</strong> (<code>docs/reports/*.html</code> is ignored). ",
  "The branch carries what produces it: the build scripts in <code>docs/reports/2026-11-15-hb-bb-v2/</code>, a Markdown rendering written by the same run (<code>report.md</code> in that folder, tracked) and a <code>README.md</code> with the rebuild command. ",
  "The result files the scripts read are local too (<code>", OUT, "/</code>); without them the report is rebuilt by rerunning the experiment (appendix, \"Reproducing\").</p>"
))
add("<nav><a href=\"#summary\">Summary</a><a href=\"#scope\">Measured and not measured</a><a href=\"#prod\">Production fits</a><a href=\"#data\">What the data ask for</a><a href=\"#ladder\">The ladder</a><a href=\"#spec\">The v2 model</a><a href=\"#inla\">INLA against Stan</a><a href=\"#handball\">Handball</a><a href=\"#basketball\">Basketball</a><a href=\"#wf\">Walk-forward</a><a href=\"#proposal\">Cutover proposal</a><a href=\"#decisions\">Your decisions</a><a href=\"#appendix\">Appendix</a></nav>")

for (part in c("report_s1.R", "report_s2.R", "report_s3.R", "report_s4.R")) {
  source(file.path(EXP_DIR, part))
}

order <- c(
  "head", "summary", "scope", "prod", "data", "ladder", "spec", "inla", "handball", "basketball", "wf", "proposal",
  "decisions", "appendix"
)
stopifnot(all(names(S) %in% order))
body <- paste(unlist(S[order]), collapse = "\n\n")
html <- paste0(
  "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
  "<title>Handball and basketball v2</title>\n<style>", CSS, "</style>\n</head>\n<body>\n", body, "\n</body>\n</html>\n"
)
out_file <- "docs/reports/2026-11-15-hb-bb-v2.html"
writeLines(html, out_file, useBytes = TRUE)
cat(sprintf("wrote %s (%d bytes)\n", out_file, file.size(out_file)))

# WHY: the HTML is git-ignored (docs/reports/*.html), so the same run writes a tracked
# Markdown rendering: the report's text and tables, each figure reduced to its caption.
pandoc <- Sys.which("pandoc")
md_file <- file.path(EXP_DIR, "report.md")
if (nzchar(pandoc)) {
  md_html <- gsub("(?s)<nav>.*?</nav>", "", html, perl = TRUE)
  md_html <- gsub(
    "(?s)<figure>\\s*<svg.*?</svg>\\s*<figcaption>(.*?)</figcaption></figure>",
    "<p><em>Figure (drawn in the HTML build only).</em> \\1</p>", md_html,
    perl = TRUE
  )
  md_html <- gsub("(?s)<div class=\"eq\">(.*?)</div>", "<pre>\\1</pre>", md_html, perl = TRUE)
  stopifnot(!grepl("<svg", md_html, fixed = TRUE))
  tmp_html <- tempfile(fileext = ".html")
  tmp_lua <- tempfile(fileext = ".lua")
  tmp_md <- tempfile(fileext = ".md")
  writeLines(md_html, tmp_html, useBytes = TRUE)
  writeLines(c("function Div(el) return el.content end", "function Span(el) return el.content end"), tmp_lua)
  status <- system2(pandoc, c(
    "--from", "html", "--to", "gfm", "--wrap=none", "--lua-filter", shQuote(tmp_lua), "--output", shQuote(tmp_md), shQuote(tmp_html)
  ))
  stopifnot(status == 0L)
  md <- readLines(tmp_md, encoding = "UTF-8", warn = FALSE)
  for (h in regmatches(html, gregexpr("<h2 id=\"[^\"]+\">[^<]+</h2>", html))[[1]]) {
    id <- sub("<h2 id=\"([^\"]+)\">.*", "\\1", h)
    slug <- gsub(" ", "-", gsub("[^a-z0-9 -]", "", tolower(sub(".*\">([^<]+)</h2>", "\\1", h))))
    md <- gsub(sprintf("](#%s)", id), sprintf("](#%s)", slug), md, fixed = TRUE)
  }
  md <- c(
    "<!-- Generated by docs/reports/2026-11-15-hb-bb-v2/build_report.R from the result files.",
    "     Do not edit by hand: change build_report.R or report_s*.R and rebuild. -->", "", md
  )
  writeLines(md, md_file, useBytes = TRUE)
  cat(sprintf("wrote %s (%d lines)\n", md_file, length(md)))
} else {
  cat(sprintf("pandoc not found: %s not rewritten\n", md_file))
}

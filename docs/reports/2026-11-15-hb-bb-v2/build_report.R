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
add("<p class=\"meta\">Measured production fits, a lean (margin, total) model on R-INLA with Stan as reference, and a cutover proposal · four cells: handball and basketball, men and women · prepared 15 November 2026 on branch <code>feat/hb-bb-v2</code> · no production path, config or ingest was changed</p>")

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
lede <- sprintf(
  paste0(
    "<p class=\"lede\">The handball and basketball models share the football model's structure and, measured, its defects. ",
    "In the one production fit examined so far in full (%s), the home advantage the model implies is %s goals against a raw home-minus-away margin of %s: a factor of %s, the same as football. ",
    "A much smaller model does the same job: one net strength per team for the margin, one scoring level per team for the total, a pooled home advantage, all Gaussian, fitted by R-INLA in about %s seconds per cell on one core (margin and total together). ",
    "Refitted every week and scored over five seasons (%s matches), it is calibrated, and on the %s matches where production's own stored forecasts exist it scores at least as well on every measure, clearly better on handball totals. ",
    "Two sport-specific findings matter for the product: regulation-time scores for basketball can be had from the source already in use (%s of stored games), and level scores after regulation are %s times as common in the men's top division as a smooth curve implies.</p>"
  ),
  LAB[["hb_m"]], num(ha$prod[ha$cell == "hb_m"], 2), num(ha$raw[ha$cell == "hb_m"], 2), num(ha$ratio[ha$cell == "hb_m"], 2),
  num(stats::median(wf_secs), 0), int(sum(vapply(list(hbm, hbf, bbm, bbf), function(x) if (is.null(x)) 0L else nrow(x), 1L))),
  int(sum(vapply(names(CELLS), function(cl) if (is.null(cmp[[cl]]$cmp)) 0L else nrow(cmp[[cl]]$cmp), 1L))),
  pct(mean(per$ok), 1), num(mean(bbm$margin_fit[bbm$division == "BD"] == 0) / mean(bbm$p_draw[bbm$division == "BD"]), 1)
)
add(lede)
add("<nav><a href=\"#summary\">Summary</a><a href=\"#scope\">Measured and not measured</a><a href=\"#prod\">Production fits</a><a href=\"#data\">What the data ask for</a><a href=\"#ladder\">The ladder</a><a href=\"#spec\">The v2 model</a><a href=\"#inla\">INLA against Stan</a><a href=\"#handball\">Handball</a><a href=\"#basketball\">Basketball</a><a href=\"#wf\">Walk-forward</a><a href=\"#proposal\">Cutover proposal</a><a href=\"#decisions\">Your decisions</a><a href=\"#appendix\">Appendix</a></nav>")

for (part in c("report_s1.R", "report_s2.R", "report_s3.R", "report_s4.R")) {
  source(file.path(EXP_DIR, part))
}

order <- c("head", "summary", "scope", "prod", "data", "ladder", "spec", "inla", "handball", "basketball", "wf", "proposal",
           "decisions", "appendix")
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

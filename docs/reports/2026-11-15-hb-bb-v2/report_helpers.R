# Small HTML helpers for build_report.R: escaping, number formatting, tables, bar figures.

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Fixed decimals with a real minus sign; NA becomes a dash.
num <- function(x, d = 2, plus = FALSE) {
  out <- formatC(abs(x), format = "f", digits = d, big.mark = ",")
  sign <- ifelse(x < 0, "−", if (plus) "+" else "")
  out <- paste0(sign, out)
  out[is.na(x)] <- "–"
  out
}
int <- function(x) {
  out <- formatC(round(x), format = "d", big.mark = ",")
  out[is.na(x)] <- "–"
  out
}
pct <- function(x, d = 1) ifelse(is.na(x), "–", paste0(formatC(100 * x, format = "f", digits = d), " %"))
pm <- function(x, se, d = 3) paste0(num(x, d, plus = TRUE), " ± ", formatC(se, format = "f", digits = d))

# A table from a data frame of ready-made strings. `right` names the right-aligned
# columns; `hl` is a logical vector of rows to highlight; cells are NOT escaped, so the
# caller can put markup in them.
tbl <- function(df, right = NULL, hl = NULL, head = names(df), note = NULL) {
  right <- names(df) %in% right
  th <- paste0(sprintf("<th%s>%s</th>", ifelse(right, " class=\"n\"", ""), head), collapse = "")
  rows <- vapply(seq_len(nrow(df)), function(i) {
    td <- paste0(sprintf("<td%s>%s</td>", ifelse(right, " class=\"n\"", ""), unlist(df[i, ], use.names = FALSE)), collapse = "")
    sprintf("<tr%s>%s</tr>", if (!is.null(hl) && isTRUE(hl[i])) " class=\"hl\"" else "", td)
  }, character(1))
  paste0(
    "<div class=\"wrap\"><table><thead><tr>", th, "</tr></thead><tbody>", paste(rows, collapse = "\n"),
    "</tbody></table></div>", if (!is.null(note)) sprintf("<p class=\"note\">%s</p>", note) else ""
  )
}

# Grouped horizontal bars: one group per label, one bar per series. Values share a scale.
svg_bars <- function(labels, series, values, unit = "", width = 720, bar_h = 15, label_w = 150, digits = 2) {
  n_g <- length(labels)
  n_s <- length(series)
  gap <- 14
  top <- 26
  h <- top + n_g * (n_s * bar_h + gap) + 6
  vmax <- max(values, na.rm = TRUE) * 1.12
  x0 <- label_w
  w <- width - x0 - 70
  cls <- c("b1", "b2", "b3", "b4")
  out <- sprintf("<svg viewBox=\"0 0 %d %d\" role=\"img\" width=\"%d\">", width, h, width)
  lx <- x0
  for (s in seq_len(n_s)) {
    out <- c(out, sprintf(
      "<rect x=\"%.0f\" y=\"4\" width=\"11\" height=\"11\" class=\"%s\"/><text x=\"%.0f\" y=\"14\">%s</text>",
      lx, cls[s], lx + 15, esc(series[s])
    ))
    lx <- lx + 22 + 7.2 * nchar(series[s])
  }
  for (g in seq_len(n_g)) {
    y <- top + (g - 1) * (n_s * bar_h + gap)
    out <- c(out, sprintf("<text x=\"%d\" y=\"%.0f\" text-anchor=\"end\">%s</text>", x0 - 8, y + n_s * bar_h / 2 + 4, esc(labels[g])))
    for (s in seq_len(n_s)) {
      v <- values[g, s]
      if (is.na(v)) next
      bw <- max(w * v / vmax, 0)
      out <- c(out, sprintf(
        "<rect x=\"%d\" y=\"%.0f\" width=\"%.1f\" height=\"%d\" class=\"%s\"/><text x=\"%.1f\" y=\"%.0f\" class=\"soft\">%s%s</text>",
        x0, y + (s - 1) * bar_h, bw, bar_h - 3, cls[s], x0 + bw + 5, y + (s - 1) * bar_h + bar_h - 5,
        formatC(v, format = "f", digits = digits), unit
      ))
    }
  }
  paste(c(out, "</svg>"), collapse = "\n")
}

# Two overlaid step profiles over integer positions (observed bars, expected line).
svg_profile <- function(k, observed, expected, width = 720, height = 230, y_fmt = function(v) paste0(formatC(100 * v, format = "f", digits = 0), " %"),
                        obs_label = "observed", exp_label = "smooth curve") {
  left <- 44
  bottom <- height - 28
  top <- 24
  w <- width - left - 12
  bw <- w / length(k)
  ymax <- max(c(observed, expected)) * 1.12
  sy <- function(v) bottom - (bottom - top) * v / ymax
  out <- sprintf("<svg viewBox=\"0 0 %d %d\" role=\"img\" width=\"%d\">", width, height, width)
  out <- c(out, sprintf("<line x1=\"%d\" y1=\"%d\" x2=\"%d\" y2=\"%d\" class=\"ax\"/>", left, bottom, width - 12, bottom))
  for (v in pretty(c(0, ymax), 4)) {
    if (v > ymax) next
    out <- c(out, sprintf(
      "<line x1=\"%d\" y1=\"%.1f\" x2=\"%d\" y2=\"%.1f\" class=\"ax\"/><text x=\"%d\" y=\"%.1f\" text-anchor=\"end\" class=\"soft\">%s</text>",
      left, sy(v), width - 12, sy(v), left - 5, sy(v) + 4, y_fmt(v)
    ))
  }
  for (i in seq_along(k)) {
    x <- left + (i - 1) * bw
    out <- c(out, sprintf(
      "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" class=\"%s\"/>", x + 2, sy(observed[i]), bw - 4,
      bottom - sy(observed[i]), if (k[i] == 0) "b2" else "b1"
    ))
    out <- c(out, sprintf("<line x1=\"%.1f\" y1=\"%.1f\" x2=\"%.1f\" y2=\"%.1f\" class=\"ln\"/>", x, sy(expected[i]), x + bw, sy(expected[i])))
    if (k[i] %% 2 == 0) {
      out <- c(out, sprintf(
        "<text x=\"%.1f\" y=\"%d\" text-anchor=\"middle\" class=\"soft\">%s</text>", x + bw / 2, bottom + 15,
        if (k[i] < 0) paste0("−", abs(k[i])) else k[i]
      ))
    }
  }
  out <- c(out, sprintf("<rect x=\"%d\" y=\"4\" width=\"11\" height=\"11\" class=\"b1\"/><text x=\"%d\" y=\"14\">%s</text>", left, left + 15, esc(obs_label)))
  lx <- left + 26 + 7.2 * nchar(obs_label)
  out <- c(out, sprintf("<line x1=\"%.0f\" y1=\"10\" x2=\"%.0f\" y2=\"10\" class=\"ln\"/><text x=\"%.0f\" y=\"14\">%s</text>", lx, lx + 16, lx + 21, esc(exp_label)))
  paste(c(out, "</svg>"), collapse = "\n")
}

CSS <- '
:root{--bg:#fff;--ink:#1b1c1f;--soft:#5a5d66;--rule:#dcdde1;--tint:#f4f5f6;--accent:#0b6b62;--ok:#1d7a3e;--warn:#a85b00;--bad:#b3261e;--b2:#a85b00;--b3:#8a8d96;
--serif:Charter,"Iowan Old Style","Source Serif 4",ui-serif,Georgia,serif;--sans:ui-sans-serif,system-ui,-apple-system,"Segoe UI",sans-serif}
@media (prefers-color-scheme:dark){:root{--bg:#111316;--ink:#eceef1;--soft:#a3a7b0;--rule:#2c3036;--tint:#1a1d21;--accent:#5fc4b8;--ok:#6fcf8f;--warn:#e0a458;--bad:#ef8a80;--b2:#e0a458;--b3:#6c7078}}
*{box-sizing:border-box}
html{background:var(--bg);color:var(--ink)}
body{font:17px/1.58 var(--serif);margin:0 auto;padding:2.5rem 1.1rem 5rem;max-width:54rem}
h1{font-size:2.1rem;line-height:1.15;margin:0 0 .4rem;letter-spacing:-.01em}
h2{font-size:1.45rem;margin:3rem 0 .6rem;padding-top:1rem;border-top:1px solid var(--rule)}
h3{font-size:1.1rem;margin:1.8rem 0 .4rem}
p,li{max-width:44rem}
a{color:var(--accent)}
.meta{font:14px/1.4 var(--sans);color:var(--soft);margin:0 0 1.6rem}
.lede{font-size:1.12rem}
nav{font:14px/1.9 var(--sans);border:1px solid var(--rule);padding:.7rem 1rem;margin:1.5rem 0;display:flex;flex-wrap:wrap;gap:0 1.1rem}
nav a{white-space:nowrap;text-decoration:none}
.wrap{overflow-x:auto;margin:1rem 0}
table{border-collapse:collapse;font:14.5px/1.42 var(--sans);width:100%;min-width:34rem}
th,td{text-align:left;vertical-align:top;padding:.45rem .6rem;border-bottom:1px solid var(--rule)}
th{font-weight:600;border-bottom:2px solid var(--ink)}
td.n,th.n{text-align:right;font-variant-numeric:tabular-nums}td.n{white-space:nowrap}
tr.hl td{background:var(--tint)}
.note{font:14.5px/1.5 var(--sans);color:var(--soft);max-width:44rem}
.box{border-left:3px solid var(--accent);background:var(--tint);padding:.8rem 1rem;margin:1.2rem 0;max-width:46rem}
.box p{margin:.3rem 0}
.c{color:var(--ok);font-weight:600}.u{color:var(--soft)}.x{color:var(--bad);font-weight:600}.w{color:var(--warn);font-weight:600}
.cols{display:grid;grid-template-columns:repeat(2,1fr);gap:1rem;margin:1rem 0}
.cols>div{border:1px solid var(--rule);padding:.8rem .9rem;font:14.5px/1.45 var(--sans)}
.cols h3{margin:.1rem 0 .4rem;font-family:var(--serif)}
.cols ul{margin:.2rem 0;padding-left:1.1rem}.cols li{margin:.25rem 0}
.pick{border-color:var(--accent)!important;border-width:2px!important}
svg{max-width:100%;height:auto;display:block}
svg text{font:12px var(--sans);fill:var(--ink)}
svg .ax{stroke:var(--rule);stroke-width:1}svg .soft{fill:var(--soft)}
svg .b1{fill:var(--accent)}svg .b2{fill:var(--b2)}svg .b3{fill:var(--b3)}svg .b4{fill:var(--soft)}
svg .ln{stroke:var(--ink);stroke-width:2}
figure{margin:1.4rem 0}figcaption{font:14px/1.45 var(--sans);color:var(--soft);max-width:44rem;margin-top:.4rem}
code{font:.88em ui-monospace,Menlo,monospace;background:var(--tint);padding:.05em .3em}
details{margin:1rem 0}summary{font:15px/1.4 var(--sans);cursor:pointer;color:var(--accent)}
.eq{font:15px/1.6 ui-monospace,Menlo,monospace;background:var(--tint);padding:.7rem .9rem;overflow-x:auto;white-space:pre;max-width:46rem}
@media (max-width:760px){.cols{grid-template-columns:1fr}body{font-size:16px}h1{font-size:1.7rem}}
'

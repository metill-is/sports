# Self-contained HTML report of the GISKO backtest: where the model's optimal
# picks are doing well, what each pick was, and how it went vs the field.
# Built from gisko_scorecard_data(); no external assets, no network at view time.

.gisko_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.gisko_css <- paste0(
  "*{box-sizing:border-box}",
  "body{margin:0;background:#f4f2ee;color:#1c1b1a;",
  "font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;",
  "line-height:1.5;-webkit-font-smoothing:antialiased}",
  "main{max-width:880px;margin:0 auto;padding:32px 20px 64px}",
  "h1{font-size:26px;margin:0 0 4px;letter-spacing:-.01em}",
  "h2{font-size:15px;text-transform:uppercase;letter-spacing:.08em;color:#6b6864;",
  "margin:38px 0 14px;font-weight:600}",
  ".sub{color:#6b6864;margin:0 0 22px;font-size:15px}",
  ".tldr{background:#fff;border:1px solid #e6e3de;border-left:3px solid #2f6f6a;",
  "border-radius:8px;padding:16px 18px;font-size:16px}",
  ".tldr b{color:#1c1b1a}",
  "table{border-collapse:collapse;width:100%;font-size:14px}",
  "table.num td,table.num th{font-variant-numeric:tabular-nums}",
  "th{text-align:left;color:#6b6864;font-weight:600;font-size:12px;text-transform:uppercase;",
  "letter-spacing:.05em;padding:8px 10px;border-bottom:1px solid #e6e3de}",
  "td{padding:8px 10px;border-bottom:1px solid #ededea}",
  ".sc td{font-size:16px}.sc .me{font-weight:700}",
  ".gap-pos{color:#2f7d4f;font-weight:700}.gap-neg{color:#b4453a;font-weight:700}.gap-zero{color:#6b6864}",
  ".cards{display:flex;gap:14px;flex-wrap:wrap}",
  ".card{flex:1 1 240px;background:#fff;border:1px solid #e6e3de;border-radius:8px;padding:14px 16px}",
  ".card .k{font-size:12px;text-transform:uppercase;letter-spacing:.06em;color:#6b6864}",
  ".card .v{font-size:24px;font-weight:700;margin:4px 0}",
  ".card.good{border-top:3px solid #2f7d4f}.card.warn{border-top:3px solid #c08a2e}",
  ".card p{margin:6px 0 0;font-size:13px;color:#6b6864}",
  ".bars{background:#fff;border:1px solid #e6e3de;border-radius:8px;padding:14px 16px}",
  ".bar{display:flex;align-items:center;gap:10px;margin:6px 0;font-size:13px}",
  ".bar .lab{width:74px;color:#4a4844}",
  ".bar .track{flex:1;background:#ededea;border-radius:4px;height:18px;overflow:hidden}",
  ".bar .fill{height:100%;border-radius:4px}",
  ".bar .n{width:34px;text-align:right;font-variant-numeric:tabular-nums;color:#4a4844}",
  ".chip{display:inline-block;min-width:20px;text-align:center;padding:1px 7px;border-radius:5px;",
  "font-weight:700;font-size:13px;font-variant-numeric:tabular-nums}",
  ".p0{background:#f4dcd8;color:#9c3a30}.p1{background:#f6ead2;color:#8a6418}",
  ".p2{background:#eef0e3;color:#5f6b34}.p3{background:#dce8f4;color:#2f5b86}",
  ".p5{background:#d6ead9;color:#236b40}",
  ".fill.p0{background:#cd6a5e}.fill.p1{background:#d6a648}.fill.p2{background:#a8b06a}",
  ".fill.p3{background:#5a8fc0}.fill.p5{background:#56a673}",
  ".jk{display:inline-block;margin-left:6px;font-size:11px;color:#2f6f6a;font-weight:700}",
  ".pick{font-variant-numeric:tabular-nums;color:#6b6864}",
  ".rgrp td{background:#efece7;font-weight:600;font-size:12px;text-transform:uppercase;letter-spacing:.05em;color:#4a4844}",
  ".ord{font-size:13px}.ord .hit{color:#236b40;font-weight:600}.ord .miss{color:#9c3a30}",
  "footer{margin-top:44px;padding-top:16px;border-top:1px solid #e6e3de;color:#6b6864;font-size:13px}",
  "code{background:#e9e6e1;padding:1px 5px;border-radius:4px;font-size:12px}",
  "@media(max-width:560px){.bar .lab{width:60px}main{padding:20px 14px}}"
)

.gisko_bar <- function(label, count, maxn, cls) {
  w <- if (maxn > 0) round(100 * count / maxn) else 0
  paste0(
    "<div class='bar'><span class='lab'>", label, "</span>",
    "<span class='track'><span class='fill ", cls, "' style='width:", w, "%'></span></span>",
    "<span class='n'>", count, "</span></div>"
  )
}

.gisko_gap_cell <- function(gap) {
  cls <- if (gap > 0) "gap-pos" else if (gap < 0) "gap-neg" else "gap-zero"
  txt <- if (gap > 0) paste0("+", gap) else as.character(gap)
  paste0("<td class='", cls, "'>", txt, "</td>")
}

#' Render the GISKO backtest as a self-contained HTML report
#'
#' @param data Output of [gisko_scorecard_data()].
#' @param path Output `.html` path.
#' @param leader Named list `match`, `pool`, `qual` (leader's points).
#' @param field_size Number of entrants on the leaderboard.
#' @param generated_at Display timestamp string.
#' @return `path`, invisibly.
#' @export
gisko_render_report <- function(data, path,
                                leader = list(match = 145L, pool = 9L, qual = 10L),
                                field_size = 2624L,
                                generated_at = as.character(Sys.Date())) {
  picks <- data$picks
  leader_total <- leader$match + leader$pool + leader$qual
  total_gap <- data$model_total - leader_total
  exact <- sum(picks$points == 5L)
  mean_pts <- round(mean(picks$points), 2)
  br <- data$by_round
  br$mean <- round(br$base / br$n, 2)
  best_md <- br$round[which.max(br$mean)]
  struct_pts <- data$pool_pts + data$qual_pts
  struct_leader <- leader$pool + leader$qual
  jk_keys <- paste(br$round, br$joker_match)

  # scorecard rows
  sc_rows <- function() {
    rws <- list(
      c("Match predictions", data$match_total, leader$match),
      c("Pool placement", data$pool_pts, leader$pool),
      c("Qualification (R32)", data$qual_pts, leader$qual)
    )
    body <- vapply(rws, function(r) {
      g <- as.integer(r[2]) - as.integer(r[3])
      paste0(
        "<tr><td>", r[1], "</td><td class='me'>", r[2], "</td><td>", r[3], "</td>",
        .gisko_gap_cell(g), "</tr>"
      )
    }, character(1))
    tot <- paste0(
      "<tr style='border-top:2px solid #d8d4cd'><td><b>TOTAL</b></td>",
      "<td class='me'>", data$model_total, "</td><td>", leader_total, "</td>",
      .gisko_gap_cell(total_gap), "</tr>"
    )
    paste0(paste(body, collapse = ""), tot)
  }

  # points distribution
  dist <- table(factor(picks$points, levels = c(0, 1, 2, 3, 5)))
  maxn <- max(dist)
  dist_bars <- paste(vapply(c(0, 1, 2, 3, 5), function(p) {
    lab <- paste0(p, if (p == 5) " (exact)" else if (p == 0) " (miss)" else "")
    .gisko_bar(lab, as.integer(dist[as.character(p)]), maxn, paste0("p", p))
  }, character(1)), collapse = "")

  # matchday table
  md_rows <- paste(vapply(seq_len(nrow(br)), function(i) {
    paste0(
      "<tr><td>", br$round[i], "</td><td>", br$n[i], "</td><td>", br$base[i],
      "</td><td>", br$mean[i], "</td><td class='pick'>", .gisko_esc(br$joker_match[i]),
      " (+", br$joker_bonus[i], ")", if (!br$complete[i]) " *" else "", "</td></tr>"
    )
  }, character(1)), collapse = "")

  # structural pools
  struct_html <- if (is.null(data$structural)) {
    "<p class='sub'>No pools fully resolved yet.</p>"
  } else {
    st <- data$structural
    paste0(
      "<table class='num'><thead><tr><th>Pool</th><th>Placement</th><th>Qualif.</th>",
      "<th>Model ranking vs actual</th></tr></thead><tbody>",
      paste(vapply(seq_len(nrow(st)), function(i) {
        paste0(
          "<tr><td><b>", st$pool[i], "</b></td><td>", st$placement[i], "/4</td><td>",
          st$qualification[i], "/4</td><td class='ord'>", .gisko_esc(st$model_order[i]),
          "<br><span style='color:#9a968f'>actual: ", .gisko_esc(st$actual_order[i]),
          "</span></td></tr>"
        )
      }, character(1)), collapse = ""),
      "</tbody></table>"
    )
  }

  # match log grouped by matchday
  log_rows <- character(0)
  for (rd in unique(picks$round)) {
    sub <- picks[picks$round == rd, , drop = FALSE]
    log_rows <- c(log_rows, paste0(
      "<tr class='rgrp'><td colspan='5'>Matchday ",
      sub("G", "", rd), " &middot; ", nrow(sub), " played</td></tr>"
    ))
    for (i in seq_len(nrow(sub))) {
      is_jk <- paste(sub$round[i], sub$label[i]) %in% jk_keys
      log_rows <- c(log_rows, paste0(
        "<tr><td>", .gisko_esc(sub$label[i]),
        if (is_jk) "<span class='jk'>JOKER x2</span>" else "", "</td>",
        "<td class='pick'>", sub$pick[i], "</td><td class='pick'>", sub$actual[i], "</td>",
        "<td><span class='chip p", sub$points[i], "'>", sub$points[i], "</span></td>",
        "<td class='pick'>", round(sub$exp_points[i], 2), "</td></tr>"
      ))
    }
  }
  log_html <- paste0(
    "<table class='num'><thead><tr><th>Match</th><th>Pick</th><th>Actual</th>",
    "<th>Pts</th><th>E[pts]</th></tr></thead><tbody>",
    paste(log_rows, collapse = ""), "</tbody></table>"
  )

  html <- paste0(
    "<!doctype html><html lang='en'><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>GISKO scorecard</title><style>", .gisko_css, "</style></head><body><main>",
    "<h1>GISKO &mdash; model vs the field</h1>",
    "<p class='sub'>World Cup 2026 &middot; ", generated_at,
    " &middot; leak-free backtest of the model's optimal picks</p>",
    "<div class='tldr'>If you'd run the model from the start you'd have <b>",
    data$model_total, " points</b> &mdash; vs the leader's <b>", leader_total,
    "</b> (of <b>", format(field_size, big.mark = ","), "</b> entrants). ",
    "<b>Level on structure</b> (", struct_pts, " vs ", struct_leader,
    "); the whole gap is on match scorelines (", data$match_total, " vs ", leader$match,
    ") &mdash; the safe, expected-points play hit only ", exact, " exact scores of ",
    data$n_played, ".</div>",
    "<h2>Scorecard</h2><table class='sc num'><thead><tr><th>Category</th><th>Model</th>",
    "<th>Leader</th><th>Gap</th></tr></thead><tbody>", sc_rows(), "</tbody></table>",
    "<h2>Where it stands</h2><div class='cards'>",
    "<div class='card good'><div class='k'>Structure</div><div class='v'>", struct_pts, " = ",
    struct_leader, "</div><p>Pool rankings + R32 qualification: tied with the leader.</p></div>",
    "<div class='card warn'><div class='k'>Match scorelines</div><div class='v'>",
    data$match_total, " &middot; ", leader$match, "</div><p>Behind by ",
    leader$match - data$match_total, " &mdash; the variance gap, not a forecasting one.</p></div>",
    "<div class='card'><div class='k'>Exact scores</div><div class='v'>", exact, " / ",
    data$n_played, "</div><p>Mean ", mean_pts, " pts/match. Ceiling ", data$ceiling,
    " (all-exact). Best matchday: ", best_md, ".</p></div></div>",
    "<h2>Points per match</h2><div class='bars'>", dist_bars, "</div>",
    "<h2>By matchday</h2><table class='num'><thead><tr><th>Round</th><th>Played</th>",
    "<th>Points</th><th>Mean</th><th>Joker (by pre-match EV)</th></tr></thead><tbody>",
    md_rows, "</tbody></table>",
    "<h2>Structure &mdash; completed pools</h2>", struct_html,
    "<h2>Every pick, and how it went</h2>", log_html,
    "<footer>Regenerate: <code>Rscript scripts/wc/gisko_report.R --leader-match ",
    leader$match, " --leader-pool ", leader$pool, " --leader-qual ", leader$qual,
    " --field-size ", field_size, "</code>. Leak-free: each match scored from its frozen ",
    "pre-match prediction; structure from the 11 Jun pre-deadline forecast. ",
    "Topping a ", format(field_size, big.mark = ","),
    "-entry field needs differentiation from the crowd, not safer play &mdash; ",
    "the expected-points optimum lands solid, not first.</footer>",
    "</main></body></html>"
  )

  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(enc2utf8(html), con, useBytes = TRUE)
  invisible(path)
}

# What the corrected reversion groups do to the walk-forward evidence.
#
# The walk-forward the model was chosen on ran before fv2_reversion_groups()
# existed (review findings 1 and 5 of pull request 104). This puts the same
# comparisons from the re-run on the fixed code beside the values from before,
# and measures how far the recommended model's forecasts moved match by match.
#
#   Rscript research/football-v2/compare_rerun.R <old_out> <new_out>
# `old_out` holds the walk-forward as the report first used it, `new_out` the
# re-run. Writes rerun_comparison.rds to `new_out`. Run from the repo root
# after scan_reversion_groups.R.
options(width = 200)
suppressMessages(devtools::load_all(".", quiet = TRUE))
source("research/football-v2/data.R")
fv2_source()
args <- commandArgs(trailingOnly = TRUE)
old_out <- args[[1]]
new_out <- args[[2]]

rec <- "rev80_sq_static_lvl"
no_rev <- "sq_static_lvl"
old <- fv2_read_wf(old_out, c(rec, "base", "brk", "brk_rev80"))$pred
new <- fv2_read_wf(new_out, c(rec, no_rev, "base", "brk_rev80"))$pred
seasons_of <- function(pred) table(unique(pred[c("variant", "sex", "season")])$variant)
stopifnot(all(seasons_of(old) == 10L), all(seasons_of(new) == 10L))

# Variants without a reversion have no reversion groups, so the fix cannot
# reach them: their runs from before stand for the fixed code as well. The
# re-run of the reference measures how true that is in practice.
take <- function(pred, variant, label) {
  d <- pred[pred$variant == variant, , drop = FALSE]
  d$variant <- label
  d
}
paired <- function(a, b, metric) {
  d <- rbind(take(a$pred, a$variant, "a"), take(b$pred, b$variant, "b"))
  r <- fv2_compare(d, "b", NULL, metric)
  r <- r[r$variant == "a", ]
  c(n = r$n, diff = r$diff, se = r$se, se_week = r$se_week)
}
row <- function(comparison, metric, old_a, old_b, new_a, new_b, note = "") {
  o <- paired(old_a, old_b, metric)
  n <- paired(new_a, new_b, metric)
  data.frame(
    comparison = comparison, metric = c(ls_score = "scoreline", ls_1x2 = "1X2")[[metric]],
    n = n[["n"]], old = o[["diff"]], old_se = o[["se"]], new = n[["diff"]], new_se = n[["se"]],
    new_se_week = n[["se_week"]], note = note
  )
}
before <- function(v) list(pred = old, variant = v)
after <- function(v) list(pred = new, variant = v)
headline <- rbind(
  row("Recommended against the reference", "ls_score", before(rec), before("base"), after(rec), after("base")),
  row("Recommended against the reference", "ls_1x2", before(rec), before("base"), after(rec), after("base")),
  row(
    "Reversion 0.8 against none: attack and defence walks against the reference", "ls_score",
    before("brk_rev80"), before("base"), after("brk_rev80"), after("base")
  ),
  row(
    "Reversion 0.8 against none: attack and defence walks, both on the break clock", "ls_score",
    before("brk_rev80"), before("brk"), after("brk_rev80"), before("brk"),
    "the model without reversion was not re-run: it has no reversion groups"
  ),
  row(
    "Reversion 0.8 against none, in the recommended model", "ls_score", before(rec), after(no_rev), after(rec), after(no_rev),
    "the model without reversion was first run on the fixed code; it has no reversion groups"
  ),
  row(
    "Reversion 0.8 against none, in the recommended model", "ls_1x2", before(rec), after(no_rev), after(rec), after(no_rev),
    "the model without reversion was first run on the fixed code; it has no reversion groups"
  )
)

# Against production's stored forecasts and the Lengjan line, 2026.
priced <- fv2_priced_matches()
rounds <- rbind(fv2_round_history(fv2_results("male"), "male"), fv2_round_history(fv2_results("female"), "female"))
against <- function(pred, ext, source) {
  d <- fv2_benchmark(pred, ext, source, rec)
  p <- fv2_paired(d$ls_model - d[[paste0("ls_", source)]])
  c(n = p$n, diff = p$mean, se = p$se, se_week = fv2_paired(d$ls_model - d[[paste0("ls_", source)]], paste(d$sex, d$cutoff))$se)
}
bench_row <- function(comparison, ext, source) {
  o <- against(old, ext, source)
  n <- against(new, ext, source)
  data.frame(
    comparison = comparison, metric = "1X2", n = n[["n"]], old = o[["diff"]], old_se = o[["se"]],
    new = n[["diff"]], new_se = n[["se"]], new_se_week = n[["se_week"]], note = ""
  )
}
headline <- rbind(
  headline,
  bench_row("Recommended against production's last forecast, priced matches 2026", priced, "prod"),
  bench_row("Recommended against production's pre-round forecast, 2026", rounds, "prodround"),
  bench_row("Recommended against the Lengjan line, priced matches 2026", priced, "line")
)
rownames(headline) <- NULL

# Match by match: how far did each forecast move? A forecast is one match;
# its move is the largest change among the three outcome probabilities.
scan <- readRDS(file.path(old_out, "group_change_scan.rds"))$scan
moves <- function(variant) {
  a <- old[old$variant == variant, , drop = FALSE]
  b <- new[new$variant == variant, , drop = FALSE]
  b <- b[match(a$key, b$key), , drop = FALSE]
  stopifnot(!anyNA(b$key), nrow(a) == sum(new$variant == variant))
  s <- scan[match(paste(a$sex, a$cutoff), paste(scan$sex, scan$cutoff)), ]
  stopifnot(!anyNA(s$differing))
  data.frame(
    variant = variant, a[c("sex", "season", "phase", "division", "match_date", "cutoff", "home_team", "away_team")],
    p_home_old = a$p_home, p_draw_old = a$p_draw, p_away_old = a$p_away,
    p_home_new = b$p_home, p_draw_new = b$p_draw, p_away_new = b$p_away,
    move = pmax(abs(b$p_home - a$p_home), abs(b$p_draw - a$p_draw), abs(b$p_away - a$p_away)),
    d_score = b$ls_score - a$ls_score, d_1x2 = b$ls_1x2 - a$ls_1x2,
    # Which fits the corrected groups reach at all, from the scan of the
    # recommended model's specifications: at the newest season break (the
    # early-season case) or only at an older one.
    prior = ifelse(s$differing == 0, "unchanged", ifelse(s$differing_newest > 0, "newest break", "older break only"))
  )
}
mv <- rbind(moves(rec), moves("brk_rev80"), moves("base"))
summarise_moves <- function(d) {
  week <- paste(d$sex, d$cutoff)
  data.frame(
    n = nrow(d), mean_move = mean(d$move), median_move = stats::median(d$move),
    q99_move = stats::quantile(d$move, 0.99, names = FALSE), max_move = max(d$move),
    share_over_001 = mean(d$move > 0.01), share_over_002 = mean(d$move > 0.02), share_over_005 = mean(d$move > 0.05),
    d_score = mean(d$d_score), d_score_se = fv2_paired(d$d_score, week)$se,
    d_1x2 = mean(d$d_1x2), d_1x2_se = fv2_paired(d$d_1x2, week)$se
  )
}
by <- function(d, cols) {
  groups <- split(d, d[cols], drop = TRUE)
  out <- do.call(rbind, lapply(groups, function(g) cbind(g[1L, cols, drop = FALSE], summarise_moves(g))))
  rownames(out) <- NULL
  out
}
move_all <- by(mv, "variant")
move_prior <- by(mv, c("variant", "prior"))
move_sex <- by(mv[mv$variant == rec, ], c("variant", "sex"))
move_phase <- by(mv[mv$variant == rec, ], c("variant", "phase"))
largest <- mv[mv$variant == rec, ]
largest <- largest[order(-largest$move)[1:10], ]

saveRDS(
  list(
    generated = format(Sys.time(), "%Y-%m-%d %H:%M"), old_out = old_out, new_out = new_out,
    headline = headline, move_all = move_all, move_prior = move_prior, move_sex = move_sex, move_phase = move_phase,
    largest = largest, moves = mv[mv$variant == rec, c("sex", "season", "phase", "prior", "move", "d_score", "d_1x2")]
  ),
  file.path(new_out, "rerun_comparison.rds")
)

fmt <- function(x, digits = 4) print(format(x, digits = digits), row.names = FALSE)
cat("== headline comparisons, before and after the fix (positive: the first model scores better)\n")
fmt(headline[setdiff(names(headline), "note")])
cat("\n== how far the forecasts moved (largest change among the three outcome probabilities)\n")
fmt(move_all)
cat("\n== by whether the corrected groups reach the fit's prior\n")
fmt(move_prior)
cat("\n== the recommended model, by sex and by season phase\n")
fmt(move_sex)
fmt(move_phase)
cat("\n== the ten largest moves, recommended model\n")
fmt(largest[c("sex", "match_date", "cutoff", "division", "home_team", "away_team", "p_home_old", "p_home_new", "p_draw_old", "p_draw_new", "p_away_old", "p_away_new", "move", "prior")])
cat("\nsaved", file.path(new_out, "rerun_comparison.rds"), "\n")

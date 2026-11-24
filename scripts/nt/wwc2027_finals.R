#!/usr/bin/env Rscript
# 2027 Women's World Cup finals forecast from the women's v2 fit
# (scripts/nt/wwc2027_fit.R) and the groups of the 11 December 2026 draw.
#
# groups.csv has columns `group`, `team` and optionally `candidates`. At the
# draw three places belong to the inter-confederation play-off of February
# 2027; give such a row a placeholder `team` label and its candidates as
# "Team A:0.5;Team B:0.3;Team C:0.2" (weights are normalised). Every
# combination of candidates is simulated with its share of `sims`, so the
# pooled numbers are the mixture over who qualifies.
#
# Groups are ranked by the 2023 order and Brazil plays its knockout matches at
# home: the defaults of nt_wwc2027_structure(), from the owner's decision of
# 24 November 2026 ("Take all three"). group_rule= overrides the order if the
# 2027 regulations differ.
#
# Usage:
#   Rscript scripts/nt/wwc2027_finals.R <fit.rds> <groups.csv> <out.rds> \
#     [sims=20000] [group_rule=wwc_2023]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
fit <- readRDS(args[[1]])$fit
draw <- utils::read.csv(args[[2]], stringsAsFactors = FALSE, encoding = "UTF-8")
out_path <- args[[3]]
n_total <- as.integer(opt("sims", "20000"))
group_rule <- opt("group_rule", NULL)

if (!"candidates" %in% names(draw)) draw$candidates <- ""
draw$candidates[is.na(draw$candidates)] <- ""
holders <- which(nzchar(draw$candidates))
cand <- lapply(holders, function(i) {
  parts <- strsplit(strsplit(draw$candidates[i], ";", fixed = TRUE)[[1]], ":", fixed = TRUE)
  w <- as.numeric(vapply(parts, `[`, "", 2))
  stats::setNames(w / sum(w), trimws(vapply(parts, `[`, "", 1)))
})
combos <- if (length(holders) == 0L) {
  data.frame(row.names = 1L)
} else {
  expand.grid(lapply(cand, names), stringsAsFactors = FALSE)
}
weight <- if (length(holders) == 0L) {
  1
} else {
  Reduce(`*`, lapply(seq_along(holders), function(k) cand[[k]][combos[[k]]]))
}
n_sims <- as.integer(round(n_total * weight))
keep <- n_sims > 0L
cat(sprintf(
  "%d placeholder(s), %d combination(s), %d with simulations\n",
  length(holders), max(1L, nrow(combos)), sum(keep)
))

cols <- c(
  "p_first", "p_second", "p_third", "p_fourth", "p_advance", "p_qf", "p_sf",
  "p_final", "p_champion", "p_bronze"
)
pooled <- NULL
for (c in which(keep)) {
  d <- draw
  for (k in seq_along(holders)) d$team[holders[k]] <- combos[c, k]
  groups <- split(d$team, d$group)[LETTERS[1:8]]
  s <- nt_wwc2027_structure(groups)
  sim <- nt_simulate_world_cup(fit, s, n_sims = n_sims[c], group_rule = group_rule, seed = 1000L + c)
  t <- sim$teams
  t$placeholder <- ifelse(t$team %in% d$team[holders], draw$team[holders][match(t$team, d$team[holders])], NA_character_)
  t$n <- n_sims[c]
  pooled <- rbind(pooled, t[c("team", "group", "placeholder", "n", cols)])
}
n_used <- sum(n_sims[keep])
res <- do.call(rbind, lapply(split(pooled, pooled$team), function(x) {
  data.frame(
    team = x$team[1], group = x$group[1], placeholder = x$placeholder[1],
    p_in_finals = sum(x$n) / n_used,
    t(colSums(as.matrix(x[cols]) * x$n) / n_used)
  )
}))
res <- tibble::as_tibble(res[order(-res$p_champion), ])
saveRDS(list(
  teams = res, group_rule = group_rule, n_sims = n_used, combos = combos,
  weight = weight, fit_cutoff = readRDS(args[[1]])$cutoff
), out_path)
print(as.data.frame(res), digits = 3)

#!/usr/bin/env Rscript
# 2027 Women's World Cup finals forecast from the women's v2 fit
# (scripts/nt/wwc2027_fit.R) and the groups of the 11 December 2026 draw,
# published to data/publish/world_cup/kvenna/ (groups.json,
# tournament_placements.json, meta.json; playoff.json is kept). Sports-side
# JSON only (owner, 24 November 2026: "Sports-side JSON only").
#
# groups.csv has columns `group`, `team` and optionally `candidates`. At the
# draw three places belong to the inter-confederation play-off of February
# 2027; give such a row a placeholder `team` label and its candidates as
# "Team A:0.5;Team B:0.3;Team C:0.2" (weights are normalised). Every
# combination of candidates is simulated with its share of `sims`, so the
# pooled numbers are the mixture over who qualifies (nt_wwc2027_finals()).
#
# Groups are ranked by the 2023 order and Brazil plays its knockout matches at
# home: the defaults of nt_wwc2027_structure(), from the owner's decision of
# 24 November 2026 ("Take all three"). group_rule= overrides the order if the
# 2027 regulations differ. Every team needs an Icelandic name in
# data/wc/structure/country_names_is.csv; the publisher stops on a missing one.
#
# Usage:
#   Rscript scripts/nt/wwc2027_finals.R <fit.rds> <groups.csv> \
#     [sims=20000] [group_rule=wwc_2023] [root=data] [out=<results.rds>]

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 160)

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[[1]])
}
saved <- readRDS(args[[1]])
draw <- utils::read.csv(args[[2]], stringsAsFactors = FALSE, encoding = "UTF-8")

finals <- nt_wwc2027_finals(saved$fit, draw,
  n_sims = as.integer(opt("sims", "20000")),
  group_rule = opt("group_rule", "wwc_2023")
)
out <- publish_wwc2027_finals(finals,
  fit_date = saved$cutoff, data_through = saved$last_match,
  n_draws = length(saved$fit$draws$mu), root = opt("root", here::here("data"))
)
rds <- opt("out", NA)
if (!is.na(rds)) saveRDS(c(finals, list(fit_cutoff = saved$cutoff)), rds)
cat(sprintf(
  "Wrote %s: %d runs over %d combination(s), group rule %s\n",
  out, finals$n_sims, max(1L, nrow(finals$combos)), finals$group_rule
))
print(as.data.frame(finals$teams), digits = 3)

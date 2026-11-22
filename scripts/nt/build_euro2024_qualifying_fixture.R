#!/usr/bin/env Rscript
# Build the Euro 2024 qualifying fixture that validates the UEFA group-ranking
# code (R/nt-qualifying.R): every group match, the official final tables, the
# overall European Qualifiers ranking, and the Nations League ranking used as
# the last tie-breaker.
#
# Scores come from the martj42 international results file (CC0-1.0, the
# model's own data source) and are checked one by one against the scores in
# English Wikipedia's template of the official tables; any disagreement
# aborts. Tables, tie-break notes and the overall ranking come from that
# template, the Nations League ranking from the seeding pots of the main
# qualifying article (Germany, the host, and Russia, banned, are in those
# pots but not in qualifying).
#
# Usage:
#   Rscript scripts/nt/build_euro2024_qualifying_fixture.R \
#     <template.wiki> <main.wiki> <martj42 results.csv> <out dir>
# where template.wiki is the raw wikitext of
# Template:UEFA_Euro_2024_qualifying_group_tables (action=raw) and main.wiki
# that of the article UEFA_Euro_2024_qualifying.

suppressMessages(devtools::load_all(quiet = TRUE))
options(width = 150)

args <- commandArgs(trailingOnly = TRUE)
template <- paste(readLines(args[[1]], encoding = "UTF-8", warn = FALSE), collapse = "\n")
main <- paste(readLines(args[[2]], encoding = "UTF-8", warn = FALSE), collapse = "\n")
results <- nt_read_results(args[[3]])
out_dir <- args[[4]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

codes <- c(
  ALB = "Albania", AND = "Andorra", ARM = "Armenia", AUT = "Austria",
  AZE = "Azerbaijan", BLR = "Belarus", BEL = "Belgium",
  BIH = "Bosnia and Herzegovina", BUL = "Bulgaria", CRO = "Croatia",
  CYP = "Cyprus", CZE = "Czech Republic", DEN = "Denmark", ENG = "England",
  EST = "Estonia", FRO = "Faroe Islands", FIN = "Finland", FRA = "France",
  GEO = "Georgia", GIB = "Gibraltar", GRE = "Greece", HUN = "Hungary",
  ISL = "Iceland", ISR = "Israel", ITA = "Italy", KAZ = "Kazakhstan",
  KOS = "Kosovo", LVA = "Latvia", LIE = "Liechtenstein", LTU = "Lithuania",
  LUX = "Luxembourg", MLT = "Malta", MDA = "Moldova", MNE = "Montenegro",
  NED = "Netherlands", MKD = "North Macedonia", NIR = "Northern Ireland",
  NOR = "Norway", POL = "Poland", POR = "Portugal", IRL = "Republic of Ireland",
  ROU = "Romania", SMR = "San Marino", SCO = "Scotland", SRB = "Serbia",
  SVK = "Slovakia", SVN = "Slovenia", ESP = "Spain", SWE = "Sweden",
  SUI = "Switzerland", TUR = "Turkey", UKR = "Ukraine", WAL = "Wales"
)
name_of <- function(code) {
  out <- unname(codes[code])
  if (anyNA(out)) stop("unknown team code: ", paste(code[is.na(out)], collapse = ", "))
  out
}

section <- function(key) {
  start <- regexpr(paste0("\n\\|", key, "=\\{\\{#invoke:Sports table"), template)
  if (start < 0) stop("section not found: ", key)
  rest <- substring(template, start + 1L)
  nxt <- regexpr("\n\\|(Group [A-J]|Overall)=\\{\\{#invoke", rest)
  if (nxt > 0) substring(rest, 1L, nxt) else rest
}
field <- function(sec, name) {
  m <- regmatches(sec, regexpr(paste0("\\|", name, "=[^|\n]*"), sec))
  if (length(m) == 0L) NA_character_ else trimws(sub("^\\|[^=]*=", "", m))
}
team_order <- function(sec) {
  m <- regmatches(sec, regexpr("\\|team_order=[^|]*", sec))
  m <- gsub("<!--[^>]*-->", "", sub("^\\|team_order=", "", m))
  codes_found <- trimws(strsplit(m, ",")[[1]])
  codes_found[nzchar(codes_found)]
}
team_stats <- function(sec, code) {
  num <- function(f) as.integer(field(sec, paste0(f, "_", code)))
  data.frame(
    won = num("win"), drawn = num("draw"), lost = num("loss"),
    gf = num("gf"), ga = num("ga")
  )
}

groups <- LETTERS[1:10]
tables <- list()
wiki_matches <- list()
for (g in groups) {
  sec <- section(paste("Group", g))
  ord <- team_order(sec)
  st <- do.call(rbind, lapply(ord, function(cd) team_stats(sec, cd)))
  notes <- vapply(ord, function(cd) {
    note <- field(sec, paste0("hth_", cd))
    if (is.na(note) || nchar(note) <= 3L) "" else note
  }, "")
  tables[[g]] <- data.frame(
    group = g, position = seq_along(ord), team = name_of(ord), st,
    points = 3L * st$won + st$drawn, tiebreak_note = unname(notes)
  )
  # WHY: one score in the template uses a hyphen instead of an en dash.
  mm <- gregexpr("\\|match_([A-Z]{3})_([A-Z]{3})=\\[\\[[^|]*\\|([0-9]+)[-\u2013]([0-9]+)\\]\\]", sec)
  hits <- regmatches(sec, mm)[[1]]
  parts <- regmatches(hits, regexec(
    "\\|match_([A-Z]{3})_([A-Z]{3})=\\[\\[[^|]*\\|([0-9]+)[-\u2013]([0-9]+)\\]\\]", hits
  ))
  wiki_matches[[g]] <- data.frame(
    group = g,
    home_team = name_of(vapply(parts, `[`, "", 2)),
    away_team = name_of(vapply(parts, `[`, "", 3)),
    wiki_home = as.integer(vapply(parts, `[`, "", 4)),
    wiki_away = as.integer(vapply(parts, `[`, "", 5))
  )
  n <- length(ord)
  if (nrow(wiki_matches[[g]]) != n * (n - 1L)) {
    stop("Group ", g, ": ", nrow(wiki_matches[[g]]), " scores, expected ", n * (n - 1L))
  }
}
tables <- do.call(rbind, tables)
wiki_matches <- do.call(rbind, wiki_matches)

qual <- results[
  results$tournament == "UEFA Euro qualification" &
    results$match_date >= as.Date("2023-03-01") &
    results$match_date <= as.Date("2023-12-31"), ,
  drop = FALSE
]
m <- merge(
  wiki_matches, qual[, c("match_date", "home_team", "away_team", "home_score", "away_score", "neutral")],
  by = c("home_team", "away_team"), all.x = TRUE
)
if (anyNA(m$home_score)) {
  print(m[is.na(m$home_score), ])
  stop("matches in Wikipedia's tables missing from the martj42 file")
}
bad <- m$home_score != m$wiki_home | m$away_score != m$wiki_away
if (any(bad)) {
  print(m[bad, ])
  stop("martj42 and Wikipedia disagree on ", sum(bad), " scores")
}
if (nrow(m) != nrow(qual)) stop("martj42 has ", nrow(qual), " qualifying matches, Wikipedia ", nrow(m))
matches <- m[order(m$match_date, m$group, m$home_team), c(
  "match_date", "group", "home_team", "away_team", "home_score", "away_score", "neutral"
)]

sec <- section("Overall")
ord <- team_order(sec)
st <- do.call(rbind, lapply(ord, function(cd) team_stats(sec, cd)))
notes <- vapply(ord, function(cd) {
  note <- field(sec, paste0("hth_", cd))
  if (is.na(note) || nchar(note) <= 3L) "" else note
}, "")
overall <- data.frame(
  rank = seq_along(ord), team = name_of(ord),
  group = tables$group[match(name_of(ord), tables$team)],
  group_position = tables$position[match(name_of(ord), tables$team)],
  st, points = 3L * st$won + st$drawn, tiebreak_note = unname(notes)
)

pot_rows <- regmatches(main, gregexpr(
  "\\{\\{fb\\|([A-Z]{3})\\}\\}[^\n]*\n\\| style=\"text-align:center\" \\| ([0-9]+)", main
))[[1]]
pot <- regmatches(pot_rows, regexec("\\{\\{fb\\|([A-Z]{3})\\}\\}.*\\| ([0-9]+)$", pot_rows))
pot_code <- vapply(pot, `[`, "", 2)
in_qualifying <- pot_code %in% names(codes)
nl <- data.frame(
  team = name_of(pot_code[in_qualifying]),
  rank = as.integer(vapply(pot, `[`, "", 3))[in_qualifying]
)
nl <- nl[!duplicated(nl$team), ]
nl <- nl[nl$team %in% tables$team, ]
nl <- nl[order(nl$rank), ]
if (nrow(nl) != 53L) stop("Nations League ranks found for ", nrow(nl), " of 53 teams")

# Disciplinary points are not in any open data source; the only values the
# validation needs are the two the overall ranking quotes in its note.
disciplinary <- data.frame(team = c("Bosnia and Herzegovina", "Latvia"), points = c(20L, 29L))

write.csv(matches, file.path(out_dir, "matches.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(tables, file.path(out_dir, "tables.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(overall, file.path(out_dir, "overall.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(nl, file.path(out_dir, "nations_league_rank.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(disciplinary, file.path(out_dir, "disciplinary.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf(
  "wrote %d matches (all %d scores agree with Wikipedia), %d table rows, %d overall ranks, %d Nations League ranks to %s\n",
  nrow(matches), nrow(m), nrow(tables), nrow(overall), nrow(nl), out_dir
))
